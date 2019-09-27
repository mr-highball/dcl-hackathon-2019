unit controller.registration;

{$mode delphi}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fpWeb,
  FPImage,
  BGRABitmap,
  BGRABitmapTypes,
  controller.base,
  controller.registration.dto,
  controller.image.dto;

type

  TRegistrationValidateCallback = function(Const AURL : String) : Boolean;

var
  //to add validations rules for url's, add them to this array
  VALIDATE_CALLBACKS : TArray<TRegistrationValidateCallback>;
  MAX_IMAGE_WIDTH : Integer;
  MAX_IMAGE_HEIGHT : Integer;

type

  { TRegistrationController }
  (*
    controller that handles the starting step of processing
    image requests. endpoints will allow image client to validate
    whether or not a URL is acceptable, begin process, as well as cancelling a
    a request
  *)
  TRegistrationController = class(TBaseController)
  private
    //action for handling validating urls
    procedure ValidateAction(Sender : TObject; ARequest : TRequest;
      AResponse : TResponse; var Handled : Boolean);

    //action for handling registering an image to be processed
    procedure RegisterAction(Sender : TObject; ARequest : TRequest;
      AResponse : TResponse; var Handled : Boolean);

    //action to attempt to cancel an in progress image request
    procedure CancelAction(Sender : TObject; ARequest : TRequest;
      AResponse : TResponse; var Handled : Boolean);

    procedure InitRegistrationTable;
  strict protected
    procedure DoInitialize(); override;
    function DoInitializeActions(): TActions; override;

    //async processing for a url
    class procedure StartProcessing(Constref ARequest : TRegisterURLRequest;
      Constref AResponse : TRegisterURLResponse); static;
  public
    (*
      uses validation rules to determine whether or not a particular
      url is valid for processing. this could be as simple as domain
      checking, or could do text analysis on the resource, etc...

      note:
        by default all images that *can* be processed will be accepted.
        it's up to individual land owners who use this service to
        implement the custom validation rules
    *)
    function ValidateURL(Constref AValidation : TValidateURLRequest) : Boolean;

    //begins the processing step as long as the URL is valid
    function RegisterImage(Constref ARequest : TRegisterURLRequest;
      out Response : TRegisterURLResponse) : Boolean;

    //will cancel the request as long as the process is in-progress
    function CancelRequest() : Boolean;

    //provided an external image id, will return the internal primary key
    function LookupImageID(Const AImageToken : String; Out ID : Integer) : Boolean;
  end;


function DitherImageStream(Const AImage : TStream; Const AURL : String;
  Const ABitmap : TBGRABitmap; Out Error : String) : Boolean;

function ConstructDCLImageResponse(Const AImage : TBGRABitmap;
  Out Response : TImageResponse; Out Error : String) : Boolean;

var
  RegistrationController: TRegistrationController;

implementation
uses
  Interfaces,
  Graphics,
  bgradithering,
  BGRAPalette,
  BGRAColorQuantization,
  fphttpclient,
  controller.status,
  controller.dto,
  ezthreads,
  ezthreads.collection,
  controller.image,
  opensslsockets,
  fgl,
  fpjson,
  jsonparser;
var
  Collection : IEZCollection;
  Palette : TBGRAApproxPalette;

{$R *.lfm}

procedure InitializePalette;
begin
  //for now we initialize this in here rather than exposing the
  //palette to a config, since we don't want to overload dcl
  Palette := TBGRAApproxPalette.Create(
    [
      TBGRAPixel.New(255, 0, 0), //red
      TBGRAPixel.New(0, 255, 0), //green
      TBGRAPixel.New(0, 0, 255), //blue
      TBGRAPixel.New(0, 0, 0), //black
      TBGRAPixel.New(128, 128, 128), //grey
      TBGRAPixel.New(255, 255, 255), //white
      TBGRAPixel.New(255, 255, 0), //yellow
      TBGRAPixel.New(255, 128, 0), //orange
      TBGRAPixel.New(128, 0, 255) //purple
    ]
  );
end;

function DitherImageStream(Const AImage : TStream; Const AURL : String;
  Const ABitmap : TBGRABitmap; Out Error : String) : Boolean;
var
  LTask: TDitheringTask;
  LReader: TFPCustomImageReader;
  LBitmap , LResampled, LDithered: TBGRACustomBitmap;
  LExt: String;
  LFormat: TBGRAImageFormat;
begin
  Result := False;

  try
    AImage.Position := 0;
    LExt := AURL.Substring(AURL.LastIndexOf('.'));

    if LExt.IsEmpty then
    begin
      Error := 'DitherImageStream::invalid/empty extension';
      Exit;
    end;

    //initialize reader objects
    LFormat := SuggestImageFormat(LExt);
    LReader := DefaultBGRAImageReader[LFormat].Create;
    LBitmap := TBGRABitmap.Create;

    try
      //try to load
      LBitmap.LoadFromStream(AImage, LReader, []);

      //resize the image to the configured dimensions
      LResampled := LBitmap.Resample(MAX_IMAGE_WIDTH, MAX_IMAGE_HEIGHT);

      //initialize the dither task
      LTask := CreateDitheringTask(
        daNearestNeighbor, //algorithm for dithering
        LResampled, //source image
        Palette, //use the reduced color palette
        True, //ignore alpha for now
        LResampled.GetImageBounds()
      );
      LTask.DrawMode := dmSet;

      try
        //begin dithering
        LDithered := LTask.Execute;

        //update result with the dithered result
        ABitmap.Assign(LDithered);

        //cleanup
        LDithered.Free;

        //success
        Result := True;
      finally
        LResampled.Free;
        LTask.Free;
      end;
    finally
      LBitmap.Free;
      LReader.Free;
    end;
  except on E : Exception do
    Error := E.Message;
  end;
end;

function ConstructDCLImageResponse(Const AImage : TBGRABitmap;
  Out Response : TImageResponse; Out Error : String) : Boolean;
type
  TRectangle = packed record
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight: TPoint;
  end;

  TColorRect = packed record
    Rect : TRectangle;
    Red : Byte;
    Green : Byte;
    Blue : Byte;
  end;

  TColorRectArray = TArray<TColorRect>;

  TLookupArray = packed array of array of Boolean;

  //key is int value of color
  TColorMap = TFPGMap<Integer, TColorRectArray>;

var
  W, H, BR, LCurrentRow, TL, TR,
  BL, J, K, I, Top, Bottom: Integer;
  LPoints : TRectangle;
  LRect : TColorRect;
  LRects : TColorRectArray;
  LLookup : TLookupArray;
  LColor: TBGRAPixel;
  LCurPixel: TBGRAPixel;
  LAreaCalc, LFound, LCloseRect: Boolean;
  LCommands : TDrawCommands;
  LCommand : TDrawCommand;
begin
  (*
    essentially the below algorithm will attempt to "vectorize" an input
    raster image into a series of draw rect Response (color/rect). this
    won't be perfect, and isn't a trivial problem to solve, but due to the fact that
    dcl doesn't have native canvas commands or support to load a damn web image,
    it's the best stab I have at the problem right now...
    the upside to this, is that it should be renderable to any client that can
    draw to a canvas and who needs to dithered scaleable image
    (which may be useful for other things...)
  *)
  Result := False;
  SetLength(LRects, 0);

  try
    W := AImage.Width;
    H := AImage.Height;

    if (W <= 0) or (H <= 0) then
      raise Exception.Create('ConstructDCLImageJSON::invalid dimensions');

    //for every color in the palette, we need to construct rects to draw
    SetLength(LLookup, W, H); //define to the size of our image

    for I := 0 to Pred(Palette.Count) do
    begin
      //get the color we're searching for from the palette
      LColor := Palette.Color[I];
      LFound := False;

      //iterate the pixels of the image to populate the lookup array
      for J := 0 to Pred(W) do
        for K := 0 to Pred(H) do
        begin
          LCurPixel := AImage.GetPixel(J, K);

          if (LCurPixel.red = LColor.red) and (LCurPixel.green = LColor.green)
            and (LCurPixel.blue = LColor.blue)
          then
          begin
            LLookup[J][K] := True;
            LFound := True;
          end
          else
            LLookup[J][K] := False;
        end;

      //didn't find this color, move to the next
      if not LFound then
        Continue;

      //current working column in the array
      LCurrentRow := 0;

      //continue as long as we have un-filled areas
      while True do
      begin
        //covered this entire color
        if LCurrentRow >= H then
          Break;

        //initialize positional markers for the rect
        Top := LCurrentRow; //row marker for top corners
        TL := -1; //top-left corner
        TR := -1; //top-right corner
        BL := -1; //bottom-left corner
        BR := -1; //bottom-right corner
        Bottom := LCurrentRow; //row marker for bottom corners
        LCloseRect := False;

        while True do
        begin
          //first we find the top corners
          if TL < 0 then
          begin
            for J := 0 to Pred(W) do
            begin
              if (TL < 0) and LLookup[J][Top] then
                TL := J
              //break in color
              else if not LLookup[J][Top] then
              begin
                //continue scanning for a top left if we still have pixels
                if (TL < 0) and (J < Pred(W)) then
                  Continue
                else
                begin
                  //if right corner hasn't been set yet, then it equals the top-left
                  if TR < 0 then
                    TR := TL;

                  //this row is no longer valid
                  Break;
                end;
              end
              else
                TR := J;
            end;

            if TR <= TL then
              TR := TL;
          end
          //otherwise we have the top, need to find the bottom
          else
          begin
            //move the bottom further than top
            Bottom := Succ(Bottom);

            //exceeded bounds
            if Bottom >= H then
            begin
              if BL < 0 then
                BL := TL;

              if BR < 0 then
                BR := TR;

              Bottom := Pred(Bottom);
              LCloseRect := True;
            end
            else
            begin
              //flag for area calculation
              LAreaCalc := False;

              for J := TL to TR do
              begin
                (*
                  since we already know the top left, make sure we can
                  anchor the bottom left at the current row aligned to top
                  otherwise we will need to perform area calculations to find
                  if adjusting the top will be beneficial (largest rect possible)
                *)
                if (BL < 0) and LLookup[TL][Bottom] then
                  BL := J
                //found color, but bottom-left is different than top-left
                else if (BL < 0) and LLookup[J][Bottom] then
                begin
                  BL := J;
                  LAreaCalc := True;
                end
                //break in color
                else if not LLookup[J][Bottom] then
                begin
                  //continue scanning for a bottom left if we still have pixels
                  if (BL < 0) and (J < Pred(TR)) then
                    Continue
                  //invalid row
                  else if BL < 0 then
                  begin
                    Dec(Bottom);
                    LCloseRect := True;
                    Break;
                  end
                  //valid row, but possible area recalc
                  else
                  begin
                    //if right corner hasn't been set yet, then it equals the bottom-left
                    if BR < 0 then
                      BR := BL;

                    //we've broken earlier than the previous rows
                    if BR < TR then
                      LAreaCalc := True
                    //finished this rect
                    else
                    begin
                      //discard this bottom
                      Dec(Bottom);
                      LCloseRect := True;
                      Break;
                    end;
                  end;
                end
                else
                  BR := J;

                if LAreaCalc then
                begin
                  //check if the previous area is greater than the new
                  if (Succ(TR) - TL) * (Bottom - Top) //previous
                    >
                    (Succ(BR) - BL) * (Succ(Bottom) - Top) //new
                  then
                  begin
                    Dec(Bottom);
                    BL := TL;
                    BR := TR;
                    LCloseRect := True;
                    Break;
                  end
                  //reduced corners but having new row was greater area
                  else
                  begin
                    //shrink top to fit bottom
                    TL := BL;
                    TR := BR;
                  end;
                end;
              end;
            end;

          end;

          //found a rect or failed to find one
          if LCloseRect or (TL < 0) then
            Break;
        end;

        //when building the rect, we exhausted this rows pixels
        if TL < 0 then
        begin
          Inc(LCurrentRow);
          Continue;
        end
        //add the rect
        else
        begin
          //update the color
          LRect.Red := LColor.red;
          LRect.Green := LColor.green;
          LRect.Blue := LColor.blue;

          //update the rectangle portion
          with LPoints do
          begin
            TopLeft.X := TL; TopLeft.Y := Top;
            TopRight.X := TR; TopRight.Y := Top;
            BottomLeft.X := BL; BottomLeft.Y := Bottom;
            BottomRight.X := BR; BottomRight.Y := Bottom;
          end;
          LRect.Rect := LPoints;

          //increase size and add to result
          SetLength(LRects, Succ(Length(LRects)));
          LRects[High(LRects)] := LRect;

          //clear the lookup for this rect
          for J := TL to TR do
            for K := Top to Bottom do
              LLookup[J][K] := False;
        end;

        //right corner went to bounds
        if TR >= Pred(W) then
          Inc(LCurrentRow);
      end;
    end;

    SetLength(LCommands, Length(LRects));
    for J := 0 to High(LCommands) do
    begin
      //translate rect to command
      LRect := LRects[J];
      LCommand.Red := LRect.Red;
      LCommand.Green := LRect.Green;
      LCommand.Blue := LRect.Blue;
      LCommand.Alpha := 255; //no alpha support right now

      //translate x's
      LCommand.TopLeftX := LRect.Rect.TopLeft.X;
      LCommand.TopRightX := LRect.Rect.TopRight.X;
      LCommand.BottomLeftX := LRect.Rect.BottomLeft.X;
      LCommand.BottomRightX := LRect.Rect.BottomRight.X;

      //translate y's
      LCommand.TopLeftY := LRect.Rect.TopLeft.Y;
      LCommand.TopRightY := LRect.Rect.TopRight.Y;
      LCommand.BottomLeftY := LRect.Rect.BottomLeft.Y;
      LCommand.BottomRightY := LRect.Rect.BottomRight.Y;

      //update the command
      LCommands[J] := LCommand;
    end;

    //now set the commands in the response
    Response.Commands := LCommands;

    //success
    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

{ TRegistrationController }

procedure TRegistrationController.ValidateAction(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
  LValidate : TValidateURLRequest;
  LResult : TBoolResponse;
begin
  try
    LResult.Success := False;

    AResponse.ContentType := 'application/json';
    AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
    Handled := True;
    LogRequester('ValidateAction::', ARequest);

    //try and parse the token
    LValidate.FromJSON(ARequest.Content);

    //try to validate
    if not ValidateURL(LValidate) then
      AResponse.Content := LResult.ToJSON
    else
    begin
      LResult.Success := True;
      AResponse.Content := LResult.ToJSON;
    end;
  except on E : Exception do
  begin
    LogError(E.Message);
    AResponse.Content := GetErrorJSON('unable to validate - server error');
  end
  end;
end;

procedure TRegistrationController.RegisterAction(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
  LRequest : TRegisterURLRequest;
  LResult : TRegisterURLResponse;
  LGuid, LError: String;
begin
  try
    AResponse.ContentType := 'application/json';
    AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
    Handled := True;
    LogRequester('RegisterAction::', ARequest);

    //try and parse the request
    LRequest.FromJSON(ARequest.Content);

    //perform registrations
    if not RegisterImage(LRequest, LResult) then
    begin
      AResponse.Content := GetErrorJSON('unable to register image');
      Exit;
    end;

    //success
    AResponse.Content := LResult.ToJSON;
  except on E : Exception do
  begin
    LogError(E.Message);
    AResponse.Content := GetErrorJSON('unable to register image - server error');
  end
  end;
end;

procedure TRegistrationController.CancelAction(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
begin

end;

procedure TRegistrationController.InitRegistrationTable;
var
  LError: String;
begin
  //initialize our table if it doesn't exist
  if not ExecuteSQL(
    'CREATE TABLE IF NOT EXISTS registration(' +
    ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,' +
    ' "token" varchar(128) NOT NULL,' +
    ' "user_key" varchar(50) NOT NULL,' +
    ' "parcel_identity" varchar(25) NOT NULL,' +
    ' "url" varchar(2000) NOT NULL,' +
    ' "create_date" datetime NOT NULL DEFAULT (datetime(''now'')));',
    LError
  ) then
    LogError(LError);
end;

procedure TRegistrationController.DoInitialize();
begin
  inherited DoInitialize();
  InitRegistrationTable;
end;

function TRegistrationController.DoInitializeActions(): TActions;
var
  I : Integer;
  LAction : TAction;
begin
  Result := inherited DoInitializeActions();

  //set the starting index to the last index of result
  I := High(Result);

  //if base added some default actions, then increment the index
  if I >= 0 then
    Inc(I);

  //initialize the registration route actions
  SetLength(Result, Length(Result) + 3);

  LAction.Name := 'validate';
  LAction.Action := ValidateAction;
  Result[I] := LAction;

  LAction.Name := 'register';
  LAction.Action := RegisterAction;
  Result[I + 1] := LAction;

  LAction.Name := 'cancel';
  LAction.Action := CancelAction;
  Result[I + 2] := LAction;
end;

function TRegistrationController.ValidateURL(Constref
  AValidation: TValidateURLRequest): Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := 0 to High(VALIDATE_CALLBACKS) do
    if not VALIDATE_CALLBACKS[I](AValidation.URL) then
      Exit(False);
end;

function TRegistrationController.RegisterImage(Constref
  ARequest: TRegisterURLRequest; out Response: TRegisterURLResponse): Boolean;
var
  LGuid,
  LError: String;
begin
  Result := False;

  try
    //make a call to make sure the requested token matches the details
    //...

    //try to validate the url
    if not ValidateURL(ARequest.Validation) then
    begin
      LogError('RegisterImage::validation failed for ' + ARequest.ToJSON);
      Exit;
    end;

    //save the job to db with the generated token
    LGuid := TGuid.NewGuid.ToString();

    if not ExecuteSQL(
      'INSERT INTO registration(token, user_key, parcel_identity, url)' +
      ' SELECT ' + QuotedStr(LGuid) + ',' +
      ' ' + QuotedStr(ARequest.Authentication.UserKey) + ',' +
      ' ' + QuotedStr(ARequest.Authentication.ParcelID) + ',' +
      ' ' + QuotedStr(ARequest.Validation.URL) + ';',
      LError
    ) then
    begin
      LogError('RegisterImage::' + LError);
      Exit;
    end;

    Response.Token := LGuid;

    //kick off the background process to perform the actual image deconstruction
    StartProcessing(ARequest, Response);

    Result := True;
  except on E : Exception do
    LogError('RegisterImage::' + E.Message);
  end;
end;

function TRegistrationController.CancelRequest(): Boolean;
begin
  //not implemented, but need to look in collection and
  //then add some kind of flag to the thread (if found) to stop processing
  Result := False;
end;

function TRegistrationController.LookupImageID(const AImageToken: String; out
  ID: Integer): Boolean;
var
  LData: TDatasetResponse;
  LError: String;
  LRow: TJSONData;
begin
  Result := False;

  try
    //try to fetch the id
    if not GetSQLResultsJSON(
      'SELECT id as id FROM registration WHERE token = ' + QuotedStr(AImageToken) + ' LIMIT 1;',
      LData,
      LError
    ) then
    begin
      LogError('LookupImageID::' + LError);
      Exit;
    end;

    //if the caller provided an invalid token, warn and bail
    if LData.Count < 1 then
    begin
      LogWarn('LookupImageID::[' + AImageToken + '] is not a valid token');
      Exit;
    end;

    LRow := GetJSON(LData[0]);
    try
      if not Assigned(LRow) or (LRow.JSONType <> jtObject) then
      begin
        LogError('LookupImageID::malformed json for token [' + AImageToken + ']');
        Exit;
      end;

      //assign the id
      ID := TJSONObject(LRow).Get('id');
    finally
      LRow.Free;
    end;

    //success
    Result := True;
  except on E : Exception do
    LogError('LookupImageID::' + E.Message);
  end;
end;

class procedure TRegistrationController.StartProcessing(Constref
  ARequest: TRegisterURLRequest; Constref AResponse: TRegisterURLResponse);
var
  LThread : IEZThread;

  procedure Start(Const AThread : IEZThread);
  var
    LStatus : TStatusController;
    LImage : TImageController;
    LData : TMemoryStream;
    LClient : TFPHTTPClient;
    LError : String;
    LResp : TImageResponse;
    LBitmap : TBGRABitmap;
  begin
    LStatus := TStatusController.Create(nil);
    LImage := TImageController.Create(nil);
    LClient := TFPHTTPClient.Create(nil);
    LData := TMemoryStream.Create;
    LBitmap := TBGRABitmap.Create;
    try
      try
        //update status to in-progress
        if not LStatus.UpdateStatus(AThread['token'], isInProgress, LError) then
          raise Exception.Create(LError);

        //attempt to download the image
        LClient.Get(AThread['url'], LData);

        //we need to dither the image to a reduced color space so our
        //dcl client can draw it in some reasonable amount of time
        //(currently uses color components to draw, this may change later...)
        if not DitherImageStream(LData, AThread['url'], LBitmap, LError) then
        begin
          AThread.AddArg('error', LError);
          Exit;
        end;

        //after dithering has been completed, we can construct to a
        //dcl readable format
        if not ConstructDCLImageResponse(LBitmap, LResp, LError) then
        begin
          AThread.AddArg('error', LError);
          Exit;
        end;

        //write the image response to the database
        if not LImage.UpdateImage(AThread['token'], LResp, LError) then
          AThread.AddArg('error', LError);
      finally
        LBitmap.Free;
        LData.Free;
        LClient.Free;
        LStatus.Free;
        LImage.Free;
      end;
    except on E : Exception do
      AThread.AddArg('error', E.Message);
    end;
  end;

  procedure Finish(Const AThread : IEZThread);
  var
    LStatus : TStatusController;
    LError: String;
  begin
    LStatus := TStatusController.Create(nil);
    try
      if not AThread.Exists['error'] then
      begin
         //mark the status as complete
         if not LStatus.UpdateStatus(AThread['token'], isCompleted, LError) then
           if not LStatus.UpdateStatus(AThread['token'], isFailed, LError) then
             Exit;
      end
      //mark the status as failure
      else
        LStatus.UpdateStatus(AThread['token'], isFailed, LError);
    finally
      //remove from collection
      Collection.Remove(AThread);
    end;
  end;

begin
  //setup a processing thread
  LThread := TEZThreadImpl.Create;
  LThread
    .Setup(
      Start, //handles initialize of status and processing
      nil,
      Finish //updates the status to finished/failed and saves the result
    )
    .AddArg('url', ARequest.Validation.URL)
    .AddArg('token', AResponse.Token)
    .Settings
      .Await
        (*
          update the group id to be the thread id since we won't have
          access to the thread from the controller (gets freed after the request)
          and we may need to index back into the collection. alternatively,
          could use a different collection type (map<token,thread> for example)
        *)
        .UpdateGroupID(LThread.Settings.Await.ThreadID);

  //before starting the thread, add it to the monitor collection
  Collection.Add(LThread);

  //good to start now
  LThread.Start;
end;

initialization
  MAX_IMAGE_HEIGHT := 64;
  MAX_IMAGE_WIDTH := 64;
  Collection := TEZCollectionImpl.Create;
  InitializePalette;
  RegisterHTTPModule(GetControllerRoute('registration'), TRegistrationController);
finalization
  Collection := nil;
  Palette.Free;
end.

