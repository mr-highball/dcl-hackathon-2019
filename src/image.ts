let IMAGE_SERVICE_ROOT : string = 'https://highballs.world'
export default IMAGE_SERVICE_ROOT;

export interface ICommand 
{
    topLX: number;
    topRX: number;
    botLX: number;
    botRX: number;
    topLY: number;
    topRY: number;
    botLY: number;
    botRY: number;
    r: number;
    g: number;
    b: number;
    a: number;
}

export interface ICommands 
{
    commands: ICommand[];
}

export interface IImageToken
{
    token : string;
}
  
export interface IImageStatus
{
    status : string;
}

export enum ImageStatuses
{
    IN_PROGRESS = 'In-Progress',
    COMPLETED = 'Completed',
    FAILED = 'Failed'
}

export function DupeStr(value : string, amount : number) : string
{
    let result = "";

    for (let i : number = 0; i < amount; i++)
        result += value;

    log(result);
    return result;
}

function Sleep(amount : number)
{
    //record starting time
    let start = new Date();
    start.getDate();

    while(true)
    {
        //subtract msecs of now / then to determine if amount has been reached
        if (Date.now() - start.getTime() > amount)
            return;
    }
}

export function DrawRect(canvas : UIShape, x : number, y : number, width : number, height : number, color : Color4) : void
{
    //safe way to convert the dimensions to a number, however percentages won't work right
    const w = <number><unknown>canvas.width.toString().replace("px", "").replace("%", "");
    const h = <number><unknown>canvas.height.toString().replace("px", "").replace("%", "");

    //initialize our rect
    const rect = new UIContainerRect(canvas);
    rect.width = width;
    rect.height = height;

    //make this map to the windows paradigm (upper left corner 0,0)
    //currently, dcl positions 0,0 to the center of the shape, but this may change
    rect.positionX = x + width / 2 - w / 2;
    rect.positionY = (y + height / 2 - h / 2) * -1; 

    rect.color = color;
}

export async function RegisterImage(
    url : string
) : Promise<IImageToken>
{
    log('RegisterImage::url is [' + url + ']');

    return new Promise<IImageToken>(async (resolve, reject) =>
        {
            let task = await fetch(
                IMAGE_SERVICE_ROOT + '/controller/registration?a=register',
                {
                    body : JSON.stringify({ validation : { url : url, validation : { token :  'notusedrightnow'} }, authentication : { userKey : '0xblahblah', parcelIdentity : '0,0' } }),
                    method : 'POST'
                }
            ).then(
                async success =>
                    {
                        let json = await success.json();
            
                        try
                        {
                            let result = json as IImageToken;
                            resolve(result);
                        }
                        catch (error)
                        {
                            reject('RegisterImage::' + error);
                        }        
                    },
                failure =>
                    {
                        reject('RegisterImage::failed to register');
                    }
            )
        }
    );
}

export async function ImageStatus(
    token : IImageToken
) : Promise<boolean>
{
    log('ImageStatus::token is [' + token + ']');
    const RETRY_COUNT = 5;

    return new Promise<boolean>(async (resolve, reject) =>
        {
            let count = 0;

            while (count < RETRY_COUNT)
            {
                await fetch(
                    IMAGE_SERVICE_ROOT + '/controller/status?a=fetch',
                    {
                        body : JSON.stringify(token),
                        method : 'POST'
                    }
                ).then(
                    async success =>
                        {
                            try
                            {
                                log('attempt: ' + count);
                                let json = await success.json();
                                let result = json as IImageStatus;

                                if (result.status === ImageStatuses.COMPLETED)
                                {
                                    log('ImageStatus::completed status for token [' + token.token + ']');
                                    resolve(true);
                                    count = RETRY_COUNT;
                                    return;
                                }
                                else if (result.status === ImageStatuses.FAILED)
                                {
                                    log('ImageStatus::failed status for token [' + token.token + ']');
                                    resolve(false);
                                    count = RETRY_COUNT;
                                    return;
                                }
                            }
                            catch (error)
                            {
                                log('ImageStatus::handled exception [' + error + ']')
                            }        
                        },
                    failure =>
                        {
                            log('ImageStatus::OnRejected reached')
                        }
                ); 

                //increment counter 
                ++count;
                Sleep(2000);
            }

            log('ImageStatus::maximum tries exceeded')
            resolve(false);
        }
    );
}

export async function FetchImage(
    parent : UIShape, //can be the canvas or any other parent
    token : IImageToken, //image token provided by the call to register
    scale : number = 1.0 //a multiplier that can be applied to the final result image
) : Promise<UIShape>
{
    
    return new Promise<UIShape>((resolve, reject) =>
        {
            let s = scale > 0 ? scale : 1;
            
            executeTask(async () =>
                {
                    
                    let response = await fetch(
                        IMAGE_SERVICE_ROOT + '/controller/image?a=fetch',
                        {
                            body : JSON.stringify({ image : token, authentication : { token : '{74DB8A78-D394-43FB-ADA9-F3E6F6963216}'}}),
                            method : 'POST'
                        }
                    ).then(
                        async success =>
                        {
                            let json = await success.json();
                            try 
                            {
                                let result = new UIContainerRect(parent);
                                result.visible = false;
                                
                                //todo - these need to come from the response object, not hardcoded
                                result.width = 48 * s;
                                result.height = 48 * s;
                                let c = json as ICommands;
                                
                                //for each of the commands we call draw rect after adapting to the 
                                //our command signature
                                c.commands.forEach(command => 
                                    {
                                        DrawRect(
                                            result, 
                                            command.topLX * s, 
                                            command.topLY * s, 
                                            ((command.topRX + 1) - command.topLX) * s, 
                                            ((command.botRY + 1) - command.topRY) * s,
                                            Color4.FromInts(
                                                command.r, 
                                                command.g, 
                                                command.b, 
                                                command.a
                                            )
                                        );
                                    }
                                );
        
                                resolve(result);
                            } 
                            catch (error) 
                            {
                                log('bad image fetch: ' + error);
                                reject(null);
                            }
                        },
                        failure  =>
                        {
                            let result = new UIText(parent);
                            result.value = 'whomp whomp... :(';
                            log('image service failed: ' + failure);
                            
                            reject(result);
                        }
                    );  
                }
            );
        }
    );
}