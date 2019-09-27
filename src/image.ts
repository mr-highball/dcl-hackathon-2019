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
  
export function DupeStr(value : string, amount : number) : string
{
    let result = "";

    for (let i : number = 0; i < amount; i++)
        result += value;

    log(result);
    return result;
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
    return new Promise<IImageToken>(async (resolve, reject) =>
        {
            let task = await fetch(
                'http://highball.dcl.dev.com:8083/controller/registration?a=register',
                {
                    body : JSON.stringify({ validation : { url : url, validation : { token :  ''} }, authentication : { userKey : '', parcelIdentity : '0,0' } }),
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
                            reject(null);
                        }        
                    },
                failure =>
                    {
                        reject(null);
                    }
            )
        }
    );
}

export async function FetchImage(
    parent : UIShape, //can be the canvas or any other parent
    token : string, //image token provided by the call to register
    scale : number = 1.0 //a multiplier that can be applied to the final result image
) : Promise<UIShape>
{
    
    return new Promise<UIShape>((resolve, reject) =>
        {

            executeTask(async () =>
                {
                    
                    let response = await fetch(
                        'http://highball.dcl.dev.com:8083/controller/image?a=fetch',
                        {
                            body : JSON.stringify({ image : { token : '{4FA8E3CE-63A1-43CF-9EBF-1480607FFF2A}'}, authentication : { token : '{74DB8A78-D394-43FB-ADA9-F3E6F6963216}'}}),
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
                                result.width = 64;
                                result.height = 64;
                                let c = json as ICommands;
                                
                                //for each of the commands we call draw rect after adapting to the 
                                //our command signature
                                c.commands.forEach(command => 
                                    {
                                        DrawRect(
                                            result, 
                                            command.topLX, 
                                            command.topLY, 
                                            (command.topRX + 1) - command.topLX, 
                                            (command.botRY + 1) - command.topRY,
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