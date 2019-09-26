export interface ICommand {
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

export interface ICommands {
    commands: ICommand[];
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

export function FetchImage(
    parent : UIShape, //can be the canvas or any other parent
    token : string, //image token provided by the call to register
    scale : number = 1.0 //a multiplier that can be applied to the final result image
) : void
{
    log('1');
    let result = new UIContainerRect(parent);
    log('2');
    result.visible = false;
    result.width = 64;
    result.height = 64;
    log('3');
    executeTask(async () => 
        {
            log('4'); 
            let response = await fetch(
                'http://highball.dcl.dev.com:8080/controller/image?a=fetch',
                {
                    body : JSON.stringify({ image : { token : '{728CECC6-B8A2-423A-9089-33F4B5925731}'}, authentication : { token : '{74DB8A78-D394-43FB-ADA9-F3E6F6963216}'}}),
                    method : 'POST',
                }
            );
            log('5');
            let json = await response.json()
            let c = json as ICommands;
            log('6');
            
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
                
            //now mark the result image visible
            result.visible = true;
        }
    ).catch(
        error => log(error)
    );
}