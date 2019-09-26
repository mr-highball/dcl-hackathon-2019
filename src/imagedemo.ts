export class DemoUI
{
    ///////////////////////////////////////////////////////////////////////////////////////////
    //PUBLIC
    ///////////////////////////////////////////////////////////////////////////////////////////
    
    /*
        initializes internal controls and paints to screen 
    */
    public Init() : void
    {
        try
        {
            this.visible = false;
            this.DoInit();
            this.visible = true;
            this.DoAfterInit();
        }
        catch (error)
        {
            log('Init::' + error);
        }
    }

    /*
        determines the visibility of the UI element
    */
    get visible()
    {
        return this._visible;
    }
    
    /*
        sets the visiblility for all controls
    */
    set visible(value : boolean)
    {
        //iterate controls and set the visible property
        for(let i = 0; i < this._controls.length; i++)
            this._controls[i].visible = value;
    }

    constructor(container : UIShape)
    {
        try 
        {
            this._controls = [];
            this.DoCreateControls(container, this._controls);
            this._visible = false;
        } 
        catch (error) 
        {
            log('ConstructionError::' + error);
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////
    //PROTECTED
    ///////////////////////////////////////////////////////////////////////////////////////////
    /*
        children override this method to initialize any child control before 
        displaying in scene
    */
    protected DoInit() : void
    {
        //nothing in base
    }

    /*
        occurs directly after a succesfuly call to DoInit()
    */
    protected DoAfterInit() : void
    {
        //nothing in base
    }

    /*
        children override this method to return any controls that make up 
        this UI element
    */
    protected DoCreateControls(container : UIShape, controls : UIShape[]) : void
    {
        //nothing in base
    }

    protected ControlByName(name : string) : UIShape
    {
        for(let i = 0; i < this._controls.length; i++)
            if (this._controls[i].name === name)
                return this._controls[i];

        return null;
    }
    
    ///////////////////////////////////////////////////////////////////////////////////////////
    //PRIVATE
    ///////////////////////////////////////////////////////////////////////////////////////////
    private _controls : UIShape[];
    private _visible : boolean;
}

export enum UIAnchor
{
    TOP_LEFT,
    TOP_CENTER,
    TOP_RIGHT,
    CENTER_LEFT,
    CENTER,
    CENTER_RIGHT,
    BOTTOM_LEFT,
    BOTTOM_CENTER,
    BOTTOM_RIGHT,
    FILL
}

export class AlignAdapter
{
    horizontal : string = '';
    vertical : string = '';
    
    public Adapt(anchor : UIAnchor) : void
    {
        switch(anchor)
        {
            case UIAnchor.TOP_LEFT:
            {
                this.horizontal = 'left';
                this.vertical = 'top';
                break;
            }
            case UIAnchor.TOP_CENTER:
            {
                this.horizontal = 'center';
                this.vertical = 'top';
                break;
            }
            case UIAnchor.TOP_RIGHT:
            {
                this.horizontal = 'right';
                this.vertical = 'top';
                break;
            }
            case UIAnchor.CENTER_LEFT:
            {
                this.horizontal = 'left';
                this.vertical = 'center';
                break;
            }
            //treat center and fill the same, it will be up to the 
            //component to handle it
            case UIAnchor.CENTER, UIAnchor.FILL:
            {
                this.horizontal = 'center';
                this.vertical = 'center';
                break;
            }
            case UIAnchor.CENTER_RIGHT:
            {
                this.horizontal = 'right';
                this.vertical = 'center';
                break;
            }
            case UIAnchor.BOTTOM_LEFT:
            {
                this.horizontal = 'left';
                this.vertical = 'bottom';
                break;
            }
            case UIAnchor.BOTTOM_CENTER:
            {
                this.horizontal = 'center';
                this.vertical = 'bottom';
                break;
            }
            case UIAnchor.BOTTOM_RIGHT:
            {
                this.horizontal = 'right';
                this.vertical = 'bottom';
                break;
            }
            default:
                break;
        }
    }

    public AdaptShape(anchor : UIAnchor, shape : UIShape) : void
    {
        this.Adapt(anchor);
        shape.vAlign = this.vertical;
        shape.hAlign = this.horizontal;
    }
}

/*
    helper function for return an alignment adapter class provided the 
    anchor desired
*/
export function CreateAlignAdapter(anchor : UIAnchor) : AlignAdapter
{
    let result = new AlignAdapter();
    result.Adapt(anchor);

    return result;
}

export class DemoButtonUI extends DemoUI
{
    ///////////////////////////////////////////////////////////////////////////////////////////
    //PUBLIC
    ///////////////////////////////////////////////////////////////////////////////////////////
    public get buttonText() : string
    {
        return this._text;
    }

    public set buttonText(value : string)
    {
        this._text = value;
    }

    public get buttonClickEvent() : OnClick
    {
        return this._onClick;
    }

    public set buttonClickEvent(value : OnClick)
    {
        this._onClick = value;
    }

    public get buttonColor() : Color4
    {
        return this._color;
    }

    public set buttonColor(value : Color4)
    {
        this._color = value;
    }

    public get fontColor() : Color4
    {
        return this._fontColor;
    }

    public set fontColor(value : Color4)
    {
        this._fontColor = value;
    }

    public get widthPercentage() : number
    {
        return this._width;
    }

    public set widthPercentage(value : number)
    {
        this._width = value > 0 ? value > 100 ? 100 : value : 0;
    }

    public get heightPercentage() : number
    {
        return this._height;
    }

    public set heightPercentage(value : number)
    {
        this._height = value > 0 ? value > 100 ? 100 : value : 0;
    }

    public get anchor() : UIAnchor
    {
        return this._anchor;
    }

    public get background() : UIShape
    {
        return this.ControlByName('startButton');
    }

    public set anchor(value : UIAnchor)
    {
        this._anchor = value;
    }

    public get fontAnchor() : UIAnchor
    {
        return this._fontAnchor;
    }

    public set fontAnchor(value : UIAnchor)
    {
        this._fontAnchor = value;
    }

    public get fontSize() : number
    {
        return this._fontSize;
    }

    public set fontSize(value : number)
    {
        this._fontSize = value;
    }
    

    ///////////////////////////////////////////////////////////////////////////////////////////
    //PROTECTED
    ///////////////////////////////////////////////////////////////////////////////////////////
    protected DoInit() : void
    {
        //get an adapter for the position of this button
        let adapter = CreateAlignAdapter(this._anchor);

        //set the position of our start button
        let button = this.ControlByName('startButton') as UIContainerRect;
        button.color = this.buttonColor;
        button.opacity = 0.8;
        button.hAlign = adapter.horizontal;
        button.vAlign = adapter.vertical;
        //button.positionX = -25; //todo - define a margin?
        //button.positionY = 50;
        if (this._anchor != UIAnchor.FILL)
        {
            button.width = this._width + '%';
            button.height = this._height + '%';
        }
        else
        {
            button.width = '100%';
            button.height = '100%';
        }
        
        button.isPointerBlocker = false;

        //get and initialize our text
        adapter = CreateAlignAdapter(this._fontAnchor);
        let text = this.ControlByName('startButtonText') as UIText;
        text.hAlign = adapter.horizontal;
        text.vAlign = adapter.vertical;
        text.value = this._text;
        text.fontAutoSize = this._fontSize > 0 ? false : true;
        text.fontSize = this._fontSize;
        text.outlineWidth = 0.15;
        text.outlineColor = Color4.Black();
        text.isPointerBlocker = false;

        //since we can't remove UI controls right now, just hide this clicker
        //if we had one (subsequent calls to Init())
        if (this._clicker)
        {
            this._clicker.visible = false;
            this._clicker.onClick = null;
        }
    }

    protected DoAfterInit() : void
    {
        super.DoAfterInit();

        //get the parent button
        let button = this.ControlByName('startButton') as UIContainerRect;

        this._clicker = new UIImage(button.parent, null);
        this._clicker.opacity = 0;//1; //uncomment this to visualize the bounds of the image
        this._clicker.positionX = 0;
        this._clicker.positionY = 0;
        this._clicker.isPointerBlocker = true;
        this._clicker.name = "startButtonClicker";

        //set to the bounds of the "parent"
        this._clicker.hAlign = button.hAlign;
        this._clicker.vAlign = button.vAlign;
        this._clicker.width = button.width;
        this._clicker.height = button.height;
        
        //add the event
        this._clicker.onClick = this._onClick;
        this._clicker.visible = true;    
    }

    protected DoCreateControls(container : UIShape, controls : UIShape[]) : void
    {
        super.DoCreateControls(container, controls);

        //create a new button which will kick off the demo
        let button = new UIContainerRect(container);
        button.name = 'startButton';

        //create a text shape
        let text = new UIText(button);
        text.name = 'startButtonText';

        //add the controls to the list
        controls.push(button)
        controls.push(text);     
    }

    ///////////////////////////////////////////////////////////////////////////////////////////
    //PRIVATE
    ///////////////////////////////////////////////////////////////////////////////////////////
    private _text : string = 'Start!';
    private _onClick : OnClick = null;
    private _color : Color4 = Color4.Green();
    private _fontColor : Color4 = Color4.White();
    private _anchor : UIAnchor = UIAnchor.BOTTOM_RIGHT;
    private _clicker : UIImage = null;
    private _fontAnchor : UIAnchor = UIAnchor.CENTER;
    private _width : number = 10;
    private _height : number = 10;
    private _fontSize : number = -1;
}