import * as demo from 'imagedemo';
import * as img from 'image';

const scene = new Entity()
const transform = new Transform({
  position: new Vector3(0, 0, 0),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
scene.addComponentOrReplace(transform)
engine.addEntity(scene)

const floorBlock_04 = new Entity()
floorBlock_04.setParent(scene)
const gltfShape = new GLTFShape('models/FloorBlock_04/FloorBlock_04.glb')
floorBlock_04.addComponentOrReplace(gltfShape)
const transform_2 = new Transform({
  position: new Vector3(8, 0, 1),
  rotation: new Quaternion(0, -0.7071067811865477, 0, 0.7071067811865477),
  scale: new Vector3(1, 1, 1)
})
floorBlock_04.addComponentOrReplace(transform_2)
engine.addEntity(floorBlock_04)

const floorBlock_04_2 = new Entity()
floorBlock_04_2.setParent(scene)
floorBlock_04_2.addComponentOrReplace(gltfShape)
const transform_3 = new Transform({
  position: new Vector3(8, 0, 3),
  rotation: new Quaternion(0, -0.7071067811865477, 0, 0.7071067811865477),
  scale: new Vector3(1, 1, 1)
})
floorBlock_04_2.addComponentOrReplace(transform_3)
engine.addEntity(floorBlock_04_2)

const floorBlock_04_3 = new Entity()
floorBlock_04_3.setParent(scene)
floorBlock_04_3.addComponentOrReplace(gltfShape)
const transform_4 = new Transform({
  position: new Vector3(8, 0, 5),
  rotation: new Quaternion(0, -0.7071067811865477, 0, 0.7071067811865477),
  scale: new Vector3(1, 1, 1)
})
floorBlock_04_3.addComponentOrReplace(transform_4)
engine.addEntity(floorBlock_04_3)

const floorBlock_04_4 = new Entity()
floorBlock_04_4.setParent(scene)
floorBlock_04_4.addComponentOrReplace(gltfShape)
const transform_5 = new Transform({
  position: new Vector3(8, 0, 7),
  rotation: new Quaternion(0, -0.7071067811865477, 0, 0.7071067811865477),
  scale: new Vector3(1, 1, 1)
})
floorBlock_04_4.addComponentOrReplace(transform_5)
engine.addEntity(floorBlock_04_4)

const floorBlock_04_5 = new Entity()
floorBlock_04_5.setParent(scene)
floorBlock_04_5.addComponentOrReplace(gltfShape)
const transform_6 = new Transform({
  position: new Vector3(10, 0, 7),
  rotation: new Quaternion(0, -0.7071067811865477, 0, 0.7071067811865477),
  scale: new Vector3(1, 1, 1)
})
floorBlock_04_5.addComponentOrReplace(transform_6)
engine.addEntity(floorBlock_04_5)

const floorBlock_04_6 = new Entity()
floorBlock_04_6.setParent(scene)
floorBlock_04_6.addComponentOrReplace(gltfShape)
const transform_7 = new Transform({
  position: new Vector3(10, 0, 9),
  rotation: new Quaternion(0, -0.7071067811865477, 0, 0.7071067811865477),
  scale: new Vector3(1, 1, 1)
})
floorBlock_04_6.addComponentOrReplace(transform_7)
engine.addEntity(floorBlock_04_6)

const floorBlock_04_7 = new Entity()
floorBlock_04_7.setParent(scene)
floorBlock_04_7.addComponentOrReplace(gltfShape)
const transform_8 = new Transform({
  position: new Vector3(8, 0, 9),
  rotation: new Quaternion(0, -0.7071067811865477, 0, 0.7071067811865477),
  scale: new Vector3(1, 1, 1)
})
floorBlock_04_7.addComponentOrReplace(transform_8)
engine.addEntity(floorBlock_04_7)

const floorBlock_04_8 = new Entity()
floorBlock_04_8.setParent(scene)
floorBlock_04_8.addComponentOrReplace(gltfShape)
const transform_9 = new Transform({
  position: new Vector3(6, 0, 9),
  rotation: new Quaternion(0, -0.7071067811865477, 0, 0.7071067811865477),
  scale: new Vector3(1, 1, 1)
})
floorBlock_04_8.addComponentOrReplace(transform_9)
engine.addEntity(floorBlock_04_8)

const floorBlock_04_9 = new Entity()
floorBlock_04_9.setParent(scene)
floorBlock_04_9.addComponentOrReplace(gltfShape)
const transform_10 = new Transform({
  position: new Vector3(6, 0, 7),
  rotation: new Quaternion(0, -0.7071067811865477, 0, 0.7071067811865477),
  scale: new Vector3(1, 1, 1)
})
floorBlock_04_9.addComponentOrReplace(transform_10)
engine.addEntity(floorBlock_04_9)

const floorBaseGrass_02 = new Entity()
floorBaseGrass_02.setParent(scene)
const gltfShape_2 = new GLTFShape('models/FloorBaseGrass_02/FloorBaseGrass_02.glb')
floorBaseGrass_02.addComponentOrReplace(gltfShape_2)
const transform_11 = new Transform({
  position: new Vector3(8, 0, 8),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
floorBaseGrass_02.addComponentOrReplace(transform_11)
engine.addEntity(floorBaseGrass_02)

const fencePicketWhiteMedium_01 = new Entity()
fencePicketWhiteMedium_01.setParent(scene)
const gltfShape_3 = new GLTFShape('models/FencePicketWhiteMedium_01/FencePicketWhiteMedium_01.glb')
fencePicketWhiteMedium_01.addComponentOrReplace(gltfShape_3)
const transform_12 = new Transform({
  position: new Vector3(5, 0, 9),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
fencePicketWhiteMedium_01.addComponentOrReplace(transform_12)
engine.addEntity(fencePicketWhiteMedium_01)

const fencePicketWhiteMedium_01_2 = new Entity()
fencePicketWhiteMedium_01_2.setParent(scene)
fencePicketWhiteMedium_01_2.addComponentOrReplace(gltfShape_3)
const transform_13 = new Transform({
  position: new Vector3(11, 0, 9),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
fencePicketWhiteMedium_01_2.addComponentOrReplace(transform_13)
engine.addEntity(fencePicketWhiteMedium_01_2)

const tableBar_01 = new Entity()
tableBar_01.setParent(scene)
const gltfShape_4 = new GLTFShape('models/TableBar_01/TableBar_01.glb')
tableBar_01.addComponentOrReplace(gltfShape_4)
const transform_14 = new Transform({
  position: new Vector3(13, 0, 11.5),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
tableBar_01.addComponentOrReplace(transform_14)
engine.addEntity(tableBar_01)

const pot_02 = new Entity()
pot_02.setParent(scene)
const gltfShape_5 = new GLTFShape('models/Pot_02/Pot_02.glb')
pot_02.addComponentOrReplace(gltfShape_5)
const transform_15 = new Transform({
  position: new Vector3(6, 0, 4.5),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
pot_02.addComponentOrReplace(transform_15)
engine.addEntity(pot_02)

const pot_02_2 = new Entity()
pot_02_2.setParent(scene)
pot_02_2.addComponentOrReplace(gltfShape_5)
const transform_16 = new Transform({
  position: new Vector3(10, 0, 4.5),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
pot_02_2.addComponentOrReplace(transform_16)
engine.addEntity(pot_02_2)

const pot_02_3 = new Entity()
pot_02_3.setParent(scene)
pot_02_3.addComponentOrReplace(gltfShape_5)
const transform_17 = new Transform({
  position: new Vector3(10, 0, 1.5),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
pot_02_3.addComponentOrReplace(transform_17)
engine.addEntity(pot_02_3)

const pot_02_4 = new Entity()
pot_02_4.setParent(scene)
pot_02_4.addComponentOrReplace(gltfShape_5)
const transform_18 = new Transform({
  position: new Vector3(6, 0, 1.5),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
pot_02_4.addComponentOrReplace(transform_18)
engine.addEntity(pot_02_4)

const chair_03 = new Entity()
chair_03.setParent(scene)
const gltfShape_6 = new GLTFShape('models/Chair_03/Chair_03.glb')
chair_03.addComponentOrReplace(gltfShape_6)
const transform_19 = new Transform({
  position: new Vector3(13, 0, 13),
  rotation: new Quaternion(0, -0.5555702330196024, 0, 0.8314696123025453),
  scale: new Vector3(1, 1, 1)
})
chair_03.addComponentOrReplace(transform_19)
engine.addEntity(chair_03)

const geckoStone_01 = new Entity()
geckoStone_01.setParent(scene)
const gltfShape_7 = new GLTFShape('models/GeckoStone_01/GeckoStone_01.glb')
geckoStone_01.addComponentOrReplace(gltfShape_7)
const transform_20 = new Transform({
  position: new Vector3(13, 0.5, 13),
  rotation: new Quaternion(0, -0.9569403357322092, 0, 0.2902846772544627),
  scale: new Vector3(1, 1, 1)
})
geckoStone_01.addComponentOrReplace(transform_20)
engine.addEntity(geckoStone_01)

const bush_01 = new Entity()
bush_01.setParent(scene)
const gltfShape_8 = new GLTFShape('models/Bush_01/Bush_01.glb')
bush_01.addComponentOrReplace(gltfShape_8)
const transform_21 = new Transform({
  position: new Vector3(2.5, 0, 13),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
bush_01.addComponentOrReplace(transform_21)
engine.addEntity(bush_01)

const flower_04 = new Entity()
flower_04.setParent(scene)
const gltfShape_9 = new GLTFShape('models/Flower_04/Flower_04.glb')
flower_04.addComponentOrReplace(gltfShape_9)
const transform_22 = new Transform({
  position: new Vector3(10, 0.5, 4.5),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
flower_04.addComponentOrReplace(transform_22)
engine.addEntity(flower_04)

const flower_04_2 = new Entity()
flower_04_2.setParent(scene)
flower_04_2.addComponentOrReplace(gltfShape_9)
const transform_23 = new Transform({
  position: new Vector3(10, 0.5, 1.5),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
flower_04_2.addComponentOrReplace(transform_23)
engine.addEntity(flower_04_2)

const flower_04_3 = new Entity()
flower_04_3.setParent(scene)
flower_04_3.addComponentOrReplace(gltfShape_9)
const transform_24 = new Transform({
  position: new Vector3(6, 0.5, 1.5),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
flower_04_3.addComponentOrReplace(transform_24)
engine.addEntity(flower_04_3)

const flower_04_4 = new Entity()
flower_04_4.setParent(scene)
flower_04_4.addComponentOrReplace(gltfShape_9)
const transform_25 = new Transform({
  position: new Vector3(6, 0.5, 4.5),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
flower_04_4.addComponentOrReplace(transform_25)
engine.addEntity(flower_04_4)

const bush_02 = new Entity()
bush_02.setParent(scene)
const gltfShape_10 = new GLTFShape('models/Bush_02/Bush_02.glb')
bush_02.addComponentOrReplace(gltfShape_10)
const transform_26 = new Transform({
  position: new Vector3(14, 0, 2.5),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
bush_02.addComponentOrReplace(transform_26)
engine.addEntity(bush_02)

const rockMediumMoss_01 = new Entity()
rockMediumMoss_01.setParent(scene)
const gltfShape_11 = new GLTFShape('models/RockMediumMoss_01/RockMediumMoss_01.glb')
rockMediumMoss_01.addComponentOrReplace(gltfShape_11)
const transform_27 = new Transform({
  position: new Vector3(4, 0, 4.5),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
rockMediumMoss_01.addComponentOrReplace(transform_27)
engine.addEntity(rockMediumMoss_01)

const rockMedium_01 = new Entity()
rockMedium_01.setParent(scene)
const gltfShape_12 = new GLTFShape('models/RockMedium_01/RockMedium_01.glb')
rockMedium_01.addComponentOrReplace(gltfShape_12)
const transform_28 = new Transform({
  position: new Vector3(15, 0, 10.5),
  rotation: new Quaternion(0, 0, 0, 1),
  scale: new Vector3(1, 1, 1)
})
rockMedium_01.addComponentOrReplace(transform_28)
engine.addEntity(rockMedium_01)

//add our billboard model
const billboard = new Entity()
billboard.setParent(scene)
const gltfShape_13 = new GLTFShape('models/billboard/billboard.glb')
billboard.addComponentOrReplace(gltfShape_13)
const transform_29 = new Transform({
  position: new Vector3(8, 0, 10),
  rotation: new Quaternion(0, -1.0000000000000002, 0, 1.8041124150158794e-16),
  scale: new Vector3(4.5, 2, 1.5)
})
billboard.addComponentOrReplace(transform_29)
engine.addEntity(billboard);

//method to reset demo to initial state
function Reset() : void
{
  startButton.buttonColor = Color4.Green();
  startButton.buttonText = 'Start!';
  input.visible = false;
  working = false;
  startButton.Init();
}

//create a canvas for our ui elemnts
let canvas = new UICanvas();
canvas.width = '100%';
canvas.height = '100%'
canvas.visible = true;


//create an input control for collecting the URL
let input = new UIInputText(canvas);
input.name = 'Please paste in a valid image URL (.png, jpeg, etc...):';
input.placeholder = 'paste a url...';

let adapter = demo.CreateAlignAdapter(demo.UIAnchor.CENTER);
adapter.AdaptShape(demo.UIAnchor.CENTER, input);
input.autoStretchWidth = true;
input.height = 30;
input.width = 15 * input.placeholder.length;
input.textWrapping = false;
input.color = Color4.Black(); //these don't seem to do anything?
input.background = Color4.Gray(); //these don't seem to do anything?
input.fontSize  = 30;
input.visible = false;

//create some instructions
let instructionsBack = new UIContainerRect(canvas);
instructionsBack.color = Color4.FromInts(255, 255, 255, 125);
instructionsBack.width = 300;
instructionsBack.height = 100;
adapter.AdaptShape(demo.UIAnchor.TOP_CENTER, instructionsBack);

let instructions = new UIText(instructionsBack);
instructions.color = Color4.Black();
instructions.fontSize = 14;
instructions.width = instructionsBack.width;
instructions.height = instructionsBack.height;
instructions.textWrapping = true;
instructions.fontAutoSize = false;
adapter.AdaptShape(demo.UIAnchor.CENTER, instructions);
instructions.value = 'pasting urls currently does not work in DCL so as a convenience,' +
  ' you can type a number from (1 - 10) and press enter which will use a hard coded url. alternatively,' +
  ' you can type a url by hand (with some patience). enjoy  :) ';
instructions.visible = true;

//setup a demo button
let startButton = new demo.DemoButtonUI(canvas);
startButton.buttonText = "Start!";
startButton.anchor = demo.UIAnchor.CENTER_RIGHT;
startButton.fontAnchor = demo.UIAnchor.CENTER;
startButton.widthPercentage = 10;
startButton.heightPercentage = 15;
startButton.buttonClickEvent = new OnClick(() =>
  {
    if (working)
      return;

    input.visible = true;
    working = true;

    if (lastImage)
      lastImage.visible = false;

    
  }
);

Reset();

//render control to scene
startButton.Init();

//flag to prevent double clicks
let working : boolean = false;

let lastImage : UIShape = null;

//setup the submit event for capturing
input.onTextSubmit = new OnTextSubmit(async event => 
  {
    log('the text is: [' + event.text + ']');

    //below are shortcuts since UIInputText doesn't allow pasting at the time of writing
    let text : string = event.text;
    
    //text can't directly be compared to value... not sure why, just substringing for now
    if (text.substr(0, 2) == '10')
      text = 'http://www.spiritedsingapore.com/wp-content/uploads/2015/04/Highball.jpg';
    else if (text.substr(0, 2) == '69')
      text = 'https://www.drawingnow.com/file/pic/gallery/67660_view.jpg';
    else if (text.substr(0, 1) == '1')
      text = 'https://upload.wikimedia.org/wikipedia/commons/thumb/9/97/Classic_smiley.svg/1026px-Classic_smiley.svg.png';
    else if (text.substr(0, 1) == '2')
      text = 'https://i.ytimg.com/vi/XiOpyRsQWwc/maxresdefault.jpg';
    else if (text.substr(0, 1) == '3')
      text = 'https://i.ytimg.com/vi/PX7zPlQjAr8/maxresdefault.jpg';
    else if (text.substr(0, 1) == '4')
      text = 'https://i.ytimg.com/vi/d5XasoYyk8I/hqdefault.jpg';
    else if (text.substr(0, 1) == '5')
      text = 'https://seeklogo.net/wp-content/uploads/2016/01/github-octocat-logo-vector-download-400x400.jpg';
    else if (text.substr(0, 1) == '6')
      text = 'https://i.ytimg.com/vi/yZQ18hAZMfA/hqdefault.jpg';
    else if (text.substr(0, 1) == '7')
      text = 'https://vignette2.wikia.nocookie.net/rickandmorty/images/5/5a/Evil_Morty_Close-Up.png';
    else if (text.substr(0, 1) == '8')
      text = 'https://vignette.wikia.nocookie.net/rickandmorty/images/4/4c/Screen_Shot_2017-12-29_at_4.48.33_PM.png';
    else if (text.substr(0, 1) == '9')
      text = 'https://i.pinimg.com/736x/fd/69/87/fd698766b20d78fa88fe775546882226--beer-cans-product-packaging.jpg';
    
    log('going to try and fetch this url: ' + text)
    
    //kick of the registration
    let register = img.RegisterImage(text);
    
    input.visible = false;
    startButton.buttonText = 'Waiting...';
    startButton.buttonColor = Color4.Yellow();
    startButton.Init();

    //create a container to hold the image
    let image = new UIContainerRect(canvas);
    lastImage = image;
    image.color = Color4.White();
    let adapter = new demo.AlignAdapter();
    adapter.AdaptShape(demo.UIAnchor.CENTER, image);
    
    //set some loading text
    let loadingText = new UIText(image);
    loadingText.value = 'loading...';
    loadingText.color = Color4.Black();
    adapter.AdaptShape(demo.UIAnchor.CENTER, loadingText);

    //wait until we have the token
    let token = await register;
    log('finished token: ' + token.token);

    //check the status of the image
    let success = await img.ImageStatus(token);
    log('finished status: ' + success);

    if (success)
    {
      //now submit the 
      let result = await img.FetchImage(
        image,
        token,
        4
      );
  
      startButton.buttonText = "Finished!";
      startButton.buttonColor = Color4.Gray();
      startButton.Init();
  
      loadingText.value = '';
      image.color = Color4.Clear();
      
      if (result)
        result.visible = true;
      else
        log('invalid image result');
    }
    else
    {
      log('unable to retrieve image');
    } 

    //reset input / start button
    Reset();
  }
);