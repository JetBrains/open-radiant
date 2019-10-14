module Layer.FSS.Model exposing (..)


type Msg
    = RebuildFss SerializedScene
    --| RebuildOnClient Layer.Index FSS.SerializedScene
    | ChangeRenderMode RenderMode
    | ChangeFaces Faces
    | AlterFaces FacesChange
    | ChangeLightSpeed Int
    | ChangeVignette Vignette
    | ChangeIris Iris
    | AlterAmplitude AmplitudeChange
    | ShiftColor ColorShiftPatch
    | ChangeOpacity Opacity
    | MirrorOn
    | MirrorOff


type RenderMode
    = Triangles
    | Lines
    | PartialLines
    | Points


type alias Mouse = { x: Int, y: Int }
type alias Faces = { x: Int, y: Int }
type alias Clip = { x: Float, y: Float }
type alias Mirror = Float
type alias Amplitude =
    { amplitudeX: Float
    , amplitudeY: Float
    , amplitudeZ: Float
    }
type alias ColorShift =
    { hue: Float
    , saturation: Float
    , brightness: Float
    }
type alias Opacity = Float
type alias FacesChange =
    { xChange: Maybe Int
    , yChange: Maybe Int
    }
type alias AmplitudeChange =
    { xChange: Maybe Float
    , yChange: Maybe Float
    , zChange: Maybe Float
    }
type alias ColorShiftPatch =
    { hueShift: Maybe Float
    , saturationShift: Maybe Float
    , brightnessShift: Maybe Float
    }
type alias Time = Float -- FIXME
type alias LightSpeed = Int
type alias Vignette = Float
type alias Iris = Float


type alias PortModel =
    { renderMode : String
    , amplitude : Amplitude
    , colorShift : ColorShift
    , opacity : Opacity
    , vignette : Vignette
    , iris : Iris
    , faces : Faces
    , mirror : Bool
    , clip : Maybe Clip -- max and min values of X for clipping
    , lightSpeed : LightSpeed
    , shareMesh : Bool
    }


type alias Model =
    { renderMode : RenderMode
    , amplitude : Amplitude
    , colorShift : ColorShift
    , opacity : Opacity
    , vignette : Vignette
    , iris : Iris
    , faces : Faces
    , mirror : Bool
    , clip : Maybe Clip -- max and min values of X for clipping
    , lightSpeed : LightSpeed
    , shareMesh : Bool
    }


type alias SColor =
    { rgba : List Float
    , hex : String
    , opacity : Float
    }


type alias SLight =
    { ambient : SColor
    , diffuse : SColor
    , speed : Float
    , position : List Float
    , ray : List Float
    }


type alias SMaterial =
    { ambient : SColor
    , diffuse : SColor
    , slave : SColor
    }


type alias SPlane =
    { width : Int
    , height : Int
    , triangles : List STriangle
    , vertices : List SVertex
    , segmentWidth : Float
    , sliceHeight : Float
    }


type alias SSide = Float


type alias STriangle =
    { a : SVertex
    , b : SVertex
    , c : SVertex
    , centroid : List Float
    , color : SColor
    , normal : List Float
    , u : List Float
    , v : List Float
    , vertices : List SVertex
    }


type alias SVertex =
    { position : List Float
    , v0 : List Float
    , anchor : List Float
    , time : Float
    , gradient : Float
    }


type alias SMesh =
    { geometry : SPlane
    , material : SMaterial
    , position : List Float
    , side : SSide
    --, depth : Int
    }


type alias SerializedScene =
    { lights : List SLight
    , meshes : List SMesh
    }


-- Base logic

defaultAmplitude : Amplitude
defaultAmplitude = { amplitudeX = 0.3, amplitudeY = 0.3, amplitudeZ = 0.3 }


defaultColorShift : ColorShift
defaultColorShift = { hue = 0.0, saturation = 0.0, brightness = 0.0 }


defaultVignette : Vignette
defaultVignette = 0.0


defaultIris : Iris
defaultIris = 0.07


defaultOpacity : Opacity
defaultOpacity = 1.0


defaultMirror : Mirror
defaultMirror = 0.50001


defaultFaces : Faces
defaultFaces = { x = 17, y = 17 }


defaultLightSpeed : LightSpeed
defaultLightSpeed = 1000


noClip : Clip
noClip = { x = -1, y = -1 }


init : Model
init =
    { faces = defaultFaces
    , renderMode = Triangles
    , amplitude = defaultAmplitude
    , colorShift = defaultColorShift
    , opacity = defaultOpacity
    , vignette = defaultVignette
    , iris = defaultIris
    , mirror = False
    , clip = Nothing -- (-1, -1) -- max and min values of X for clipping
    , lightSpeed = defaultLightSpeed
    , shareMesh = False
    }
