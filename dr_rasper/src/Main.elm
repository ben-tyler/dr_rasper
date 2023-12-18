module Main exposing (..)

import Browser
import Browser.Events as E
import Color
import Game.TwoD as Game
import Game.TwoD.Camera as Camera
import Game.TwoD.Render as Render exposing (circle, rectangle)
import Html exposing (..)
import Game.TwoD.Render exposing (Renderable)
import Game.TwoD.Render exposing (Float2)
import Platform.Cmd as Cmd
import Json.Decode as D
import Dict exposing (keys)
import Html.Attributes exposing (style)
import Html.Attributes exposing (height)
import Game.TwoD.Shapes
import Pixels exposing (Pixels)
import Map
import Rasper
-- 
-- the injustices in australia
-- war3 rts style in modern setting


type alias MapData = 
    { pos : Float2
    , char : Char
    , isCollision : Bool
    }

type alias NPC = 
    { pos : Float2
    , isCollision : Bool
    }


type GameState = Exploring | Combat Rasper.Model

type alias Model =
    { player : ( Float, Float )
    , enemy : NPC
    , cameraPosition : ( Float, Float)
    , keys : Keys
    , map : List Renderable
    , mapData : List MapData
    , fps : Float
    , gameState : GameState
    }

type alias Keys =
  { up : Bool
  , left : Bool
  , down : Bool
  , right : Bool
  , space : Bool
  }

noKeys : Keys
noKeys =
  Keys False False False False False

updateKeys : Bool -> String -> Keys -> Keys
updateKeys isDown key keys =
  case key of
    " "          -> { keys | space = isDown }
    "ArrowUp"    -> { keys | up    = isDown }
    "ArrowLeft"  -> { keys | left  = isDown }
    "ArrowDown"  -> { keys | down  = isDown }
    "ArrowRight" -> { keys | right = isDown }
    _            -> keys

type Msg
    = Tick Float
    | KeyChanged Bool String
    | CombatRasper Rasper.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    let
        (map, mapdata) =  parseMap Map.map1
            |> List.unzip
    in
    
    ( { player = ( 3, -3 )
      , enemy = NPC ( 6, -6 ) True
      , cameraPosition = ( 3, -3 )
      , keys = noKeys
      , map = map
      , mapData = mapdata
      , fps = 0
      , gameState = Exploring
      }
    , Cmd.none
    )


subs : Model -> Sub Msg
subs m =
    Sub.batch
        [ E.onAnimationFrameDelta Tick
        , E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
        , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CombatRasper m -> 
            case model.gameState of 
                Combat rasp -> 
                    ({model | gameState = Combat (Rasper.update m rasp )}, Cmd.none)

                _ ->
                    (model, Cmd.none)
            
        Tick dt ->
            let
                newFPS = 1000 / dt  -- Calculate FPS based on delta time
                wt = { model | fps = newFPS } 
            in
            ( -- Update the model with the new FPS value
             tick (dt / 1000) wt
            , Cmd.none
            )

        KeyChanged isDown key ->
            ( { model | keys = updateKeys isDown key model.keys }
            , Cmd.none
            )


charToRenderable : Char -> Float2 -> Renderable
charToRenderable char (x, y) =
    case char of
        '1' ->
            Render.shape rectangle { color = Color.green, position = ( x, y ), size = ( 1, 1 ) }

        _ ->
            Render.shape rectangle { color = Color.black, position = ( x, y ), size = ( 1, 1 ) }

charToMapData : Char -> Float2 -> MapData
charToMapData char (x, y) =
    case char of
        '1' ->
            { pos = ( x, y ), char = char, isCollision = False }

        _ ->
            { pos = ( x, y ), char = char, isCollision = True }


parseMap : String -> List (Render.Renderable, MapData)
parseMap mapString =
    let
        rows =
            String.lines mapString

        addRow : Int -> String ->  List (Render.Renderable, MapData)
        addRow rowIdx row =
            let
                colChars =
                    String.toList row
            in
            List.indexedMap
                (\colIdx char ->
                    let
                        pos = (toFloat colIdx, toFloat -rowIdx)
                    in
                    (charToRenderable char pos, charToMapData char pos )
                )
                colChars
    in
    List.indexedMap addRow rows |> List.concat
    


moveByKey : Keys -> (Float, Float) -> (Float, Float) 
moveByKey keys pos = 
    pos
        |> (\ (px, py) -> 
                if keys.left then 
                    (px - 0.1, py)
                else 
                    (px, py)
            )
        |> (\ (px, py) -> 
                if keys.right then 
                    (px + 0.1, py)
                else 
                    (px, py)
            )
        |> (\ (px, py) -> 
                if keys.up then 
                    (px, py + 0.1)
                else 
                    (px, py)
            )
        |> (\ (px, py) -> 
                if keys.down then 
                    (px, py - 0.1)
                else 
                    (px, py)
            )



checkCollision : Float2 -> List MapData -> Bool
checkCollision (x, y) mapData =
    List.any (\data -> 
        let (bx, by) = data.pos
        in x > bx - 0.5 && x < bx + 0.5 && y > by - 0.5 && y < by + 0.5 && data.isCollision
    ) mapData

checkCollisionNpc : Float2 -> List NPC -> Bool
checkCollisionNpc (x, y) npc =
    List.any (\data -> 
        let (bx, by) = data.pos
        in x > bx - 0.5 && x < bx + 0.5 && y > by - 0.5 && y < by + 0.5 && data.isCollision
    ) npc


movePlayer : Model -> Model
movePlayer model = 
    let
        newPosition =
            moveByKey model.keys model.player
    in
    if checkCollision newPosition model.mapData then
        model
    else if checkCollisionNpc newPosition [model.enemy] then 
        { model | gameState = Combat Rasper.initialModel }
    else
        { model | player = newPosition }

moveCamera : Model -> Model
moveCamera model = 
    { model | 
        cameraPosition = 
            moveByKey model.keys model.cameraPosition
    }


handleKeys : Model ->  Model
handleKeys model =
    model 
        |> moveCamera
        |> movePlayer
    
    

tick : Float -> Model -> Model
tick dt model =
    model
        |> handleKeys


renderConfig model = 
    { time = 0
    , camera = Camera.fixedArea (4 * 8) model.cameraPosition
    , size = ( 600, 400 ) 
    }

view : Model -> Html Msg
view model =
    let
        player = 
            Render.shape 
                circle 
                { color = Color.blue
                , position = model.player
                , size = ( 0.5, 0.5 ) 
                }

        enemy = 
            Render.shape 
                circle 
                { color = Color.red
                , position = model.enemy.pos
                , size = ( 0.5, 0.5 ) 
                }

        rendered = 
           [ player, enemy ] ++ model.map
                |> Game.render (renderConfig model)

    in
    div []
        [ div [] [ text <| "FPS: " ++ String.fromFloat model.fps ]  
        , div [ style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "height" "80vh"
            ] 
            [  div 
                [ style "width" "50%"
                , style "height" "50%"
                ] 
                [rendered]
            , case model.gameState of 
                Combat r -> 
                    Rasper.view r |> Html.map CombatRasper

                _ -> 
                    div [] []


            
        -- , div [] [ text <| Debug.toString m ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subs
        }