module Gems exposing (main)

import Array2 exposing (Array2)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, div, text, br)
import Html.Attributes as Attr
import Json.Decode as Decode
import Random



-- MODEL

width = 6
height = 15

type alias Model =
  { field : Array2 Gem
  , falling :
    { gem : Dango
    , pos : Pos
    }
  , state : State
  }

type Gem
  = Gem Int
  | Void

type alias Dango =
  { top : Gem
  , middle : Gem
  , bottom : Gem
  }

type alias Pos =
  { x : Int
  , y : Int
  }

type State
  = Palying
  | GameOver

gemToChar gem =
  case gem of
    Gem 0 -> '0'
    Gem 1 -> '1'
    Gem 2 -> '2'
    Gem 3 -> '3'
    Void -> '.'
    _ -> '/'

initialModel : Model
initialModel =
  { field = Array2.repeat width height Void
  , falling =
    { gem = { top = Void, middle = Void, bottom = Void }
    , pos = entrance
    }
  , state = Palying
  }

entrance : Pos
entrance =
  { x = 2
  , y = height - 3
  }

-- UPDATE

type Msg
  = Key Direction
  | Reload Dango
  | Restart
  | Nop

type Direction
  = Up
  | Down
  | Left
  | Right

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model.state of
    Palying ->
      case msg of
        Key Down ->
          ( model |> drop
          , Random.generate Reload dangoGenerator
          )

        Key Left ->
          ( model |> left
          , Cmd.none
          )

        Key Right ->
          ( model |> right
          , Cmd.none
          )

        Reload gems ->
          ( model
            |> reloadAndPop gems
          , Cmd.none
          )

        _ ->
          ( model
          , Cmd.none
          )

    GameOver ->
      case msg of
        Restart ->
          ( model |> reset
          , Random.generate Reload dangoGenerator
          )

        _ ->
          ( model
          , Cmd.none
          )

reset : Model -> Model
reset _ =
  initialModel

drop : Model -> Model
drop model =
  let
    { x, y } = model.falling.pos
    { top, middle, bottom } = model.falling.gem
    
    floor =
      List.range 0 (height - 1)
        |> List.filter (\y_ ->
          model.field
            |> Array2.get x y_
            |> Maybe.withDefault Void
            |> (==) Void
        )
        |> List.head
        |> Maybe.withDefault entrance.y

    field =
      model.field
        |> Array2.set x (floor + 0) bottom
        |> Array2.set x (floor + 1) middle
        |> Array2.set x (floor + 2) top
  in
    { model | field = field }

move : Int -> Int -> Model -> Model
move x y model =
  model.field
    |> Array2.get x y
    |> (\maybeGem ->
      case maybeGem of
        Just Void ->
          let
              falling = model.falling
              falling_ = { falling | pos = { x = x, y = y } }
          in
            { model | falling = falling_ }
        
        _ -> model
    )

left : Model -> Model
left model =
  let
    { x, y } = model.falling.pos
  in
    model |> move (x - 1) y 

right : Model -> Model
right model =
  let
    { x, y } = model.falling.pos
  in
    model |> move (x + 1) y

dangoGenerator : Random.Generator Dango
dangoGenerator =
  let
    helper =
      Random.int 0 3
        |> Random.map Gem
  in
    Random.map3 Dango helper helper helper

reloadAndPop : Dango -> Model -> Model
reloadAndPop dango model =
  { model
  | falling =
    { gem = dango
    , pos = entrance
    }
  , state =
      case model.field |> Array2.get entrance.x entrance.y of
        Just Void -> Palying
        _ -> GameOver
  }



-- VIEW

view : Model -> Html Msg
view model =
  let
    { x, y } = model.falling.pos
    { top, middle, bottom } = model.falling.gem
  in
    model.field
      |> fieldView
      |> Array2.set x (y + 0) (bottom |> gemToChar)
      |> Array2.set x (y + 1) (middle |> gemToChar)
      |> Array2.set x (y + 2) (top    |> gemToChar)
      |> Array2.toListByRow
      |> List.reverse
      |> List.map (String.fromList >> text)
      |> List.intersperse (br[][])
      |> div []
      |> List.singleton
      |> (\f -> text (if model.state == GameOver then "GameOver" else "")::f)
      |> div [Attr.id "gems"]

fieldView : Array2 Gem -> Array2 Char
fieldView field =
  Array2.map gemToChar field



-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  onKeyDown keyDecoder

keyDecoder : Decode.Decoder Msg
keyDecoder =
  let
    keyToMsg key =
      case key of
        "ArrowUp"    -> Key Up
        "ArrowDown"  -> Key Down
        "ArrowRight" -> Key Right
        "ArrowLeft"  -> Key Left
        "r"          -> Restart
        _            -> Nop

    keyDecoder_ =
      Decode.field "key" Decode.string
  in
    Decode.map keyToMsg keyDecoder_



-- MAIN --

main =
  Browser.element
  { init = \() -> ( initialModel, Random.generate Reload dangoGenerator )
  , view = view
  , update = update
  , subscriptions = subscriptions
  }



-- UTILS --
