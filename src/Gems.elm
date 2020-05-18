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
    { gem : ( Gem, Gem, Gem )
    , pos : ( Int, Int )
    }
  , state : State
  }

type Gem
  = Gem Int
  | Void

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
    { gem = ( Gem 1, Gem 2, Gem 3 )
    , pos = ( 2, height - 3 )
    }
  , state = Palying
  }



-- UPDATE

type Msg
  = Key Direction
  | Reload ( Gem, Gem, Gem )
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
          , Random.generate Reload gemGenerator
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
          , Random.generate Reload gemGenerator
          )

        _ ->
          ( model
          , Cmd.none
          )

reset model =
  initialModel

drop model =
  let
    ( posX, posY ) = model.falling.pos
    ( g0, g1, g2 ) = model.falling.gem
    
    floor =
      List.range 0 (height - 1)
        |> List.filter (\y ->
          model.field
            |> Array2.get posX y
            |> Maybe.withDefault Void
            |> (==) Void
        )
        |> List.head
        |> Maybe.withDefault (height - 3)

    field =
      model.field
        |> Array2.set posX (floor + 0) g0
        |> Array2.set posX (floor + 1) g1
        |> Array2.set posX (floor + 2) g2
  in
    { model | field = field }

move x y model =
  model.field
    |> Array2.get x y
    |> (\maybeGem ->
      case maybeGem of
        Just Void ->
          let
              falling = model.falling
              falling_ = { falling | pos = ( x, y ) }
          in
            { model | falling = falling_ }
        
        _ -> model
    )

left model =
  let
    ( posX, posY ) = model.falling.pos
  in
    model |> move (posX - 1) posY 

right model =
  let
    ( posX, posY ) = model.falling.pos
  in
    model |> move (posX + 1) posY

gemGenerator =
  let
    helper =
      Random.int 0 3
        |> Random.map Gem
  in
    Random.pair helper (Random.pair helper helper)
      |> Random.map (\( g0, ( g1, g2 ) ) -> ( g0, g1, g2 ))
    

reloadAndPop gems model =
  { model
  | falling =
    { gem = gems
    , pos = ( 2, height - 3 )
    }
  , state =
      case model.field |> Array2.get 2 (height - 3) of
        Just Void -> Palying
        _ -> GameOver
  }



-- VIEW

view : Model -> Html Msg
view model =
  let
    ( posX, posY ) = model.falling.pos
    ( g0, g1, g2 ) = model.falling.gem
  in
    model.field
      |> fieldView
      |> Array2.set posX (posY + 0) (g0 |> gemToChar)
      |> Array2.set posX (posY + 1) (g1 |> gemToChar)
      |> Array2.set posX (posY + 2) (g2 |> gemToChar)
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
  { init = \() -> ( initialModel, Random.generate Reload gemGenerator )
  , view = view
  , update = update
  , subscriptions = subscriptions
  }



-- UTILS --
