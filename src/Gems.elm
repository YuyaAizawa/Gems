module Gems exposing (main)

import Array2 exposing (Array2)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, div, text, br)
import Html.Attributes as Attr
import Json.Decode as Decode


-- MODEL

width = 6
height = 15

type alias Model =
  { field : Array2 Gem
  , falling :
    { gem : ( Gem, Gem, Gem )
    , pos : ( Int, Int )
    }
  }

type alias Gem = Char
void : Gem
void = '.'

initialModel : Model
initialModel =
  { field = Array2.repeat width height void
  , falling =
    { gem = ( 'X', 'X', 'X' )
    , pos = ( 2, height - 3 )
    }
  }



-- UPDATE

type Msg
  = Key Direction
  | Nop

type Direction
  = Up
  | Down
  | Left
  | Right

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    model_ =
      case msg of
        Key Down ->
          model |> drop
        Key Left ->
          model |> left
        Key Right ->
          model |> right

        _ ->
          model
  in
    ( model_, Cmd.none )


drop model =
  let
    ( posX, posY ) = model.falling.pos
    ( g0, g1, g2 ) = model.falling.gem
    
    floor =
      List.range 0 (height - 1)
        |> List.filter (\y ->
          model.field
            |> Array2.get posX y
            |> Maybe.withDefault void
            |> (==) void
        )
        |> List.head
        |> Maybe.withDefault (height - 3)

    field =
      model.field
        |> Array2.set posX (floor + 0) g0
        |> Array2.set posX (floor + 1) g1
        |> Array2.set posX (floor + 2) g2
  in
    { model | field = field, falling = next }

next =
  { gem = ( 'O', 'O', 'X' )
  , pos = ( 2, height - 3 )
  }

move x y model =
  model.field
    |> Array2.get x y
    |> (\maybeGem ->
      case maybeGem of
        Just '.' ->
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
      |> div [Attr.id "gems"]

fieldView : Array2 Gem -> Array2 Char
fieldView field =
  Array2.map gemToChar field

gemToChar gem =
  gem


-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  onKeyDown keyDecoder

keyDecoder : Decode.Decoder Msg
keyDecoder =
  let
    keyToMsg key =
      case key of
        "ArrowUp" -> Key Up
        "ArrowDown" -> Key Down
        "ArrowRight" -> Key Right
        "ArrowLeft" -> Key Left
        _ -> Nop

    keyDecoder_ =
      Decode.field "key" Decode.string
  in
    Decode.map keyToMsg keyDecoder_



-- MAIN --

main =
  Browser.element
  { init = \() -> ( initialModel, Cmd.none )
  , view = view
  , update = update
  , subscriptions = subscriptions
  }



-- UTILS --
