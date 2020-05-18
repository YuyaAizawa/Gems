module Gems exposing (main)

import Array2 exposing (Array2)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, div, text, br, span)
import Html.Attributes as Attr
import Json.Decode as Decode
import Process
import Random
import Task



-- MODEL

width = 6
height = 15

type alias Model =
  { field : Array2 Gem
  , falling :
    { gem : Dango
    , pos : Pos
    }
  , collecting : Array2 Gem
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

emptyField =
  Array2.repeat width height Void

initialModel : Model
initialModel =
  { field = emptyField
  , falling =
    { gem = { top = Void, middle = Void, bottom = Void }
    , pos = entrance
    }
  , collecting = emptyField
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
  | LineCheck
  | Collapsed
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
          , lineCheck
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
          ( model |> reloadAndPop gems
          , Cmd.none
          )

        LineCheck ->
          let
            model_ =
              model |> checkLine

            collectAny =
              model_.collecting
                |> Array2.fold
                  (\gem b ->
                    b || case gem of
                      Gem _ -> True
                      Void -> False)
                  False

            cmd =
              if collectAny
              then waitToCollapse
              else reload
          in
            ( model_, cmd )

        Collapsed ->
          ( model |> collapse
          , lineCheck
          )

        _ ->
          ( model
          , Cmd.none
          )

    GameOver ->
      case msg of
        Restart ->
          ( model |> reset
          , reload
          )

        _ ->
          ( model
          , Cmd.none
          )

reload =
  Random.generate Reload dangoGenerator

lineCheck =
  Task.succeed ()
    |> Task.perform (\_ -> LineCheck)

waitToCollapse =
  Process.sleep 500
    |> Task.perform (\_ -> Collapsed)

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

checkLine : Model -> Model
checkLine model =
  let
    field = model.field

    helper (x0, y0) (x1, y1) (x2, y2) array2 =
      case
        ( field |> Array2.get x0 y0
        , field |> Array2.get x1 y1
        , field |> Array2.get x2 y2
        )
      of
        ( Just (Gem k), Just (Gem l), Just (Gem m) ) ->
          if k == l && l == m
          then
            array2
              |> Array2.set x0 y0 (Gem k)
              |> Array2.set x1 y1 (Gem k)
              |> Array2.set x2 y2 (Gem k)
          else
            array2

        _ -> array2

    basePoints =
      List.range 0 (height - 1)
        |> List.concatMap (\y ->
          List.range 0 (width - 1)
            |> List.map (\x -> (x, y))
        )

    collecting =
      basePoints
        |> List.foldl (\(x, y) array2 ->
          array2
            |> helper (x + 0, y) (x + 1, y) (x + 2, y)
            |> helper (x, y + 0) (x, y + 1) (x, y + 2)
            |> helper (x + 0, y + 0) (x + 1, y + 1) (x + 2, y + 2)
            |> helper (x + 2, y + 0) (x + 1, y + 1) (x + 0, y + 2)
        ) emptyField
  in
    { model | collecting = collecting }

collapse : Model -> Model
collapse model =
  let
    collapseColumn x field =
      List.range 0 (height - 1)
        |> List.foldl (\y list ->
          case
            ( model.collecting |> Array2.get x y
            , model.field      |> Array2.get x y
            )
          of
            ( Just Void, Just (Gem g) ) ->
              (Gem g) :: list

            _ ->
              list
        ) []
        |> List.reverse
        |> List.foldl (\gem ( y, f ) ->
          ( y + 1, f |> Array2.set x y gem )
        ) ( 0, field )
        |> Tuple.second

    collapsed =
      List.range 0 (width - 1)
        |> List.foldl collapseColumn emptyField
  in
    { model
    | field = collapsed
    , collecting = emptyField
    }

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
    posX = model.falling.pos.x
    posY = model.falling.pos.y
    { top, middle, bottom } = model.falling.gem
  in
    model.field
      |> fieldView
      |> Array2.set posX (posY + 0) (bottom |> gemToChar)
      |> Array2.set posX (posY + 1) (middle |> gemToChar)
      |> Array2.set posX (posY + 2) (top    |> gemToChar)
      |> Array2.indexedMap (\x y c ->
        let
          span_ =
            case model.collecting |> Array2.get x y of
              Just (Gem _) ->
                span [Attr.style "color" "red"]

              _ ->
                span [Attr.style "color" "black"]
        in
          c
            |> String.fromChar
            |> text
            |> List.singleton
            |> span_
      )
      |> Array2.toListByRow
      |> List.reverse
      |> List.map (span [])
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
