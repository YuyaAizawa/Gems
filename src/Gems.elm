module Gems exposing (main)

import Array
import Array2 exposing (Array2)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, div, text, br, span)
import Html.Attributes as Attr
import Json.Decode as Decode
import Process
import Random
import Task
import Time



-- MODEL

width = 6
height = 15

type alias Model =
  { field : Array2 Gem
  , fallingGem : Dango
  , fallingPos : Pos
  , fallingStop : Bool
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

emptyField =
  Array2.repeat width height Void

initialModel : Model
initialModel =
  { field = emptyField
  , fallingGem =
    { top = Void
    , middle = Void
    , bottom = Void
    }
  , fallingPos = entrance
  , fallingStop = False
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
  = Fall
  | Key Direction
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
        Fall ->
          let
            { x, y } = model.fallingPos
          in
            case model.field |> Array2.get x (y - 1) of
              Just Void ->
                ( { model | fallingPos = { x = x, y = y - 1 } }
                , Cmd.none
                )

              _ ->
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

        Key Down ->
          ( model |> drop
          , lineCheck
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
                |> Array2.toList
                |> List.all ((==) Void)
                |> not

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

lineCheck : Cmd Msg
lineCheck =
  Task.succeed ()
    |> Task.perform (\_ -> LineCheck)

reload : Cmd Msg
reload =
  Random.generate Reload dangoGenerator

waitToCollapse : Cmd Msg
waitToCollapse =
  Process.sleep 500
    |> Task.perform (\_ -> Collapsed)

left : Model -> Model
left model =
  let
    { x, y } = model.fallingPos
  in
    model |> move (x - 1) y

right : Model -> Model
right model =
  let
    { x, y } = model.fallingPos
  in
    model |> move (x + 1) y

drop : Model -> Model
drop model =
  let
    { x, y } = model.fallingPos
    { top, middle, bottom } = model.fallingGem

    bottomHeight =
      model.field
        |> Array2.toIndexedList
        |> List.filterMap (\( ( x_, y_ ), g ) ->
          if x_ == x && g == Void
          then Just y_
          else Nothing
        )
        |> List.minimum
        |> Maybe.withDefault entrance.y

    field =
      model.field
        |> Array2.set x (bottomHeight + 0) bottom
        |> Array2.set x (bottomHeight + 1) middle
        |> Array2.set x (bottomHeight + 2) top
  in
    { model | field = field, fallingPos = { x = x, y = bottomHeight } }

reloadAndPop : Dango -> Model -> Model
reloadAndPop dango model =
  { model
  | fallingGem = dango
  , fallingPos = entrance
  , fallingStop = False
  , state =
      case model.field |> Array2.get entrance.x entrance.y of
        Just Void -> Palying
        _ -> GameOver
  }

checkLine : Model -> Model
checkLine model =
  let
    helper (x0, y0) (x1, y1) (x2, y2) array2 =
      case
        ( model.field |> Array2.get x0 y0
        , model.field |> Array2.get x1 y1
        , model.field |> Array2.get x2 y2
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

    collecting =
      emptyField
        |> Array2.toIndexedList
        |> List.map Tuple.first
        |> List.foldl (\(x, y) array2 ->
          array2
            |> helper (x + 0, y    ) (x + 1, y    ) (x + 2, y    )
            |> helper (x    , y + 0) (x    , y + 1) (x    , y + 2)
            |> helper (x + 0, y + 0) (x + 1, y + 1) (x + 2, y + 2)
            |> helper (x + 2, y + 0) (x + 1, y + 1) (x + 0, y + 2)
        ) emptyField

    field =
      collecting
        |> Array2.toIndexedList
        |> List.foldl (\( ( x, y ), g ) field_ ->
          case g of
            Gem _ ->
              field_ |> Array2.set x y Void

            Void ->
              field_
        ) model.field
  in
    { model
    | collecting = collecting
    , field = field
    , fallingStop = True
    }

collapse : Model -> Model
collapse model =
  let
    acc =
      { height = Array.repeat width 0
      , field = emptyField
      }

    collapsed =
      model.field
        |> Array2.toIndexedList
        |> List.foldl (\( ( x, _ ), gem ) acc_ ->
          case gem of
            Gem _ ->
              let
                h =
                  acc_.height
                    |> Array.get x
                    |> Maybe.withDefault 0
              in
                { height =
                    acc_.height
                      |> Array.set x (h + 1)

                , field =
                    acc_.field
                      |> Array2.set x h gem
                }

            Void ->
              acc_
        ) acc
        |> .field
  in
    { model
    | field = collapsed
    , collecting = emptyField
    }

reset : Model -> Model
reset _ =
  initialModel

dangoGenerator : Random.Generator Dango
dangoGenerator =
  let
    helper =
      Random.int 0 3
        |> Random.map Gem
  in
    Random.map3 Dango helper helper helper

move : Int -> Int -> Model -> Model
move x y model =
  model.field
    |> Array2.get x y
    |> (\maybeGem ->
      case maybeGem of
        Just Void ->
          { model | fallingPos = { x = x, y = y} }

        _ -> model
    )



-- VIEW

view : Model -> Html Msg
view model =
  let
    posX = model.fallingPos.x
    posY = model.fallingPos.y
    { top, middle, bottom } = model.fallingGem
  in
    model.field
      |> Array2.set posX (posY + 0) bottom
      |> Array2.set posX (posY + 1) middle
      |> Array2.set posX (posY + 2) top
      |> Array2.indexedMap (\x y fromField ->
        let
          cell =
            case model.collecting |> Array2.get x y of
              Just (Gem fromCollecting) ->
                { color = "red", gem = Gem fromCollecting }
              _ ->
                { color = "black", gem = fromField }
        in
          span
          [ Attr.style "color" cell.color ]
          [ text <| String.fromChar <| gemToChar <| cell.gem ]
      )
      |> Array2.toListByRow
      |> List.reverse
      |> List.map (span [])
      |> List.intersperse (br[][])
      |> div []
      |> List.singleton
      |> (\f -> text (if model.state == GameOver then "GameOver" else "")::f)
      |> div [Attr.id "gems"]

gemToChar gem =
  case gem of
    Gem 0 -> '0'
    Gem 1 -> '1'
    Gem 2 -> '2'
    Gem 3 -> '3'
    Void -> '.'
    _ -> '/'



-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    collectNone =
      model.collecting
        |> Array2.toList
        |> List.all ((==) Void)

    pushIf bool content list =
      if bool
      then content :: list
      else list

    subs =
      []
        |> pushIf collectNone (onKeyDown keyDecoder)
        |> pushIf (not model.fallingStop) (Time.every 1000 (\_ -> Fall))
  in
    Sub.batch subs

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
  { init = \() -> ( initialModel, reload )
  , view = view
  , update = update
  , subscriptions = subscriptions
  }



-- UTILS --
