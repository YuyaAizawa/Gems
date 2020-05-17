module Array2 exposing
  ( Array2
  , empty
  , isEmpty
  , width
  , height
  , initialize
  , repeat
  , get
  , set
  , toListByRow
  , map
  )

import Array exposing (Array)

type Array2 a = Array2 (Array a) Int Int

empty : Array2 a
empty =
  Array2 Array.empty 0 0

isEmpty : Array2 a -> Bool
isEmpty (Array2 contents _ _) =
  Array.isEmpty contents

width : Array2 a -> Int
width (Array2 _ width_ _) =
  width_

height : Array2 a -> Int
height (Array2 _ _ height_) =
  height_

initialize : Int -> Int -> (Int -> Int -> a) -> Array2 a
initialize width_ height_ fn =
  let
    fn_ i =
      let
        x = modBy width_ i
        y = (//)  width_ i 
      in
       fn x y

    contents =
      Array.initialize
      (width_ * height_)
      fn_
  in
    Array2 contents width_ height_

repeat : Int -> Int -> a -> Array2 a
repeat width_ height_ e =
  initialize width_ height_ (\_ _ -> e)

get : Int -> Int -> Array2 a -> Maybe a
get x y (Array2 contents width_ height_) =
  if 0 <= x && x < width_ && 0 <= y && y < height_
  then Array.get (y * width_ + x) contents
  else Nothing

set : Int -> Int -> a -> Array2 a -> Array2 a
set x y a (Array2 contents width_ height_) =
  if 0 <= x && x < width_ && 0 <= y && y < height_
  then
    Array2
    (Array.set (y * width_ + x) a contents)
    width_
    height_
  else 
    (Array2 contents width_ height_)

toListByRow : Array2 a -> List (List a)
toListByRow (Array2 contents width_ height_) =
  List.range 0 (height_ - 1)
    |> List.map (\row ->
      contents
        |> Array.slice (row * width_) ((row + 1) * width_)
        |> Array.toList
    )

map : (a -> b) -> Array2 a -> Array2 b
map fn (Array2 contents width_ height_) =
  let
    contents_ =
      Array.map fn contents
  in
    Array2 contents_ width_ height_
    