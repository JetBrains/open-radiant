module Noise exposing (PermutationTable, permutationTable, noise4d, noise3d, noise2d)

{-| This is a library to generate simplex noise in Elm.

The code is a port of the [simplex noise JavaScript version](https:--github.com/jwagner/simplex-noise.js) by Jonas Wagner.

## Example usage

    (perm, newSeed) = permutationTable (initialSeed 42) -- generate the permutation table
    noiseValue = noise3d perm 1 1 1

# Permutation tables
@docs permutationTable, PermutationTable

# Genarate noise
@docs noise4d, noise3d, noise2d

-}
import Array exposing (Array)
import Random.Array exposing (shuffle)
import Random
import Bitwise exposing (and)


type alias CornerOffset3D =
  { i1: Int, j1: Int, k1: Int
  , i2: Int, j2: Int, k2: Int
  }


f2 : Float
f2 = 0.5 * ((sqrt 3) - 1)


g2 : Float
g2 = (3 - sqrt 3) / 6


f3 : Float
f3 = 1 / 3


g3 : Float
g3 = 1 / 6


f4 : Float
f4 = ((sqrt 5) - 1) / 4


g4 : Float
g4 = (5 - (sqrt 5)) / 20


get : Array a -> Int -> a
get arr i =
  case (Array.get i arr) of
    Just x -> x
    Nothing -> Debug.todo <| "Error getting item"


reverseArray : Array a -> Array a
reverseArray array =
  Array.toList array |> List.reverse |> Array.fromList


generatePermMod12 : Array Int -> Array Int
generatePermMod12 perm =
      Array.map (\i -> modBy 12 i) perm


grad3 : Array Float
grad3 =
  Array.fromList [
  1, 1, 0, -1, 1, 0, 1, -1, 0,
  -1, -1, 0, 1, 0, 1, -1, 0, 1,
  1, 0, -1, -1, 0, -1, 0, 1, 1,
  0, -1, 1, 0, 1, -1, 0, -1, -1]


grad4 : Array Float
grad4 =
  Array.fromList [0, 1, 1, 1, 0, 1, 1, -1, 0, 1, -1, 1, 0, 1, -1, -1,
                  0, -1, 1, 1, 0, -1, 1, -1, 0, -1, -1, 1, 0, -1, -1, -1,
                  1, 0, 1, 1, 1, 0, 1, -1, 1, 0, -1, 1, 1, 0, -1, -1,
                  -1, 0, 1, 1, -1, 0, 1, -1, -1, 0, -1, 1, -1, 0, -1, -1,
                  1, 1, 0, 1, 1, 1, 0, -1, 1, -1, 0, 1, 1, -1, 0, -1,
                  -1, 1, 0, 1, -1, 1, 0, -1, -1, -1, 0, 1, -1, -1, 0, -1,
                  1, 1, 1, 0, 1, 1, -1, 0, 1, -1, 1, 0, 1, -1, -1, 0,
                  -1, 1, 1, 0, -1, 1, -1, 0, -1, -1, 1, 0, -1, -1, -1, 0]


addUp : List Bool -> Int
addUp  bs = List.foldl (\  b i ->if b then (i + 1) else i) 0 bs


getCornerOffset2d : Float -> Float -> (Int, Int)
getCornerOffset2d x y =
  if (x > y) then
    (1, 0)
  else
    (0, 1)


getCornerOffset3d : Float -> Float -> Float -> CornerOffset3D
getCornerOffset3d x y z =
  if (x >= y) then
    if (y >= z) then
      CornerOffset3D 1 0 0 1 1 0
    else if (x >= z) then
      CornerOffset3D 1 0 0 1 0 1
    else
      CornerOffset3D 0 0 1 1 0 1
  else
    if (y < z) then
      CornerOffset3D 0 0 1 0 1 1
    else if (x < z) then
      CornerOffset3D 0 1 0 0 1 1
    else
      CornerOffset3D 0 1 0 1 1 0


getN2d : Float -> Float -> Int -> Int -> Array Int -> Array Int -> Float
getN2d x y i j perm permMod12 =
  let
    t = 0.5 - x * x - y * y
  in
    if (t < 0) then
      0
    else
      let
        gi = (get permMod12 (i + get perm j)) * 3
        tt = t * t
      in
        tt * tt * ((get grad3 gi ) * x + (get grad3 (gi + 1)) * y)


getN3d : Float -> Float -> Float -> Int -> Int -> Int -> Array Int -> Array Int -> Float
getN3d x y z i j k  perm permMod12 =
  let
    t = 0.6 - x * x - y * y - z * z
  in
    if (t < 0) then
      0
    else
      let
        gi = (get permMod12 (i + get perm (j + get perm k))) * 3
        tt = t * t
      in
        tt * tt * ((get grad3 gi) * x + (get grad3 (gi + 1)) * y + (get grad3 (gi + 2)) * z)


getN4d :  Float -> Float -> Float -> Float -> Int -> Int -> Int -> Int -> Array Int -> Array Int -> Float
getN4d x y z w i j k l  perm permMod12 =
  let
    t = 0.6 - x * x - y * y - z * z - w * w
  in
    if (t < 0) then
      0
    else
      let
        gi = (modBy 32 (get perm i + (get perm j + (get perm k + (get perm l))))) * 4
        tt = t * t
      in
        tt * tt * ((get grad4 gi) * x + (get grad4 (gi + 1)) * y + (get grad4 (gi + 2)) * z + (get grad4 (gi + 3)) * w)


{-|
Permutation table that is needed to generate the noise value.
-}
type alias PermutationTable = {perm: Array Int, permMod12: Array Int}


permGenerattor : Random.Generator (Array Int)
permGenerattor =
  List.range 0 255
    |> Array.fromList
    |> Random.Array.shuffle

{-|
Genrate the permutation tables that are needed to calculate the noise value.
The function takes a seed and returns the table and  a new seed.
-}
permutationTable : Random.Seed -> (PermutationTable, Random.Seed)
permutationTable seed =
  let
    (perm, seed_) =
        Random.step permGenerattor seed
        |> \( list, seed__ ) ->
            ( Array.append list (reverseArray list)
            , seed__
            )
  in
    ({perm= perm, permMod12 = generatePermMod12 perm}, seed_)


{-|
Generates a noise value between `-1` and `1` based on the given x and y value and a seeded permutation table.
Using the same permutation table will always return the same result for the same coordinate.
-}
noise2d : PermutationTable -> Float -> Float -> Float
noise2d {perm, permMod12} xin yin =
  let
    s = (xin + yin) * f2
    i = floor (xin + s)
    j = floor (yin + s)
    t = toFloat (i + j) * g2
    x0_ = (toFloat i) - t
    y0_ = (toFloat j) - t
    x0 = xin - x0_
    y0 = yin - y0_
    (i1, j1) = getCornerOffset2d x0 y0
    x1 = x0 - (toFloat i1) + g2
    y1 = y0 - (toFloat j1) + g2
    x2 = x0 - 1 + 2 * g2
    y2 = y0 - 1 + 2 * g2

    ii = and i 255
    jj = and j 255

    n0 = getN2d x0 y0 ii jj perm permMod12
    n1 = getN2d x1 y1 (ii + i1)  (jj + j1) perm permMod12
    n2 = getN2d x2 y2 (ii + 1)  (jj + 1) perm permMod12
  in
    70 * (n0 + n1 + n2)


{-|
Generates a noise value between `-1` and `1` based on the given x, y and z value and a seeded permutation table.
Using the same permutation table will always return the same result for the same coordinate.
-}
noise3d : PermutationTable -> Float -> Float -> Float -> Float
noise3d {perm, permMod12} xin yin zin =
  let
    s = (xin + yin + zin) * f3
    i = floor (xin + s)
    j = floor (yin + s)
    k = floor (zin + s)
    t =  toFloat (i + j + k) * g3
    x0_ = (toFloat i) - t
    y0_ = (toFloat j) - t
    z0_ = (toFloat k) - t
    x0 = xin - x0_
    y0 = yin - y0_
    z0 = zin - z0_
    {i1, j1, k1, i2, j2, k2} = getCornerOffset3d x0 y0 z0

    x1 = x0 - (toFloat i1) + g3
    y1 = y0 - (toFloat j1) + g3
    z1 = z0 - (toFloat k1) + g3
    x2 = x0 - (toFloat i2) + 2 * g3
    y2 = y0 - (toFloat j2) + 2 * g3
    z2 = z0 - (toFloat k2) + 2 * g3
    x3 = x0 - 1 + 3 * g3
    y3 = y0 - 1 + 3 * g3
    z3 = z0 - 1 + 3 * g3

    ii = and i 255
    jj = and j 255
    kk = and k 255

    n0 = getN3d x0 y0 z0 ii jj kk perm permMod12
    n1 = getN3d x1 y1 z1 (ii + i1) (jj + j1) (kk + k1) perm permMod12
    n2 = getN3d x2 y2 z2 (ii + i2) (jj + j2) (kk + k2)  perm permMod12
    n3 = getN3d x3 y3 z3 (ii + 1) (jj + 1) (kk + 1)  perm permMod12
  in
   32 * (n0 + n1 + n2 + n3)


{-|
Generates a noise value between `-1` and `1` based on the given x, y, z and w value and a seeded permutation table.
Using the same permutation table will always return the same result for the same coordinate.
-}
noise4d : PermutationTable -> Float -> Float -> Float -> Float -> Float
noise4d {perm ,permMod12} x y z w =
  let

    s = (x + y + z + w) * f4
    i = floor (x + s)
    j = floor (y + s)
    k = floor (z + s)
    l = floor (w + s)
    t = toFloat (i + j + k + l) * g4
    x0_ = (toFloat i) - t
    y0_ = (toFloat j) - t
    z0_ = (toFloat k) - t
    w0_ = (toFloat l) - t

    x0 =  (x - x0_)
    y0 =  (y - y0_)
    z0 =  (z - z0_)
    w0 =  (w - w0_)

    rankx = (addUp [x0 > y0, x0 > z0, x0 > w0])
    ranky = (addUp [x0 <= y0, y0 > z0, y0 > z0])
    rankz = (addUp [x0 <= z0, y0 <= z0, z0 > w0])
    rankw = (addUp [x0 <= w0, y0 <= w0, z0 <= w0])

    i1 = if rankx >= 3 then  1 else 0
    j1 = if ranky >= 3 then  1 else 0
    k1 = if rankz >= 3 then  1 else 0
    l1 = if rankw >= 3 then  1 else 0

    i2 = if rankx >= 2 then 1 else 0
    j2 = if ranky >= 2 then 1 else 0
    k2 = if rankz >= 2 then 1 else 0
    l2 = if rankw >= 2 then 1 else 0

    i3 = if rankx >= 1 then 1 else 0
    j3 = if ranky >= 1 then 1 else 0
    k3 = if rankz >= 1 then 1 else 0
    l3 = if rankw >= 1 then 1 else 0

    x1 = x0 - i1 + g4
    y1 = y0 - j1 + g4
    z1 = z0 - k1 + g4
    w1 = w0 - l1 + g4
    x2 = x0 - i2 + 2 * g4
    y2 = y0 - j2 + 2 * g4
    z2 = z0 - k2 + 2 * g4
    w2 = w0 - l2 + 2 * g4
    x3 = x0 - i3 + 3 * g4
    y3 = y0 - j3 + 3 * g4
    z3 = z0 - k3 + 3 * g4
    w3 = w0 - l3 + 3 * g4
    x4 = x0 - 1 + 4 * g4
    y4 = y0 - 1 + 4 * g4
    z4 = z0 - 1 + 4 * g4
    w4 = w0 - 1 + 4 * g4

    ii = and i 255
    jj = and j 255
    kk = and k 255
    ll = and l 255

    n0 = getN4d x0 y0 z0 w0 ii jj kk ll perm permMod12
    n1 = getN4d x1 y1 z1 w1 (ii + i1) (jj + j1) (kk + k1) (ll + l1) perm permMod12
    n2 = getN4d x2 y2 z2 w2 (ii + i2) (jj + j2) (kk + k2) (ll + l2) perm permMod12
    n3 = getN4d x3 y3 z3 w3 (ii + i3) (jj + j3) (kk + k3) (ll + l3) perm permMod12
    n4 = getN4d x4 y4 z4 w4 (ii + 1) (jj + 1) (kk + 1) (ll + 1) perm permMod12
  in
    27 * (n0 + n1 + n2 + n3 + n4)
