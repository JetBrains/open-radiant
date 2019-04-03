-- https://en.wikipedia.org/wiki/BMP_file_format

{- From https://package.elm-lang.org/packages/justgook/elm-image-encode/latest/ -}

{-
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   | Offset | Size |  Hex Value  |              Value             |                                    Description                                    |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |                                                                    BMP Header                                                                    |
   +--------------------------------------------------------------------------------------------------------------------------------------------------+
   |   0h   |   2  |    42 4D    |              "BM"              |                                ID field (42h, 4Dh)                                |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   2h   |   4  | 46 00 00 00 |        70 bytes (54+16)        |                                Size of the BMP file                               |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   6h   |   2  |    00 00    |             Unused             |                                Application specific                               |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   8h   |   2  |    00 00    |             Unused             |                                Application specific                               |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   Ah   |   4  | 36 00 00 00 |        54 bytes (14+40)        |              Offset where the pixel array (bitmap data) can be found              |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |                                                                    DIB Header                                                                    |
   +--------------------------------------------------------------------------------------------------------------------------------------------------+
   |   Eh   |   4  | 28 00 00 00 |            40 bytes            |                Number of bytes in the DIB header (from this point)                |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   12h  |   4  | 02 00 00 00 | 2 pixels (left to right order) |                           Width of the bitmap in pixels                           |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   16h  |   4  | 02 00 00 00 | 2 pixels (bottom to top order) |      Height of the bitmap in pixels. Positive for bottom to top pixel order.      |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   1Ah  |   2  |    01 00    |             1 plane            |                         Number of color planes being used                         |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   1Ch  |   2  |    18 00    |             24 bits            |                              Number of bits per pixel                             |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   1Eh  |   4  | 00 00 00 00 |                0               |                      BI_RGB, no pixel array compression used                      |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   22h  |   4  | 10 00 00 00 |            16 bytes            |                  Size of the raw bitmap data (including padding)                  |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   26h  |   4  | 13 0B 00 00 |  2835 pixels/metre horizontal  | Print resolution of the image, 72 DPI × 39.3701 inches per metre yields 2834.6472 |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   2Ah  |   4  | 13 0B 00 00 |   2835 pixels/metre vertical   |                                                                                   |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   2Eh  |   4  | 00 00 00 00 |            0 colors            |                          Number of colors in the palette                          |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   32h  |   4  | 00 00 00 00 |       0 important colors       |                          0 means all colors are important                         |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |                                                        Start of pixel array (bitmap data)                                                        |
   +--------------------------------------------------------------------------------------------------------------------------------------------------+
   |   36h  |   3  |   00 00 FF  |             0 0 255            |                                  Red, Pixel (0,1)                                 |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   39h  |   3  |   FF FF FF  |           255 255 255          |                                 White, Pixel (1,1)                                |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   3Ch  |   2  |    00 00    |               0 0              |          Padding for 4 byte alignment (could be a value other than zero)          |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   3Eh  |   3  |   FF 00 00  |             255 0 0            |                                 Blue, Pixel (0,0)                                 |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   41h  |   3  |   00 FF 00  |             0 255 0            |                                 Green, Pixel (1,0)                                |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
   |   44h  |   2  |    00 00    |               0 0              |          Padding for 4 byte alignment (could be a value other than zero)          |
   +--------+------+-------------+--------------------------------+-----------------------------------------------------------------------------------+
-}
-- https://stackoverflow.com/questions/14963182/how-to-convert-last-4-bytes-in-an-array-to-an-integer


module Image.BMP exposing (encode24, encode24With)

{-|
# BMP image creating
@docs encode24, encode24With
-}

import Algorithm.Base64.Image exposing (Order(..), Options, defaultOptions)
import BinaryBase64
import Bitwise exposing (and, shiftRightBy)


{-| ##Eexample
shortcut for [`encode24With`](#encode24With) with [`defaultOptions`](#defaultOptions)
    encode24With :
        Width
        -> Height
        -> List Pixels
        -> Base64String
-}
encode24 : Int -> Int -> List Int -> String
encode24 w h pixels =
    encode24With w h pixels defaultOptions


{-|
    encode24With :
        Width
        -> Height
        -> List Pixels
        -> Options
        -> Base64String
-}
encode24With : Int -> Int -> List Int -> Options a -> String
encode24With w h pixels { defaultColor, order } =
    let
        result =
            [ 0x42, 0x4D ]
                ++ int32LittleEndian (54 + (4 - (modBy 4 (w * 3)) * h))
                ++ [ 0x00
                   , 0x00
                   , 0x00
                   , 0x00
                   , 0x36
                   , 0x00
                   , 0x00
                   , 0x00
                   , 0x28
                   , 0x00
                   , 0x00
                   , 0x00
                   ]
                ++ int32LittleEndian w
                ++ int32LittleEndian h
                ++ [ 0x01
                   , 0x00
                   , 0x18
                   , 0x00
                   , 0x00
                   , 0x00
                   , 0x00
                   , 0x00
                   , 0x10
                   , 0x00
                   , 0x00
                   , 0x00
                   , 0x13
                   , 0x0B
                   , 0x00
                   , 0x00
                   , 0x13
                   , 0x0B
                   , 0x00
                   , 0x00
                   , 0x00
                   , 0x00
                   , 0x00
                   , 0x00
                   , 0x00
                   , 0x00
                   , 0x00
                   , 0x00
                   ]
                ++ (case order of
                        RightUp ->
                            (pixels ++ List.repeat (w * h - List.length pixels) defaultColor)
                                |> lineFolderRight w []

                        RightDown ->
                            (List.repeat (w * h - List.length pixels) defaultColor ++ List.reverse pixels)
                                |> lineFolderLeft w []

                        LeftUp ->
                            (pixels ++ List.repeat (w * h - List.length pixels) defaultColor)
                                |> lineFolderLeft w []

                        LeftDown ->
                            (List.repeat (w * h - List.length pixels) defaultColor ++ List.reverse pixels)
                                |> lineFolderRight w []
                   )

        good =
            result
                |> BinaryBase64.encode
    in
        "data:image/bmp;base64," ++ good


colorAdder : Int -> List Int -> List Int
colorAdder a acc =
    acc ++ int24LittleEndian a


lineFolderLeft : Int -> List Int -> List Int -> List Int
lineFolderLeft w acc xs =
    lineFolder True w acc xs


lineFolderRight : Int -> List Int -> List Int -> List Int
lineFolderRight w acc xs =
    lineFolder False w acc xs


lineFolder : Bool -> Int -> List Int -> List Int -> List Int
lineFolder reverse w acc xs =
    let
        folding =
            if reverse then
                List.foldr
            else
                List.foldl

        lineParser line =
            -- [e1] -> 3 bytes -> add 1 to get a multiple of 4
            -- [e1,e2] -> 6 bytes -> add 2 to get a multiple of 4
            -- [e1,e2,e3] -> 9 bytes -> add 3 to get a multiple of 4
            -- [e1,e2,e3,e4] -> 12 bytes -> add 0 to get a multiple of 4
            folding colorAdder [] line ++ List.repeat (modBy 4 <| List.length line) 0x00
    in
        case ( List.take w xs, List.drop w xs ) of
            ( [], [] ) ->
                acc

            ( line, [] ) ->
                acc ++ lineParser line

            ( line, rest ) ->
                lineFolder reverse w (acc ++ lineParser line) rest


int24LittleEndian : Int -> List Int
int24LittleEndian num =
    [ and num 0xFF
    , shiftRightBy 8 (and num 0xFF00)
    , shiftRightBy 16 (and num 0x00FF0000)
    ]


int32LittleEndian : Int -> List Int
int32LittleEndian num =
    [ and num 0xFF
    , shiftRightBy 8 (and num 0xFF00)
    , shiftRightBy 16 (and num 0x00FF0000)
    , shiftRightBy 24 (and num 0xFF000000)
    ]
