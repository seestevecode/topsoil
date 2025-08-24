module Messages exposing (..)

import Types exposing (Coord)


type Msg
    = Undo
    | NoOp
    | Harvest Coord
    | PlaceTokenOnBoard Coord
