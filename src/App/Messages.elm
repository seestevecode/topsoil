module App.Messages exposing (..)

import Domain.Types exposing (Coord)


type Msg
    = Undo
    | NoOp
    | Harvest Coord
    | PlaceTokenOnBoard Coord
