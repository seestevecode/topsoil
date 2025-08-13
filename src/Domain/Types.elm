module Domain.Types exposing (..)

import Set exposing (Set)


type GameState
    = Playing
    | GameOver


type alias Board =
    { grid : Grid
    , queue : ( Content, List Content )
    }


type alias Grid =
    List Cell


type alias Cell =
    { coord : Coord
    , base : Base
    , content : Maybe Content
    }


type alias Coord =
    ( Int, Int )


type Base
    = Base1
    | Base2
    | Base3


type Content
    = Plant Token Bonus
    | Harvester


type Token
    = Standard1
    | Standard2
    | Standard3
    | Growing1 Int
    | Growing2 Int
    | Growing3 Int
    | Grown1
    | Grown2
    | Grown3
    | Disappearing Int


type Bonus
    = Bonus
    | NoBonus


type Direction
    = Above
    | Right
    | Below
    | Left


type alias HarvestInfo =
    { toCheck : Set Coord
    , toHarvest : Set Coord
    , checked : Set Coord
    }
