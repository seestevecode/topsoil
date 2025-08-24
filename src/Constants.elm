module Constants exposing (..)

import List.Extra as ListX
import Types exposing (..)


dimensions : { axisSize : Int, queueSize : Int }
dimensions =
    { axisSize = 4, queueSize = 3 }


axis : List Int
axis =
    List.range 0 (dimensions.axisSize - 1)


coords : List Coord
coords =
    ListX.lift2 Tuple.pair axis axis


terrainScale : Float
terrainScale =
    0.15


initialTokenCount : Int
initialTokenCount =
    6


tokenThreshold : { take2 : Int, take3 : Int, take4 : Int, take5 : Int }
tokenThreshold =
    { take2 = 20, take3 = 50, take4 = 80, take5 = 100 }


bonusWeight : { yes : Float, no : Float }
bonusWeight =
    { yes = 20.0, no = 80.0 }


tokenScores : Token -> Maybe Int
tokenScores token =
    case token of
        Standard1 ->
            Just 1

        Standard2 ->
            Just 1

        Standard3 ->
            Just 1

        Grown1 ->
            Just 3

        Grown2 ->
            Just 6

        Grown3 ->
            Just 15

        Disappearing _ ->
            Just 2

        _ ->
            Nothing


tokenView :
    Token
    ->
        { image : String
        , name : String
        }
tokenView token =
    case token of
        Standard1 ->
            { image = "high-grass", name = "Grass" }

        Standard2 ->
            { image = "daisy", name = "Daisy" }

        Standard3 ->
            { image = "spoted-flower"
            , name = "Spotted flower"
            }

        Growing1 _ ->
            { image = "sesame", name = "Seeds" }

        Growing2 _ ->
            { image = "bud", name = "Bud" }

        Growing3 _ ->
            { image = "bulb", name = "Bulb" }

        Grown1 ->
            { image = "sunflower", name = "Sunflower" }

        Grown2 ->
            { image = "rose", name = "Rose" }

        Grown3 ->
            { image = "viola", name = "Viola" }

        Disappearing _ ->
            { image = "dandelion-flower", name = "Dandelion" }
