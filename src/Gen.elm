module Gen exposing
    ( bonusGenerator
    , contentGenerator
    , initCoordsGenerator
    , initTokensGenerator
    , queueGenerator
    , tokenGenerator
    )

import Constants as Const
import Random
import Random.List
import Types exposing (..)


queueGenerator : Int -> Random.Generator (List Content)
queueGenerator score =
    Random.list Const.dimensions.queueSize (contentGenerator score)


initCoordsGenerator : Random.Generator (List Coord)
initCoordsGenerator =
    Random.map (List.take Const.initialTokenCount) <| Random.List.shuffle Const.coords


initTokensGenerator : Int -> Random.Generator (List Content)
initTokensGenerator score =
    Random.list Const.initialTokenCount (contentGenerator score)


contentGenerator : Int -> Random.Generator Content
contentGenerator score =
    Random.map2 Plant (tokenGenerator score) bonusGenerator


tokenGenerator : Int -> Random.Generator Token
tokenGenerator score =
    if score < Const.tokenThreshold.take2 then
        Random.uniform Standard1 <| List.take 2 nextTokens

    else if score < Const.tokenThreshold.take3 then
        Random.uniform Standard1 <| List.take 3 nextTokens

    else if score < Const.tokenThreshold.take4 then
        Random.uniform Standard1 <| List.take 4 nextTokens

    else if score < Const.tokenThreshold.take5 then
        Random.uniform Standard1 <| List.take 5 nextTokens

    else
        Random.uniform Standard1 nextTokens


bonusGenerator : Random.Generator Bonus
bonusGenerator =
    Random.weighted ( Const.bonusWeight.yes, Bonus ) [ ( Const.bonusWeight.no, NoBonus ) ]


nextTokens : List Token
nextTokens =
    [ Standard2, Standard3, Growing1 2, Growing2 3, Growing3 5, Disappearing 3 ]
