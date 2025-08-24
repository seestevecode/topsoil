module Utils exposing (..)


applyUntil : (a -> Bool) -> (a -> a) -> a -> a
applyUntil pred step a =
    if pred a then
        a

    else
        applyUntil pred step (step a)
