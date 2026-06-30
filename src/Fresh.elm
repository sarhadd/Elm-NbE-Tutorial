module Fresh exposing (..)

import Types exposing (..)

-- When a fresh name is needed, take starting name and append tick marks until
-- it is not included in the used names.
freshen : List Name -> Name -> Name
freshen used x =
    if List.member x used then 
        freshen used (nextName x)
    else
        x

nextName : Name -> Name
nextName x = x ++ "'"