module TestStore

import DataStore


testStore : DataStore (SString .+. SString .+. SInt)
testStore = addToStore ("Mercury", "Mariner 10", 1974) $
            addToStore ("Venus", "Venera", 1961) $
            addToStore ("Uranus", "Voyager 2", 1986) $
            addToStore ("Pluto", "New Horizons", 2015) $
            empty


listItems : DataStore schema -> List (SchemaType schema)
listItems input with (storeView input)
    listItems empty | SNil = []
    listItems (addToStore item store) | (SAdd rec)
        = item :: listItems store | rec


filterKeys : (pred : SchemaType val_schema -> Bool) ->
             DataStore (SString .+. val_schema) ->
             List String
filterKeys pred store with (storeView store)
    filterKeys pred empty | SNil = []
    filterKeys pred (addToStore (key, value) store) | (SAdd rec)
        = if pred value
            then key :: filterKeys pred store | rec
            else filterKeys pred store | rec
