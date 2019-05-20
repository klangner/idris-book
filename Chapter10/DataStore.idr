module DataStore

import Data.Vect
import Data.String


infixr 5 .+.


public export
data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

public export
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (s1 .+. s2) = (SchemaType s1, SchemaType s2)

export      -- Export type without constructors
record DataStore (schema : Schema) where
    constructor MkData
    size : Nat
    items : Vect size (SchemaType schema)


export
empty : DataStore schema
empty = MkData _ []


export
addToStore : SchemaType schema -> DataStore schema -> DataStore schema
addToStore item (MkData size' items) = MkData _ (item :: items)


public export
data StoreView : DataStore schema -> Type where
    SNil : StoreView empty
    SAdd : (rec : StoreView store) -> StoreView (addToStore value store)


storeViewHelper : (items : Vect size (SchemaType schema)) ->
                  StoreView (MkData size items)
storeViewHelper [] = SNil
storeViewHelper (val :: xs) = SAdd (storeViewHelper xs)


export
storeView : (store : DataStore schema) -> StoreView store
storeView (MkData size items) = storeViewHelper items


testStore : DataStore (SString .+. SString .+. SInt)
testStore = addToStore ("Mercury", "Mariner 10", 1974) $
            addToStore ("Venus", "Venera", 1961) $
            addToStore ("Uranus", "Voyager 2", 1986) $
            addToStore ("Pluto", "New Horizons", 2015) $
            empty
