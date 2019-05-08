module Main

import Data.Vect
import Data.String


infixr 5 .+.


data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (s1 .+. s2) = (SchemaType s1, SchemaType s2)


record DataStore where
    constructor MkData
    schema : Schema
    size : Nat
    items : Vect size (SchemaType schema)

initDatastore : Schema -> DataStore
initDatastore schema = MkData schema _ []

addItem : (store : DataStore) -> SchemaType (schema store) ->  DataStore
addItem (MkData schema' size' items') item = MkData schema' _ (addLast items')
    where
        addLast : Vect n (SchemaType schema') -> Vect (S n) (SchemaType schema')
        addLast [] = [item]
        addLast (d :: ds) = d :: addLast ds


getItem : Integer -> (store : DataStore) -> Maybe (SchemaType (schema store))
getItem pos store = map (\idx => index idx (items store)) (integerToFin pos (size store))


data Command = CSchema Schema
             | Add String
             | Get Integer
             | Quit


             {-
parseCommand : String -> String -> Maybe Command
parseCommand "add" item = Just $ Add item
parseCommand "get" val = map (\idx => Get idx) (parseInteger val)
parseCommand "quit" _ = Just (Quit)
parseCommand _ _ = Nothing


executeCommand : Command -> DataStore -> Maybe (String, DataStore)
executeCommand (Add item) ds = Just ("Item added\n", addItem item ds)
executeCommand (Get idx)  ds = case getItem idx ds of
    Just item => Just (item ++ "\n", ds)
    Nothing   => Just ("Item " ++ show idx ++ " not found\n", ds)
executeCommand  Quit      _  = Nothing


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput ds cmd =
    let (action, params) = break (== ' ') cmd
    in case parseCommand action params of
        Just cmd => executeCommand cmd ds
        Nothing => Just ("Wrong command\n", ds)


main : IO ()
main = replWith initDatastore "Command: " processInput

-}
