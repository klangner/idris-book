module Main

import Data.Vect
import Data.String


data DataStore : Type where
    MkData : (size : Nat) ->
             (items : Vect size String) ->
             DataStore

size : DataStore -> Nat
size (MkData size' _) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

initDatastore : DataStore
initDatastore = MkData 0 (fromList [])

addItem : String -> DataStore -> DataStore
addItem item (MkData s ds) = MkData _ (addLast item ds)
    where
        addLast : a -> Vect n a -> Vect (S n) a
        addLast item [] = [item]
        addLast item (d :: ds) = d :: addLast item ds


getItem : Integer -> DataStore -> Maybe String
getItem pos store = map (\idx => index idx (items store)) (integerToFin pos (size store))

        
data Command = Add String
             | Get Integer
             | Quit

             
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
