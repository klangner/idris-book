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


addItem : (store : DataStore) -> SchemaType (schema store) ->  DataStore
addItem (MkData schema' size' items') item = MkData schema' _ (addLast items')
    where
        addLast : Vect n (SchemaType schema') -> Vect (S n) (SchemaType schema')
        addLast [] = [item]
        addLast (d :: ds) = d :: addLast ds


getItem : (store : DataStore) -> Integer -> Maybe (SchemaType (schema store))
getItem store pos = map (\idx => index idx (items store)) (integerToFin pos (size store))


display : SchemaType schema -> String
display {schema = SString} item = "\"" ++ item ++ "\""
display {schema = SInt} item = show item
display {schema = x .+. y} (iteml, itemr) = (display iteml) ++ ", " ++ (display itemr)


data Command : Schema -> Type where
    SetSchema : (newSchema : Schema) -> Command schema
    Add : SchemaType schema -> Command schema
    Get : Integer -> Command schema
    Quit: Command schema


parseSchema : List String -> Maybe Schema
parseSchema ("Int" :: xs)
    = case xs of
        [] => Just SInt
        _  => case parseSchema xs of
            Nothing => Nothing
            Just xs_sch => Just (SInt .+. xs_sch)
parseSchema ("String" :: xs)
    = case xs of
        [] => Just SString
        _  => case parseSchema xs of
            Nothing => Nothing
            Just xs_sch => Just (SString .+. xs_sch)
parseSchema _ = Nothing


parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
    where
        getQuoted : List Char -> Maybe (String, String)
        getQuoted ('"' :: xs) = case span (/= '"') xs of
                                (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                _ => Nothing
        getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of
                                ("", rest)  => Nothing
                                (num, rest) => Just (cast num, rest)
parsePrefix (s1 .+. s2) input = case parsePrefix s1 input of
                                    Nothing => Nothing
                                    Just (lval, rest) => case parsePrefix s2 (ltrim rest) of
                                                            Nothing => Nothing
                                                            Just (rval, rest') => Just ((lval, rval), rest')


parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema str =
    case parsePrefix schema str of
        Just (res, "") => Just res
        Just _  => Nothing
        Nothing => Nothing


parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand _      "schema" args = SetSchema <$> (parseSchema (words args))
parseCommand schema "add"    args = Add <$> (parseBySchema schema args)
parseCommand _      "get"    val  = Get <$> (parseInteger val)
parseCommand _      "quit"   _    = Just Quit
parseCommand _      _        _    = Nothing


executeCommand : (store : DataStore) -> Command (schema store) -> Maybe (String, DataStore)
executeCommand store (SetSchema newSchema) = case size store of
                                                Z => Just ("New schema set", (MkData newSchema _ []))
                                                _ => Nothing
executeCommand store (Add item) = Just ("ID: " ++ cast (size store) ++ "\n", addItem store item)
executeCommand store (Get idx)  = case getItem store idx of
    Just item => Just (display item ++ "\n", store)
    Nothing   => Just ("Item " ++ show idx ++ " not found\n", store)
executeCommand  _ Quit          = Nothing


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store cmd =
    let (action, params) = break (== ' ') cmd
    in case parseCommand (schema store) action (ltrim params) of
        Just cmd => executeCommand store cmd
        Nothing => Just ("Wrong command\n", store)


main : IO ()
main = replWith ds "Command: " processInput
    where
        ds = MkData (SString .+. SInt) _ []
