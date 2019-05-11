module Main

import Data.Vect
import Data.String


infixr 5 .+.


data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
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


getAll : (store : DataStore) -> List (SchemaType (schema store))
getAll store = toList (items store)


display : SchemaType schema -> String
display {schema = SString} item = "\"" ++ item ++ "\""
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = x .+. y} (iteml, itemr) = (display iteml) ++ ", " ++ (display itemr)


parseType : String -> Maybe Schema
parseType "Int"     = Just SInt
parseType "String"  = Just SString
parseType "Char"    = Just SChar
parseType _         = Nothing


parseSchema : List String -> Maybe Schema
parseSchema [str] = parseType str
parseSchema (x :: xs) = do
    sl <- parseType x
    sr <- parseSchema xs
    pure (sl .+. sr)


||| Parse next prefix from schema definition
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
parsePrefix SChar input = case length input of
                                Z  => Nothing
                                _ => Just (strHead input, (ltrim . strTail) input)
parsePrefix (s1 .+. s2) input = do (lval, rest) <- parsePrefix s1 input
                                   (rval, rest') <- parsePrefix s2 (ltrim rest)
                                   pure ((lval, rval), rest')


parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema str =
    case parsePrefix schema str of
        Just (res, "") => Just res
        Just _  => Nothing
        Nothing => Nothing


data Command : Schema -> Type where
    SetSchema : (newSchema : Schema) -> Command schema
    Add : SchemaType schema -> Command schema
    Get : Integer -> Command schema
    GetAll : Command schema
    Quit: Command schema


parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand _      "schema" args = SetSchema <$> (parseSchema (words args))
parseCommand schema "add"    args = Add <$> (parseBySchema schema args)
parseCommand _      "get"    val  = case parseInteger val of
                                        Just num => Just (Get num)
                                        _        => Just GetAll
parseCommand _      "quit"   _    = Just Quit
parseCommand _      "q"      _    = Just Quit
parseCommand _      _        _    = Nothing


executeCommand : (store : DataStore) -> Command (schema store) -> Maybe (String, DataStore)
executeCommand store (SetSchema newSchema) = case size store of
                                                Z => Just ("New schema set\n", (MkData newSchema _ []))
                                                _ => Nothing
executeCommand store (Add item) = Just ("ID: " ++ cast (size store) ++ "\n", addItem store item)
executeCommand store (Get idx)  = case getItem store idx of
    Just item => Just (display item ++ "\n", store)
    Nothing   => Just ("Item " ++ show idx ++ " not found\n", store)
executeCommand  store GetAll    = Just (unlines items, store)
    where
        items = display <$> getAll store
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
