import qualified Data.Map as Map
import System.IO
import Data.Char
import qualified Data.ByteString as B
import Data.List
----------------------------------------------------------------------------------
{-World interaction types-}

type Thing = String

type Npc = String

type Inventory = [Thing]

type Location = ([Thing],[Npc])

type Actions = [String]

type ThingDic = Map.Map Thing Actions

type NpcDic = Map.Map Npc Actions

data Being = Being {
	id :: Npc,
	belongings :: Inventory,
	speech :: Map.Map Keywords Sentence,
	status :: StatusTree
} deriving Show

data StatusTree = Status String | Node [StatusTree] deriving Show

-----------------------------------------------------------------------------------
{-Syntax analysis types-}

data WordClass = Noun | Verb | Adjective | Adverb | Pronoun |
  Preposition | Conjunction | Determiner | Exclamation | Other deriving Show

type Keywords = [String]

type Word = String

type Sentence = [Word]

type Dico = Map.Map Word ([Word],WordClass)
------------------------------------------------------------------------------------

{- Error Handling Parser-}

type Token = (Word,WordClass)

data Result a = Result [(a,[Token])] | Error String deriving Show

newtype Parser a = Parser ([Token] -> Result a)

parse (Parser p) = p 

instance Monad Parser where
    return v = Parser (\ts -> Result [(v,ts)])
    p >>= f  = Parser (\ts -> case (parse p) ts of Error s -> Error s
                                                   Result [(v,out)] -> parse (f v) out)
    p >> q   = p >>= (\_ -> q)

item :: Parser Token
item = Parser (\cs -> case cs of [] -> Error "Empty token"
                                 (x:xs) -> Result [(x,xs)])
failure :: String -> Parser a
failure s = Parser (\cs -> Error ("Failure:"++s))

sat :: (Token -> Bool) -> Parser Token
sat p = item >>= \x -> if p x then return x else failure (show x)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\inp -> case parse p inp of Error s -> parse q inp
                                              Result [(v,out)] -> Result [(v,out)])
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = p >>= \v -> many p >>= \vs -> return (v:vs)

mkList :: Parser a -> Parser [a]
mkList p = Parser (\inp -> case parse p inp of Result [(a,xs)] -> Result [([a],xs)]
                                               Error s -> Error s)   

----------------------------------------------------------------------------------------

{- Tokenizer -}

knownKeys  = ["the","red","monkey","what","moon","hello"] :: Keywords

tokenizer :: String -> Keywords
tokenizer = filter (\w -> elem w knownKeys).map ((deleteAllBy isPunctuation).map toLower).lines.removeComments

test = tokenizer "Hello, I am the biggest cat on the moon"


-----------------------------------------------------------------------------------------
{- File processing -}

addLineToDic :: (WordClass, Word, [Word]) -> Dico -> Dico
addLineToDic l@(wc,w,syn) dic = let synList = l:[(wc,s,(delete s (w:syn))) | s <- syn]
                            in foldl' (\d (wc,w,syn) -> Map.insert w (syn,wc) d) dic synList 

addToDic :: [(WordClass, Word, [Word])] -> Dico -> Dico
addToDic ls dic = foldl' (\d l -> addLineToDic l d) dic ls

charToWc "N" = Noun
charToWc "V" = Verb
charToWc "Adj" = Adjective
charToWc "Adv" = Adverb
charToWc _ = Other

loadDico path = do contents <- readFile path
                   let ls = map (\l -> let (wc:w:syn) = words l in (charToWc wc, remUndSc w, map (\w -> remUndSc w) syn)) ((deleteAllBy (=="")).lines.removeComments $ contents)
                   return $ addToDic ls (Map.fromList [])

remUndSc :: [Char] -> [Char]
remUndSc [] = []
remUndSc (c:cs) | c == '_' = ' ':remUndSc cs
                | otherwise = c:remUndSc cs 
                     

-----------------------------------------------------------------------------------------
{-Helper-}

deleteAllBy :: (a -> Bool) -> [a] -> [a]
deleteAllBy p [] = []
deleteAllBy p (x:xs) | (p x) = deleteAllBy  p xs
                     | otherwise = x:deleteAllBy p xs

dropCom :: [Char] -> [Char]
dropCom "" = ""
dropCom ('*':'/':xs) = xs
dropCom (x:xs) = dropCom xs

removeComments :: [Char] -> [Char]
removeComments "" = ""
removeComments ('/':'/':ys) = removeComments (dropWhile (/='\n') ys)
removeComments ('/':'*':ys) = removeComments (dropCom ys)
removeComments (x:xs)   = x:removeComments xs