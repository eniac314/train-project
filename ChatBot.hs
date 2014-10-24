
module ChatBot (BotOutput (Go,Act,Answer,Invalid),BotData,initChatBot,parseInput) where
import qualified Data.Map as Map
import System.IO
import Data.Char
import qualified Data.ByteString as B
import Data.List
----------------------------------------------------------------------------------
{-to export types-}
data BotOutput = Go String | Act (Command,String) | Answer String | Invalid deriving Show

type BotData = (Dico, ThingDic, ThingDic)

data Command = Push | Pull | Open | Close | PutOn | TakeOff | TurnOn |
               TurnOff | Switch | PickUp | PutBack | Use | Give | Look | Read |
               Eat | Kick deriving Show


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
  Preposition | Conjunction | Determiner | Exclamation | Other deriving (Show,Eq)

type Keywords = [String]

type Word = String

type Sentence = String

type Dico = Map.Map Word ([Word],WordClass)

eMap = Map.fromList []

lookAheadLimit = 5

---------------------------------------------------------------------------------------
{-to export-}
initChatBot ::  IO (Dico,ThingDic,ThingDic)
initChatBot = do d1 <- loadDico "DataBase"
                 (d2,d3) <- loadWDics "WorldData"
                 return (d1,d2,d3)

parseInput :: String -> BotData-> BotOutput
parseInput s bD@(dic,thdic,npcDic) = let t = (tokenize (manyT $ finalT dic) $ words.(map toLower) $ s)
                                     in fromResult.parse (goTo bD) $ (extract t)

test = do d <- initChatBot
          return (parseInput "go to the new lady" d)
----------------------------------------------------------------------------------------

{- Tokenizer -}

knownKeys  = ["the","red","monkey","what","moon","hello"] :: Keywords

newtype Tokenizer a = Tokenizer ([Word] -> [(a,[Word])])

tokenize (Tokenizer t) = t

instance Monad Tokenizer where
    return v = Tokenizer (\ts -> [(v,ts)])
    p >>= f  = Tokenizer (\ts -> case (tokenize p) ts of [] -> []
                                                         [(v,out)] -> tokenize (f v) out)
    p >> q   = p >>= (\_ -> q)


isValid :: Dico -> Word -> Tokenizer Token
isValid d s = Tokenizer (\tl -> case (Map.lookup s d) of Nothing -> []
                                                         Just (_,r) -> [((s,r),replace2 (words s) tl)])

failT = Tokenizer (\_ -> [])

(+:+) :: Tokenizer a -> Tokenizer a -> Tokenizer a
p +:+ q = Tokenizer (\inp -> case tokenize p inp of [] -> tokenize q inp
                                                    [(v,out)] -> [(v,out)])

manyT :: Tokenizer a -> Tokenizer [a]
manyT t = many1T t +:+ return []

many1T :: Tokenizer a -> Tokenizer [a]
many1T t = t >>= \v -> manyT t >>= \vs -> return (v:vs)


lookAheadLookUp:: Map.Map Word ([Word], WordClass) -> Tokenizer Token
lookAheadLookUp d = Tokenizer (\tl -> let sl = [intercalate " " $ take n tl | n <- [1..lookAheadLimit]]
                                          go [] = failT
                                          go (x:xs) = (isValid d x) +:+ go xs
                                      in tokenize (go $ reverse sl) $ tl)

itemT :: Tokenizer Token
itemT = Tokenizer (\cs -> case cs of [] -> []
                                     (x:xs) -> [((x,Other), xs)])


finalT d = (lookAheadLookUp d) +:+ itemT

testTokenizer = do (dic,dt,dnpc) <- initChatBot
                   let t = (tokenize (manyT $ finalT dic) $ words.(map toLower) $ "go to the new lady")
                   return (parse (goTo (dic,dt,dnpc)) $ (extract t))


t = replace2 ["hello", "is"] $ words "this is a beautiful hello world"


-----------------------------------------------------------------------------------------
{- File processing -}

{- Dico -}
addLineToDic :: (WordClass, Word, [Word]) -> Dico -> Dico
addLineToDic (wc,w,syn) dic = let synList = (wc,w,("tag_"++w):syn):[(wc,s,(delete s (("tag_"++w):w:syn))) | s <- syn]
                              in foldl' (\d (wc,w,syn) -> Map.insert w (syn,wc) d) dic synList 

addToDic :: [(WordClass, Word, [Word])] -> Dico -> Dico
addToDic ls dic = foldl' (\d l -> addLineToDic l d) dic ls

charToWc "n" = Noun
charToWc "v" = Verb
charToWc "adj" = Adjective
charToWc "adv" = Adverb
charToWc _ = Other

loadDico :: FilePath -> IO Dico
loadDico path = 
  do contents <- readFile path
     let ls = (deleteAllBy (=="")).lines.(map toLower).removeComments $ contents -- tidy up and get lines
         mkEntry l = let (wc:w:syn) = words l in (charToWc wc, remUndSc w, map (\w -> remUndSc w) syn)
         entryList = map mkEntry ls 
     return $ addToDic entryList (eMap)

remUndSc :: [Char] -> [Char]
remUndSc [] = []
remUndSc (c:cs) | c == '_' = ' ':remUndSc cs
                | otherwise = c:remUndSc cs 

{- World Data -}                     

addToTDic :: [(Thing,Actions)] -> ThingDic -> ThingDic
addToTDic ls dic = foldl' (\d (k,v) -> Map.insert k v d) dic ls

addToNpcDic :: [(Thing,Actions)] -> ThingDic -> ThingDic
addToNpcDic ls dic = foldl' (\d (k,v) -> Map.insert k v d) dic ls

loadWDics :: FilePath -> IO (ThingDic,ThingDic)
loadWDics path = do contents <- readFile path
                    let ls = (deleteAllBy (=="")).lines.(map toLower).removeComments $ contents -- tidy up and get lines      
                        (tList,npcList) = partition (\l -> head l == 'T') ls
                    return $ (addToTDic (process tList) eMap, addToNpcDic (process npcList) eMap)              

process :: [String] -> [(String, [String])]                    
process = map ((\(s:ss) -> (head ss,tail ss)).words)

{- Answer Data -}
-----------------------------------------------------------------------------------------
{-Helper-}

deleteAllBy :: (a -> Bool) -> [a] -> [a]
deleteAllBy p = filter (not.p)

dropCom :: [Char] -> [Char]
dropCom "" = ""
dropCom ('*':'/':xs) = xs
dropCom (x:xs) = dropCom xs

removeComments :: [Char] -> [Char]
removeComments "" = ""
removeComments ('/':'/':ys) = removeComments (dropWhile (/='\n') ys)
removeComments ('/':'*':ys) = removeComments (dropCom ys)
removeComments (x:xs)   = x:removeComments xs

-- remove all s1 elements from s2
replace2 s1 s2 = deleteAllBy (\s -> elem s s1) s2


extract [(xs,_)] = xs
extract _ = []

fromResult (Error _) = Invalid
fromResult (Result (a,_)) = a

getTagged :: String -> Dico -> String
getTagged s dic = 
  case  Map.lookup s dic of
  	Nothing -> ""
  	Just (syn,_) -> tail $ dropWhile (\c -> c /= '_') (head syn)

------------------------------------------------------------------------------------

{- Error Handling Parser-}

type Token = (Word,WordClass)

data Result a = Result (a,[Token]) | Error String deriving Show

newtype Parser a = Parser ([Token] -> Result a)

parse (Parser p) = p 

instance Monad Parser where
    return v = Parser (\ts -> Result (v,ts))
    p >>= f  = Parser (\ts -> case (parse p) ts of Error s -> Error s
                                                   Result (v,out) -> parse (f v) out)
    p >> q   = p >>= (\_ -> q)

item :: Parser Token
item = Parser (\cs -> case cs of [] -> Error "Empty token list"
                                 (x:xs) -> Result (x,xs))
failure :: String -> Parser a
failure s = Parser (\cs -> Error ("Failure:"++s))

sat :: (Token -> Bool) -> Parser Token
sat p = item >>= \x -> if p x then return x else failure (show x)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\inp -> case parse p inp of Error s -> parse q inp
                                              Result (v,out) -> Result (v,out))
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = p >>= \v -> many p >>= \vs -> return (v:vs)

mkList :: Parser a -> Parser [a]
mkList p = Parser (\inp -> case parse p inp of Result (a,xs) -> Result ([a],xs)
                                               Error s -> Error s)   

matchVal :: Word -> Dico -> Parser Token
matchVal s d = sat (\(v,k) -> elem v $ retrieve s d)

matchWC :: WordClass -> Parser Token
matchWC s = sat (\(v,k) -> k == s)

matchAct :: Word -> BotData -> Parser Token
matchAct s (d,td,npcd) =
    sat (\(v,k) -> let v' = (getTagged v d) in elem s $ ((retrieve2 v' td) ++ (retrieve2 v' npcd)))

-- add key to synonyms
retrieve s dic = case Map.lookup s dic of Nothing -> []
                                          Just (ss,_) -> s:ss
retrieve2 s dic = case Map.lookup s dic of Nothing -> []
                                           Just ss -> ss

--goTo :: BotData -> Parser BotOutput
--goTo (d,td,npcd) = 
--  do (w1,_) <- matchVal "go" d
--     (w2,_) <- matchWC Noun
--     let w3 = getTagged w2 d
--     return (Go w3)

goTo :: BotData -> Parser BotOutput
goTo bD@(d,td,npcd) = 
  do (w1,_) <- matchVal "go" d
     (w2,_) <- matchAct "go" bD
     return (Go w2)



--action d1 d2 = 