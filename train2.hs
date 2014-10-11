import System.Random
import Control.Monad
import GHC.Word
import GHC.Int
import GHC.ForeignPtr
import Data.Bits
import Data.List
import Data.Maybe (fromJust)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.Image as SDLI
import Graphics.UI.SDL.Mixer
import qualified Data.Map as Map
import System.IO



{-# LANGUAGE BangPatterns #-}


osc :: Int -> Int -> Int -> [Int]
osc a b c = cycle $ [a,a+c..b] ++ [b,b-c..a+c]

slow :: Int -> [a] -> [a]
slow 0 xs = xs
slow n [] = []
slow n (x:xs) = go n x [] ++ (slow n xs) where go 0 _ xs = xs
                                               go n x xs = go (n-1) x (x:xs)

delay start stop = let res = 30 - (stop - start)
                   in if (res > 0 && res < 100 ) then res else 0

roll (x:xs) (y:ys) = x-y:roll xs ys 

ext ::  [a] -> [a]
ext [] = []
ext (x:[]) = cycle [x]
ext (x:xs) = x:ext xs

interpolate :: [(Int,Int)] -> [(Int,Int)]
interpolate [] = []
interpolate (x:[]) = [x]
interpolate ((x1,y1):t@(x2,y2):xs) | x1 == x2 = let step = if y1 < y2 then 5 else (-5) in [(x1,y) | y <- [y1,y1+step..(y2-step)]]  ++ interpolate (t:xs)
                                   | y1 == y2 = let step = if x1 < x2 then 5 else (-5) in [(x,y1) | x <- [x1,x1+step..(x2-step)]]  ++ interpolate (t:xs)

-------------------------------------------------------------------------------------------------
{- Graphics -}

type Point = (Double,Double)

getPixel :: Word8 -> Word8 -> Word8 -> SDL.Pixel
getPixel r g b = SDL.Pixel $ (shiftL (fi r) 24 .|. shiftL (fi g) 16 .|. shiftL (fi b) 8 .|. (fi 255)) where 
	fi a = fromIntegral a

pixelsToScreen :: [Point] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
pixelsToScreen xs s p = map (\(x,y) -> SDLP.pixel s (fromIntegral.round $ x) (fromIntegral.round $ y) p) xs

data Entity = Entity {
             name      :: String,
             surface   :: SDL.Surface,
             frameList :: [Maybe SDL.Rect],
             posList   :: [Maybe SDL.Rect],
             audioData :: Sound
             }

data World = World {
	         avatar  :: Entity,
	         land    :: [Entity],
	         fg      :: [Entity],
	         bg      :: [Entity],
	         miscFg  :: [Entity],
	         miscBg  :: [Entity],
	         changes :: Change,
	         screen  :: SDL.Surface,
           canvas  :: Entity
             }

data Change = Change {
           misc    :: [String],
           mouseX  :: Int,
           mouseY  :: Int,
           clicked :: Bool,
           enter   :: Bool,
           usrStr  :: String,
           updated :: Bool,
           mapData  :: [(Int,Int)],
           gridWH   :: (Int,Int)
}

data Sound = Sound {
           effects :: [IO Chunk],
           music :: [Music]
}

makeSound = Sound [] []

buildWorld :: [Entity] -> Entity -> [Entity] -> [Entity] -> [Entity] -> [Entity] -> Change -> SDL.Surface -> Entity -> World
buildWorld l av f b m1 m2 c s ca = World { avatar  = av,
	                                    land    = l,
	                                    fg      = f,
	                                    bg      = b,
	                                    miscFg  = m1,
	                                    miscBg  = m2,
	                                    changes = c,
	                                    screen  = s,
                                      canvas  = ca
                                      } 


updateWorld :: World -> World
updateWorld wo = if ((updated $ changes wo ) == False) then wo   
                 else let w = applyChanges wo in w 



fixCamera :: World -> World
fixCamera w = let (_,(apx,apy)) = getCurrent $ avatar w
                  ((cfx,cfy),_) = getCurrent $ canvas w
                  
                  
                  newFX | s == 0 = cfx
                        | (apx > cfx + 300) && (cfx + (abs s) < (canvasWidth - screenWidth)) = cfx + (abs s) 
                        | (apx < cfx + 45 ) && (cfx - (abs s) > 0) = cfx - (abs s)
                        | otherwise = cfx
                  
                  newFl [] = []
                  newFl (f:fs) = case f of Nothing -> []
                                           Just r -> Just (r {SDL.rectX = newFX}): newFl fs 
                  
              in w {canvas = (canvas w) {frameList = newFl $ frameList (canvas w)}}              
              
              where s = computeSpeed $ avatar w


computeSpeed :: Entity -> Int
computeSpeed i = case posList $ i of (Just a: Just b:rs) -> SDL.rectX b - SDL.rectX a
                                     _ -> 0

applyChanges :: World -> World
applyChanges w |(clicked $ changes w) = let((cfx,cfy),_) = getCurrent $ canvas w 
                                           newW = goTo2 (cfx + (mouseX $ changes w),cfy + (mouseY $ changes w)) w
                                        in newW {changes = (changes newW){clicked = False,updated = False}}
               |otherwise = w

loadImage :: String -> IO SDL.Surface
loadImage filename = SDLI.load filename  >>= SDL.displayFormatAlpha

nextEntity :: Entity -> Entity
nextEntity img = Entity {surface = surface img,
                       frameList = tail $ frameList img,
                       posList = tail $ posList img,
                       name = name img,
                       audioData = audioData img
                     }


makeBg :: SDL.Surface -> String -> Entity
makeBg src name = Entity { name = name,
                          surface = src,
                          frameList = cycle [Nothing],
                          posList = cycle [Just SDL.Rect { SDL.rectX = 0, SDL.rectY = 0, SDL.rectW = 0, SDL.rectH = 0 }],
                          audioData = makeSound
                        }

makeEntity :: SDL.Surface -> (Int,Int) -> ([Int],[Int]) -> ([Int],[Int]) ->String -> Entity
makeEntity src (fw,fh) (fxs,fys) (xs,ys) name = Entity { name = name,
                                                   surface = src,
                                                   frameList = makeRect (fw,fh) fxs fys,
                                                   posList = makeRect (0,0) xs ys,
                                                   audioData = makeSound
                                                   }
  where makeRect _ [] [] = cycle [Nothing]
        makeRect  (w,h) (x:xs) (y:ys) = Just SDL.Rect { SDL.rectX = x,
     	                                                 SDL.rectY = y,
     	                                                 SDL.rectW = w,
     	                                                 SDL.rectH = h } : makeRect (w,h) xs ys


displayImg :: Entity -> SDL.Surface -> IO Entity
displayImg src dest = do SDL.blitSurface (surface src) (head.frameList $ src)  dest (head.posList $ src)
                         return (nextEntity src)


drawSurfaces :: [Entity] -> SDL.Surface -> IO [Entity] 
drawSurfaces xs dest = sequence $ map (\im -> displayImg im dest) xs


drawWorld :: World -> IO World
drawWorld wo = let w = fixCamera wo in 
              do  l         <- drawSurfaces (land w) (surface $ canvas w)
                  newBg     <- drawSurfaces (bg w) (surface $ canvas w)
                  newMiscBg <- drawSurfaces (miscBg w) (surface $ canvas w)
                  av        <- displayImg (avatar w) (surface $ canvas w)
                  newMiscFg <- drawSurfaces (miscFg w) (surface $ canvas w)
                  newFg     <- drawSurfaces (fg w) (surface $ canvas w)
                  newCan    <- displayImg (canvas w) (screen w)

                  
                  let newWorld = World { avatar  = av,
                                       land    = l,
                                       fg      = newFg,
                                       bg      = newBg,
                                       miscFg  = newMiscFg,
                                       miscBg  = newMiscBg,
                                       changes = changes w,
                                       screen  = screen w,
                                       canvas  = newCan
                                       }
                  return newWorld 



rectList2Coord :: [Maybe SDL.Rect] -> ([Int],[Int])
rectList2Coord (x:xs) =  case x of Nothing -> ([],[])
                                   Just r  -> let newX = SDL.rectX r
                                                  newY = SDL.rectY r
                                                  (newXs,newYs) = rectList2Coord xs
                                               in (newX:newXs,newY:newYs)

getCurrent :: Entity -> ((Int,Int),(Int,Int))
getCurrent i = let frames = frameList i
                   pos    = posList i
               in case (frames, pos) of ((Just f:fs),(Just p:ps)) -> ((SDL.rectX f,SDL.rectY f),(SDL.rectX p,SDL.rectY p))
                                        _                         -> ((0,0),(0,0))



makePosRect :: [(Int,Int)] -> [Maybe SDL.Rect]
makePosRect [] = []
makePosRect (x:xs) = Just SDL.Rect {SDL.rectX = fst x,
                                    SDL.rectY = snd x,
                                    SDL.rectH = 0,
                                    SDL.rectW = 0
                                   }:makePosRect xs

makeFrameRect :: [(Int,Int)] -> Int -> Int -> [Maybe SDL.Rect]
makeFrameRect [] _ _= []
makeFrameRect (x:xs) h w = Just SDL.Rect {SDL.rectX = fst x,
                                    SDL.rectY = snd x,
                                    SDL.rectH = h,
                                    SDL.rectW = w
                                   }:makeFrameRect xs h w



goTo :: (Int,Int) -> World -> World
goTo (x,y) w = let (_,(apx,apy)) = getCurrent $ avatar w
                   ((cfx,cfy),_) = getCurrent $ canvas w
                   spriteW = div (SDL.surfaceGetWidth $ surface $ avatar w) nbrFrameSprite
                   spriteH = SDL.surfaceGetHeight $ surface $ avatar w
                   destX = x+cfx-(quot spriteW 2)
                   destY = y+cfy-(quot spriteH 2)
                   
                   hori | destX > apx = zip [apx,apx+5..destX] (ext [apy])
                        | destX < apx = zip [apx,apx-5..destX] (ext [apy])
                        | otherwise = []
                   
                   hframe | destX > apx = zip (take (length hori) $ slow 10 $ cycle [960,1280..1600]) (ext [0])
                          | destX < apx = zip (take (length hori) $ slow 10 $ cycle [0,320..640]) (ext [0])
                          | otherwise = []
                   
                   verti | destY > apy = zip (ext [destX]) [apy,apy+5..destY]
                         | destY < apy = zip (ext [destX]) [apy,apy-5..destY]
                         | otherwise = []

                   vframe | destY > apy = zip (take (length verti) $ ext [1920]) (ext [0])
                          | destY < apy = zip (take (length verti) $ ext [2240]) (ext [0])
                          | otherwise = []

                   path = makePosRect (ext $ hori++verti)
                   frames = makeFrameRect (ext $ hframe++vframe) spriteH spriteW
               in w {avatar = (avatar w){ posList = path, frameList = frames}}

pixToMap :: World -> (Int,Int) -> (Int,Int)
pixToMap w (x,y) = let (gW,gH) = gridWH.changes $ w
                       [dx,dy,dGw,dGh] = map fromIntegral [x,y,gW,gH] 
                   in  (floor (dx/dGw),floor (dy/dGh))

mapToPix :: World -> (Int,Int) -> (Int,Int)
mapToPix  w (x,y) = let (gW,gH) = gridWH.changes $ w
                        [dx,dy,dGw,dGh] = map fromIntegral [x,y,gW,gH] 
                    in  (dx*dGw,dy*dGh)                   

avatarCenter :: World -> (Int,Int)
avatarCenter w = let (_,(apx,apy)) = getCurrent $ avatar w
                     spriteW = div (SDL.surfaceGetWidth $ surface $ avatar w) nbrFrameSprite
                     spriteH = SDL.surfaceGetHeight $ surface $ avatar w
                 in (apx+(quot spriteW 2),apy+(quot spriteH 2))


fixPath :: World -> [(Int,Int)] -> [(Int,Int)]
fixPath w xs = let spriteW = div (SDL.surfaceGetWidth $ surface $ avatar w) nbrFrameSprite
                   spriteH = SDL.surfaceGetHeight $ surface $ avatar w

                   go [] = []
                   go ((x,y):xs) = (x-(quot spriteW 2),y-(quot spriteH 2)):go xs
              in go xs



makeFrameList w xs = let upFrameList = zip (ext [1920]) (ext [0])
                         downFrameList = zip (ext [2240]) (ext [0])
                         leftFrameList = zip (slow 10 $ cycle [0,320..640]) (ext [0])
                         rightFrameList = zip (slow 10 $ cycle [960,1280..1600]) (ext [0])

                         spriteW = div (SDL.surfaceGetWidth $ surface $ avatar w) nbrFrameSprite
                         spriteH = SDL.surfaceGetHeight $ surface $ avatar w

                         frames [] _ = []
                         frames ((x1,y1):(x2,y2):[]) [u,d,l,r] | x1 == x2 = if y1 > y2
                                                                            then [head d]
                                                                            else [head u]
                                                               
                                                               | y1 == y2 = if x1 > x2 
                                                                            then [head l]
                                                                            else [head r]
                    
                         frames ((x1,y1):t@(x2,y2):xs) [u,d,l,r] | x1 == x2 = if y1 > y2
                                                                              then head d:frames (t:xs) (map tail [u,d,l,r])
                                                                              else head u:frames (t:xs) (map tail [u,d,l,r])
                                                                 
                                                                 | y1 == y2 = if x1 > x2
                                                                              then head l:frames (t:xs) (map tail [u,d,l,r])
                                                                              else head r:frames (t:xs) (map tail [u,d,l,r])
                     
                     in makeFrameRect (frames xs [upFrameList,downFrameList,leftFrameList,rightFrameList]) spriteH spriteW

goTo2 :: (Int,Int) -> World -> World
goTo2 (x,y) w = let origin = (pixToMap w).avatarCenter $ w
                    dest = pixToMap w (x,y)
                    newDest = if elem dest (mapData.changes $ w) 
                              then dest
                              else findClosest (mapData.changes $ w) dest

                    mapPath = (pathFinder (mapData.changes $ w) origin newDest)
                    path = interpolate.(fixPath w) $ map (\c -> mapToPix w c) mapPath
                                                
                    fl = ext (makeFrameList w path)

                in w {avatar = (avatar w){ posList = makePosRect.ext $ path, frameList = fl}}


linkEntitys :: Entity -> Entity -> Entity
linkEntitys main child = Entity { name = name child,
                                surface = surface child,
                                frameList = frameList main,
                                posList = posList main,
                                audioData = audioData child
                              }



updateMouse :: (Word16,Word16) -> World -> World
updateMouse (x,y) w = w {changes = (changes w) {mouseX = fromIntegral x,mouseY = fromIntegral y,clicked = True,updated = True}}

addMusic :: Music -> World -> World
addMusic m w = w {canvas = (canvas w){audioData = (audioData (canvas w)) {music = [m]}}}

--refreshMus w = touchForeignPtr (head $ music (audioData (canvas w))) 

refreshMus = playingMusic

playMus w = playMusic (head $ music (audioData (canvas w))) (-1) 

---------------------------------------------------------------------------------------------------
{- File processing -}

saveData :: World -> IO ()
saveData w = let path = head.misc.changes $ w
                 toWrite = (show.mapData.changes $ w) ++ "\n" ++ (show.gridWH.changes $ w) 
              in writeFile path toWrite

loadMap :: String -> IO ([(Int,Int)],(Int,Int))
loadMap path = do handle <- openFile path ReadMode
                  strMap <- hGetLine handle
                  strGrid <- hGetLine handle
                  hClose handle
                  return ((read strMap :: [(Int,Int)]),read strGrid :: (Int,Int))

---------------------------------------------------------------------------------------------------
 
screenWidth = 640
screenHeight = 400
canvasWidth = 1920
canvasHeight = 450

nbrFrameSprite = 8

yure = slow 5 $ osc 0 25 1



main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode screenWidth screenHeight 32 [SDL.SWSurface]

    SDL.setAlpha screen [SDL.SWSurface] 0
    
    SDL.setCaption "train test" []

    openAudio 22050 AudioS16LSB 1 4096

    music   <- loadMUS "tookah.wav"
    mData   <- loadMap "mapFile" 

    l0 <- loadImage "Layer.png" 
    bg <- loadImage "Background.png"
    l1 <- loadImage "Layer #1.png" 
    ch <- loadImage "Layerch.png"
    l2 <- loadImage "Layer #3.png"
    s1 <- loadImage "spriteSheet1.png"
    s2 <- loadImage "spriteSheet2.png"
    ca <- loadImage "blanck.png"
    

    SDL.setAlpha ca [SDL.SWSurface] 0

    let canvas = makeEntity ca (screenWidth,screenHeight) (ext[0], yure) (ext [0], ext [0]) "canvas" 
        land = makeEntity l2 (canvasWidth,canvasHeight) (cycle [3840,3830..0],ext [0]) (ext [0],ext [0]) "land"
        av   = makeEntity s2 (320,240) (ext[1920],ext [0]) (ext[50],ext [150]) "avatar"
        b1   = makeEntity bg (canvasWidth,canvasHeight) (ext [0],ext [0]) (ext [0],ext [0]) "back1"
        b2   = linkEntitys b1 $ makeBg l0 "back2"
        f1   = linkEntitys b1 $ makeBg l1 "back3"
        
        smallLady = makeEntity s1 (320,240) (slow 30 $ cycle [0,320..1280],ext [0]) (ext [250],ext [71])  "smallLady"
        newLady   = makeEntity s2 (320,240) (slow 10 $ cycle [0,320..2240],ext [0]) (ext [650],ext [150]) "newLady"
        
        
        world = addMusic music $ buildWorld [land] av [f1] [b1,b2] [newLady] [smallLady] (Change [] 0 0 False False [] False (fst mData) (snd mData)) screen canvas

        -- buildWorld l av f b m1 m2 c s 

    -- playMus world
    loop world
    closeAudio

    where loop w = do
            
            start <- SDL.getTicks 
            
            refreshMus

            rawWorld <- drawWorld w
            SDL.flip $ screen w 
            (quit,worldWithChanges) <- whileEvents rawWorld
            
            
            let !nextWorld = updateWorld worldWithChanges
            stop <- SDL.getTicks
            let del = (delay start stop)
            
            
            --putStrLn $ show (del)
            --putStrLn.show.misc.changes $ w
            
            unless (del == 0) (SDL.delay del)
            unless quit (loop nextWorld)

    
          whileEvents w = do
             
             event      <- SDL.pollEvent
             case event of
                  SDL.Quit -> return (True,w)
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _)) -> whileEvents w
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT _ _)) -> whileEvents w
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _ _)) -> whileEvents w
                  (SDL.MouseButtonDown x y _) -> whileEvents $ updateMouse (x,y) w 
                  

                  SDL.NoEvent -> return (False,w)
                  _           -> whileEvents w


--------------------------------------------------------------------------------------------------------
{- PathFinding -}


{-

#######
# S####
# # G##
#    ##
#######

-}

map1 = [(1,1),(1,2),(2,1),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4)] :: [(Int,Int)]

findNearby :: (Int,Int,Int) -> [(Int,Int,Int)]
findNearby (x,y,n) = [(x+1,y,n+1),(x,y+1,n+1),(x-1,y,n+1),(x,y-1,n+1)]



deleteAllBy :: (a -> Bool) -> [a] -> [a]
deleteAllBy p [] = []
deleteAllBy p (x:xs) | (p x) = deleteAllBy  p xs
                     | otherwise = x:deleteAllBy p xs
{-
  1 2 3 4 5 6 7 8
X X X X X X X X X X
X _ _ _ X X _ X _ X 1
X _ X _ _ X _ _ _ X 2
X S X X _ _ _ X _ X 3
X 6 X 6 _ X _ _ _ X 4
X 5 6 5 X X 6 X _ X 5
X 4 X 4 3 X 5 X _ X 6
X 3 X X 2 3 4 X _ X 7
X 2 1 0 1 X 5 6 _ X 8
X X X X X X X X X X

  1 2 3 4 5 6 7 8
X X X X X X X X X X
X _ _ _ X X _ X _ X 1
X _ X _ _ X _ _ _ X 2
X S X X _ _ _ X _ X 3
X _ X _ _ X _ _ _ X 4
X _ _ _ X X _ X _ X 5
X _ X _ _ X _ X _ X 6
X _ X X _ _ _ X _ X 7
X _ _ O _ X _ _ _ X 8
X X X X X X X X X X



-}

map2 = [(1,1),(1,2),(1,3),(1,6),(1,8),
        (2,1),(2,3),(2,4),(2,6),(2,7),(2,8),
        (3,1),(3,4),(3,5),(3,6),(3,8),
        (4,1),(4,3),(4,4),(4,6),(4,7),(4,8),
        (5,1),(5,2),(5,3),(5,6),(5,8),
        (6,1),(6,3),(6,4),(6,6),(6,8),
        (7,1),(7,4),(7,5),(7,6),(7,8),
        (8,1),(8,2),(8,3),(8,4),(8,6),(8,7),(8,8)] :: [(Int,Int)]



map3 = [(0,20),(0,19),(0,18),(0,17),(0,16),(40,15),(43,14),(42,14),(41,14),(40,14),(39,14),(38,14),(37,14),(37,15),(38,15),(39,15),(41,15),(42,15),(43,15),(43,16),(40,16),(39,16),(38,16),(37,17),(41,17),(42,17),(43,17),(42,16),(41,16),(40,17),(39,17),(38,17),(37,16),(39,18),(41,18),(37,18),(38,18),(40,18),(42,18),(43,18),(43,19),(42,19),(41,19),(40,19),(39,19),(38,19),(37,19),(37,20),(38,20),(39,20),(40,20),(41,20),(42,20),(43,20),(44,18),(45,14),(45,16),(45,15),(45,17),(45,18),(45,19),(46,19),(47,19),(46,20),(47,20),(45,20),(44,20),(44,19),(44,17),(44,16),(44,15),(44,14),(47,15),(47,18),(46,17),(46,18),(47,17),(47,16),(46,16),(46,15),(45,13),(43,11),(43,12),(43,13),(44,13),(46,14),(44,10),(44,11),(44,12),(45,12),(46,12),(47,14),(47,13),(45,10),(45,11),(46,11),(46,13),(44,9),(43,9),(43,10),(36,14),(36,15),(36,17),(36,16),(36,18),(36,20),(36,19),(35,19),(35,20),(35,18),(35,17),(35,16),(35,15),(35,14),(36,13),(41,13),(42,12),(42,13),(40,13),(39,13),(38,13),(37,13),(35,13),(35,12),(36,12),(37,12),(38,12),(39,12),(40,11),(41,11),(42,11),(41,12),(40,12),(39,11),(38,9),(39,9),(39,10),(38,11),(38,10),(37,11),(36,11),(35,11),(31,19),(30,20),(31,20),(32,20),(33,20),(34,20),(34,19),(33,19),(32,19),(30,19),(30,18),(31,18),(32,18),(33,18),(34,18),(34,17),(33,17),(32,17),(31,17),(30,17),(33,16),(34,16),(32,16),(31,16),(30,16),(31,15),(30,15),(32,15),(33,15),(34,15),(34,14),(33,14),(32,14),(31,14),(30,14),(31,13),(32,13),(33,13),(34,13),(30,13),(32,12),(31,12),(30,12),(34,12),(33,9),(34,9),(34,10),(34,11),(33,11),(33,10),(33,12),(32,11),(31,11),(30,11),(28,19),(28,20),(27,20),(25,20),(26,20),(24,20),(23,20),(24,19),(23,19),(25,19),(26,19),(26,18),(27,18),(29,19),(29,20),(29,18),(28,18),(27,19),(25,18),(24,18),(23,18),(25,17),(26,17),(27,17),(28,17),(29,17),(29,16),(28,16),(27,16),(26,16),(25,16),(28,14),(29,14),(28,15),(29,15),(29,13),(28,13),(28,12),(29,12),(29,11),(29,10),(29,9),(28,10),(28,9),(28,11),(25,13),(26,15),(25,15),(27,15),(27,14),(26,14),(25,14),(26,13),(27,12),(27,13),(26,12),(25,12),(25,11),(27,11),(26,11),(24,16),(24,15),(24,17),(23,17),(23,16),(23,15),(24,14),(23,14),(23,13),(24,13),(24,7),(24,12),(23,12),(24,11),(23,11),(23,10),(24,9),(24,10),(24,8),(23,9),(20,20),(21,20),(22,20),(22,19),(21,19),(20,19),(20,17),(22,18),(21,18),(20,18),(21,17),(22,17),(22,16),(21,16),(20,16),(21,15),(22,15),(20,15),(20,14),(21,14),(22,14),(22,11),(22,13),(22,12),(21,13),(20,13),(20,12),(20,11),(21,11),(21,12),(22,10),(21,10),(20,10),(19,18),(19,20),(19,19),(19,17),(18,20),(18,19),(18,18),(18,17),(16,20),(17,20),(17,19),(17,18),(17,17),(16,17),(16,18),(16,19),(15,20),(15,19),(15,18),(15,17),(15,16),(15,15),(16,15),(16,16),(17,16),(18,16),(19,16),(19,15),(18,15),(17,15),(15,14),(16,14),(17,14),(18,14),(19,14),(19,13),(18,12),(18,13),(17,13),(16,13),(15,13),(15,12),(16,12),(17,12),(19,12),(19,11),(18,11),(19,9),(19,10),(19,8),(19,7),(18,8),(18,9),(18,10),(17,11),(16,11),(15,11),(10,12),(11,12),(12,12),(12,11),(11,11),(10,11),(14,15),(14,13),(14,14),(14,16),(14,17),(14,18),(14,19),(14,20),(13,20),(13,19),(12,19),(12,20),(11,20),(11,19),(10,19),(10,20),(9,20),(9,19),(9,18),(3,20),(3,19),(2,19),(2,20),(1,20),(1,19),(8,18),(8,20),(8,19),(7,20),(7,19),(6,19),(6,20),(5,20),(5,19),(4,20),(4,19),(11,18),(8,7),(9,7),(9,8),(8,8),(8,9),(9,9),(9,10),(13,7),(14,7),(14,8),(13,8),(13,10),(13,9),(14,10),(14,9),(14,12),(14,11),(13,11),(13,12),(13,13),(13,14),(13,15),(13,16),(13,17),(13,18),(12,18),(12,17),(12,16),(12,15),(12,14),(12,13),(11,13),(10,13),(11,15),(10,14),(11,14),(10,15),(10,17),(10,16),(11,16),(11,17),(10,18),(8,17),(9,17),(9,16),(8,16),(9,14),(9,15),(8,14),(8,15),(9,13),(8,13),(9,11),(9,12),(8,10),(8,11),(8,12),(7,18),(7,17),(7,15),(7,16),(7,14),(7,13),(6,14),(6,16),(6,15),(6,13),(5,14),(5,15),(3,10),(3,11),(5,16),(6,17),(5,17),(6,18),(5,18),(1,14),(2,15),(1,15),(1,16),(1,17),(4,18),(3,18),(2,18),(1,18),(2,16),(4,16),(3,17),(2,17),(3,16),(4,17),(2,14),(3,14),(3,15),(4,15),(4,14),(4,13),(3,13),(2,13),(3,12),(2,12),(4,12)] :: [(Int,Int)]


pathFinder :: [(Int,Int)] -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
pathFinder m s@(x1,y1) g@(x2,y2) = let grid = mapGrid [(x1,y1,0)] [(x1,y1,0)]
                                   in [(x,y) | (x,y,_) <- makePath (tail grid) [(head grid)]]
    

    where mapGrid [] xs = xs
          mapGrid l@(x@(x',y',n):xs) r = let nearby = findNearby x
                                       in if any (\(a,b,n) -> (a,b) == g) nearby then (x2,y2,n+1):r

                                       else let selected  = deleteAllBy (p r) nearby                          
                                            in mapGrid (xs++selected) (selected++r)
          
         
          makePath [] r = r
          makePath xs (r@(_,_,n):rs) = let nextPos = minPos r xs
                                           ns = dropWhile (\e -> e /= nextPos) xs
                                           in case ns of [] -> (nextPos:r:rs)
                                                         _  -> makePath (tail ns) (nextPos:r:rs)


          sel _ [] = False
          sel (a,b,n) ((c,d,m):xs) | (a,b) == (c,d) && m <= n = True
                                   | otherwise = sel (a,b,n) xs 
          
          p res = (\(a,b,n) -> (not $ elem (a,b) m) || sel (a,b,n) res)

          minPos x@(_,_,n) xs = let nearby = filter (\e -> any (customEq e) (findNearby x)) (xs)
                                in case nearby of [] -> x
                                                  _  -> foldl' minBy (head nearby) nearby

          customEq (a,b,c) (e,f,d) | a == e && b == f = True
                                   | otherwise = False

          minBy (a,b,c) (e,f,g) | c <= g = (a,b,c)
                                | otherwise = (e,f,g)

findClosest :: [(Int,Int)] -> (Int,Int) -> (Int,Int)
findClosest m dest = let (rx,ry) = (fromIntegral $ fst dest, fromIntegral $ snd dest)
                         distMap = map (\(x,y) -> (sqrt ((fromIntegral x - rx)^2+(fromIntegral y - ry)^2),(x,y))) m
                         minDist l@(a,_) r@(b,_) | a < b = l
                                                 |otherwise = r 
                     
                     in snd $ foldl' minDist (head distMap) distMap                                   