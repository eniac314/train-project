import System.Random
import Control.Monad
import GHC.Word
import GHC.Int
import Data.Bits
import Data.List
import Data.Maybe (fromJust)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.Image as SDLI
import qualified Data.Map as Map 
{-# LANGUAGE BangPatterns #-}



osc :: Int -> Int -> Int -> [Int]
osc a b c = cycle $ [a,a+c..b] ++ [b,b-c..a+c]

slow :: Int -> [Int] -> [Int]
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


-------------------------------------------------------------------------------------------------
{- Graphics -}

type Point = (Double,Double)

getPixel :: Word8 -> Word8 -> Word8 -> SDL.Pixel
getPixel r g b = SDL.Pixel $ (shiftL (fi r) 24 .|. shiftL (fi g) 16 .|. shiftL (fi b) 8 .|. (fi 255)) where 
	fi a = fromIntegral a

pixelsToScreen :: [Point] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
pixelsToScreen xs s p = map (\(x,y) -> SDLP.pixel s (fromIntegral.round $ x) (fromIntegral.round $ y) p) xs

data Image = Image {
             name      :: String,
             surface   :: SDL.Surface,
             frameList :: [Maybe SDL.Rect],
             posList   :: [Maybe SDL.Rect]
             } deriving Show

data World = World {
	         avatar  :: Image,
	         land    :: [Image],
	         fg      :: [Image],
	         bg      :: [Image],
	         miscFg  :: [Image],
	         miscBg  :: [Image],
	         changes :: Change,
	         screen  :: SDL.Surface,
           canvas  :: Image
             } deriving Show

data Change = Change {
           evList  :: [SDL.Event],
           mouseX  :: Int,
           mouseY  :: Int,
           clicked :: Bool,
           enter   :: Bool,
           usrStr  :: String,
           updated :: Bool
} deriving Show

buildWorld :: [Image] -> Image -> [Image] -> [Image] -> [Image] -> [Image] -> Change -> SDL.Surface -> Image -> World
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


computeSpeed :: Image -> Int
computeSpeed i = case posList $ i of (Just a: Just b:rs) -> SDL.rectX b - SDL.rectX a
                                     _ -> 0

applyChanges :: World -> World
applyChanges w |(clicked $ changes w) = let newW = goTo (mouseX $ changes w,mouseY $ changes w) w
                                        in newW {changes = (changes newW){clicked = False,updated = False}}
               |otherwise = w

loadImage :: String -> IO SDL.Surface
loadImage filename = SDLI.load filename  >>= SDL.displayFormatAlpha

nextImage :: Image -> Image
nextImage img = Image {surface = surface img,
                       frameList = tail $ frameList img,
                       posList = tail $ posList img,
                       name = name img}


makeBg :: SDL.Surface -> String -> Image
makeBg src name = Image { name = name,
                          surface = src,
                          frameList = cycle [Nothing],
                          posList = cycle [Just SDL.Rect { SDL.rectX = 0, SDL.rectY = 0, SDL.rectW = 0, SDL.rectH = 0 }]
                        }

makeImage :: SDL.Surface -> (Int,Int) -> ([Int],[Int]) -> ([Int],[Int]) -> String -> Image
makeImage src (fw,fh) (fxs,fys) (xs,ys) name = Image { name = name,
                                                   surface = src,
                                                   frameList = makeRect (fw,fh) fxs fys,
                                                   posList = makeRect (0,0) xs ys
                                                   }
  where makeRect _ [] []         = cycle [Nothing]
        makeRect  (w,h) (x:xs) (y:ys) = Just SDL.Rect { SDL.rectX = x,
     	                                                 SDL.rectY = y,
     	                                                 SDL.rectW = w,
     	                                                 SDL.rectH = h } : makeRect (w,h) xs ys


displayImg :: Image -> SDL.Surface -> IO Image
displayImg src dest = do SDL.blitSurface (surface src) (head.frameList $ src)  dest (head.posList $ src)
                         return (nextImage src)

liftIO :: (a -> b) -> (IO a -> IO b)
liftIO f = liftM f

drawSurfaces :: [Image] -> SDL.Surface -> IO [Image] 
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

getCurrent :: Image -> ((Int,Int),(Int,Int))
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

linkImages :: Image -> Image -> Image
linkImages main child = Image { name = name child,
                                surface = surface child,
                                frameList = frameList main,
                                posList = posList main}


printWorld :: World -> IO ()
printWorld w = do putStrLn "World status"
                  putStrLn $ show (changes w)

updateMouse :: (Word16,Word16) -> World -> World
updateMouse (x,y) w = w {changes = (changes w) {mouseX = fromIntegral x,mouseY = fromIntegral y,clicked = True,updated = True}}

 -----------------------------------------------------------------------------------------------------

--data App = App {screen :: SDL.Surface, frames :: [Image], changes :: [String]}

screenWidth = 640
screenHeight = 400
canvasWidth = 1920
canvasHeight = 450

spriteH = 240
spriteW = 320

yure = slow 5 $ osc 0 25 1



main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode 640 400 32 [SDL.SWSurface]

    SDL.setAlpha screen [SDL.SWSurface] 0
    
    SDL.setCaption "train test" []
    
    l0 <- loadImage "Layer.png" 
    bg <- loadImage "Background.png"
    l1 <- loadImage "Layer #1.png" 
    ch <- loadImage "Layerch.png"
    l2 <- loadImage "Layer #3.png"
    s1 <- loadImage "spriteSheet1.png"
    s2 <- loadImage "spriteSheet2.png"
    ca <- loadImage "blanck.png"
    
    SDL.setAlpha ca [SDL.SWSurface] 0

    let canvas = makeImage ca (screenWidth,screenHeight) (ext[0], yure) (ext [0], ext [0]) "canvas" 
        land = makeImage l2 (canvasWidth,canvasHeight) (cycle [3840,3830..0],ext [0]) (ext [0],ext [0]) "land"
        av   = makeImage s2 (320,240) (ext[1920],ext [0]) (ext[50],ext [150]) "avatar"
        b1   = makeImage bg (canvasWidth,canvasHeight) (ext [0],ext [0]) (ext [0],ext [0]) "back1"
        b2   = linkImages b1 $ makeBg l0 "back2"
        f1   = linkImages b1 $ makeBg l1 "back3"
        
        smallLady = makeImage s1 (320,240) (slow 30 $ cycle [0,320..1280],ext [0]) (ext [250],ext [71])  "smallLady"
        newLady   = makeImage s2 (320,240) (slow 10 $ cycle [0,320..2240],ext [0]) (ext [650],ext [150]) "newLady"
        
        
        world = buildWorld [land] av [f1] [b1,b2] [newLady] [smallLady] (Change [] 0 0 False False [] False) screen canvas

        -- buildWorld l av f b m1 m2 c s 



    drawWorld world

    SDL.flip screen

    loop world

    where loop w = do
            -- printWorld w
            start <- SDL.getTicks
            
            rawWorld <- drawWorld w
            SDL.flip $ screen w 
            (quit,worldWithChanges) <- whileEvents rawWorld
            
            
            let !nextWorld = updateWorld worldWithChanges
            stop <- SDL.getTicks
            let del = (delay start stop)
            
            
            --putStrLn $ show (del)
            --putStrLn $ concat $ changes w
            
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
