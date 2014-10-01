import System.Random
import Control.Monad
import GHC.Word
import GHC.Int
import Data.Bits
import Data.List
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
             }

data World = World {
	         avatar  :: Image,
	         land    :: Image,
	         fg      :: [Image],
	         bg      :: [Image],
	         miscFg  :: [Image],
	         miscBg  :: [Image],
	         changes :: [String],
	         screen  :: SDL.Surface
             }

buildWorld :: Image -> Image -> [Image] -> [Image] -> [Image] -> [Image] -> [String] -> SDL.Surface -> World
buildWorld l av f b m1 m2 c s = World { avatar  = av,
	                                    land    = l,
	                                    fg      = f,
	                                    bg      = b,
	                                    miscFg  = m1,
	                                    miscBg  = m2,
	                                    changes = c,
	                                    screen  = s
                                      } 


updateWorld :: World -> World
updateWorld wo = if changes wo == [] then wo   
                 else let w = applyChanges wo
                          (ref,av)  = follow (avatar w) (head $ bg w) 
                          newBg     = ref:( map (\i -> linkImages ref i) (tail $ bg w))
                          newFg     = map (\i -> linkImages ref i) (fg w) 
                          newMiscFg = map (\i -> anchor ref i) (miscFg w)
                          newMiscBg = map (\i -> anchor ref i) (miscBg w)
                
                in World { avatar  = av,
	                       land    = land w,
	                       fg      = newFg,
	                       bg      = newBg,
	                       miscFg  = newMiscFg,
	                       miscBg  = newMiscBg,
	                       changes = changes w,
	                       screen  = screen w
                          }



applyChanges :: World -> World
applyChanges w | (head $ changes w) == "space" = w {changes = [],avatar = (avatar w) {posList = makePosRect [(x,150) | x <- (osc 150 600 4)]}}
               | (head $ changes w) == "right" = w {changes = [],avatar = (avatar w) {posList = move (5,0) (avatar w)}}

loadImage :: String -> IO SDL.Surface
loadImage filename = SDLI.load filename  >>= SDL.displayFormatAlpha

nextImage :: Image -> Image
nextImage img = Image {surface = surface img,
                       frameList = tail $ frameList img,
                       posList = tail $ posList img,
                       name = name img}

ext ::  [a] -> [a]
ext [] = []
ext (x:[]) = cycle [x]
ext (x:xs) = x:ext xs

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

{-

drawWorld :: World -> IO World
drawWorld w = do  l         <- displayImg (land w) (screen w)
                  newBg     <- drawSurfaces (bg w) (screen w)
                  newMiscBg <- drawSurfaces (miscBg w) (screen w)
                  av        <- displayImg (avatar w) (screen w)
                  newMiscFg <- drawSurfaces (miscFg w) (screen w)
                  newFg     <- drawSurfaces (fg w) (screen w)
                  
                  let newWorld = World { avatar  = av,
	                                     land    = l,
	                                     fg      = newFg,
	                                     bg      = newBg,
	                                     miscFg  = newMiscFg,
	                                     miscBg  = newMiscBg,
	                                     changes = changes w,
	                                     screen  = screen w
                                       }
                  return newWorld                  

                	              
-}

drawWorld :: World -> IO World
drawWorld w = let back = surface $ land w in -- surface $ head $ bg w in
              do  SDL.setAlpha back [SDL.SWSurface] 0
              	  l         <- displayImg (land w) (back)
                  
                  newBg     <- drawSurfaces (bg w) (back)
                  newMiscBg <- drawSurfaces (miscBg w) (back)
                  av        <- displayImg (avatar w) (back)
                  newMiscFg <- drawSurfaces (miscFg w) (back)
                  newFg     <- drawSurfaces (fg w) (back)
                  SDL.blitSurface back (head.frameList $ (head $ bg w)) (screen w) (head.posList $ (head $ bg w)) 

                  
                  let newWorld = World { avatar  = av,
                                       land    = l,
                                       fg      = newFg,
                                       bg      = newBg,
                                       miscFg  = newMiscFg,
                                       miscBg  = newMiscBg,
                                       changes = changes w,
                                       screen  = screen w
                                       }
                  return newWorld 

animate :: [Image] -> SDL.Surface -> IO [Image]
animate xs dest = do nextFrames <- drawSurfaces xs dest
                     SDL.flip dest
                     SDL.delay 20
                     animate nextFrames dest

applySurface :: Int -> Int -> SDL.Surface -> SDL.Surface -> IO Bool
applySurface x y src dst = SDL.blitSurface src Nothing dst offset
 where offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 0, SDL.rectH = 0 }

rectList2Coord :: [Maybe SDL.Rect] -> ([Int],[Int])
rectList2Coord (x:xs) =  case x of Nothing -> ([],[])
                                   Just r  -> let newX = SDL.rectX r
                                                  newY = SDL.rectY r
                                                  (newXs,newYs) = rectList2Coord xs
                                               in (newX:newXs,newY:newYs)

getCurrent :: Image -> ((Int,Int),(Int,Int))
getCurrent i = let frames = frameList i
                   pos    = posList i
               in case (frames, pos) of ((Just f:fs),(Just p:ps)) -> ((SDL.rectX f,SDL.rectY f),(SDL.rectX p,SDL.rectY f))
                                        _                         -> ((0,0),(0,0))



makePosRect :: [(Int,Int)] -> [Maybe SDL.Rect]
makePosRect [] = []
makePosRect (x:xs) = Just SDL.Rect {SDL.rectX = fst x,
                                    SDL.rectY = snd x,
                                    SDL.rectH = 0,
                                    SDL.rectW = 0
                                   }:makePosRect xs

move :: (Int,Int) -> Image -> [Maybe SDL.Rect]
move (x,y) i = let (tmpX,tmpY) = rectList2Coord (posList i)
                   (oldX,oldY) = ( head tmpX, head tmpY)
	           in Just SDL.Rect {SDL.rectX = oldX + x,
                                 SDL.rectY = oldY + y,
                                 SDL.rectH = 0,
                                 SDL.rectW = 0
                                 }:move (x,y) i 



rectListOffset :: [Maybe SDL.Rect] -> [Maybe SDL.Rect] -> [Maybe SDL.Rect]
rectListOffset [] _ = []
rectListOffset (m:ms) (c:cs) = case (m,c) of 
    (Nothing,Nothing) -> []
    (Just mRect, Just cRect) -> let newX = SDL.rectX cRect - SDL.rectX mRect
                                    newY = SDL.rectY cRect - SDL.rectY mRect
                                    w = SDL.rectW cRect
                                    h = SDL.rectH cRect
                                                  
                                in Just SDL.Rect {SDL.rectX = newX,
                                                  SDL.rectY = newY,
                                                  SDL.rectW = w,
                                                  SDL.rectH = h }:rectListOffset ms cs
    _ -> []


cameraOffset :: [Maybe SDL.Rect] -> [Maybe SDL.Rect] -> Int -> ([Maybe SDL.Rect],[Maybe SDL.Rect])
cameraOffset [] _ _ = ([],[])
cameraOffset (m:ms) (c:cs) o = case (m,c) of 
    (Nothing,Nothing) -> ([],[])
    (Just mRect, Just cRect) -> let newO = SDL.rectX cRect + computeNewO speed
                                    newX = newO + SDL.rectX cRect
                                    newY = SDL.rectY cRect
                                    speed = computeSpeed 
                                    w = SDL.rectW cRect
                                    h = SDL.rectH cRect
                                                  
                                in let (a,b) = cameraOffset ms cs newO
                                   in (Just SDL.Rect { SDL.rectX = newX,
                                                       SDL.rectY = newY,
                                                       SDL.rectW = w,
                                                       SDL.rectH = h }:a,
                                       Just SDL.Rect { SDL.rectX = SDL.rectX mRect - (newO),
                                                       SDL.rectY = SDL.rectY mRect- SDL.rectY cRect,
                                                       SDL.rectW = SDL.rectW mRect,
                                                       SDL.rectH = SDL.rectH mRect }:b)

                                where computeNewO s | SDL.rectX mRect - o > (screenWidth - 250)  && (SDL.rectX cRect + o ) < 1280 = o + s
                                                    | SDL.rectX mRect - o < (-20) && (SDL.rectX cRect + o ) > 0 = o - s
                                                    | otherwise = o

                                      computeSpeed = case ms of [] -> 0
                                                                (x:xs) -> case x of Nothing -> 0
                                                                                    Just r  -> abs (SDL.rectX r - SDL.rectX mRect)  
    _ -> ([],[])


anchor :: Image -> Image -> Image
anchor main child = Image { name = name child,
                                   surface = surface child,
                                   frameList = frameList child,
                                   posList = rectListOffset (frameList main) (posList child) }

follow :: Image -> Image -> (Image,Image)
follow main child = let (newF,newP) = cameraOffset (posList main) (frameList child) (fst $ fst $ getCurrent child)
	                in ( Image { name = name child,
                                 surface = surface child,
                                 frameList = newF,
                                 posList = posList child},
                         Image { name = name main,
                                 surface = surface main,
                                 frameList = frameList main,
                                 posList = newP} )



linkImages :: Image -> Image -> Image
linkImages main child = Image { name = name child,
                                surface = surface child,
                                frameList = frameList main,
                                posList = posList main}

 -----------------------------------------------------------------------------------------------------

--data App = App {screen :: SDL.Surface, frames :: [Image], changes :: [String]}

screenWidth = 640
screenHeight = 400
wagonWidth = 1920
wagonHeight = 450

yure = slow 5 $ osc 0 25 1



main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode 640 400 32 [SDL.SWSurface]

    SDL.setAlpha screen [SDL.SWSurface] 0
    
    SDL.setCaption "train test" []
    
    l0 <- loadImage "Layer.png" 
    bg <- loadImage "Background.png"
    l1 <- loadImage "Layer #1.png" 
    ch <- loadImage "Layerch.png"
    l2 <- loadImage "layer3.png"
    s1 <- loadImage "spriteSheet1.png"
    s2 <- loadImage "spriteSheet2.png"
    

    let land = makeImage l2 (640,480) (cycle [1280,1270..0],ext [0]) (ext [0],ext [0]) "land"
        av   = makeImage s2 (320,240) (ext[1280],ext [0]) (ext[150],ext [150]) "avatar"
        (b1,c1) = follow av (makeImage bg (640,480) (ext [0],ext [0]) (ext [0],ext [0]) "back1")
        b2   = linkImages b1 $ makeBg l0 "back2"
        f1   = linkImages b1 $ makeBg l1 "back3"
        
        smallLady = anchor b1 $ makeImage s1 (320,240) (slow 30 $ cycle [0,320..1280],ext [0]) (ext [250],ext [71])  "smallLady"
        newLady   = anchor b1 $ makeImage s2 (320,240) (slow 10 $ cycle [0,320..2240],ext [0]) (ext [650],ext [150]) "newLady"
        
        
        world = buildWorld land c1 [f1] [b1,b2] [newLady] [smallLady] [] screen

        -- buildWorld l av f b m1 m2 c s 



    drawWorld world

    SDL.flip screen

    loop world

    where loop w = do
            start <- SDL.getTicks
            
            rawWorld <- drawWorld w
            SDL.flip $ screen w 
            (quit,worldWithChanges) <- whileEvents rawWorld
            
            
            let !nextWorld = updateWorld worldWithChanges
            stop <- SDL.getTicks
            let del = (delay start stop)
            
            --SDL.delay (delay start stop)
            --putStrLn $ show (del)
            unless (del == 0) (SDL.delay del)
            unless quit (loop nextWorld)

    
          whileEvents w = let updateChange w s = w {changes = s:changes w}
                          in do
             
             event      <- SDL.pollEvent
             case event of
                  SDL.Quit -> return (True,w)
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _)) -> whileEvents (updateChange w "space")
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT _ _)) -> whileEvents (updateChange w "left")
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _ _)) -> whileEvents (updateChange w "right")

                  SDL.NoEvent -> return (False,w)
                  _           -> whileEvents w
