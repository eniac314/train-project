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
import System.Environment
import System.IO
import Graphics.UI.SDL.Mixer

-------------------------------------------------------------------------------------------------
{- Graphics -}

type Point = (Double,Double)

getPixel :: Word8 -> Word8 -> Word8 -> SDL.Pixel
getPixel r g b = SDL.Pixel $ (shiftL (fi r) 24 .|. shiftL (fi g) 16 .|. shiftL (fi b) 8 .|. (fi 255)) where 
	fi a = fromIntegral a

pixelsToScreen :: [Point] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
pixelsToScreen xs s p = map (\(x,y) -> SDLP.pixel s (fromIntegral.round $ x) (fromIntegral.round $ y) p) xs

linesToScreen :: [(Point,Point)] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
linesToScreen xs s p = map (\((x1,y1),(x2,y2)) -> SDLP.line s (fromIntegral.round $ x1) (fromIntegral.round $ y1) (fromIntegral.round $ x2) (fromIntegral.round $ y2) p) xs


loadImage :: String -> IO SDL.Surface
loadImage filename = SDLI.load filename  >>= SDL.displayFormatAlpha

applySurface :: Int -> Int -> SDL.Surface -> SDL.Surface -> IO Bool
applySurface x y src dst = SDL.blitSurface src Nothing dst offset
    where offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 0, SDL.rectH = 0 }


drawGrid :: World -> IO [Bool]
drawGrid wo  = let (w ,h) = (fromIntegral.fst.gridWH.changes $ wo, fromIntegral.snd.gridWH.changes $ wo)
                   (cw,ch) = (fromIntegral.SDL.surfaceGetWidth.surface.canvas $ wo, fromIntegral.SDL.surfaceGetHeight.surface.canvas $ wo)
                   s = surface.canvas $ wo
                   
                   rectList = map (\(x,y) -> Just SDL.Rect {SDL.rectX = x*w, SDL.rectY = y*h, SDL.rectW = w, SDL.rectH = h}) (mapData.changes $ wo)
                   srcFrame = Just SDL.Rect {SDL.rectX = 0, SDL.rectY = 0, SDL.rectW = w, SDL.rectH = h}

                   hori  = [((0,y),(cw,y)) | y <- [0,0+(fromIntegral h)..ch]]
                   verti = [((x,0),(x,ch)) | x <- [0,0+(fromIntegral w)..cw]]
                   
                   filledGrid = map (\r -> SDL.blitSurface (surface.head.fg $ wo) srcFrame s r) rectList 
                   
                   color = (getPixel 21 128 243)
               in sequence $ filledGrid ++ (linesToScreen hori s color) ++ (linesToScreen verti s color)

---------------------------------------------------------------------------------------------------
{- Engine -}
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
           misc     :: [String],
           mouseX   :: Int,
           mouseY   :: Int,
           lClicked :: Bool,
           rClicked :: Bool,
           enter    :: Bool,
           usrStr   :: String,
           updated  :: Bool,
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
                  cW = SDL.surfaceGetWidth $ surface $ canvas w 
                  
                  
                  newFX | s == 0 = cfx
                        | (apx > cfx + 300) && (cfx + (abs s) < (cW - screenWidth)) = cfx + (abs s) 
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
applyChanges w |(lClicked $ changes w) = let newW = goTo (mouseX $ changes w,mouseY $ changes w) w
                                        in newW {changes = (changes newW){lClicked = False,updated = False}}
               |(rClicked $ changes w) = let newW = updateMap w
                                        in newW {changes = (changes newW){rClicked = False,updated = False}}
               |otherwise = w

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

nextEntity :: Entity -> Entity
nextEntity img = Entity {surface = surface img,
                       frameList = tail $ frameList img,
                       posList = tail $ posList img,
                       name = name img,
                       audioData = audioData img
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
makeBg :: SDL.Surface -> String -> Entity
makeBg src name = Entity { name = name,
                          surface = src,
                          frameList = cycle [Nothing],
                          posList = cycle [Just SDL.Rect { SDL.rectX = 0, SDL.rectY = 0, SDL.rectW = 0, SDL.rectH = 0 }],
                          audioData = makeSound
                        }

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
                   -- newFg     <- drawSurfaces (fg w) (surface $ canvas w)
                   drawGrid w 
                   newCan    <- displayImg (canvas w) (screen w)

                   
                   let newWorld = World { avatar  = av,
                                          land    = l,
                                          fg      = fg w,
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


updateLMouse :: (Word16,Word16) -> World -> World
updateLMouse (x,y) w = w {changes = (changes w) {mouseX = fromIntegral x,mouseY = fromIntegral y,lClicked = True,updated = True}}

updateRMouse :: (Word16,Word16) -> World -> World
updateRMouse (x,y) w = w {changes = (changes w) {mouseX = fromIntegral x,mouseY = fromIntegral y,rClicked = True,updated = True}}

updateGrid :: World -> Int -> Int -> World
updateGrid w x y = let (gW,gH) = (gridWH.changes $ w) in w { changes = (changes w) { gridWH = (gW+x,gH+y)} }

updateMap :: World -> World
updateMap w = let p = pixToMap w 
                  m = (mapData.changes $ w)
                  in if elem p m then w { changes = (changes w) { mapData = delete p m} }
                  	 else w { changes = (changes w) { mapData = p:(mapData.changes $ w)} }
---------------------------------------------------------------------------------------------------
{- Misc -}

 
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

pixToMap :: World -> (Int,Int)
pixToMap w = let (gW,gH) = gridWH.changes $ w
                 (mX,mY) = (mouseX.changes $ w, mouseY.changes $ w)
                 ((cfx,cfy),_) = getCurrent.canvas $ w
                 (x,y) = (mX+cfx,mY+cfy)
                 [dx,dy,dGw,dGh] = map fromIntegral [x,y,gW,gH] 
                 
             in  (floor (dx/dGw),floor (dy/dGh))


printMapData :: World -> String
printMapData w = let (gW,gH) = gridWH.changes $ w
                     (mX,mY) = (mouseX.changes $ w, mouseY.changes $ w)
                     ((cfx,cfy),_) = getCurrent.canvas $ w
                     (x,y) = (mX+cfx,mY+cfy)
                     (cW,cH) = (SDL.surfaceGetWidth.surface.canvas $ w, SDL.surfaceGetHeight.surface.canvas $ w)
                     [dx,dy,dCw,dCh,dGw,dGh] = map fromIntegral [x,y,cW,cH,gW,gH]
                     res = (floor (dx/dGw),floor (dy/dGh))
                     newDiv a b = if mod a b == 0 then div a b else 1 + div a b

                 in "Res: "++ show res ++ " Mouse: " ++ show (mX,mY) ++ " Pos: " ++ show (dx,dy) ++ " Can: " ++ show (dCw,dCh) ++ "\n"
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
{-Main-}

screenWidth = 800
screenHeight = 450
nbrFrameSprite = 8



menu args | args == [] = do putStr "\x1b[2J\x1b[;H \n \x1b[44m Map Editor 1.0 \x1b[0m \n Please Input Data (image, map filenames,grid width,length) \n" >> hFlush stdout
                            pic <- getLine
                            map <- getLine
                            w   <- getLine
                            h   <- getLine
                            return (pic,map,w,h)
          | otherwise = return (args !! 0,args !! 1,args !! 2,args !! 3)

main = SDL.withInit [SDL.InitEverything] $ do
    args <- getArgs
    screen <- SDL.setVideoMode screenWidth screenHeight 32 [SDL.SWSurface]
    s2 <- loadImage "spriteSheet2.png"

    SDL.setAlpha screen [SDL.SWSurface] 0
    
    SDL.setCaption "Map Editor" []

    (pic',map',w',h') <- menu args

    pic <- loadImage pic'
    bg <- loadImage pic'
    bl <- loadImage "blanck.png"
    fg <- loadImage "blue.png"

    SDL.setAlpha pic [SDL.SWSurface] 0
    SDL.setAlpha bl [SDL.SWSurface] 0
    mData <- loadMap map'
    let (cW,cH) = ((SDL.surfaceGetWidth pic),(SDL.surfaceGetHeight pic))        
    

    let canvas = makeEntity pic (screenWidth,screenHeight) (ext[0], ext[0]) (ext [0], ext [0]) "canvas"
        av   = makeEntity s2 (320,240) (ext[1920],ext [0]) (ext[50],ext [150]) "avatar"
        b1   = makeEntity bg (cW,cH) (ext [0],ext [0]) (ext [0],ext [0]) "back1"
        b0   = makeEntity bl (cW,cH) (ext [0],ext [0]) (ext [0],ext [0]) "white"
        f1   = makeEntity fg (cW,cH) (ext [0],ext [0]) (ext [0],ext [0]) "white"

        world = buildWorld [] av [f1] [b0,b1] [] [] (Change [map'] 0 0 False False False [] False (fst mData) (snd mData)) screen canvas
        spriteW = SDL.surfaceGetWidth $ surface $ avatar world
        spriteH = SDL.surfaceGetHeight $ surface $ avatar world 
 
    applySurface 0 0 pic screen
    putStrLn (show spriteH ++ " " ++ show spriteW)
    
    loop world

    where loop w = do
            
            start <- SDL.getTicks 
            
            rawWorld <- drawWorld w
            SDL.flip $ screen w 
            (quit,worldWithChanges) <- whileEvents rawWorld
            
            
            let !nextWorld = updateWorld worldWithChanges
            stop <- SDL.getTicks
            let del = (delay start stop)
            
            
            --putStrLn $ show (del)
            --putStrLn.show.mapData.changes $ w
            
            unless (del == 0) (SDL.delay del)
            unless quit (loop nextWorld)

    
          whileEvents w = do
             
             event      <- SDL.pollEvent
             case event of
                  SDL.Quit -> return (True,w)
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _)) -> whileEvents w
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT _ _)) -> whileEvents (updateGrid w (-1) 0)
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _ _)) -> whileEvents (updateGrid w 1 0)
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_UP _ _)) -> whileEvents (updateGrid w 0 (-1))
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_DOWN _ _)) -> whileEvents (updateGrid w 0 1)
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_s _ _)) -> saveData w >> whileEvents w
                  (SDL.MouseButtonDown x y SDL.ButtonLeft) -> whileEvents $ updateLMouse (x,y) w
                  (SDL.MouseButtonDown x y SDL.ButtonRight) -> whileEvents $ updateRMouse (x,y) w 
                  

                  SDL.NoEvent -> return (False,w)
                  _           -> whileEvents w

    
