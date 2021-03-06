import System.Random
import Control.Monad
import GHC.Word
import GHC.Int
import GHC.ForeignPtr
import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe (fromJust)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.TTF as SDLT
import qualified Graphics.UI.SDL.Image as SDLI
import Graphics.UI.SDL.Mixer
import qualified Data.Map as Map
import System.IO
import ChatBot


{-# LANGUAGE BangPatterns #-}

--------------------------------------------------------------------------------------------------------
{-Data structures-}

data Entity = Entity {
             name      :: String,
             surface   :: SDL.Surface,
             frameList :: [Maybe SDL.Rect],
             posList   :: [Maybe SDL.Rect],
             nbrFrame  :: Int,
             audioList :: [AudioTag],
             audioData :: Sound
             }

data World = World {
           avatar  :: Entity,
           land    :: [Entity],
           fg      :: [Entity],
           bg      :: [Entity],
           miscFg  :: [Entity],
           miscBg  :: [Entity],
           canvas  :: Entity,
           menu    :: [Entity],
           inventory :: [Entity], 
           screen  :: SDL.Surface,
           changes :: Change,
           mapData  :: [(Int,Int)],
           gridWH   :: (Int,Int),
           font     :: SDLT.Font,
           botData  :: BotData
           }

data Change = Change {
           misc    :: [String],
           mouseX  :: Int,
           mouseY  :: Int,
           clicked :: Bool,
           enter   :: Bool,
           buffer  :: [String],
           usrStr  :: String,
           updated :: Bool
}

data Sound = SoundTrack Music | Effects [Effect] | None

type AudioTag = Maybe (String,Bool,Int,Int)

data Effect = Effect {
           tag   :: String,
           played :: Bool,
           loop :: Int,
           audio :: Chunk,
           vol :: Int,
           channel :: Maybe Int
}

---------------------------------------------------------------------------------------------------
 {- Main -}

screenWidth = 1000
screenHeight = 650
canvasWidth = 1920
canvasHeight = 450
terminalHeight = screenHeight - canvasHeight :: Int
terminalWidth = round (0.70 * fI screenWidth) :: Int

fontName = "DejaVuSansMono.ttf"
fontSize = 14 :: Int

lineFormat = round $ 0.9 * ((fI terminalWidth) / ((fI fontSize) / 2)) :: Int

nbrFrameSprite = 8
defVol = 50

yure = slow 5 $ osc 0 25 1



main = SDL.withInit [SDL.InitEverything] $ do
    SDLT.init
    screen <- SDL.setVideoMode screenWidth screenHeight 32 [SDL.SWSurface]

    SDL.setAlpha screen [SDL.SWSurface] 0
    
    SDL.setCaption "train" []

    openAudio 22050 AudioS16LSB 1 4096

    music   <- loadMUS "train.mp3"
    step    <- loadWAV "step.wav"
    bubble  <- loadWAV "bubbles.wav" 
    mData   <- loadMap "mapFile" 

    l0 <- loadImage "Layer.png" 
    bg <- loadImage "Background.png"
    l1 <- loadImage "Layer #1.png" 
    ch <- loadImage "Layerch.png"
    l2 <- loadImage "Layer #3.png"
    s1 <- loadImage "spriteSheet1.png"
    s2 <- loadImage "spriteSheet2.png"
    --ca <- SDL.createRGBSurface [SDL.SWSurface] canvasWidth canvasHeight 32 255 255 255 0
    ca <- loadImage "blanck.png"
    fi <- loadImage "fish.png"
    iN <- loadImage "black.png"
    bL <- loadImage "black.png"

    cu <- loadImage "cursor.png"

    dics <- initChatBot

    fnt <- SDLT.openFont fontName fontSize

    SDL.setAlpha ca [SDL.SWSurface] 0
    SDL.setAlpha iN [SDL.SWSurface] 0

    let canvas = addMusic music $ makeEntity ca (surfaceSize ca) (ext[0], ext[0]) static 1 "canvas" 
        land = makeEntity l2 (canvasWidth,canvasHeight) (cycle [3840,3830..0],ext [0]) static 1 "land"
        av   = addSound step "step" $ makeEntity s2 (320,240) (ext[1920],ext [0]) (ext[50],ext [150]) 8 "avatar"
        b1   = makeEntity bg (canvasWidth,canvasHeight) (ext [0],ext [0]) static 1 "back1"
        b2   = linkEntities b1 $ makeBg l0 "back2"
        f1   = linkEntities b1 $ makeBg l1 "back3"
        int  = makeEntity iN (surfaceSize iN) (ext[0], ext[0]) (pos (0,canvasHeight)) 1 "interface"
        bl   = makeEntity bL (surfaceSize bL) (ext[0], ext[0]) (static) 1 "black"
        curs = makeEntity cu (surfaceSize cu) (slow 30 $ cycle [0,10],ext [0]) (ext [5],ext [terminalHeight-26]) 2 "cursor"

        smallLady = makeEntity s1 (320,240) (slow 30 $ cycle [0,320..1280],ext [0]) (pos (250,71))  5 "smalllady"
        newLady   = makeEntity s2 (320,240) (slow 10 $ cycle [0,320..2240],ext [0]) (pos (650,150)) 8 "newlady"
        fish      = addSound bubble "bubble" $ makeEntity fi (64,38) (slow 10 $ cycle [0,64..448],ext [0]) (pos (610,155)) 8 "sushi"
        
        world = World av [land] [f1] [b1,b2] [newLady] [smallLady,fish] canvas [int,bl,curs] [] screen (Change [] 0 0 False False [] [] False ) (fst mData) (snd mData) fnt dics
        

    playMus world
    setMusicVolume 24
    loop world
    closeAudio

    where loop w = do
            
            start <- SDL.getTicks 
            
            refreshMus

            rawWorld <- renderWorld w
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
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) -> return (True,w)
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_LCTRL _ _)) -> whileEvents (applyToEntity w "sushi" (\e -> e {audioList = ext [Just ("bubble",True,0,2)]}))
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT _ _)) -> whileEvents w
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _ _)) -> whileEvents w
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_RETURN _ _)) -> let out = toChatBot w (reverse.usrStr.changes $ w) in whileEvents $ processOutput out (w{changes = (changes w){usrStr = []}})
                  (SDL.MouseButtonDown x y _) -> whileEvents $ updateMouse (x,y) w 
                  

                  SDL.NoEvent -> return (False,w)
                  _       -> whileEvents $ keyToString w event

---------------------------------------------------------------------------------------------------
{-Data Initialisation-}

makeBg :: SDL.Surface -> String -> Entity
makeBg src name = Entity { name = name,
                          surface = src,
                          frameList = cycle [Nothing],
                          posList = cycle [Just SDL.Rect { SDL.rectX = 0,
                                                           SDL.rectY = 0,
                                                           SDL.rectW = 0,
                                                           SDL.rectH = 0 }],
                          nbrFrame = 1,
                          audioData = None,
                          audioList = ext [Nothing]
                        }

makeEntity :: SDL.Surface -> (Int,Int) -> ([Int],[Int]) -> ([Int],[Int]) -> Int -> String -> Entity
makeEntity src (fw,fh) (fxs,fys) (xs,ys) n name = Entity { name = name,
                                                   surface = src,
                                                    frameList = makeRect (fw,fh) fxs fys,
                                                    posList = makeRect (0,0) xs ys,
                                                    nbrFrame = n,
                                                    audioData = None,
                                                    audioList = ext [Nothing]
                                                    }
  where makeRect _ [] [] = cycle [Nothing]
        makeRect  (w,h) (x:xs) (y:ys) = Just SDL.Rect { SDL.rectX = x,
                                                       SDL.rectY = y,
                                                       SDL.rectW = w,
                                                       SDL.rectH = h } : makeRect (w,h) xs ys

----------------------------------------------------------------------------------------------------
{- events processing -}

updateWorld :: World -> World
updateWorld wo = if ((updated $ changes wo ) == False) then wo   
                 else let w = applyChanges wo in w 

applyChanges :: World -> World
applyChanges w |(clicked $ changes w) = let((cfx,cfy),_) = getCurrent $ canvas w 
                                           newW = goTo (cfx + (mouseX $ changes w),cfy + (mouseY $ changes w)) w
                                        in newW {changes = (changes newW){clicked = False,updated = False}}
               |otherwise = w

updateMouse :: (Word16,Word16) -> World -> World
updateMouse (x,y) w = w {changes = (changes w) {mouseX = fI x, mouseY = fI y,clicked = True,updated = True}}

-----------------------------------------------------------------------------------------------------
{-Rendering-}

renderEntity :: Entity -> SDL.Surface -> IO Entity
renderEntity src dest = do SDL.blitSurface (surface src) (head.frameList $ src)  dest (head.posList $ src)
                           newSrc <- playSounds.prepSound $ src
                           return (nextEntity newSrc)

renderEntities :: [Entity] -> SDL.Surface -> IO [Entity] 
renderEntities xs dest = sequence $ map (\im -> renderEntity im dest) xs

renderNamedEntity :: String -> [Entity] -> SDL.Surface -> IO [Entity]
renderNamedEntity _ [] _ = return []
renderNamedEntity s (e:es) out | (name e) == s = (renderEntity e out) >>= (\newE -> return (newE:es))
                               | otherwise = (renderNamedEntity s es out) >>= (\tl -> return (e:tl))

renderMenu :: World -> IO World
renderMenu w = do let ls = linesToSur [((0,canvasHeight),(screenWidth-1 ,canvasHeight)),
                                          ((0,screenHeight-1),(screenWidth-1,screenHeight-1)),
                                          ((0,canvasHeight),(0,screenHeight-1)),
                                          ((screenWidth-1,canvasHeight),(screenWidth - 1,screenHeight-1)),
                                          ((terminalWidth, canvasHeight),(terminalWidth,screenHeight)),
                                          ((0,screenHeight - 30),(terminalWidth,screenHeight - 30)),
                                          ((terminalWidth,canvasHeight+30),(screenWidth,canvasHeight+30))] (screen w) (getPixel 0 0 255)

                  let inter = (fromJust $ find (\e -> name e == "interface") (menu w))
                      black = (fromJust $ find (\e -> name e == "black") (menu w))
                      cursor = (fromJust $ find (\e -> name e == "cursor") (menu w))
                      m0 = inter:black:[fixCursor w cursor (lineFormat - 20)]
                  
                  m1 <- renderNamedEntity "black" (m0) (surface inter)
                  textToSur "Inventory:" (font w) (terminalWidth + 5) 10 1 20 (surface inter)
                  textToSur (reverse.usrStr.changes $ w) (font w) 5 (terminalHeight - 25) 1 (lineFormat - 20) (surface inter)  
                  textToSur  (concat.buffer.changes $ w)(font w) 5 10 8 lineFormat (surface inter)
                  textToSur (inventoryToString w) (font w) (terminalWidth + 5) 40 8 40 (surface inter)
                  m2 <- renderNamedEntity "cursor" m1 (surface inter)
                  newInt <- renderEntity inter (screen w)

                  sequence ls
                  return w {menu = m2}


renderWorld :: World -> IO World
renderWorld wo = do  w         <- renderMenu.fixRelPos.fixCamera $ wo 
                     l         <- renderEntities (land w) (surface $ canvas w)
                     newBg     <- renderEntities (bg w) (surface $ canvas w)
                     newMiscBg <- renderEntities (miscBg w) (surface $ canvas w)
                     av        <- renderEntity (avatar w) (surface $ canvas w)
                     newMiscFg <- renderEntities (miscFg w) (surface $ canvas w)
                     newFg     <- renderEntities (fg w) (surface $ canvas w)
                     newCan    <- renderEntity (canvas w) (screen w)

                     return w { avatar  = av,
                                        land    = l,
                                        fg      = newFg,
                                        bg      = newBg,
                                        miscFg  = newMiscFg,
                                        miscBg  = newMiscBg,
                                        canvas  = newCan
                                       }


fixCamera :: World -> World
fixCamera w =
 let (_,(apx,apy)) = getCurrent $ avatar w
     ((cfx,cfy),_) = getCurrent $ canvas w
                  
                  
     newFX | s == 0 = cfx
           | (apx > cfx + screenWidth - 400) && (cfx + (abs s) < (canvasWidth - screenWidth)) = cfx + (abs s) 
           | (apx < cfx + 45 ) && (cfx - (abs s) > 0) = cfx - (abs s)
           | otherwise = cfx
                  
     newFl [] = []
     newFl (Nothing:fs) = []
     newFl (Just r:fs) = Just (r {SDL.rectX = newFX}): newFl fs 
                  
     in w {canvas = (canvas w) {frameList = newFl $ frameList (canvas w)}}              
              
     where s = computeSpeed $ avatar w


fixRelPos :: World -> World
fixRelPos w = let (_,posy) = avatarCenter w
                  mFg = miscFg w
                  mBg = miscBg w
                  
                  p = (\e -> snd (entityCenter e) > posy)

                  (xs,ys) = partition p mFg
                  (ws,zs) = partition p mBg

                  newFg = xs ++ ws
                  newBg = ys ++ zs
              in w {miscFg = newFg, miscBg = newBg}

nextEntity :: Entity -> Entity
nextEntity e = e { frameList = tail $ frameList e,
                   posList = tail $ posList e,
                   audioData = audioData e,
                   audioList = tail $ audioList e
                  }

--------------------------------------------------------------------------------------------------
{- avatar movment-}

goTo :: (Int,Int) -> World -> World
goTo (x,y) w = if (x > canvasWidth) || (y > canvasHeight) then w else
               let origin = (pixToMap w).avatarCenter $ w
                   dest = pixToMap w (x,y)
                   newDest = if elem dest (mapData w) 
                             then dest
                             else findClosest (mapData w) dest

                   mapPath = (pathFinder (mapData w) origin newDest)
                   path = interpolate.(fixPath w) $ map (\c -> mapToPix w c) mapPath
                                                
                   fl = ext (makeFrameList w path)

                   audLi = ext  ((take (length path - 10) $ cycle [Just ("step",True,(0),25)])++[Just ("step",False,(0),25),Nothing])

               in w {avatar = (avatar w){ posList = makePosRect.ext $ path, frameList = fl, audioList = audLi}}

goToward :: World -> String -> World
goToward w s =
 case getEntity ((miscBg w)++(miscFg w)) s of
                       Nothing -> w
                       Just e  -> goTo (entityCenter e) w


avatarCenter :: World -> (Int,Int)
avatarCenter w = let (_,(apx,apy)) = getCurrent $ avatar w
                     spriteW = div (SDL.surfaceGetWidth $ surface $ avatar w) (nbrFrame.avatar $ w)
                     spriteH = SDL.surfaceGetHeight $ surface $ avatar w
                 in (apx+(quot spriteW 2),apy+(quot spriteH 2))


fixPath :: World -> [(Int,Int)] -> [(Int,Int)]
fixPath w xs = let spriteW = div (SDL.surfaceGetWidth $ surface $ avatar w) nbrFrameSprite
                   spriteH = SDL.surfaceGetHeight $ surface $ avatar w

                   go [] = []
                   go ((x,y):xs) = (x-(quot spriteW 2),y-(quot spriteH 2)):go xs
              in go xs

{- Generate the frameList corresponding to the planned path xs-}
makeFrameList:: (Ord a, Ord a1) => World -> [(a1, a)] -> [Maybe SDL.Rect]
makeFrameList w xs = 
  let upFrameList = zip (ext [1920]) (ext [0])
      downFrameList = zip (ext [2240]) (ext [0])
      leftFrameList = zip (slow 10 $ cycle [0,320..640]) (ext [0])
      rightFrameList = zip (slow 10 $ cycle [960,1280..1600]) (ext [0])

      spriteW = div (SDL.surfaceGetWidth $ surface $ avatar w) nbrFrameSprite
      spriteH = SDL.surfaceGetHeight $ surface $ avatar w

      frames [] _ = []
      frames ((x1,y1):(x2,y2):[]) (u,d,l,r) | x1 == x2 = if y1 > y2
                                                         then [head d]
                                                         else [head u]
                                                               
                                            | y1 == y2 = if x1 > x2 
                                                         then [head l]
                                                         else [head r]
                    
      frames ((x1,y1):t@(x2,y2):xs) (u,d,l,r) | x1 == x2 = if y1 > y2
                                                           then head d:frames (t:xs) (u,tail d,l,r)
                                                           else head u:frames (t:xs) (tail u,d,l,r)
                                                                 
                                              | y1 == y2 = if x1 > x2
                                                           then head l:frames (t:xs) (u,d,tail l,r)
                                                           else head r:frames (t:xs) (u,d,l,tail r)
                     
  in makeFrameRect (frames xs (upFrameList,downFrameList,leftFrameList,rightFrameList)) spriteH spriteW


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
findClosest m dest = let (rx,ry) = (fI $ fst dest, fI $ snd dest)
                         distMap = map (\(x,y) -> (sqrt ((fI x - rx)^2+(fI y - ry)^2),(x,y))) m
                         minDist l@(a,_) r@(b,_) | a < b = l
                                                 |otherwise = r 
                     
                     in snd $ foldl' minDist (head distMap) distMap 

-------------------------------------------------------------------------------------------------
{- Graphics -}

type Point = (Double,Double)

getPixel :: Word8 -> Word8 -> Word8 -> SDL.Pixel
getPixel r g b = SDL.Pixel $ (shiftL (fi r) 24 .|. shiftL (fi g) 16 .|. shiftL (fi b) 8 .|. (fi 255)) where 
  fi a = fI a

pixelsToScreen :: [Point] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
pixelsToScreen xs s p = map (\(x,y) -> SDLP.pixel s (fI.round $ x) (fI.round $ y) p) xs

linesToSur :: [((Int,Int),(Int,Int))] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
linesToSur xs s p = map (\((x1,y1),(x2,y2)) -> SDLP.line s (fI x1) (fI y1) (fI x2) (fI y2) p) xs

loadImage :: String -> IO SDL.Surface
loadImage filename = SDLI.load filename  >>= SDL.displayFormatAlpha

applySurface :: Int -> Int -> SDL.Surface -> SDL.Surface -> IO Bool
applySurface x y src dst = SDL.blitSurface src clip dst offset
 where offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 0, SDL.rectH = 0 }
       clip = Just SDL.Rect { SDL.rectX = 0, SDL.rectY = 0, SDL.rectW = fst $ surfaceSize src, SDL.rectH = snd $ surfaceSize src }

---------------------------------------------------------------------------------------------------
{- Terminal Interface -}

getTextSurface :: String -> SDLT.Font -> IO SDL.Surface
getTextSurface s f = SDLT.renderTextSolid f s (SDL.Color 191 191 191)

textToSur :: String -> SDLT.Font -> Int -> Int-> Int -> Int -> SDL.Surface -> IO Bool
textToSur text f x y nl nc out =
  let ts = concat $ (format nc) (lines text)
      sl = map (flip getTextSurface $ f) (drop (length ts - nl) ts)
  in do ls <- sequence sl
        foldl' (\a (s,(x,y)) -> do b1 <- a
                                   b2 <- applySurface x y s out
                                   return (b1 && b2)) (return True) (zip ls [(x,y) | y <- [y,y+20..]])

toChatBot :: World -> String -> BotOutput
toChatBot w s = parseInput s (botData w)

processOutput :: BotOutput -> World -> World
processOutput (Go s) w = 
  case getEntity (miscObj w) s of Nothing -> addToBuffer w ("System\\> Sorry, can't find "++s)
                                  Just e -> goTo (entityCenter e) (addToBuffer w ("System\\> Going toward: " ++ s))
processOutput o w = addToBuffer w ("ChatBot\\> "++ show o)

---------------------------------------------------------------------------------------------------
{- Sound -}


addMusic :: Music -> Entity -> Entity
addMusic m e = e {audioData = SoundTrack m}

addSound :: Chunk -> String -> Entity -> Entity
addSound s t e = 
  let ad = audioData e
  in case ad of None -> e {audioData = Effects [Effect t False 0 s defVol Nothing]}
                SoundTrack _ -> e {audioData = Effects [Effect t False 0 s defVol Nothing]}
                Effects eff  -> e {audioData = Effects ((Effect t False 0 s defVol Nothing):eff)}


refreshMus = playingMusic

playMus :: World -> IO ()
playMus w = case audioData.canvas $ w of
             SoundTrack m -> playMusic m (-1)
             otherwise -> return ()


playSounds :: Entity -> IO Entity
playSounds e =
  case audioData e of
    None -> return e
    SoundTrack _ -> return e
    Effects fs -> do newEff <- sequence (map play fs)
                     return e {audioData = Effects newEff}

    where play f | not $ played f = return f
                 | otherwise = 
                    case (channel f) of 
                      Nothing -> do newC <- playChannel (-1) (audio f) (loop f)
                                    volume newC (vol f)
                                    return f { channel = Just newC }

                      Just ch -> do playing <- isChannelPlaying ch
                                    if playing
                                    then return f
                                    else playChannel ch (audio f) (loop f) >> volume ch (vol f) >> return f


prepSound :: Entity -> Entity
prepSound e =
 case head.audioList $ e of
  Nothing -> e
  Just (tag,played,loop,vol) -> changeSound e tag played loop vol


changeSound :: Entity -> String -> Bool -> Int -> Int -> Entity
changeSound e t b l v =
 case audioData e of
  None -> e
  SoundTrack _ -> e
  Effects eff  -> let p f | (tag f) == t = f {played = b,
                                              loop = l,
                                              vol = v,
                                              channel = if b then channel f else Nothing}
                          | otherwise = f  
                      
                      newEff = map p eff
                  in e {audioData = Effects newEff}  
---------------------------------------------------------------------------------------------------
{- File processing -}

saveData :: World -> IO ()
saveData w = let path = head.misc.changes $ w
                 toWrite = (show.mapData $ w) ++ "\n" ++ (show.gridWH $ w) 
              in writeFile path toWrite

loadMap :: String -> IO ([(Int,Int)],(Int,Int))
loadMap path = do handle <- openFile path ReadMode
                  strMap <- hGetLine handle
                  strGrid <- hGetLine handle
                  hClose handle
                  return ((read strMap :: [(Int,Int)]),read strGrid :: (Int,Int))


----------------------------------------------------------------------------------------------------
{- Engine Helper functions-}

rectList2Coord :: [Maybe SDL.Rect] -> ([Int],[Int])
rectList2Coord (x:xs) =  case x of Nothing -> ([],[])
                                   Just r  -> let newX = SDL.rectX r
                                                  newY = SDL.rectY r
                                                  (newXs,newYs) = rectList2Coord xs
                                               in (newX:newXs,newY:newYs)


getCurrent :: Entity -> ((Int,Int),(Int,Int))
getCurrent i = 
  let frames = frameList i
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


pixToMap :: World -> (Int,Int) -> (Int,Int)
pixToMap w (x,y) = let (gW,gH) = gridWH $ w
                       [dx,dy,dGw,dGh] = map fI [x,y,gW,gH] 
                   in  (round (dx/dGw),round (dy/dGh))



mapToPix :: World -> (Int,Int) -> (Int,Int)
mapToPix  w (x,y) = let (gW,gH) = gridWH $ w
                        [dx,dy,dGw,dGh] = map fI [x,y,gW,gH] 
                    in  (dx*dGw,dy*dGh)                   





linkEntities :: Entity -> Entity -> Entity
linkEntities main child = child {posList = posList main, frameList = frameList main}

computeSpeed :: Entity -> Int
computeSpeed i = case posList $ i of (Just a: Just b:rs) -> SDL.rectX b - SDL.rectX a

getEntity :: [Entity] -> String -> Maybe Entity
getEntity xs s = find (\e -> name e == s) xs

applyToEntity :: World -> String -> (Entity -> Entity) -> World
applyToEntity w tag f | tag == "avatar" = w {avatar = f (avatar w)}
                      | tag == "canvas" = w {canvas = f (canvas w)}
                      | otherwise =
                         let newLand = go (land w)
                             newFg = go (fg w)
                             newBg = go (bg w)
                             newMiscFg = go (miscFg w)
                             newMiscBg = go (miscBg w)
                             newMenu = go (menu w)
                         in w {land = newLand,
                               fg = newFg,
                               bg = newBg,
                               miscFg = newMiscFg,
                               miscBg = newMiscBg,
                               menu = newMenu
                             }
                         
                         where go []     = []
                               go (e:es) | (name e) == tag = (f e):go es
                                         | otherwise       = e:go es 

entityCenter :: Entity -> (Int,Int)
entityCenter e = let (_,(apx,apy)) = getCurrent e
                     spriteW = div (SDL.surfaceGetWidth $ surface e) (nbrFrame e)
                     spriteH = SDL.surfaceGetHeight $ surface e
                 in (apx+(quot spriteW 2),apy+(quot spriteH 2))

surfaceSize :: SDL.Surface -> (Int,Int)
surfaceSize s = (SDL.surfaceGetWidth s, SDL.surfaceGetHeight s)

static = (ext [0],ext [0])
pos (x,y) = (ext [x],ext [y])

miscObj w = (miscBg w) ++ (miscFg w)

inventoryToString :: World -> String
inventoryToString w = concatMap (\e -> name e ++ " ") (inventory w)

fixCursor :: World -> Entity -> Int -> Entity
fixCursor w c nc = let len = length (usrStr.changes $ w)
                       ((_,_),(_,apy)) = getCurrent c
                       newPosList = makePosRect.ext $ [((if len >= nc then 8*(mod len nc) else (5+len * 8)),apy)]
                   in c {posList = newPosList}


addToBuffer w s = w {changes = (changes w){buffer = (buffer.changes $ w)++[s++"\n"]}}

keyToString :: World -> SDL.Event -> World
keyToString w e = 
  let str = (usrStr.changes $ w)
      addC c w = w {changes = (changes w){usrStr = c:str}} in

      case e of (SDL.KeyDown (SDL.Keysym SDL.SDLK_a _ _ )) -> addC 'a' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_b _ _ )) -> addC 'b' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_c _ _ )) -> addC 'c' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_d _ _ )) -> addC 'd' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_e _ _ )) -> addC 'e' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_f _ _ )) -> addC 'f' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_g _ _ )) -> addC 'g' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_h _ _ )) -> addC 'h' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_i _ _ )) -> addC 'i' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_j _ _ )) -> addC 'j' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_k _ _ )) -> addC 'k' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_l _ _ )) -> addC 'l' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_m _ _ )) -> addC 'm' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_n _ _ )) -> addC 'n' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_o _ _ )) -> addC 'o' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_p _ _ )) -> addC 'p' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_q _ _ )) -> addC 'q' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_r _ _ )) -> addC 'r' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_s _ _ )) -> addC 's' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_t _ _ )) -> addC 't' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_u _ _ )) -> addC 'u' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_v _ _ )) -> addC 'v' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_w _ _ )) -> addC 'w' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_x _ _ )) -> addC 'x' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_y _ _ )) -> addC 'y' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_z _ _ )) -> addC 'z' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_QUOTE _ _ )) -> addC '\'' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_EXCLAIM _ _ )) -> addC '!' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _ )) -> addC ' ' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_COMMA _ _ )) -> addC '?' w
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_DELETE _ _ )) -> w {changes = (changes w){usrStr = []}}
                (SDL.KeyDown (SDL.Keysym SDL.SDLK_BACKSPACE _ _ )) -> w {changes = (changes w){usrStr = if str == [] then str else tail str}}
                _ -> w



--------------------------------------------------------------------------------------------------------
{- misc helper functions-}

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
interpolate ((x1,y1):t@(x2,y2):xs)
            | x1 == x2 = let step = if y1 < y2
                                    then 5
                                    else (-5) in [(x1,y) | y <- [y1,y1+step..(y2-step)]]  ++ interpolate (t:xs)
                                    
            | y1 == y2 = let step = if x1 < x2 
                                    then 5 
                                    else (-5) in [(x,y1) | x <- [x1,x1+step..(x2-step)]]  ++ interpolate (t:xs)


findNearby :: (Int,Int,Int) -> [(Int,Int,Int)]
findNearby (x,y,n) = [(x+1,y,n+1),(x,y+1,n+1),(x-1,y,n+1),(x,y-1,n+1)]



deleteAllBy :: (a -> Bool) -> [a] -> [a]
deleteAllBy p = filter (not.p)

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

format :: Int -> [String] -> [[String]]
format  n xs = let chunk _ [] = ([],[])
                   chunk n (x:xs) | n == 0 = ([x],xs)
                               | n < 15 && x == ' ' = ([x],xs) 
                               | otherwise = let (a,b) = chunk (n-1) xs in (x:a,b)
                   
                   go cs = case (chunk n cs) of (c,[]) -> [c]
                                                (c,r)  -> [c] ++ go r

               in map go xs