move :: [Image] -> [(Int,Int)] -> [Image]
move frames xs = modifOthers.modifTrain.modifChar $ (frames,xs)
                 
                 where modifChar ([], _) = [] 
                       modifChar ((f:fs),xs) | (name f == "char") = Image { name = name f,
                                                                          surface = surface f,
                                                                          frameList = frameList f,
                                                                          posList = makePosRect xs }:modifChar (fs,xs)
                                             | otherwise = f:modifChar (fs,xs)

                       modifTrain frames =  case find (\x -> name x == "char") frames
                                            of Nothing -> [] 
                       	                       Just char -> let go [] = []
                                                                go (f:fs) | name f == "train" = (follow char f):go fs
                                                                          | otherwise = f:go fs
                                                            in go frames

                       modifOthers frames = case find (\x -> name x == "char") frames
                                            of Nothing -> [] 
                       	                       Just train -> let go [] = []
                                                                 go (f:fs) | (name f /= "train") && (name f /= "char")= (anchor train f):go fs
                                                                           | otherwise = f:go fs
                                                             in go frames
                                                         

move :: [Image] -> [(Int,Int)] -> [Image]
move frames xs = case find (\x -> name x == "char") frames
                 of Nothing -> []
                    Just char -> let newChar = Image { name = name char,
                                                       surface = surface char,
                                                       frameList = frameList char,
                                                       posList = makePosRect xs }



--changeDic = Map.fromList [("space",dummyChange),("toto",map)]

--keishiki = ()
{-
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
    

    let keishiki = makeImage l2 (640,480) (cycle [1280,1270..0],ext [0]) (ext [0],ext [0]) "keishiki1"
        --b1        = makeImage bg (640,480) (osc 0 100 1, slow 5 $ osc 0 25 1) (ext [0],ext [0]) "train"
        --b2        = linkImages b1 $ makeBg l0 "train"
        --f1        = linkImages b1 $ makeBg l1 "train"
        --char      = anchor b1 $ makeImage ch (640,480) ([],[]) (osc 150 700 2,(slow 5 $ osc 150 125 (-1))) "char"
        
        char      = makeImage s2 (320,240) (ext[1280],ext [0]) (ext [150],ext[150]) "char"
        (b1,c1)   = follow char $ makeImage bg (640,480) (ext [0], slow 5 $ osc 0 25 1) (ext [0],ext [0]) "train"
        b2        = linkImages b1 $ makeBg l0 "train"
        f1        = linkImages b1 $ makeBg l1 "train"
        
        smallLady = anchor b1 $ makeImage s1 (320,240) (slow 30 $ cycle [0,320..1280],ext [0]) (ext [250],ext[71]) "smallLady"
        newLady   = anchor b1 $ makeImage s2 (320,240) (slow 10 $ cycle [0,320..2240],ext [0]) (ext [650],ext[150]) "newLady"
        frames = keishiki:b1:b2:smallLady:c1:newLady:[f1]



    drawSurfaces frames screen

    SDL.flip screen

    loop frames screen []

    where loop frames screen changes = do
            start <- SDL.getTicks
            
            rawFrames <- drawSurfaces frames screen
            SDL.flip screen 
            (quit,changes) <- whileEvents changes
            
            
            let !(nextFrames, remainingChanges) = applyChanges changes rawFrames
            stop <- SDL.getTicks
            let del = (delay start stop)
            
            --SDL.delay (delay start stop)
            --putStrLn $ show (del)
            unless (del == 0) (SDL.delay del)
            unless quit (loop nextFrames screen remainingChanges)

    
          whileEvents changes = do
             
             event      <- SDL.pollEvent
             case event of
                  SDL.Quit -> return (True,changes)
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _)) -> whileEvents ("space":changes)
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT _ _)) -> whileEvents ("left":changes)
                  (SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _ _)) -> whileEvents ("right":changes)

                  SDL.NoEvent -> return (False,changes)
                  _        -> whileEvents changes

          applyChanges [] frames = (frames,[])
          applyChanges (x:xs) frames | x == "space" = (dummyChange frames "smallLady",xs)
                                     | otherwise = let (newFr,newCh) = applyChanges xs frames in (newFr,x:newCh)  
    

-}


drawWorld :: World -> IO World
drawWorld w = let back = surface $ head $ bg w in
              do  l         <- displayImg (land w) (screen w)
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