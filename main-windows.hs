
module Main where

import System.Console.ANSI (setSGR, clearScreen, hideCursor, setCursorPosition, showCursor, xterm6LevelRGB)
import System.Console.ANSI (SGR(Reset, SetPaletteColor), ConsoleLayer(Foreground))
import System.IO.NoBufferingWorkaround
import System.Random (randomRIO)
import System.IO

import Control.Concurrent (threadDelay)
import Data.List.Unique (sortUniq)
import Data.Colour.SRGB (sRGB24)
import Data.Word
import Data.Char

type Grid = [[Word8]]
type Stats = (Word32, Word16, Word8)
type Position = (Word8, Word8)


main :: IO()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    initGetCharNoBuffering
    play

mainGrid :: Grid
mainGrid =  [[1,1,1,1,1,1],
            [1,1,1,1,1,1],
            [1,1,1,1,1,1],
            [1,1,1,1,1,1],
            [1,1,1,1,1,1],
            [1,1,1,1,1,1],
            [1,1,1,1,1,1],
            [1,1,1,1,1,1],
            [1,1,1,1,1,1],
            [1,1,1,1,1,1],
            [1,1,1,1,1,1],
            [1,1,1,1,1,1],
            [1,1,1,1,1,1],
            [1,1,1,1,1,1],
            [1,1,1,1,1,1]]

gridWidth :: Word8
gridWidth = 6

gridHeight :: Word8
gridHeight = 15

gArr :: Word8 -> [t] -> t
gArr 0 (x:xs) = x
gArr n (x:xs) = gArr (n-1) xs

uArr :: Word8 -> a -> [a] -> [a]
uArr 0 v (x:xs) = v : xs
uArr p v (x:xs) = x : uArr (p-1) v xs

getPos :: Word8 -> Word8 -> [[a]] -> a
getPos l c x = gArr c (gArr l x)

setPos :: Word8 -> Word8 -> a -> [[a]] -> [[a]]
setPos 0 c v (x:xs) = (uArr c v x) : xs
setPos l c v (x:xs) = x : setPos (l-1) c v xs
setPos _ _ _ [] = []

countArr :: [a] -> Word8
countArr [] = 0
countArr (x:xs) = 1 + countArr xs

countArr16 :: [a] -> Word16
countArr16 [] = 0
countArr16 (x:xs) = 1 + countArr16 xs

-------------------------------------------------------------
moveDown :: Grid -> (Grid,Bool)
moveDown grid = checkPieces (gridHeight-2) (gridWidth-1) grid False
    where checkPieces :: Word8 -> Word8 -> Grid -> Bool -> (Grid,Bool)
          checkPieces l c grid1 moved
              | (inRange l c) && ((getPos l c grid1) /= 0) && ((getPos (l+1) c grid1)==0) = checkPieces l (c-1) (setPos l c 0 (setPos (l+1) c (getPos l c grid1) grid1)) True
              | inRange l c = checkPieces l (c-1) grid1 moved
              | c == 255 = checkPieces (l-1) (gridWidth-1) grid1 moved
              | l == 255 = (grid1,moved)

inRange :: Word8 -> Word8 -> Bool
inRange a b = ((a >= 0) && (a < gridHeight)) && ((b >= 0) && (b < gridWidth))

inRangeT :: Position -> Bool
inRangeT (a,b) = ((a >= 0) && (a < gridHeight)) && ((b >= 0) && (b < gridWidth))

word16_to_word32 :: Word16 -> Word32
word16_to_word32 = fromIntegral

word8_to_word32 :: Word8 -> Word32
word8_to_word32 = fromIntegral

drawLine :: Grid -> Word8 -> IO()
drawLine grid n = do
    let line = gArr n grid
    foldr (>>) (putStr "") (map (printC "██" ) line)
    
printGrid :: Grid -> Grid -> Stats -> IO()
printGrid grid nextPiece (score,blocks,lvl) = do
    setCursorPosition 0 0
    let l = getLvlColor lvl
    printC ("   ╔════"              ++                   "╗   ╔═1═2═3═4═5═6══"         ++          "╗   ╔═════════╗\n") l
    putStr  "   ║ "   >> drawLine nextPiece 0 >> printC " ║   ║ "  l >> drawLine grid 2 >> printC " ║   ║  Score  ║\n"  l
    putStr  "   ║ "   >> drawLine nextPiece 1 >> printC " ║   ║ "  l >> drawLine grid 3 >> printC(" ║   ║ "++(getScore score)++" ║\n") l
    putStr  "   ║ "   >> drawLine nextPiece 2 >> printC " ║   ║ "  l >> drawLine grid 4 >> printC " ║   ║  Block  ║\n"  l
    putStr ("   ╚════"               ++                  "╝   ║ ")   >> drawLine grid 5 >> printC(" ║   ║  "++(getBlocks blocks)++"  ║\n") l
    putStr                                       "            ║ "    >> drawLine grid 6 >> printC " ║   ║  Level  ║\n"  l
    putStr                                       "            ║ "    >> drawLine grid 7 >> printC(" ║   ║   "++(getLvl lvl)++"   ║\n") l
    putStr                                       "            ║ "    >> drawLine grid 8 >> printC " ║   ╚═════════╝\n"  l
    putStr                                       "            ║ "    >> drawLine grid 9 >> printC " ║\n" l
    putStr                                       "            ║ "    >> drawLine grid 10>> printC " ║\n" l
    putStr                                       "            ║ "    >> drawLine grid 11>> printC " ║\n" l
    putStr                                       "            ║ "    >> drawLine grid 12>> printC " ║\n" l
    putStr                                       "            ║ "    >> drawLine grid 13>> printC " ║\n" l
    putStr                                       "            ║ "    >> drawLine grid 14>> printC " ║\n" l
    putStr                                      ("            ╚═1═2═3═4═5═6══"         ++          "╝\n")

printStats :: Stats -> IO()
printStats (score,blocks,lvl) = do
    let l = getLvlColor lvl
    setCursorPosition 2 33 >> printC(getScore score) l
    setCursorPosition 4 34 >> putStr(getBlocks blocks)
    setCursorPosition 6 35 >> putStr(getLvl lvl)

printGridOnly :: Grid -> Grid -> IO()
printGridOnly grid nextPiece = do
    setCursorPosition 1 14  >> drawLine grid 2 
    setCursorPosition 2 14  >> drawLine grid 3 
    setCursorPosition 3 14  >> drawLine grid 4 
    setCursorPosition 4 14  >> drawLine grid 5
    setCursorPosition 5 14  >> drawLine grid 6
    setCursorPosition 6 14  >> drawLine grid 7
    setCursorPosition 7 14  >> drawLine grid 8
    setCursorPosition 8 14  >> drawLine grid 9
    setCursorPosition 9 14  >> drawLine grid 10
    setCursorPosition 10 14 >> drawLine grid 11
    setCursorPosition 11 14 >> drawLine grid 12
    setCursorPosition 12 14 >> drawLine grid 13
    setCursorPosition 13 14 >> drawLine grid 14
    setCursorPosition 1 5   >> drawLine nextPiece 0 
    setCursorPosition 2 5   >> drawLine nextPiece 1 
    setCursorPosition 3 5   >> drawLine nextPiece 2 

-- Usado pelas funções de Print
getLvlColor :: Word8 -> Word8
getLvlColor n = (n `mod` 9) + 6

getScore :: Word32 -> String
getScore n = zeros (7 - countArr(show(n))) ++ show(n)

getBlocks :: Word16 -> String
getBlocks n = zeros (5 - countArr(show(n))) ++ show(n)

getLvl :: Word8 -> String
getLvl n = zeros (3 - countArr(show(n))) ++ show(n)

zeros :: Word8 -> String
zeros 0 = []
zeros n = '0' : zeros (n-1)
----------------------------
play :: IO ()
play = do
    hideCursor
    resetScreen
    a <- randomRIO (1,6)
    b <- randomRIO (1,6)
    c <- randomRIO (1,6)
    let nextPiece = [[a],[b],[c]]
    let matchList = getMatchesGrid (gridHeight-1) (gridWidth-1) mainGrid
    let removedGrid = setList matchList 0 mainGrid
    let score = 0
    printGrid removedGrid nextPiece (0,0,1)
    playLoop removedGrid nextPiece (0,0,1) False
        where
        playLoop :: Grid -> Grid -> Stats -> Bool -> IO ()
        playLoop grid nextPiece (score,blocks,lvl) lvlUp = do
            setCursorPosition 0 0
            pause 30
            printGridOnly grid nextPiece
            let newGrid = moveDown grid
            if (snd newGrid) then do
                playLoop (fst newGrid) nextPiece (score,blocks,lvl) lvlUp
            else do
                let matchList = getMatchesGrid (gridHeight-1) (gridWidth-1) (fst newGrid)
                let b = countArr16(matchList)
                score  <- return (score + ((word16_to_word32 b)*(100+( word8_to_word32 (lvl*2))))) 
                blocks <- return (blocks + b) 
                newLvl <- return (calcLvl blocks)
                lvlUp  <- return ((newLvl /= lvl) || lvlUp)
                lvl    <- return (newLvl)
                let delay = 30

                if (matchList /= []) then do
                    deleteAnim delay matchList (fst newGrid) nextPiece (score,blocks,lvl)
                    let removedGrid = setList matchList 0 (fst newGrid)
                    printStats (score,blocks,lvl)
                    playLoop removedGrid nextPiece (score,blocks,lvl) lvlUp
                else do 
                    
                    if (lvlUp) then do
                        setCursorPosition 0 0
                        printGrid (fst newGrid) ([[255],[255],[255]]) (score,blocks,lvl)
                        col <- getChar
                        a <- randomRIO (1,6)
                        b <- randomRIO (1,6)
                        c <- randomRIO (1,6)

                        if ((charToWord8 col) == 255) then do
                            playLoop (fst newGrid) ([[255],[255],[255]]) (score,blocks,lvl) lvlUp
                        else do
                            let nextGrid = addNewPiece (fst newGrid) ([[255],[255],[255]]) (charToWord8 col)
                            let colorToRemove = getFstFromColumn (charToWord8 col) (fst newGrid)
                            bonus <- playLoopBonus nextGrid ([[255],[255],[255]]) (score,blocks,lvl)

                            if (colorToRemove /= 255) then do
                                let removeList = (getAllPosColor (gridHeight-1) (gridWidth-1) colorToRemove bonus) ++ (getMatchesGrid (gridHeight-1) (gridWidth-1) bonus)
                                let b1 = countArr16(removeList)
                                score  <- return (score + ((word16_to_word32 b1)*(100+( word8_to_word32 (lvl*2))))) 
                                blocks <- return (blocks + b1) 
                                lvl    <- return (newLvl)
                                deleteAnim delay removeList bonus nextPiece (score,blocks,lvl)
                                let removedGrid = setList removeList 0 (fst newGrid)
                                playLoop removedGrid ([[a],[b],[c]]) (score,blocks,lvl) False
                            else do
                                playLoop bonus ([[a],[b],[c]]) (score,blocks,lvl) False
                    else do

                        if (not (gameOver (fst newGrid))) then do
                            col <- getChar

                            if (col == ' ') then
                                playLoop (fst newGrid) [[getPos 2 0 nextPiece],[getPos 0 0 nextPiece],[getPos 1 0 nextPiece]] (score,blocks,lvl) lvlUp
                            else do

                                if ((charToWord8 col) == 255) then do
                                    playLoop (fst newGrid) nextPiece (score,blocks,lvl) lvlUp
                                else do
                                    a <- randomRIO (1,6)
                                    b <- randomRIO (1,6)
                                    c <- randomRIO (1,6)
                                    let nextGrid = addNewPiece (fst newGrid) nextPiece (charToWord8 col)
                                    playLoop nextGrid ([[a],[b],[c]]) (score,blocks,lvl) lvlUp
                        else do
                            showCursor
                            setCursorPosition 4 16
                            printC "GAMEOVER" 3
                            setCursorPosition 15 0
                            setSGR [Reset]
                            return ()

        playLoopBonus :: Grid -> Grid -> Stats -> IO Grid
        playLoopBonus grid nextPiece (score,blocks,lvl) = do
            setCursorPosition 0 0
            pause 30
            printGridOnly grid nextPiece
            let newGrid = moveDown grid

            if (snd newGrid) then do
                playLoopBonus (fst newGrid) nextPiece (score,blocks,lvl)
            else do 
                return (fst newGrid)

getAllPosColor :: Word8 -> Word8 -> Word8 -> Grid -> [Position] 
getAllPosColor l c color grid = aux l 0  
    where
        aux :: Word8 -> Word8 -> [Position] 
        aux l1 c1  
            | (c1 > c) && (l1 /= 255) = (aux (l1-1) 0)
            | (l1 /= 255) && ((getPos l1 c1 grid) == color) = (l1,c1) : (aux l1 (c1+1))
            | (l1 /= 255) = (aux l1 (c1+1))
            | otherwise = []

getFstFromColumn :: Word8 -> Grid -> Word8
getFstFromColumn col grid = aux 0
    where
        aux :: Word8 -> Word8 
        aux l
            | (l == gridHeight) = 255
            | (getPos l col grid) /= 0 = getPos l col grid
            | otherwise = aux (l+1)

deleteAnim :: Int -> [Position] -> Grid -> Grid -> Stats -> IO ()
deleteAnim delay list grid nxtPc stats = do
    colorChange 24
    where
        colorChange :: Word8 -> IO ()
        colorChange 0 = do 
            return ()
        colorChange n = do
            let grid1 = setList list ((n`mod`6)+1) grid
            setCursorPosition 0 0
            printGridOnly grid1 nxtPc
            pause delay 
            colorChange (n-1)

calcLvl :: Word16 -> Word8
calcLvl n
    | n >= 70 = 1 + calcLvl (n-70)
    | otherwise = 1 

setList :: [Position] -> Word8 -> Grid -> Grid
setList [] _ g = g
setList (x:xs) v g = setList xs v (setItem x v g)

setItem :: Position -> Word8 -> Grid -> Grid
setItem (a,b) v g = setPos a b v g

charToWord8 :: Char -> Word8
charToWord8 c
    | c =='1' = 0 
    | c =='2' = 1 
    | c =='3' = 2 
    | c =='4' = 3 
    | c =='5' = 4 
    | c =='6' = 5
    | otherwise = 255

gameOver :: Grid -> Bool
gameOver [] = True
gameOver g = ((foldr (+) 0 (gArr 0 g)) + (foldr (+) 0 (gArr 1 g))) > 0

addNewPiece :: Grid -> Grid -> Word8 -> Grid
addNewPiece grid pc n = (setPos 2 n (getPos 2 0 pc) (setPos 1 n (getPos 1 0 pc) (setPos 0 n (getPos 0 0 pc) grid)))

getColor :: Word8 -> Word8
getColor x 
    | x == 0 = xterm6LevelRGB 0 0 0 -- Black / Empty
    -- Pieces Colors
    | x == 1 = xterm6LevelRGB 0 3 5 -- Light Blue
    | x == 2 = xterm6LevelRGB 0 4 0 -- Green
    | x == 3 = xterm6LevelRGB 5 0 1 -- Red
    | x == 4 = xterm6LevelRGB 5 2 0 -- Orange
    | x == 5 = xterm6LevelRGB 3 0 5 -- Purple
    | x == 6 = xterm6LevelRGB 4 5 0 -- Yellow
    -- Background Colors
    | x == 7 = xterm6LevelRGB 5 2 1 -- 
    | x == 8 = xterm6LevelRGB 2 2 2 --
    | x == 9 = xterm6LevelRGB 5 0 5 --
    | x == 10 = xterm6LevelRGB 0 2 2 --
    | x == 11 = xterm6LevelRGB 4 0 4 --
    | x == 12 = xterm6LevelRGB 0 5 1 --
    | x == 13 = xterm6LevelRGB 5 1 0 --
    | x == 14 = xterm6LevelRGB 0 1 5 --
    -- other
    | x == 15 = xterm6LevelRGB 5 5 5 -- White
    | x == 16 = xterm6LevelRGB 4 4 4 -- Light Grey
    | x == 255 = xterm6LevelRGB 5 5 5 -- bonus piece
    | otherwise = xterm6LevelRGB 5 5 5


printC :: String -> Word8 -> IO()
printC x n = do
     setSGR [SetPaletteColor Foreground $ (getColor n)]
     putStr x

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

pause :: Int -> IO ()
pause n = do
    hFlush stdout
    -- 0.001 second pause
    threadDelay (n * 1000)

getMatchesGrid :: Word8 -> Word8 -> Grid -> [Position]
getMatchesGrid h w grid = sortUniq (getAux h 0 grid) 
    where 
        getAux :: Word8 -> Word8 -> Grid -> [Position]
        getAux h1 w1 grid1
            | (w1 > (w)) && (h1 /= 255) = (getAux (h1-1) 0 grid1)
            | (h1 /= 255) = (getMatchesPos (h1,w1) grid1) ++ (getAux h1 (w1+1) grid1)
            | otherwise = []

        getMatchesPos:: Position -> Grid -> [Position]
        getMatchesPos pos grid = do
            let val = getPos (fst pos) (snd pos) mainGrid
            if (val /= 0) then do
                let matches = gtAllDir pos 5 grid
                if (matches /= []) then do
                    pos : matches
                else []
            else []

        gtAllDir :: Position -> Word8 -> Grid -> [Position]
        gtAllDir (a,b) count grid 
            | count == 0 = []
            | otherwise = do
                let matches = getMatchesDir (a,b) count grid 
                if ((countArr matches)>1) then do
                    matches ++ gtAllDir (a,b) (count-1) grid 
                else do
                    gtAllDir (a,b) (count-1) grid 

        getMatchesDir :: Position -> Word8 -> Grid -> [Position]
        getMatchesDir (a,b) dir grid = do
            if (inRangeT (a,b)) then do
                let c = getPos a b grid
                if (c /= 0) then do
                    let nxtPos = gtNxtPos (a,b) dir
                    if (inRangeT nxtPos) then do
                        let d = getPos (fst nxtPos) (snd nxtPos) grid
                        let nxtMatches = getMatchesDir nxtPos dir grid
                        if ((c == d) ) then do
                            nxtPos : nxtMatches
                        else []
                    else []
                else []
            else []

        gtNxtPos :: Position -> Word8 -> Position
        gtNxtPos (a,b) n
            | n == 1 = (a-1,b)
            | n == 2 = (a-1,b+1)
            | n == 3 = (a,b+1)
            | n == 4 = (a+1,b+1)
            | n == 5 = (a+1,b)
            | otherwise = (a,b)