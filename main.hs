module Main where

import System.Console.ANSI (setSGR, clearScreen, hideCursor, setCursorPosition, showCursor, xterm6LevelRGB)
import System.Console.ANSI (SGR(Reset, SetPaletteColor), ConsoleLayer(Foreground))
import System.Random (randomRIO)
import System.IO

import Control.Concurrent (threadDelay)
import Data.List.Unique (sortUniq)
import Data.Colour.SRGB (sRGB24)
import Data.Word

type Grid = [[Word8]]
type Stats = (Word32, Word16, Word8) -- Pontuação, blocos removidos, nível
type Position = (Word8, Word8)

main :: IO()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    play

mainGrid :: Grid
mainGrid = [[1,1,1,1,1,1],
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

word16_to_word32 :: Word16 -> Word32
word16_to_word32 = fromIntegral

word8_to_word32 :: Word8 -> Word32
word8_to_word32 = fromIntegral

inRange :: Word8 -> Word8 -> Bool
inRange a b = ((a >= 0) && (a < gridHeight)) && ((b >= 0) && (b < gridWidth))

inRangeT :: Position -> Bool
inRangeT (a,b) = inRange a b
-------------------------------------------------------------

drawLine :: Grid -> Word8 -> IO()
drawLine grid n = do
    let line = gArr n grid
    foldr (>>) (putStr "") (map (printC "██" ) line)
    
printGrid :: Grid -> Grid -> Stats -> IO()
printGrid grid nextPiece (score,blocks,lvl) = do
    setCursorPosition 0 0
    let l = getLvlColor lvl
    printC ("   ╔════"              ++                   "╗   ╔══════════════"         ++          "╗   ╔═════════╗\n") l
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
    putStr                                      ("            ╚══════════════"         ++          "╝\n")

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
getInput :: IO Char -- Pega o ultimo char do stdin, não funciona corretamente no Windows
getInput = do
    inputReady <- hReady stdin
    if (inputReady) then do
        input <- getChar
        notLastChar <- hReady stdin
        if (notLastChar) then
            getInput
        else
            return input
    else
        return '*'
--------------------------------------------------------------------

play :: IO ()
play = do
    hideCursor
    resetScreen
    a <- randomRIO (1,6)
    b <- randomRIO (1,6)
    c <- randomRIO (1,6)
    let nextPiece = [[a],[b],[c]]
    let matchList = getMatchesGrid (gridHeight-1) (gridWidth-1) mainGrid
    let removedGrid = setList matchList 0 mainGrid -- necessário para inicializar o Grid, caso contrário apresenta comportamento inesperado
    let score = 0
    printGrid removedGrid nextPiece (0,0,1)
    playLoop removedGrid nextPiece (100,100) (0,0,1) False
        where
        playLoop :: Grid -> Grid -> Position -> Stats -> Bool -> IO ()
        playLoop grid nextPiece pcPos (score,blocks,lvl) lvlUp = do
            (tempGrid,tempPos) <- inputLoop grid nextPiece pcPos (lvlSpeed lvl) (score,blocks,lvl)
            let (newGrid, moved, piecePos) = moveDown tempGrid tempPos
            if (moved) then do
                printGridOnly newGrid nextPiece
                playLoop newGrid nextPiece piecePos (score,blocks,lvl) lvlUp
            else do
                piecePos <- return (100,100)  -- posição (100,100) -- sem peça controlável
                let matchList = getMatchesGrid (gridHeight-1) (gridWidth-1) newGrid
                let b = countArr16(matchList)
                score  <- return (score + ((word16_to_word32 b)*(100+( word8_to_word32 (lvl*2))))) 
                blocks <- return (blocks + b) 
                newLvl <- return (calcLvl blocks)
                lvlUp  <- return ((newLvl /= lvl) || lvlUp)
                lvl    <- return (newLvl)
                if (matchList /= []) then do -- Remove combinações, faz animação e move blocos
                    deleteAnim matchList newGrid nextPiece (score,blocks,lvl)
                    printStats (score,blocks,lvl)
                    let removedGrid = setList matchList 0 newGrid
                    playLoop removedGrid nextPiece (100,100) (score,blocks,lvl) lvlUp
                else do -- Adiciona nova peça ou bônus se subiu de nível
                    if (lvlUp) then do -- Bonus ao subir de nível
                        printGrid newGrid ([[255],[255],[255]]) (score,blocks,lvl)
                        let nextGrid = addNewPiece newGrid ([[255],[255],[255]]) 3
                        (bonus,column) <- playLoopBonus nextGrid ([[255],[255],[255]]) (2,3) (score,blocks,lvl) -- Move a peça bônus para baixo
                        let colorToRemove = getFstFromColumn column newGrid
                        a <- randomRIO (1,6)
                        b <- randomRIO (1,6)
                        c <- randomRIO (1,6)
                        if (colorToRemove /= 255) then do -- Se acertou alguma coisa
                            let removeList = (getAllPosColor (gridHeight-1) (gridWidth-1) colorToRemove bonus) ++ (getMatchesGrid (gridHeight-1) (gridWidth-1) bonus)
                            let b1 = countArr16(removeList)
                            score  <- return (score + ((word16_to_word32 b1)*(100+( word8_to_word32 (lvl*2))))) 
                            blocks <- return (blocks + b1) 
                            lvl    <- return (newLvl)
                            deleteAnim removeList bonus nextPiece (score,blocks,lvl)
                            printStats (score,blocks,lvl)
                            let removedGrid = setList removeList 0 newGrid
                            playLoop removedGrid ([[a],[b],[c]]) (100,100) (score,blocks,lvl) False
                        else do -- Volta ao jogo normal
                            playLoop bonus ([[a],[b],[c]]) (2,3) (score,blocks,lvl) False
                    else do -- Jogo Normal
                        if (gameOver newGrid) then do 
                            showCursor
                            setCursorPosition 4 16
                            printC "GAMEOVER" 3
                            setCursorPosition 15 0
                            setSGR [Reset]
                            return ()
                        else do
                            a <- randomRIO (1,6)
                            b <- randomRIO (1,6)
                            c <- randomRIO (1,6)
                            let nextGrid = addNewPiece newGrid nextPiece 3
                            playLoop nextGrid ([[a],[b],[c]]) (2,3) (score,blocks,lvl) lvlUp
                            

        playLoopBonus :: Grid -> Grid -> Position -> Stats -> IO (Grid,Word8)
        playLoopBonus grid nextPiece pcPos (score,blocks,lvl) = do
            i <- getInput
            let input = i
            (tempGrid,tempPos) <- inputLoop grid nextPiece pcPos (lvlSpeed lvl) (score,blocks,lvl)
            let (newGrid, moved, piecePos) = moveDown tempGrid tempPos
            if (moved) then do -- Enquanto moveu
                printGridOnly newGrid nextPiece
                playLoopBonus newGrid nextPiece piecePos (score,blocks,lvl)
            else do 
                return (newGrid,snd pcPos)   

deleteAnim :: [Position] -> Grid -> Grid -> Stats -> IO () -- Animação de Remoção
deleteAnim list grid nxtPc stats = do
    colorChange 30 -- Número de alterações de cor na animação
    where
        colorChange :: Word8 -> IO ()
        colorChange 0 = do 
            return ()
        colorChange n = do
            let grid1 = setList list ((n`mod`6)+1) grid
            printGridOnly grid1 nxtPc
            pause 33 
            colorChange (n-1)

lvlSpeed :: Word8 -> Word8 -- Velocidade do Nível -- Tempo entre moveDown's -- número de inputLoop's
lvlSpeed n
    | n > 15 = 1
    | otherwise = 30 - (2*n)

calcLvl :: Word16 -> Word8 -- Converte o número de blocos destruidos para nível
calcLvl n
    | n >= 70 = 1 + calcLvl (n-70)
    | otherwise = 1 

setList :: [Position] -> Word8 -> Grid -> Grid -- Define cada item da lista de posições do grid com o valor
setList [] _ g = g
setList (x:xs) v g = setList xs v (setItem x v g)

setItem :: Position -> Word8 -> Grid -> Grid -- Define o item na posição dada do Grid com o valor
setItem (a,b) v g = setPos a b v g

gameOver :: Grid -> Bool -- Soma as linhas superiores, se > 0 entao gameover
gameOver [] = True
gameOver g = ((foldr (+) 0 (gArr 0 g)) + (foldr (+) 0 (gArr 1 g))) > 0

addNewPiece :: Grid -> Grid -> Word8 -> Grid -- Adiciona peça para o Grid na Coluna n
addNewPiece grid pc n = (setPos 2 n (getPos 2 0 pc) (setPos 1 n (getPos 1 0 pc) (setPos 0 n (getPos 0 0 pc) grid)))

getColor :: Word8 -> Word8 -- Pega cor da Paleta
getColor x 
    | x == 0 = xterm6LevelRGB 0 0 0 -- Preto / Vazio
    -- Cores das Peças
    | x == 1 = xterm6LevelRGB 0 3 5 -- Azul Claro
    | x == 2 = xterm6LevelRGB 0 4 0 -- Verde
    | x == 3 = xterm6LevelRGB 5 0 1 -- Vermelho
    | x == 4 = xterm6LevelRGB 5 2 0 -- Laranja
    | x == 5 = xterm6LevelRGB 3 0 5 -- Roxo
    | x == 6 = xterm6LevelRGB 4 5 0 -- Amarelo
    -- Cores de Fundo
    | x == 7 = xterm6LevelRGB 5 2 1 -- 
    | x == 8 = xterm6LevelRGB 2 2 2 --
    | x == 9 = xterm6LevelRGB 5 0 5 --
    | x == 10 = xterm6LevelRGB 0 2 2 --
    | x == 11 = xterm6LevelRGB 4 0 4 --
    | x == 12 = xterm6LevelRGB 0 5 1 --
    | x == 13 = xterm6LevelRGB 5 1 0 --
    | x == 14 = xterm6LevelRGB 0 1 5 --
    -- Outros
    | x == 15 = xterm6LevelRGB 5 5 5 -- Branco
    | x == 16 = xterm6LevelRGB 4 4 4 -- Cinza Claro
    | x == 255 = xterm6LevelRGB 5 5 5 -- Peça Bonus
    | otherwise = xterm6LevelRGB 5 5 5

printC :: String -> Word8 -> IO() -- Imprime String com a cor n do getColor
printC x n = do
     setSGR [SetPaletteColor Foreground $ (getColor n)]
     putStr x

resetScreen :: IO () -- Limpa tela e cor do texto
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

pause :: Int -> IO () -- Pausa Thread por n ms
pause n = do
    hFlush stdout
    threadDelay (n * 1000)


-- Usado pela peça bonus ----------------------
getAllPosColor :: Word8 -> Word8 -> Word8 -> Grid -> [Position] -- Retorna todas as posições que é da cor especificada
getAllPosColor l c color grid = aux l 0  
    where
        aux :: Word8 -> Word8 -> [Position] 
        aux l1 c1  
            | (c1 > c) && (l1 /= 255) = (aux (l1-1) 0)
            | (l1 /= 255) && ((getPos l1 c1 grid) == color) = (l1,c1) : (aux l1 (c1+1))
            | (l1 /= 255) = (aux l1 (c1+1))
            | otherwise = []

getFstFromColumn :: Word8 -> Grid -> Word8 -- Retorna a primeira cor de uma coluna que é diferente de vazio (0), se nao há, retorna 255
getFstFromColumn col grid = aux 0
    where
        aux :: Word8 -> Word8 
        aux l
            | (l == gridHeight) = 255
            | (getPos l col grid) /= 0 = getPos l col grid
            | otherwise = aux (l+1)
------------------------------------------------------------

getMatchesGrid :: Word8 -> Word8 -> Grid -> [Position] -- Procura em todas as posições por combinações de 3 ou mais peças, retorna uma lista de posições
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

moveDown :: Grid -> Position -> (Grid, Bool, Position) -- move todos os blocos que podem ser movidos, muda a posição da peça se existir (/= (100,100))
moveDown grid (l,c) = do
    let move = checkBlocks (gridHeight-2) (gridWidth-1) grid False
    if (snd move) then 
        (fst move, snd move, ((l+1),c))
    else 
        (fst move, snd move, (100,100))
    
 
checkBlocks :: Word8 -> Word8 -> Grid -> Bool -> (Grid,Bool) -- Checa todos os blocos, se há uma posição vazia abaixo de um bloco, move-o para baixo
checkBlocks l c grid1 moved
    | (inRange l c) && ((getPos l c grid1) /= 0) && ((getPos (l+1) c grid1)==0) = checkBlocks l (c-1) (setPos l c 0 (setPos (l+1) c (getPos l c grid1) grid1)) True
    | inRange l c = checkBlocks l (c-1) grid1 moved
    | c == 255 = checkBlocks (l-1) (gridWidth-1) grid1 moved
    | l == 255 = (grid1,moved)

movePiece :: Grid -> Char -> Position -> (Grid,Position) -- move a peça para uma determinada direção, se for possivel. Inverte a peça se input = espaço
movePiece grid input (l,c) = do
    if (input == 'a' ) then 
        (moveAux (l,c) (l,c-1) grid)
    else 
        if (input == 'd' ) then 
            (moveAux (l,c) (l,c+1) grid)
        else 
            if (input == 's') then 
                (moveAux (l,c) (l+1,c) grid)
            else 
                if (input == ' ') then 
                    (inverPiece (l,c) grid, (l,c)) 
                else
                    (grid,(l,c))
    where               
        moveAux :: Position -> Position -> Grid -> (Grid,Position)
        moveAux (l1,c1) (l2,c2) grid = do
            if ((inRangeT (l2,c2))&&((getPos l2 c2 grid) == 0)) then do
                grid <- return (setPos l2 c2 (getPos l1 c1 grid) grid)
                grid <- return (setPos l1 c1 0 grid)
                grid <- return (setPos (l2-1) c2 (getPos (l1-1) c1 grid) grid)
                grid <- return (setPos (l1-1) c1 0 grid)
                grid <- return (setPos (l2-2) c2 (getPos (l1-2) c1 grid) grid)
                grid <- return (setPos (l1-2) c1 0 grid)
                (grid,(l2,c2))
            else
                (grid,(l1,c1))

inverPiece :: Position -> Grid -> Grid -- Muda a ordem dos blocos da peça: 3 -> 2 , 2 -> 1 , 1 -> 3
inverPiece (l,c) grid = do
    let aux = getPos l c grid
    grid <- return (setPos  l c    (getPos (l-1) c grid) grid)
    grid <- return (setPos (l-1) c (getPos (l-2) c grid) grid)
    grid <- return (setPos (l-2) c (aux) grid)
    grid
    
inputLoop :: Grid -> Grid -> Position -> Word8 -> Stats -> IO (Grid, Position) -- Lê input do jogador e move a peça, repete loopCount vezes
inputLoop grid nxtPc pos loopCount stats = do
    if (fst pos < 100) then do
        c <- getInput
        if (loopCount /= 0) then do
            let (newGrid, newPos) = movePiece grid c pos
            pause 17
            if (((snd newPos) < 6) && ((snd newPos) >= 0)) then do
                printGridOnly newGrid nxtPc
                inputLoop newGrid nxtPc newPos (loopCount-1) stats
            else do
                printGridOnly grid nxtPc
                inputLoop grid nxtPc pos (loopCount-1) stats
        else return (grid, pos)
    else return (grid, pos)