Para executar o projeto, é necessário instalar as seguintes dependências:

 	System.Console.ANSI - cabal install ansi-termina 
 	System.Random - cabal install random 
 	Data.List.Unique - cabal install Unique  

As demais dependências devem vir com a instalação padrão do Haskell ou como uma dependência dos pacotes instalados acima.
Caso ocorra algum erro aqui está uma lista completa das importações:

	import System.Console.ANSI (setSGR, clearScreen, hideCursor, setCursorPosition, showCursor, xterm6LevelRGB)
	import System.Console.ANSI (SGR(Reset, SetPaletteColor), ConsoleLayer(Foreground))
	import System.Random (randomRIO)
	import System.IO
	import Control.Concurrent (threadDelay)
	import Data.List.Unique (sortUniq)
	import Data.Colour.SRGB (sRGB24)
	import Data.Word

Para executar o jogo basta executar o arquivo main.hs com o GHCi ou GHC, no GHCi a função main inicia o jogo.
Os controles do jogo são os seguintes:

	A: Move a peça para a esquerda.
	D: Move a peça para a direita.
	S: Move a peça para baixo.
	Espaço: Alterna a ordem das cores da peça.
