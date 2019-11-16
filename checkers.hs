{- 
    Porject: Checkers
    Author: Davi Baltar
    Create: 2018/11/20
    Update: 2019/11/16
    Version: 1.0.4
-} 

import qualified System.Process as SP
import System.Info as SI
import Data.IORef
import System.IO (hPutStrLn, hSetEncoding, stdout, utf8)

type Jogador = Int
type X = Int
type Y = Int

type Tabuleiro = [[(Jogador, X, Y)]]
type Linha = [(Jogador, X, Y)]
type Posicao = (Jogador, X, Y)

substituirLinha::Linha -> Posicao -> Posicao -> Linha
substituirLinha [] _ _ = []
substituirLinha ((w,x,y):xs) (q,i,j) (t,u,v)
    |(x == i && y == j) && (u == 0 && v == 0) = (0,x,y):xs      -- Desocupando casa
    |(x == i && y == j) && (w == 0) = (t,x,y):xs
    |otherwise = (w,x,y):substituirLinha xs (q,i,j) (t,u,v)

substituirTabuleiro::Tabuleiro->Posicao->Posicao->Tabuleiro
substituirTabuleiro (x:[]) old new = [substituirLinha x old new]
substituirTabuleiro (x:xs) old new = (substituirLinha x old new):substituirTabuleiro xs old new

formatarTabuleiro::Linha -> String
formatarTabuleiro [] = "║"
formatarTabuleiro ((w,x,y):xs) 
    |(w == 8) = " "++show x++" ║"++formatarTabuleiro xs 
    |(w == 0) = "   "++formatarTabuleiro xs
    |(w == 1) = " ● "++formatarTabuleiro xs
    |(w == 2) = " ○ "++formatarTabuleiro xs
    |(w == 9) = "███"++formatarTabuleiro xs
    |otherwise = formatarTabuleiro xs

imprimirTabuleiro::Tabuleiro->String
imprimirTabuleiro (x:[]) = formatarTabuleiro x
imprimirTabuleiro (x:xs) = formatarTabuleiro x ++ "\n" ++ imprimirTabuleiro xs

procurarLinha::Linha -> Posicao -> Int
procurarLinha [] _ = 0
procurarLinha ((w,x,y):xs) (q,i,j)
    |(x == i && y == j && w == q) = 1
    |otherwise = procurarLinha xs (q,i,j)

procurarTabuleiro::Tabuleiro->Posicao->[Int]
procurarTabuleiro (x:[]) pos = [procurarLinha x pos]
procurarTabuleiro (x:xs) pos = (procurarLinha x pos): procurarTabuleiro xs pos

removeElementoLinha::Linha -> Posicao -> Linha
removeElementoLinha tab elem = [c | c <-tab , c/= elem]

removeElementoTab::Tabuleiro->Posicao->Tabuleiro
removeElementoTab (x:[]) elem = [removeElementoLinha x elem]
removeElementoTab (x:xs) elem = (removeElementoLinha x elem):removeElementoTab xs elem

getNum :: IO Integer 
getNum = readLn 

getPosicao :: IO (Int, Int)
getPosicao = do
    rx <- getLine
    ry <- getLine
    return (read rx, read ry)

removeZerosArray :: [Int] -> [Int]
removeZerosArray lista = [c | c <-lista , c/=0]

oponente :: Int -> Int
oponente 1 = 2
oponente x = 1

clearScreen :: IO ()
clearScreen = do
    --putStrLn SI.os
    if (SI.os == "darwin" || SI.os == "linux") then do      -- unix
        _ <- SP.system "clear"
        return ()
    else do                                                 -- win
        _ <- SP.system "cls"
        return ()
    return ()

default (Int)  -- literal desambiguation
main :: IO ()
main = do
    hSetEncoding stdout utf8 -- This has to be the first action, otherwise you get the "invalid character" error
    tabuleiro <- newIORef [ [(8,8,0),(9,9,9),(2,2,8),(9,9,9),(2,4,8),(9,9,9),(2,6,8),(9,9,9),(2,8,8)],
                            [(8,7,0),(2,1,7),(9,9,9),(2,3,7),(9,9,9),(2,5,7),(9,9,9),(2,7,7),(9,9,9)],
                            [(8,6,0),(9,9,9),(2,2,6),(9,9,9),(2,4,6),(9,9,9),(2,6,6),(9,9,9),(2,8,6)],
                            [(8,5,0),(0,1,5),(9,9,9),(0,3,5),(9,9,9),(0,5,5),(9,9,9),(0,7,5),(9,9,9)],
                            [(8,4,0),(9,9,9),(0,2,4),(9,9,9),(0,4,4),(9,9,9),(0,6,4),(9,9,9),(0,8,4)],
                            [(8,3,0),(1,1,3),(9,9,9),(1,3,3),(9,9,9),(1,5,3),(9,9,9),(1,7,3),(9,9,9)],
                            [(8,2,0),(9,9,9),(1,2,2),(9,9,9),(1,4,2),(9,9,9),(1,6,2),(9,9,9),(1,8,2)], 
                            [(8,1,0),(1,1,1),(9,9,9),(1,3,1),(9,9,9),(1,5,1),(9,9,9),(1,7,1),(9,9,9)] ]
    jogador <- newIORef 1
    pedrasJ1 <- newIORef 12
    pedrasJ2 <- newIORef 12
    loop tabuleiro jogador pedrasJ1 pedrasJ2

loop :: IORef Tabuleiro -> IORef Int -> IORef Int -> IORef Int -> IO ()
loop tabuleiro jogador pedrasJ1 pedrasJ2 = do
    clearScreen
    tab <- readIORef tabuleiro
    pedJ1 <- readIORef pedrasJ1
    pedJ2 <- readIORef pedrasJ2 

    putStrLn "        *** Checkers ***                    Player 1 (●)  |  Player 2 (○)"
    putStrLn $ "                                                "++show pedJ1++"               "++ show pedJ2
    putStrLn " Y"
    putStrLn " ↑ ╔════════════════════════╗"
    putStrLn (imprimirTabuleiro tab)
    putStrLn "   ╚════════════════════════╝"
    putStrLn "     1  2  3  4  5  6  7  8 → X"

    if (pedJ1 == 0) then do
        putStrLn "\n            THE END       " 
        putStrLn "       PLAYER 2 (○) WON!!!\n" 
    else if (pedJ2 == 0) then do
        putStrLn "\n            THE END       " 
        putStrLn "       PLAYER 1 (●) WON!!!\n" 
    else do
        jog <- readIORef jogador
        putStrLn $ "\nPlayer " ++ show jog ++ ": "
        putStrLn "\nOrigin: X [ENTER] Y"
        (x, y) <- getPosicao
        let posOrigem = (jog,x,y)
        putStrLn "\nDestiny: X [ENTER] Y"
        (i, j) <- getPosicao
        let posDestino = (0,i,j)
        
        if ((removeZerosArray (procurarTabuleiro tab (oponente jog ,succ x, succ y))) == [1] ) && (succ x == pred i) && (succ y == pred j) && ((removeZerosArray (procurarTabuleiro tab posOrigem)) == (removeZerosArray (procurarTabuleiro tab posDestino))) then do -- Eliminando pedra para Cima->Direita
            writeIORef tabuleiro (substituirTabuleiro tab (0,succ x,succ y) (0,0,0))
        else if ((removeZerosArray (procurarTabuleiro tab (oponente jog, pred x, succ y))) == [1] ) && (pred x == succ i) && (succ y == pred j) && ((removeZerosArray (procurarTabuleiro tab posOrigem)) == (removeZerosArray (procurarTabuleiro tab posDestino))) then do -- Eliminando pedra para Cima->Esquerda
            writeIORef tabuleiro (substituirTabuleiro tab (0,pred x,succ y) (0,0,0))
        else if ((removeZerosArray (procurarTabuleiro tab (oponente jog, pred x, pred y))) == [1] ) && (pred x == succ i) && (pred y == succ j) && ((removeZerosArray (procurarTabuleiro tab posOrigem)) == (removeZerosArray (procurarTabuleiro tab posDestino))) then do -- Eliminando pedra para tras->esquerda
            writeIORef tabuleiro (substituirTabuleiro tab (0,pred x,pred y) (0,0,0))
        else if ((removeZerosArray (procurarTabuleiro tab (oponente jog, succ x, pred y))) == [1] ) && (succ x == pred i) && (pred y == succ j) && ((removeZerosArray (procurarTabuleiro tab posOrigem)) == (removeZerosArray (procurarTabuleiro tab posDestino))) then do -- Eliminando pedra para tras->direita
            writeIORef tabuleiro (substituirTabuleiro tab (0,succ x,pred y) (0,0,0))
        else if (((jog == 1) && ( ((succ x == i) && (succ y == j)) || ((pred x == i) && (succ y == j)))) || ((jog == 2) && ( ((succ x == i) && (pred y == j)) || ((pred x == i) && (pred y == j))))) && ((removeZerosArray (procurarTabuleiro tab posOrigem)) == (removeZerosArray (procurarTabuleiro tab posDestino))) then do -- Andar uma casa para baixo (esquerda ou direita)
            writeIORef tabuleiro (substituirTabuleiro tab posDestino posOrigem)
            tab <- readIORef tabuleiro
            writeIORef tabuleiro (substituirTabuleiro tab posOrigem (0,0,0))
            tab <- readIORef tabuleiro

            -- Mudando de jogador
            writeIORef jogador (oponente jog)
            jog <- readIORef jogador 
            loop tabuleiro jogador pedrasJ1 pedrasJ2
        else do
            putStrLn "\nInvalid move. (press ENTER to go back)"
            enter <- getLine
            loop tabuleiro jogador pedrasJ1 pedrasJ2

        tab <- readIORef tabuleiro
        writeIORef tabuleiro (substituirTabuleiro tab posDestino posOrigem)
        tab <- readIORef tabuleiro
        writeIORef tabuleiro (substituirTabuleiro tab posOrigem (0,0,0))
        tab <- readIORef tabuleiro     

        -- Mudando de jogador
        writeIORef jogador jog
        jog <- readIORef jogador

        if jog == 1 then do
            writeIORef pedrasJ1 (pedJ1-1)
        else do
            writeIORef pedrasJ2 (pedJ2-1)
        loop tabuleiro jogador pedrasJ1 pedrasJ2