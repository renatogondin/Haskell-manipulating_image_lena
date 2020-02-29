-- RENATO GONDIN, MIGUEL CORREIA AND MIGUEL KEIM

import Data.List
import System.IO
import Data.Char
import System.Environment
import Test.QuickCheck


type RGB = (Int, Int, Int)
type RGBlist = [RGB]
type ALLRGB = [RGBlist]
data PPM = PPM {titulo :: String, width :: Int, height :: Int, maxPixel :: Int, pixels :: ALLRGB}

-- FUNÇAO MAIN DO PROGRAMA

main = do
    args <- getArgs
    progName <- getProgName

    let fileName = args !! 0
    let newFileName = args !! 1
    let comandos = drop 2 args

    contents <- readFile fileName

    let ppm = PPM "P3\n" (read (fileWidth contents)::Int) (read (fileHeight contents)::Int) 255 (splitEachInt (read (fileWidth contents)::Int)(toTuples(showIntArray(showArray(rgbOnly contents)))))

    if (elem "-hw" args && elem "-hh" args)
        then writeFile newFileName (titulo ppm ++ (divideWidth (length(elemIndices "-hw" args)*2) contents) ++ "\n" ++ (divideHeight (length(elemIndices "-hh" args)*2) contents) ++ "\n" ++ show(maxPixel ppm) ++ "\n" ++ toString(checkOperations (pixels ppm) (comandos) (read (fileWidth contents)::Int)) ++"\n")
        else if (elem "-hw" args && not(elem "-hh" args))
            then writeFile newFileName (titulo ppm ++ (divideWidth (length(elemIndices "-hw" args)*2) contents) ++ "\n" ++ show(height ppm) ++ "\n" ++ show(maxPixel ppm) ++ "\n" ++ toString(checkOperations (pixels ppm) (comandos) (read (fileWidth contents)::Int)) ++"\n")
            else if (not(elem "-hw" args) && elem "-hh" args)
                then writeFile newFileName (titulo ppm ++ show(width ppm) ++ "\n" ++ (divideHeight (length(elemIndices "-hh" args)*2) contents) ++ "\n" ++ show(maxPixel ppm) ++ "\n" ++ toString(checkOperations (pixels ppm) (comandos) (read (fileWidth contents)::Int)) ++"\n")
                else writeFile newFileName (titulo ppm ++ show(width ppm) ++ "\n" ++ show(height ppm) ++ "\n" ++ show(maxPixel ppm) ++ "\n" ++ toString(checkOperations (pixels ppm) (comandos) (read (fileWidth contents)::Int)) ++"\n")


-- FUNÇOES RESPONSAVEIS POR LER HEADERS E PIXELS

headersOnly :: String -> String
headersOnly input =
    let allLines = lines input
        headerLines = filter (\line -> ((line !! 0) == '#') || (length (words line) <= 2)) allLines
        result = unlines headerLines
    in  result

fileWidth content = (words(headersOnly content)) !! (length (words(headersOnly content)) - 3)
divideWidth int content= show((read (fileWidth content)::Int) `div` int)
fileHeight content = (words(headersOnly content)) !! (length (words(headersOnly content)) - 2)
divideHeight int content= show((read (fileHeight content)::Int) `div` int)

rgbOnly :: String -> String
rgbOnly input =
    let allLines = lines input
        rgbLines = filter (\line -> ((line !! 0) /= '#') && (length (words line) > 2)) allLines
        result = unlines rgbLines
    in  result

-- FUNÇAO QUE CONVERTE A STRING DOS PIXELS PARA UMA LISTA [[(Int,Int,Int)]]

showArray :: String -> [String]
showArray input = words input

showIntArray lista = [read i::Int | i <- lista]

toTuples :: [Int] -> RGBlist
toTuples (r:g:b:is) = (r,g,b): toTuples is
toTuples        is  = []

splitEachInt :: Int -> [a] -> [[a]]
splitEachInt n [] = []
splitEachInt n xs = ys : splitEachInt n zs
  where (ys,zs) = splitAt n xs

-- FUNÇOES DAS TRANSFORMAÇOES QUE APLICAM FLIPS, FH QUE APLICA UM FLIP HORIZONTAL E FV QUE APLICA UM VERTICAL

fh xs = [reverse i | i <- xs]
fv xs = reverse xs

-- FUNÇAO HW QUE REDUZ A LARGURA DA IMAGEM PARA METADE

sumTuplesAvg [(a,b,c)] = [(a,b,c)]
sumTuplesAvg [(a,b,c),(a1,b1,c1)] = [((a+a1)`div`2,(b+b1)`div`2,(c+c1)`div`2)]

listOfTwo lista = filter (\x -> length x > 1) lista

hw width lista = splitEachInt (width`div`2) [ (sumTuplesAvg i)!!0 | i <- listOfTwo(splitEachInt 2 (concat(lista)))]

-- FUNÇAO HH QUE REDUZ A ALTURA DA IMAGEM PARA METADE

hh :: [[(Int,Int,Int)]] -> [[(Int,Int,Int)]]
hh [] = []
hh (a:b:xs) = testeHh a b : hh xs

testeHh ::[(Int,Int,Int)] -> [(Int,Int,Int)] -> [(Int,Int,Int)]
testeHh [] _ = []
testeHh _ [] = []
testeHh (x:xs) (y:ys) = somaTup x y : testeHh (xs) (ys)

somaTup :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
somaTup (x,y,z) (a,b,c) = ((x+a) `div` 2, (y+b)`div` 2, (z+c) `div` 2)

-- FUNÇOES QUE APLICAM FILTROS DE COR NA IMAGEM. GS APLICA UM FILTRO CINZENTO, RC APLICA UM FILTRO VERMELHO, GC APLICA UM FILTRO VERDE E BC APLICA UM FILTRO AZUL

redTriple (a,b,c) = (a, 0, 0)
greenTriple (a,b,c) = (0, b, 0)
blueTriple (a,b,c) = (0, 0, c)
greyTriple (a,b,c) = (((a+b+c)`div`3),((a+b+c)`div`3),((a+b+c)`div`3))

turnRed xs = [redTriple i | i <- xs]
turnGreen xs = [greenTriple i | i <- xs]
turnBlue xs = [blueTriple i | i <- xs]
turnGrey xs = [greyTriple i | i <- xs]

rc xs = [turnRed i | i <- xs]
gc xs = [turnGreen i | i <- xs]
bc xs = [turnBlue i | i <- xs]
gs xs = [turnGrey i | i <- xs]

-- FUNÇOES RESPONSAVEIS PELA LEITURA E EXECUÇAO DAS OPERAÇOES DADAS PELA LINHA DE COMANDOS

checkOperations :: [[(Int,Int,Int)]] -> [String] -> Int -> [[(Int,Int,Int)]]
checkOperations file [] _ = file
checkOperations file (x:xs) width = doOperations file (x:xs) width

doOperations :: [[(Int,Int,Int)]] -> [String] -> Int -> [[(Int,Int,Int)]]
doOperations file [] _ = file
doOperations file (x:xs) width
    | x == "-fh" = doOperations (fh file) xs width
    | x == "-fv" = doOperations (fv file) xs width
    | x == "-gs" = doOperations (gs file) xs width
    | x == "-rc" = doOperations (rc file) xs width
    | x == "-gc" = doOperations (gc file) xs width
    | x == "-bc" = doOperations (bc file) xs width
    | x == "-hh" = doOperations (hh file) xs width
    | x == "-hw" = doOperations (hw width file) xs width
  --  | x == "-t" =  (quickCheck prop_fv_is_fv)

-- FUNÇAO QUE CRIA UM TO STRING DOS PIXELS DA IMAGEM

toStringPixels :: [(Int, Int, Int)] -> [Char]
toStringPixels (((a, b, c)):xs)
    | xs /= [] = show a ++ "\n" ++ show b ++ "\n" ++ show c ++ "\n" ++ (toStringPixels xs)
    | otherwise = show a  ++ "\n" ++ show b ++ "\n" ++ show c ++ "\n"

toString :: [[(Int, Int, Int)]] -> [Char]
toString xs = init $ concat $ map (toStringPixels) (xs)



