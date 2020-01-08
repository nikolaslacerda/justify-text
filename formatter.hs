module formatter where

import Prelude hiding (Word, getLine)
import Data.Char
import Data.List
import System.IO hiding (getLine)

-- Chapter 7

whitespace = ['\n','\t',' ']
pontuation = ['\'','\"','.',',',';']

getWord :: String -> String
getWord [] = []
getWord (x:xs)
    | elem x whitespace = []
    | otherwise         = x : getWord xs

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
    | elem x whitespace = (x:xs)
    | otherwise = dropWord xs

dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs)
    | elem x whitespace = dropSpace xs
    | otherwise = (x:xs)

type Word = String

splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

split :: String -> [Word]
split [] = []
split st = (getWord st) : split (dropSpace(dropWord st))

type Line = [Word]

lineLen :: Int
lineLen = 35

getLine :: Int -> [Word] -> Line
getLine len [] = []
getLine len (x:xs)
    | length x <= len = x : restOfLine
    | otherwise = []
    where
    newlen = len - (length x + 1)
    restOfLine = getLine newlen xs

--7.27

dropLine :: Int -> [Word] -> Line
dropLine len [] = []
dropLine len (x:xs)
    | length x <= len = restOfLine
    | otherwise = (x:xs)
    where
    newlen = len - (length x + 1)
    restOfLine = dropLine newlen xs

-- end 7.27

splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines st = getLine lineLen st : splitLines (dropLine lineLen st)


fill :: String -> [Line]
fill = splitLines . splitWords

--7.28

joinLine :: Line -> String
joinLine [] = ""
joinLine (x:[]) = x
joinLine (x:xs) = x ++ " " ++ (joinLine xs)

--end 7.28


--7.29

joinLines :: [Line] -> String
joinLines [] = ""
joinLines (x:xs) = (joinLine x) ++ "\n" ++ (joinLines xs)

--end 7.29

--7.31

wordLength:: Word -> Int
wordLength [] = 0
wordLength  st = length st

lineLength :: Line -> Int
lineLength [] = 0
lineLength (x:xs) = wordLength x + lineLength xs

spacesCount :: Line -> Int -> Int
spacesCount _ 0  = 1
spacesCount st n
    | div n (length st) > 1 = 1 + (spacesCount st (n - 1))
    | otherwise             = 1

justify :: Line -> Int -> String
justify [] _     = ""
justify (x:xs) n
    | null xs   = x
    | otherwise = x ++ count ++ (justify xs (n - spaces))
    where
    spaces = spacesCount xs n
    count  = replicate spaces ' '

justifyLine :: Line -> String
justifyLine [] = ""
justifyLine st = justify st spaces
   where
   spaces = lineLen - lineLength st

justifyLines :: [Line] -> String
justifyLines []     = ""
justifyLines (x:xs) 
    | null xs   = joinLine x
    | otherwise = (justifyLine x) ++ "\n" ++ (justifyLines xs)

--end 7.31

--7.32


wc :: String -> (Int, Int, Int)
wc s = count s caracterCount wordCount 1
    where
    caracterCount = length s
    wordCount = length (words s)
    count :: String -> Int -> Int -> Int -> (Int, Int, Int)
    count [] c w l = (c, w, l)
    count (x:xs) c w l
        | x == '\n' && xs /= []  = count xs c w (l + 1)
        | otherwise              = count xs c w l

--end 7.32

--7.33 

dropAllSpaces :: String -> String
dropAllSpaces [] = []
dropAllSpaces (x:xs)
    | elem x whitespace = dropAllSpaces xs
    | otherwise = x : dropAllSpaces xs

dropPontuations :: String -> String
dropPontuations [] = []
dropPontuations (x:xs)
    | elem x pontuation = dropPontuations xs
    | otherwise = x : dropPontuations xs

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toUpper xs
    
isPalin :: String -> Bool
isPalin st = s1 == s2
    where
    s1 = capitalize(dropPontuations(dropAllSpaces st)) 
    s2 = reverse s1

--end 7.33
  
--7.34

subst :: String -> String -> String -> String
subst [] oldSub newSub = []
subst (x:xs) oldSub newSub = substRec (x:xs)
    where
    substRec [] = []
    substRec (x:xs) = 
        let (prefix, rest) = splitAt n (x:xs)
        in
          if oldSub == prefix
          then newSub ++ substRec rest
          else x : substRec (xs)
    n = length oldSub

--end 7.34

-- Processamento de texto

wcFormat :: (Int, Int, Int) -> String
wcFormat (c, w, l) = "\n" ++ "Dados: " ++ "\n" ++ "Caracteres: " ++ show c ++ "\n" ++ "Palavras: " ++ show w ++ "\n" ++ "Linhas: " ++ show l

applyFill :: IO ()
applyFill = do
    text <- readFile "TextExample1.txt"
    writeFile "TextFill.txt" (joinLines(fill text) ++ wcFormat(wc(joinLines(fill text))))
    putStrLn "Gerado texto alinhado"

applyJustify :: IO ()
applyJustify = do
    text <- readFile "TextExample1.txt"
    writeFile "TextJustify.txt" (justifyLines(fill text) ++ wcFormat(wc(justifyLines(fill text))))
    putStrLn "Gerado texto justificado"

applySubst :: IO ()
applySubst = do
    text <- readFile "TextExample1.txt"
    writeFile "TextSubst.txt" ( subst text "December" "April")
    putStrLn "Gerado novo texto"



