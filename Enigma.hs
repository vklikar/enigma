-- usage:
--     encrypt (Key [_I, _II, _III] "AAA" "AAA") "PLAINTEXT"

import Data.Map (Map, fromList, toList, (!))
import Data.Tuple (swap)
import Data.Char (ord)
import Debug.Trace

data Key = Key {
    wheelOrder :: [Wheel],
    ringSetting :: String,
    basicWheelSetting :: String
    --crossPlugging :: [String]
} deriving (Show)

data Wheel = Wheel {
    wiring :: Map Char Char,
    notch :: String,
    turnover :: String,
    startPosition :: Char,
    ringOffset :: Char
} deriving (Show)

data Enigma = Enigma {
    entryWheel :: Wheel,
    leftWheel :: Wheel,
    middleWheel :: Wheel,
    rightWheel :: Wheel,
    reflector :: Wheel
} deriving (Show)

-- Enigma M3
-- http://www.cryptomuseum.com/crypto/enigma/wiring.htm#4
--------------------------------------------------------------
-- Wheel            ABCDEFGHIJKLMNOPQRSTUVWXYZ  Notch Turnover
--------------------------------------------------------------
_ETW  =  makeWheel "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ""   ""
_I    =  makeWheel "EKMFLGDQVZNTOWYHXUSPAIBRCJ" "Y"  "Q"
_II   =  makeWheel "AJDKSIRUXBLHWTMCQGZNPYFVOE" "M"  "E"
_III  =  makeWheel "BDFHJLCPRTXVZNYEIWGAKMUSQO" "D"  "V"
_IV   =  makeWheel "ESOVPZJAYQUIRHXLNFTGKDCMWB" "R"  "J"
_V    =  makeWheel "VZBRGITYUPSDNHLXAWMJQOFECK" "H"  "Z"
_VI   =  makeWheel "JPGVOUMFYQBENHZRDKASXLICTW" "HU" "ZM"
_VII  =  makeWheel "NZJHGRCXMYSWBOUFAIVLPEKQDT" "HU" "ZM"
_VIII =  makeWheel "FKQHTLXOCBJSPDZRAMEWNIUYGV" "HU" "ZM"
_UKWB =  makeWheel "YRUHQSLDPXNGOKMIEBFZCWVJAT" ""   ""
_UKWC =  makeWheel "FVPJIAOYEDRZXWGCTKUQSBNMHL" ""   ""

pad x = replicate (4 - length x) ' ' ++ x
show' x = pad $ show x

letters :: String
letters = ['A'..'Z']

lettersCount :: Int
lettersCount = 26

index :: Char -> Int
index c = ord c - ord 'A'

revIndex :: Char -> Int
revIndex c = ord 'Z' - ord c

makeWiring :: String -> Map Char Char
makeWiring = fromList . zip ['A'..'Z']

makeWheel :: String -> String -> String -> Wheel
makeWheel wiring@(startPosition:_) notch turnover = Wheel (makeWiring wiring) notch turnover startPosition 'A'

setWheel :: Wheel -> Char -> Char -> Wheel
setWheel wheel startPosition ringOffset = wheel { startPosition = startPosition, ringOffset = ringOffset }

inv :: Wheel -> Wheel
inv wheel = wheel { wiring = fromList . map swap $ toList $ wiring wheel }

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) $ drop (n `mod` length xs) $ cycle xs

translate :: Int -> Char -> Char
translate n = (!) $ makeWiring (rotate n ['A'..'Z'])

transform ::  Wheel -> Char -> Char
transform wheel = (!) (wiring wheel)

encrypt':: Enigma -> Int -> String -> String
encrypt' _ _ [] = []
encrypt' enigma@(Enigma etw l m r ukw) n (x:xs) =
    -- trace ("n: " ++ (show' n) ++ ", start: " ++ (show' $ index $ startPosition r) ++ ", mod: " ++ (show' $ i `mod` 26) ++ ",       i: " ++ (show' i) ++ " (" ++ show (letters !! (i `mod` 26)) ++ "),       j: " ++ (show' j) ++ " (" ++ show (letters !! (j `mod` 26)) ++ "),       k: " ++ (show' k) ++ " (" ++ show (letters !! (k `mod` 26)) ++ ")")$
    ( translate (-i) . transform (inv r) . translate i
    . translate (-j) . transform (inv m) . translate j
    . translate (-k) . transform (inv l) . translate k
    . transform ukw
    . translate (-k) . transform l . translate k
    . translate (-j) . transform m . translate j
    . translate (-i) . transform r . translate i) x : encrypt' enigma (n + 1) xs
    where
        i = startIndex r - ringIndex r + n
        j = startIndex m - ringIndex m + turnoverCount 1 i r
        j' = startIndex m - ringIndex m + turnoverCount 2 i r
        k = startIndex l - ringIndex l + turnoverCount 0 j' m
        turnoverCount offset ind wheel =
            sum [(ind - offset + (revIndex t)) `div` lettersCount - (if startIndex wheel > index t then 1 else 0) | t <- turnover wheel]
        startIndex = index . startPosition
        ringIndex = index . ringOffset

encrypt :: Key -> String -> String
encrypt (Key (wheelL:wheelM:wheelR:_) (ringL:ringM:ringR:_) (startL:startM:startR:_)) = encrypt' (Enigma etw l m r ukw) 1
    where
        etw = setWheel _ETW 'A' 'A'
        l = setWheel wheelL startL ringL
        m = setWheel wheelM startM ringM
        r = setWheel wheelR startR ringR
        ukw = setWheel _UKWB 'A' 'A'


--  encrypt (Key [_I, _II, _III] "AAA" "CFA") (take 100 $ repeat 'A')
--  SQPEUGJRCXZWPFYIYYBWLJEWROUVKPOZTCEUWTFJZQWPBQLOTTSRMDFLGXBXZRYQKDGJRZEZMDHJNQYPDJWCJFJLFNTRSNCNLGSK
