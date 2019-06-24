-- usage:
--     encrypt (Key [_I, _II, _III] "AAA" "AAA" []) "PLAINTEXT"
module Enigma where

import Data.Map (Map, fromList, toList, (!), union)
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import Data.List (elemIndex, reverse)
import Debug.Trace

data Key = Key {
    wheelOrder :: [Wheel],
    ringSetting :: String,
    basicWheelSetting :: String,
    crossPlugging :: [String]
} deriving (Show)

data Wheel = Wheel {
    wiring :: Map Char Char,
    notch :: String,
    turnover :: String,
    startPosition :: Char,
    ringOffset :: Char
} deriving (Eq, Show)

data Enigma = Enigma {
    alphabet :: String,
    plugboard :: Map Char Char,
    entryWheel :: Wheel,
    leftWheel :: Wheel,
    middleWheel :: Wheel,
    rightWheel :: Wheel,
    reflector :: Wheel
} deriving (Show)

-- Enigma M3
-- http://www.cryptomuseum.com/crypto/enigma/wiring.htm#4
-------------------------------------------------------------------------
-- Alphabet
-------------------------------------------------------------------------
_AZ   =  ['A'..'Z']

------------------------------------------------------------------
-- Wheel                ABCDEFGHIJKLMNOPQRSTUVWXYZ  Notch Turnover
------------------------------------------------------------------
_ETW  =  makeWheel _AZ "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ""   ""
_I    =  makeWheel _AZ "EKMFLGDQVZNTOWYHXUSPAIBRCJ" "Y"  "Q"
_II   =  makeWheel _AZ "AJDKSIRUXBLHWTMCQGZNPYFVOE" "M"  "E"
_III  =  makeWheel _AZ "BDFHJLCPRTXVZNYEIWGAKMUSQO" "D"  "V"
_IV   =  makeWheel _AZ "ESOVPZJAYQUIRHXLNFTGKDCMWB" "R"  "J"
_V    =  makeWheel _AZ "VZBRGITYUPSDNHLXAWMJQOFECK" "H"  "Z"
_VI   =  makeWheel _AZ "JPGVOUMFYQBENHZRDKASXLICTW" "HU" "ZM"
_VII  =  makeWheel _AZ "NZJHGRCXMYSWBOUFAIVLPEKQDT" "HU" "ZM"
_VIII =  makeWheel _AZ "FKQHTLXOCBJSPDZRAMEWNIUYGV" "HU" "ZM"
_UKWB =  makeWheel _AZ "YRUHQSLDPXNGOKMIEBFZCWVJAT" ""   ""
_UKWC =  makeWheel _AZ "FVPJIAOYEDRZXWGCTKUQSBNMHL" ""   ""

pad x = replicate (4 - length x) ' ' ++ x
showPadded x = pad $ show x

index :: String -> Char -> Int
index az c = fromJust $ elemIndex c az

revIndex :: String -> Char -> Int
revIndex = index . reverse

makeWiring :: String -> String -> Map Char Char
makeWiring az = fromList . zip az

makeWheel :: String -> String -> String -> String -> Wheel
makeWheel az wiring@(startPosition:_) notch turnover =
    Wheel (makeWiring az wiring) notch turnover startPosition 'A'

makePlugboard :: String -> [String] -> Map Char Char
makePlugboard az xs = fromList pairs `union` fromList (map swap pairs) `union` makeWiring az az
    where pairs = [(x, y) | (x:y:_) <- xs]

setWheel :: Wheel -> Char -> Char -> Wheel
setWheel wheel startPosition ringOffset = wheel { startPosition = startPosition, ringOffset = ringOffset }

inv :: Wheel -> Wheel
inv wheel = wheel { wiring = inv' $ wiring wheel }

inv' :: Map Char Char -> Map Char Char
inv' = fromList . map swap . toList

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) $ drop (n `mod` length xs) $ cycle xs

translate :: String -> Int -> Char -> Char
translate az n = (!) $ makeWiring az (rotate n az)

transform ::  Wheel -> Char -> Char
transform wheel = (!) (wiring wheel)

encrypt':: Enigma -> Int -> String -> String
encrypt' _ _ [] = []
encrypt' enigma@(Enigma az p etw l m r ukw) n (x:xs) =
    -- trace (show steps
    --     ++ ", n: " ++ showPadded n
    --     ++ ", mod: " ++ showPadded iMod
    --     ++ ", i: " ++ showPadded i ++ " (" ++ show iChar ++ ")"
    --     ++ ", j: " ++ showPadded j ++ " (" ++ show jChar ++ ")"
    --     ++ ", k: " ++ showPadded k ++ " (" ++ show kChar ++ ")")
    ( (!) p
    . translate az (-i) . transform (inv r) . translate az i
    . translate az (-j) . transform (inv m) . translate az j
    . translate az (-k) . transform (inv l) . translate az k
    . transform ukw
    . translate az (-k) . transform l . translate az k
    . translate az (-j) . transform m . translate az j
    . translate az (-i) . transform r . translate az i
    . (!) (inv' p)) x : encrypt' enigma (n + 1) xs
    where
        -- -------------------------------------------------------------------
        -- TODO: Delete this later, used only for debugging.
        -- -------------------------------------------------------------------
        -- azLength = length az
        -- iMod = i `mod` azLength
        -- jMod = j `mod` azLength
        -- kMod = k `mod` azLength
        -- iChar = az !! iMod
        -- jChar = az !! jMod
        -- kChar = az !! kMod
        -- p1 = (!) (inv' p) x
        -- r1 = translate az i p1
        -- r2 = transform r r1
        -- r3 = translate az (-i) r2
        -- m1 = translate az j r3
        -- m2 = transform m m1
        -- m3 = translate az (-j) m2
        -- l1 = translate az k m3
        -- l2 = transform m l1
        -- l3 = translate az (-k) l2
        -- u1 = transform ukw l3
        -- l4 = translate az k u1
        -- l5 = transform (inv l) l4
        -- l6 = translate az (-k) l5
        -- m4 = translate az j l6
        -- m5 = transform (inv m) m4
        -- m6 = translate az (-j) m5
        -- r4 = translate az i m6
        -- r5 = transform (inv r) r4
        -- r6 = translate az (-i) r5
        -- p2 = (!) p r6
        -- steps = [[x], [p1], [r1, r2, r3], [m1, m2, m3], [l1, l2, l3], [u1], [l4, l5, l6], [m4, m5, m6], [r4, r5, r6], [p2]]
        -- -------------------------------------------------------------------
        i = startIndex r - ringIndex r + n
        j = startIndex m - ringIndex m + turnoverCount 1 i r
        j' = startIndex m - ringIndex m + turnoverCount 2 i r
        k = startIndex l - ringIndex l + turnoverCount 0 j' m
        turnoverCount offset ind wheel =
            sum [(1 + ind - offset + revIndex az t) `div` length az - (if startIndex wheel > index az t then 1 else 0) | t <- turnover wheel]
        startIndex = index az . startPosition
        ringIndex = index az . ringOffset

encrypt :: Key -> String -> String
encrypt (Key
            (wheelL:wheelM:wheelR:_)
            (ringL:ringM:ringR:_)
            (startL:startM:startR:_)
            crossPlugging) =
    encrypt' (Enigma _AZ p etw l m r ukw) 1
    where
        p = makePlugboard _AZ crossPlugging
        etw = setWheel _ETW 'A' 'A'
        l = setWheel wheelL startL ringL
        m = setWheel wheelM startM ringM
        r = setWheel wheelR startR ringR
        ukw = setWheel _UKWB 'A' 'A'
