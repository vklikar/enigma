module Main (main) where

import Test.HUnit
import Enigma
import Data.Map (fromList)

referenceWiring = fromList [
    ('A','E'),
    ('B','B'),
    ('C','A'),
    ('D','C'),
    ('E','D'),
    ('F','F')
    ]

referenceWheel = Wheel {
    wiring = referenceWiring,
    notch = "C",
    turnover = "B",
    startPosition = 'E',
    ringOffset = 'A'
    }

emptyPlugboard = makePlugboard "ABCD" []
emptyWheel = makeWheel "ABCD" "ABCD" "" ""
emptyWheelWithNotch = makeWheel "ABCD" "ABCD" "" "D"
reverseWheel = makeWheel "ABCD" "DCBA" "" ""
oneChangeWheel = makeWheel "ABCD" "ABDC" "" ""
rightWheelEnigma = Enigma "ABCD" emptyPlugboard emptyWheel emptyWheel emptyWheel oneChangeWheel reverseWheel
middleWheelEnigma = Enigma "ABCD" emptyPlugboard emptyWheel emptyWheel oneChangeWheel emptyWheelWithNotch reverseWheel
leftWheelEnigma = Enigma "ABCD" emptyPlugboard emptyWheel oneChangeWheel emptyWheelWithNotch emptyWheelWithNotch reverseWheel

indexTest :: Test
indexTest = TestList [
    TestCase $ assertEqual "" 0 (index ['A'..'F'] 'A'),
    TestCase $ assertEqual "" 5 (index ['A'..'F'] 'F')
    ]

revIndexTest :: Test
revIndexTest = TestList [
    TestCase $ assertEqual "" 5 (revIndex ['A'..'F'] 'A'),
    TestCase $ assertEqual "" 0 (revIndex ['A'..'F'] 'F')
    ]

makeWiringTest :: Test
makeWiringTest = TestList [
    TestCase $ assertEqual ""
        referenceWiring
        (makeWiring ['A'..'F'] "EBACDF")
    ]

makeWheelTest :: Test
makeWheelTest = TestList [
    TestCase $ assertEqual ""
        referenceWheel
        (makeWheel ['A'..'F'] "EBACDF" "C" "B")
    ]

makePlugboardTest :: Test
makePlugboardTest = TestList [
    TestCase $ assertEqual ""
        (fromList [
            ('A','F'),
            ('B','B'),
            ('C','D'),
            ('D','C'),
            ('E','E'),
            ('F','A')]
        )
        (makePlugboard ['A'..'F'] ["CD", "FA"])
    ]

setWheelTest :: Test
setWheelTest = TestList [
    TestCase $ assertEqual "startPosition and ringOffset settable"
        referenceWheel
        (setWheel
            (Wheel {
                wiring = fromList [
                    ('A','E'),
                    ('B','B'),
                    ('C','A'),
                    ('D','C'),
                    ('E','D'),
                    ('F','F')
                ],
                notch = "C",
                turnover = "B",
                startPosition = 'C',
                ringOffset = 'D'}
            )
            'E' 'A'
        )
    ]

invTest :: Test
invTest = TestList [
    TestCase $ assertEqual ""
        referenceWheel
        (inv
            (Wheel {
                wiring = fromList [
                    ('E','A'),
                    ('B','B'),
                    ('A','C'),
                    ('C','D'),
                    ('D','E'),
                    ('F','F')
                ],
                notch = "C",
                turnover = "B",
                startPosition = 'E',
                ringOffset = 'A'}
            )
        )
    ]

rotateTest :: Test
rotateTest = TestList [
    TestCase $ assertEqual "" "BCDEFA" (rotate 1 "ABCDEF")
    ]

translateTest :: Test
translateTest = TestList [
    TestCase $ assertEqual "" 'D' (translate ['A'..'F'] 1 'C'),
    TestCase $ assertEqual "" 'A' (translate ['A'..'F'] 1 'F')
    ]

transformTest :: Test
transformTest = TestList [
    TestCase $ assertEqual ""
        'D'
        (transform (Wheel {
            wiring = fromList [
                ('A','E'),
                ('B','B'),
                ('C','A'),
                ('D','C'),
                ('E','D'),
                ('F','F')
            ],
            notch = "C",
            turnover = "B",
            startPosition = 'E',
            ringOffset = 'A'}
        ) 'E')
    ]

encryptRightWheelTest :: Test
encryptRightWheelTest = TestList [
    TestCase $ assertEqual ""
        "DCDCDCDCDCDC"
        (encrypt' rightWheelEnigma 1 "AAAAAAAAAAAA")
    ]

encryptMiddleWheelTest :: Test
encryptMiddleWheelTest = TestList [
    TestCase $ assertEqual ""
        "CCCDDDDCCCCDDDDCCCCDDDDCCCCDDDDCCCCDDDDCCCCDDDDCCC"
        (encrypt' middleWheelEnigma 1 (replicate 50 'A'))
    ]

encryptLeftWheelTest :: Test
encryptLeftWheelTest = TestList [
    TestCase $ assertEqual ""
        "CCCCCCCCCCCCDDDDDDDDDDDDDDDDCCCCCCCCCCCCCCCCDDDDDD"
        (encrypt' leftWheelEnigma 1 (replicate 50 'A'))
    ]

enigmaM3Test :: Test
enigmaM3Test = TestList [
    TestCase $ assertEqual ""
        "SQPEUGJRCXZWPFYIYYBWLOEWROUVKPOZTCEUWTFJZQWPBQLDTTSRMDFLGXBXZRYQKDGJRZEZMKHJNQYPDJWCJFJLFNTRSNCNLGSS"
        (encrypt (Key [_I, _II, _III] "AAA" "CFA" []) (replicate 100 'A'))
    ]

main :: IO Counts
main = runTestTT $ TestList [
    indexTest,
    revIndexTest,
    makeWiringTest,
    makeWheelTest,
    makePlugboardTest,
    setWheelTest,
    invTest,
    rotateTest,
    translateTest,
    transformTest,
    encryptRightWheelTest,
    encryptMiddleWheelTest,
    encryptLeftWheelTest,
    enigmaM3Test
    ]
