module AocLib
    ( parseChallengeInput
    ) where

import System.Environment
import Data.List.Split

parseChallengeInput :: IO ()
parseChallengeInput = do
  let date = 2
  let part = 1
  let inputFile = "input/" ++ show date ++ ".txt"
  fileContent <- readFile inputFile
  let input = lines fileContent
  print (getFuncForDate date part input)


-- Given the challenge to solve for, delegate to the relevant
-- function
getFuncForDate :: Int -> Int -> ([String] -> Int)
getFuncForDate date part
  | date == 1 = countFuelForMasses
  | date == 2 = runIntcodeFromStrings
  | date == 3 = dayThree part

parseStringsToInts :: [String] -> [Int]
parseStringsToInts = map read

parseInts :: [String] -> [String]
parseInts = concatMap readCSV

parseCoords :: [String] -> [[(Char, Int)]]
parseCoords = map (splitCoords . readCSV)

splitCoords :: [String] -> [(Char, Int)]
splitCoords = map (\ coord -> (head coord, read (tail coord)))

readCSV :: String -> [String]
readCSV = splitOn ","

-- Given a list of module masses, calculate the fuel required
-- to launch all those masses into spaaaaace, with the given
-- function describing the calculation strategy
countFuel :: [Int] -> (Int -> Int) -> Int
countFuel fuelCosts fuelFunc = sum (map fuelFunc fuelCosts)

-- A function for the calculation strategy that doesn't account
-- for fuel mass
countFuelForMasses :: [String] -> Int
countFuelForMasses modules =
  countFuel (parseStringsToInts modules) countFuelForMass

countFuelForMass :: Int -> Int
countFuelForMass mass = div mass 3 - 2

-- A function for the calculation strategy that accounts for
-- fuel mass
countFuelForMasses' :: [String] -> Int
countFuelForMasses' modules =
  countFuel (parseStringsToInts modules) countFuelForMass'

countFuelForMass' :: Int -> Int
countFuelForMass' mass =
  countFuelForFuelMass (countFuelForMass mass)

countFuelForFuelMass :: Int -> Int
countFuelForFuelMass newMass
  | newMass <= 0 = 0
  | newMass >  0 = newMass + countFuelForMass' newMass

runIntcodeFromStrings :: [String] -> Int
runIntcodeFromStrings unflatInts =
  iterateInputs (parseStringsToInts (parseInts unflatInts))

iterateInputs :: [Int] -> Int
iterateInputs ints = 100 * noun + verb
  where query = 19690720
        range = [0..99]
        (noun, verb, _) = head (filter (\ (_, _, result) -> result == query) (enumerateNouns range ints))

enumerateNouns :: [Int] -> [Int] -> [(Int, Int, Int)]
enumerateNouns range ints = concatMap (enumerateVerbs range ints) range

enumerateVerbs :: [Int] -> [Int] -> Int -> [(Int, Int, Int)]
enumerateVerbs range ints noun = map (trySentence ints noun) range

trySentence :: [Int] -> Int -> Int -> (Int, Int, Int)
trySentence ints noun verb = (noun, verb, head (parseIntcode (replaceParams noun verb ints)))

replaceParams :: Int -> Int -> [Int] -> [Int]
replaceParams x1 x2 ints = replaceAt x2 2 (replaceAt x1 1 ints)

parseIntcode :: [Int] -> [Int]
parseIntcode = readChunk []

readChunk :: [Int] -> [Int] -> [Int]
readChunk seen (opcode:rest)
  | opcode == 99 = ints
  | otherwise    = parseChunk seen opcode rest
  where ints = seen ++ opcode:rest
readChunk seen ints = seen ++ ints

reRead :: Int -> [Int] -> [Int]
reRead seenLength newInts =
  readChunk (take seenLength newInts) (drop seenLength newInts)

parseChunk :: [Int] -> Int -> [Int] -> [Int]
parseChunk seen opcode (x1:x2:index:rest) =
  reRead (length seen + 4) (replaceAt (x1Val `op` x2Val) index newInts)
  where newInts = seen ++ opcode:x1:x2:index:rest
        op = getOperation opcode
        x1Val = newInts !! x1
        x2Val = newInts !! x2
parseChunk seen opcode rest = seen ++ opcode:rest

getOperation :: Int -> (Int -> Int -> Int)
getOperation opcode
  | opcode == 1 = (+)
  | opcode == 2 = (*)

replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt _ _ [] = []
replaceAt value index (int:ints)
  | index == 0 = value : ints
  | index >  0 = int : replaceAt value (index - 1) ints

dayThree :: Int -> [String] -> Int
dayThree part coordStrings
  | part == 1 = crossWires coords
  | part == 2 = 0
  where coords = parseCoords coordStrings

crossWires :: [[(Char, Int)]] -> Int
crossWires _ = 0
