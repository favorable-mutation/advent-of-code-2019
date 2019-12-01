module AocLib
    ( parseChallengeInput
    ) where

import System.Environment

parseChallengeInput :: IO ()
parseChallengeInput = do
  let date = 2
  let inputFile = "input/" ++ show date ++ ".txt"
  fileContent <- readFile inputFile
  let input = map read (lines fileContent)
  print ((getFuncForDate date) input)


-- Given the challenge to solve for, delegate to the relevant
-- function
getFuncForDate :: Int -> ([Int] -> Int)
getFuncForDate date
  | date == 1 = countFuelForMasses
  | date == 2 = questionMark

-- Given a list of module masses, calculate the fuel required
-- to launch all those masses into spaaaaace, with the given
-- function describing the calculation strategy
countFuel :: [Int] -> (Int -> Int) -> Int
countFuel fuelCosts fuelFunc = sum (map fuelFunc fuelCosts)

-- A function for the calculation strategy that doesn't account
-- for fuel mass
countFuelForMasses :: [Int] -> Int
countFuelForMasses modules = countFuel modules countFuelForMass

countFuelForMass :: Int -> Int
countFuelForMass mass = div mass 3 - 2

-- A function for the calculation strategy that accounts for
-- fuel mass
countFuelForMasses' :: [Int] -> Int
countFuelForMasses' modules = countFuel modules countFuelForMass'

countFuelForMass' :: Int -> Int
countFuelForMass' mass = countFuelForFuelMass (countFuelForMass mass)

countFuelForFuelMass :: Int -> Int
countFuelForFuelMass newMass
  | newMass <= 0 = 0
  | newMass >  0 = newMass + countFuelForMass' newMass

questionMark x = 1
