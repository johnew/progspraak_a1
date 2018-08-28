{-# LANGUAGE ParallelListComp #-}

module Lib
    ( add
    , isVowel
    , Status(..)
    , amountOf
    , fib
    , ending
    , takeInt
    , fizzbuzz
    , printFizz
    , listOfEven
    , zipped
    , cartesian
    ) where

import Control.Monad (mapM_)
import Prelude hiding (take)

-- TASK 1
-- Simple functions

-- finish the function "add" that takes two integers
-- and returns the sum of them
add :: Int -> Int -> Int
add n m = n + m

-- complete the function "isVowel" which
-- takes a character and returns True
-- if it's a vowel (English language), False otherwise
-- hint: a string is a list
-- hint2: use `elem` from Prelude
isVowel :: Char -> Bool
isVowel chr = elem chr "AEIOUaeiou"

data Status = One | Two | Three | None deriving (Show, Eq)

-- complete the function "amountOf" which takes
-- a name and a list of subjects
-- it then pattern matches on the amount of subjects in the list
-- and returns a tuple of a Status (look above, describes the amount of subjects in the list)
-- and the name
h = "Haskell"
amountOf :: String -> [a] -> (Status, String)
amountOf h [_] = (One, h)
amountOf h [_,_] = (Two, h)
amountOf h [_,_,_] = (Three, h)
amountOf name list = (None, h)

-- TASK 2
-- Recursion

-- finish the function "fib" that calculates the
-- nth fibonacci number 
-- assuming that 0th = 0 and 1st = 1
-- do not optimize it
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

-- TASK 3
-- Working with lists

-- complete the function "ing" that takes a list of
-- strings and returns them with the ending -ing
-- if the string is empty, remove it from the list
ending :: [String] -> [String]
ending ([]) = []
ending ("":xs) = ending xs
ending (x:xs) = [x ++ "ing"] ++ ending xs

-- complete the function "takeInt" that
-- an integer n and a list of integers and
-- returns the first n elements of 
-- the list
takeInt :: Int -> [Int] -> [Int]
takeInt 0 iList = []
takeInt n ([]) = [] -- if i is 
takeInt n (x:xs) = [x] ++ takeInt (n-1) xs

-- implement "fizzbuzz" as described in exercise 1
ints = [1..100]
fizzbuzz :: [String]
--fizzbuzz = (map (>3) [1,2,3,4])
fizzbuzz = undefined

testStuff :: [String]
testStuff ints
    | mod x 3 == 0 = "fizz"
    | mod x 5 == 0 = "buzz"
    | otherwise = -1

printFizz :: IO ()
printFizz = mapM_ putStrLn fizzbuzz

-- TASK 4
-- List comprehensions

-- create a list "listOfEven" which contains all 
-- even numbers that are equal or greater than 0
-- use a list comprehension
listOfEven :: [Integer]
listOfEven = undefined

-- create a list of tuples, "zipped"
-- where each tuple contains the nth entry
-- in the lists [1..26] and ['a'..'z']
-- hint: parallel list comprehension
zipped :: [(Int, Char)]
zipped = undefined

-- create a list that contains the cartesian
-- product of the two vectors [4, 6, 8]
-- and [3, 7, 9]
-- use a list comprehension
cartesian :: [(Int, Int)]
cartesian = undefined
