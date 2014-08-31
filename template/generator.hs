#!/usr/bin/env runhaskell

import System.Environment
import Data.List 

cell :: [Char] -> [Char]
cell seed =  intercalate seed ["<div>", "</div>"]  

build :: Int -> ([Char] -> [Char]) -> [Char]
build level generator = apply level (cell "") generator
    where 
      apply level seed generator
          | level <= 0 =  seed
          | otherwise  =  apply (level - 1) (generator seed) generator 


-- definition goes here
generator :: [Char] -> [Char]    
generator seed = seed


main = do
    args <- getArgs
    putStrLn (build (getLevel args) generator)  
        where 
          getLevel []   = 0
          getLevel args = read (head args) :: Int
