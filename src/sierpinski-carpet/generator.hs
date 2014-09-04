import System.Environment
import Data.List 

cell :: [Char] -> [Char]
cell seed =  intercalate seed ["<div>", "</div>"]  

build :: Int -> ([Char] -> [Char]) -> [Char]
build order generator = apply order (cell "") generator
    where 
      apply order seed generator
          | order <= 0 =  seed
          | otherwise  =  apply (order - 1) (generator seed) generator 


-- | Base structure
--
--  <div>
--    <div></div>
--    <div></div>
--    <div></div>
--    <div></div>
--
--    <div></div>
--
--    <div></div>
--    <div></div>
--    <div></div>
--    <div></div>
--  </div> 
--
generator :: [Char] -> [Char]    
generator seed = cell ((base seed)  ++ (cell "") ++ (base seed))
    where base seed = intercalate "" (replicate 4 seed)   


main :: IO()
main = do
    args <- getArgs
    putStrLn (build (getLevel args) generator)  
        where 
          getLevel []   = 0
          getLevel args = read (head args) :: Int   
