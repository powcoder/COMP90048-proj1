https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
--  module Proj1 (initialGuess, nextGuess, GameState) where
import Data.List
import System.IO

pitch = ["A1","A2","A3","B1","B2","B3","C1","C2","C3","D1","D2","D3","E1","E2","E3","F1","F2","F3","G1","G2","G3"]

allPossible :: [String] -> GameState
allPossible = [(x,y,z)| x<-pitch, y<- pitch, z<-pitch, x<y, y<z]

-- randomly pick one guess
pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

type GameState = ()

initialGuess :: ([String],GameState)
initialGuess guess = pick allPossible, allPossible


feedback :: [String] -> [String] -> (Int, Int, Int)
feedback target guess = (right, rightNote, rightOctave)
    where guess'      = nub guess
          right       = length $ intersect guess' target
          num         = length guess'
          rightNote   = num - (length $ deleteFirstsBy (eqNth 0) guess' target)
                    - right
          rightOctave = num - (length $ deleteFirstsBy (eqNth 1) guess' target)
                    - right

eqNth :: Eq a => Int -> [a] -> [a] -> Bool
eqNth n l1 l2 = (l1 !! n) == (l2 !! n)

nextGuess :: ([String],GameState) → (Int,Int,Int) → ([String],GameState)


--next guess is randomly pick one of the possible pitch that has the same feedback
nextGuess guess' = 
     | feedback guess allPossible == feedback target guess = pick GameState 
     | otherwise GameState = filter(feedback guess allPossible == feedback target guess)


