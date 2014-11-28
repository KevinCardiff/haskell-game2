module HaskellGame.Rendering where

import Prelude (
                 Num(..), Show(..), Integral(..), Ord(..),
                 IO(..), Integer(), String()
               )
import qualified System.Console.ANSI as Console
import qualified Data.List as List
import Control.Concurrent (threadDelay)
import Control.Monad (mapM_)
import Data.List ((++), (!!), length, concat)
import Data.Char (toUpper)
import System.IO (hPutStr, hFlush, Handle())

import HaskellGame.Datatypes
import HaskellGame.Graphics
import HaskellGame.Utils
import HaskellGame.Battle (health, level) -- we need this for printing the status bar

{- Print a message at a screen location -}

printAt theScreen (x, y) (colour, text) = do
  Console.hSetCursorPosition theScreen y x
  Console.hSetSGR theScreen [Console.SetColor Console.Foreground Console.Vivid colour]
  hPutStr theScreen text
  Console.hSetSGR theScreen [Console.Reset]


{- Rendering the game world to the console -}

renderMap :: Handle -> Map -> IO ()
renderMap theScreen theMap = do
  let xcoords = [0..((width theMap)-1)]
  let ycoords = [0..((height theMap)-1)]
  let allCoords = [ (x, y) | x <- xcoords, y <- ycoords ]
  mapM_ (drawItem theMap theScreen) allCoords
  where
    drawItem theMap theScreen (x,y) = do
      let item = (((tiles theMap) !! y) !! x)
      printAt theScreen (x, y) (Console.White, (show item))

{- To render a thing, we need to know how to print it (Show typeclass),
   and *where* to print it (Located typeclass) -}

render :: (Graphic a, Located a) => Handle -> a -> IO ()
render theScreen obj = do
  let (x, y) = position obj
  Console.hSetSGR theScreen [Console.SetConsoleIntensity Console.BoldIntensity]
  printAt theScreen (x, y) (Console.White, [symbol obj])

renderScene :: Handle -> Scene -> IO ()
renderScene theScreen theScene = do
  renderMap theScreen (map (currentLevel theScene))
  mapM_ (render theScreen) (objects (currentLevel theScene))
  mapM_ (render theScreen) (monsters (currentLevel theScene))
  render theScreen (player theScene)

{- Print the player's status display -}

showStatus theScreen (x, y) p = do
  Console.hSetSGR theScreen [Console.SetConsoleIntensity Console.BoldIntensity]
  let displayMessage = "HP: " ++ (show (hitpoints p)) ++ " " ++
                       "XP: " ++ (show (experience p)) ++ " " ++
                       "LVL: " ++ (show (level p)) ++ " " ++
                       (concat (List.map show (stats p)))
  printAt theScreen (x, y) (Console.Green, displayMessage)

{- We need to be able to print the game time elapsed nicely -}

showTimeElapsed theScreen (x, y) estate =
  let totalSeconds = (frameNumber estate) `div` (frameRate estate)
      minutes = totalSeconds `div` 60
      seconds = totalSeconds `mod` 60
      message = (pad 2 '0' (show minutes)) ++ ":" ++ (pad 2 '0' (show seconds))
  in do Console.hSetSGR theScreen [Console.SetConsoleIntensity Console.BoldIntensity]
        printAt theScreen (x, y) (Console.Yellow, message)
  where
    pad desiredLength padChar someStr =
      if (length someStr) < desiredLength then
        pad (desiredLength - 1) padChar (padChar:someStr)
      else someStr

{- We want a legend printed with the game controls in it -}

showControls theScreen (x, y) = do
  printAt theScreen (x, y) (Console.Magenta, "Controls: [i,j,k,l] = move, a = attack, p = pick up, d = drop")
