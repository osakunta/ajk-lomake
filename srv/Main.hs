module Main (main) where

import SatO.AJK.Lomake (defaultMain)
import System.Posix.Daemonize (daemonize)

main :: IO ()
main = daemonize defaultMain
