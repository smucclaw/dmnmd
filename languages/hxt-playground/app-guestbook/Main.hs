module Main
where

import Guestbook
import Text.XML.HXT.Core
import System.Environment
import System.Exit

-- | the dummy for the boring stuff of option evaluation,
-- usually done with 'System.Console.GetOpt'


main :: IO ()
main = do
      argv <- getArgs
      (al, src, dst) <- cmdlineOpts argv
      [rc]  <- runX (application al src dst)
      if rc >= c_err
        then exitWith (ExitFailure (0-1))
        else exitWith ExitSuccess


