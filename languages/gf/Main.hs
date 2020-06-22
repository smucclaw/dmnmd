
module Main where

  import PGF2
  import DMN
  import qualified Data.Map as M

  main :: IO ()
  main = do
    gr <- readPGF "DMN.pgf"

    -- Haskell abstract syntax from DMN.hs
    let tree = gf (GRow (GInt 1) (GSingle (GColHd (GString "Season") (GFNullary (GVS (GString "Winter"))))) (GSingle (GColHd (GString "Dish") (GFNullary (GVS (GString "Stew"))))) GNoComment)

    putStrLn $ unlines [ langName ++ ": " ++ linearize lang tree
                       | (langName,lang) <- M.toList $ languages gr ]
