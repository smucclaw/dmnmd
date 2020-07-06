{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module DMN.XML.ParseDMN where

-- import Text.XML.HXT.Arrow.Pickle.Schema
-- import Text.XML.HXT.Arrow.ParserInterface
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs

data Guest = Guest { firstName, lastName :: String }
  deriving (Show, Eq)

atTag :: ArrowXml a => String -> a (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName tag)

text :: ArrowXml cat => cat (NTree XNode) String
text = getChildren >>> getText

-- | Parse a Guest in xml
getGuest2 :: ArrowXml cat => cat (NTree XNode) Guest
getGuest2 = atTag "guest" >>>
  proc x -> do
    fname <- text <<< atTag "fname" -< x
    lname <- text <<< atTag "lname" -< x
    returnA -< Guest { firstName = fname, lastName = lname }


main :: IO ()
main = do
  guests <- runX (readDocument [withValidate no] "simple1.xml" 
                    >>> getGuest2)
  print guests