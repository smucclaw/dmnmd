{-# LANGUAGE Arrows #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module DMN.XML.ParseDMN where

-- import Text.XML.HXT.Arrow.Pickle.Schema
-- import Text.XML.HXT.Arrow.ParserInterface

import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Core

data Guest = Guest {firstName, lastName :: String}
  deriving (Show, Eq)

atTag :: ArrowXml a => String -> a (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName tag)

text :: ArrowXml cat => cat (NTree XNode) String
text = getChildren >>> getText

-- |  Parse a Guest in xml
getGuest2 :: ArrowXml cat => cat (NTree XNode) Guest
getGuest2 =
  atTag "guest"
    >>> proc x -> do
      fname <- text <<< atTag "fname" -< x
      lname <- text <<< atTag "lname" -< x
      returnA -< Guest {firstName = fname, lastName = lname}

main :: IO ()
main = do
  guests <-
    runX
      ( readDocument [withValidate no] "simple1.xml"
          >>> getGuest2
      )
  print guests

getEx1 :: IO [XmlTree]
getEx1 = runX $ readDocument [] "test/simulation.dmn"

getEx2 :: IO XmlTree
getEx2 = do
  [ans] <- runX $ removeAllWhiteSpace <<< readDocument [withCheckNamespaces True] "test/simple.dmn"
  pure ans

newtype XDMN = XDMN {unDMN :: DMNInner}
  deriving (Show)

type DMNInner = (DMNDI)

unwDMN :: (DMNInner -> XDMN, XDMN -> DMNInner)
unwDMN = (XDMN, unDMN)

xmlns_dmn, xmlns_dmndi, xmlns_dc, xmlns_di, xmlns_camunda :: String
xmlns_dmn = "https://www.omg.org/spec/DMN/20191111/MODEL/"
xmlns_dmndi = "https://www.omg.org/spec/DMN/20191111/DMNDI/"
xmlns_dc = "http://www.omg.org/spec/DMN/20180521/DC/"
xmlns_di = "http://www.omg.org/spec/DMN/20180521/DI/"
xmlns_camunda = "http://camunda.org/schema/1.0/dmn"

-- id="dinnerDecisions"
-- name="Dinner Decisions"
-- namespace="http://camunda.org/schema/1.0/dmn"

instance XmlPickler XDMN where
  xpickle = dmnPickler

--   xpickle :: PU XDMN
--   xpickle = xpElemNS xmlns_dmn "dmn" "definitions" $ xpLift XDMN

withNS :: PU a -> PU a
withNS =
  xpAddNSDecl "asd" xmlns_dmn
    . xpAddNSDecl "qw1" xmlns_dmndi
    . xpAddNSDecl "qw2" xmlns_dc
    . xpAddNSDecl "qw3" xmlns_di
    . xpAddNSDecl "qw4" xmlns_camunda
    . xpAddNSDecl "qw4" "nope"

dmnPickler :: PU XDMN
dmnPickler =
  xpWrap unwDMN
    . xpElemNS xmlns_dmn "" "definitions"
    . withNS
    $ pdmndi

-- dmnPickler = xpElemNS xmlns_dmn "" "definitions" $ xpLift XDMN

data DMNDI = DMNDI
  deriving (Show)

pdmndi :: PU DMNDI
pdmndi = xpElemNS xmlns_dmndi "dmndi" "DMNDI" $ xpLift DMNDI

runEx2 :: IO [XDMN]
runEx2 =
  runX
    ( xunpickleDocument
        dmnPickler
        [withValidate no, withCheckNamespaces yes, withRemoveWS yes]
        "test/simple.dmn"
    )

ex3 :: XDMN
ex3 = XDMN DMNDI

showEx3 :: IO ()
showEx3 =
  putStrLn $
    showPickled
      [withValidate no, withCheckNamespaces yes, withRemoveWS yes]
      ex3
