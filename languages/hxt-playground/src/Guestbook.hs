{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

-- guestbook parser and pickler/unpickler using hxt

module Guestbook
where

import Text.XML.HXT.Core

import System.Environment
import System.Exit

libmain :: IO ()
libmain
    = do
      argv <- getArgs
      (al, src, dst) <- cmdlineOpts argv
      [rc]  <- runX (application al src dst)
      if rc >= c_err
        then exitWith (ExitFailure (0-1))
        else exitWith ExitSuccess

-- | the dummy for the boring stuff of option evaluation,
-- usually done with 'System.Console.GetOpt'

cmdlineOpts :: [String] -> IO (SysConfigList, String, String)
cmdlineOpts argv
    = return ([withValidate no], argv!!0, argv!!1)

-- | the main arrow

application     :: SysConfigList -> String -> String -> IOSArrow b Int
application cfg src dst
    = configSysVars (withTrace 1 : cfg)                                           -- (0)
      >>> readDocument [withRemoveWS yes] src
      >>> processChildren (processDocumentRootElement `when` isElem)
      >>> processChildren (excludeSomething "lname" `when` isElem)
      >>> withTraceLevel 4 (traceDoc "resulting document")
--      >>> writeDocument [] dst                                        -- (3)
      >>> getErrStatus


when' = flip when

-- | the dummy for the real processing: the identity filter

processDocumentRootElement      :: IOSArrow XmlTree XmlTree
processDocumentRootElement = nickname "Shakespeare" "Bill" >>>
                             nickname "Hawthorne" "Nate"

excludeSomething :: String -> IOSArrow XmlTree XmlTree
excludeSomething str = processTopDown $ none `when` hasName str
  
-- guest > fname > William
-- guest > lname > Shakespeare
-- when we find a guest with a matching lname
-- we replace its fname's txt

textMatch string = getText >>> isA (== string)

nickname :: String -> String -> IOSArrow XmlTree XmlTree
nickname lname fname =
  processTopDown $ -- match every node in the tree
     when' -- we find some node which
     (    hasName "guest" -- with a child
       /> hasName "lname"
       /> textMatch lname -- with a child that's a text node who matches
      >>> traceValue 1 (("found an lname match " ++) .show) -- then debug output
     ) ( -- and then
       processChildren $ -- of the guest node
       when' (hasName "fname") -- looking for a node named fname
             (traceValue 1 (("processing child " ++).show) >>> -- debug output
              replaceChildren (txt fname) -- and change fname's child to the new nickname
             ))




-- meng: this part may not actually be working, do not rely on it

-- let's do a pickler / unpickler for simple1.xml
-- https://wiki.haskell.org/HXT/Conversion_of_Haskell_data_from/to_XML

data Guestbook = Guestbook { guestList :: [Guest] }
  deriving (Show, Eq)

instance XmlPickler Guestbook where
  xpickle = xpGuestbook

xpGuestbook :: PU Guestbook
xpGuestbook =
  xpElem "guestbook"
  $ xpWrap (Guestbook, guestList)
  $ xpList xpickle
  
data Guest = Guest { gfirst :: String
                   , glast  :: Maybe String}
  deriving (Show, Eq)

instance XmlPickler Guest where
  xpickle = xpGuest

xpGuest :: PU Guest
xpGuest = xpElem "guest" $
  xpWrap (\ ((f,l)) -> Guest f l
         ,\ g -> (gfirst g, glast g)
         ) $
  xpPair (xpElem "fname" xpText0)
         (xpOption (xpElem "lname" xpText0))

loadPickle :: IO Guestbook
loadPickle
  = do
    [p2] <- runX
            ( xunpickleDocument xpGuestbook
                  [ withRemoveWS yes   -- remove redundant whitespace
                  , withValidate no    -- don't validate source
                  ] "simple1.xml"
            )
    return p2

showPickle :: IO ()
showPickle
  = do
    mygb <- loadPickle
    runX ( constA mygb
           >>> arrIO ( \ x -> do {print x ; return x})
           >>> xpickleDocument xpGuestbook
               [ withIndent yes        -- indent XML
               ] "-"
         )
    return ()

