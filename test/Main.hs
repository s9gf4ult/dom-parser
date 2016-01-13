module Main where

import           Control.Applicative
import           Control.Exception
import           Data.Char
import           Data.Default
import           Data.Maybe
import qualified Data.Text as T
import           Test.Hspec
import           Text.Shakespeare.Text ( lt )
import           Text.XML
import           Text.XML.DOM.Parser

cleanDoc :: Document -> Document
cleanDoc (Document p root msc)
  = Document p (cleanE root) msc
  where
    cleanE (Element n a nodes)
      = Element n a $ catMaybes $ map cleanN nodes
    cleanN (NodeContent t) | allSpaces t = Nothing
                           | otherwise   = Just $ NodeContent t
    cleanN (NodeComment _) = Nothing
    cleanN (NodeElement e) = Just $ NodeElement $ cleanE e
    allSpaces t = all isSpace $ T.unpack t

simpleDoc :: Document
simpleDoc = cleanDoc $ parseText_ def
    [lt|
<?xml version="1.0" encoding="utf-8"?>
<root>
  <a>
    <b>content</b>
  </a>
</root>
  |]

complexDoc :: Document
complexDoc = cleanDoc $ parseText_ def
    [lt|
<?xml version="1.0" encoding="utf-8"?>
<root>
  <elem1>
    <elem2>
      <elem3>
        <cont>first</cont>
      </elem3>
    </elem2>
    <elem2>
      <elem3>
        <cont>first</cont>
      </elem3>
    </elem2>
  </elem1>
  <tag1>
    <tag2>
      <tag3>
        <cont>second</cont>
        <cont>second</cont>
      </tag3>
    </tag2>
  </tag1>
</root>
  |]

specParser
  :: String                     -- ^ Name of spec
  -> Document                   -- ^ Document to parse
  -> (a -> Maybe String)        -- ^ Value checker, Nothing if all ok
  -> DomParser a                -- ^ Parser itself
  -> Spec
specParser name doc check parser = it name $ do
  result <- either (throwIO . last) return
          $ runDomParser doc parser
  case check result of
    Nothing -> return ()
    Just e  -> throwIO $ ErrorCall e

specParserEq
  :: (Eq a, Show a)
  => String            -- ^ Name of spec
  -> Document          -- ^ Document to parse
  -> a                 -- ^ Value parser should return
  -> DomParser a       -- ^ Parser itself
  -> Spec
specParserEq name doc a parser = specParser name doc check parser
  where
    check x | x == a = Nothing
            | otherwise = Just $ "should be " ++ show a
                          ++ " but got " ++ show x

specParserEqFailed :: (Show a)
                 => String
                 -> Document
                 -> DomParser a
                 -> Spec
specParserEqFailed name doc parser = it name $ do
  let result = runDomParser doc parser
  case result of
    Right a -> fail $ "Expected parser to fail, but returned" ++ (show a)
    Left _  -> return () :: IO ()

spec :: Spec
spec = describe "DOM Parser" $ do
  describe "alternative instance" $ do
    let firstP = inTags ["elem1", "elem2", "elem3" ] $ elemsContent "cont" textFromContent
        secondP = inTags ["tag1", "tag2", "tag3"] $ elemsContent "cont" textFromContent
        firstR = replicate 2 "first"
        secondR = replicate 2 "second"
    specParserEq "empty first" complexDoc firstR $ empty <|> firstP
    specParserEq "empty last" complexDoc firstR $ firstP <|> empty
    specParserEq "first first" complexDoc firstR $ firstP <|> secondP
    specParserEq "second first" complexDoc secondR $ secondP <|> firstP

  describe "simple DOM tests" $ do
    specParserEq "inElem" simpleDoc "content" $ do
      inElem "a" $ elemContent "b" textFromContent
    specParserEq "inElems" simpleDoc ["content"] $ do
      inElems "a" $ elemContent "b" textFromContent

    specParserEq "inDescendants" simpleDoc "content" $ do
      inDescendants $ elemContent "b" textFromContent
    specParserEq "inTags" simpleDoc "content" $ do
      inTags ["a"] $ elemContent "b" textFromContent

    let oneElem x = case length x of
          1 -> Nothing
          l -> Just $ "length of nodes list is wrong: " ++ show x
    specParser "currentNodes" simpleDoc oneElem $ do
      inTags ["a"] $ currentNodes

  describe "complex DOM tests" $ do
    specParserEq "inElems" complexDoc (replicate 2 "first") $ do
      inElem "elem1"
        $ inElems "elem2"
        $ inElem "elem3"
        $ elemContent "cont" textFromContent

    specParserEq "currentName" complexDoc "elem3" $ do
      inDescendants $ inElem "elem3" $ currentName

    specParserEq "first correct parser" complexDoc (replicate 2 "first") $ do
      inElem "elem1" $ do
        inElems "elem2" $ do
          inElem "elem3" $ elemContent "cont" textFromContent

    specParserEqFailed "first incorrect parser" complexDoc $ do
      inElem "elem1" $ do
        inElem "elem3" $ do     -- wrong nesting
          inElem "elem2" $ elemsContent "cont" textFromContent

    -- Same parser as previous but do not care of elements structure
    specParserEq "first simple parser" complexDoc (replicate 2 "first") $ do
      inElem "elem1" $ inDescendants $ do
        elemsContent "cont" textFromContent

    specParserEq "first simplier parser" complexDoc (replicate 2 "first") $ do
      inTags ["elem1", "elem2", "elem3"] $ elemsContent "cont" textFromContent

    specParserEq "first simplier parser other way" complexDoc (replicate 2 "first") $ do
      inTags ["elem1", "elem2", "elem3"] $ inElems "cont" $ currentContent textFromContent

    specParserEq "second correct parser" complexDoc (replicate 2 "second") $ do
      inElem "tag1" $ do
        inElem "tag2" $ do
          inElem "tag3" $ elemsContent "cont" textFromContent

    specParserEq "second simple parser" complexDoc (replicate 2 "second") $ do
      inTags ["tag1", "tag2", "tag3"] $ elemsContent "cont" textFromContent

    let lenTwo x = case length x of
          2 -> Nothing
          l -> Just $ "length is not 2: " ++ show x
    specParser "currentNodes" complexDoc lenTwo $ do
      inElem "elem1" $ do
        inTags ["elem2", "elem3"] $ do
          currentNodes

main :: IO ()
main = hspec spec
