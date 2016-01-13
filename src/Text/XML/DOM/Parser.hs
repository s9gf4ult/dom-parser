module Text.XML.DOM.Parser
       ( -- * Parser internals
         ParserData(..)
       , pdCursor, pdAxis, pdPath
       , ParserError(..)
       , pePath, peDetails
       , ParserErrors(..)
       , throwParserError
       , renderPath
         -- * Parser itself
       , DomParser
       , runDomParser
       , ContentParser
         -- * Common parsers
       , unitFromDom
       , voidFromDom
       , textFromContent
       , stringFromContent
       , charFromContent
       , intFromContent
       , integerFromContent
       , doubleFromContent
       , fixedFromContent
       , boolFromContent
         -- * Parser classes
       , FromDom(..)
       , FromContent(..)
         -- * Combinators
       , inElem, inElems, nonEmptyInElems, maybeInElem, inElemsPred
       , inAxis, inDescendants, inTags
         -- * Content getters
       , tryCurrentContent, tryCurrentContentText
       , currentContent
       , tryCurrentName , currentName
       , tryCurrentAttr , currentAttr
       , elemContent, nonEmptyElemsContent, elemsContent, maybeElemContent
         -- * Helpers
       , fromContentR
       , CurrentContent(..)
         -- * Raw node getters
       , currentNodes
         -- * Checkers
       , checkCurrentLaxName, checkCurrentName
       ) where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader (Reader, runReader)
import           Data.Fixed
import           Data.Functor.Compose
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.OpenUnion
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable
import           Data.Typeable
import           Data.Void
import           GHC.Generics (Generic)
import           Text.Read (readMaybe)
import           Text.Shakespeare.Text (st)
import           Text.XML
import           Text.XML.Cursor
import           TypeFun.Data.List hiding (Union)

-- | DOM parser error description.
data ParserError
  -- | Tag not found which should be.
  = PENotFound
    { _pePath :: ![Text]
    }

  -- | Tag contents has wrong format, (could not read text to value)
  | PEWrongFormat
    { _pePath :: ![Text]     -- ^ path of element
    , _peDetails :: Text
    }

  -- | Such tag name is not expected in this place
  | PEWrongTagName
    { _pePath    :: ![Text]
    , _peDetails :: !Text
    }

  -- | Node is not an element but should be
  | PENotElement
    { _pePath :: ![Text]
    }

  -- | Node should have text content, but it does not.
  | PEContentNotFound
    { _pePath :: ![Text]
    }

  -- | Any other error
  | PEOther
    { _pePath    :: ![Text]
    , _peDetails :: !Text
    } deriving (Eq, Ord, Show, Generic)

makeLenses ''ParserError
makePrisms ''ParserError
instance Exception ParserError

data ParserData = ParserData
    { _pdCursor :: !Cursor       -- ^ Cursor to current parser's environment
    , _pdAxis   :: !Axis         -- ^ Context axis to follow deeper
    , _pdPath   :: ![Text]       -- ^ Path for errors
    } deriving (Generic)

makeLenses ''ParserData

newtype ParserErrors = ParserErrors
  { unParserErrors :: [ParserError]
  } deriving (Ord, Eq, Show, Generic)

makeWrapped ''ParserErrors

instance Exception ParserErrors

-- | Parser monad where all parsing actions live
type DomParser = ExceptT [ParserError] (Reader ParserData)

-- | Content parser type. Parser is just a function taking Text and
-- returning either error description or successfully parsed value.
type ContentParser a = Text -> Either Text a

-- | Render path for showing error
renderPath :: [Text] -> String
renderPath [] = "document"
renderPath path
  = T.unpack
  $ mconcat
  $ L.intersperse ">" path

throwParserError :: ([Text] -> ParserError) -> DomParser a
throwParserError mkerr = do
  pd <- ask
  let err = mkerr $ pd ^. pdPath
  throwE [err]


nodeName :: Node -> Maybe Name
nodeName (NodeElement el) = Just $ elementName el
nodeName _ = Nothing


-- | Run parser on root element of Document.
runDomParser :: Document
             -> DomParser a
             -> Either [ParserError] a
runDomParser doc par =
  let cur = fromDocument doc
      Just root = nameLocalName <$> nodeName (node cur)
      pd = ParserData
           { _pdCursor = cur
           , _pdAxis = pure
           , _pdPath = [root]
           }
  in runReader (runExceptT par) pd

-- | Helper function, throws 'PENotFound' if second argument is empty
-- list
takeFirstElem :: [Text] -> [a] -> DomParser a
takeFirstElem path []    = throwE $ [PENotFound path]
takeFirstElem _    (a:_) = return a


-- | Find first element with given name in current element and run
-- parser inside of found element. Throws PENotFound error if element
-- not found.
inElem :: Text -> DomParser a -> DomParser a
inElem name p = do
  pd <- ask
  let newpath = (pd ^. pdPath) ++ [name]
  newcur <- takeFirstElem newpath
            $ (pd ^. pdCursor) $/ (pd ^. pdAxis) >=> (laxElement name)
  let newdata = ParserData
                { _pdCursor = newcur
                , _pdAxis   = pure
                , _pdPath   = newpath
                }
  local (const newdata) p

-- | Generic elements combinator. Takes predicate filtering/converting
-- list of cursors to some traversable (with posible filtering and/or
-- reordering)
inElemsPred :: (Traversable f)
            => ([Cursor] -> f Cursor) -- ^ Some predicate like 'listToMaybe'
            -> Text                   -- ^ Name of tags to find in current tag
            -> DomParser a            -- ^ Parser to run inside found cursors
            -> DomParser (f a)
inElemsPred cpred name p = do
  pd <- ask
  let newpath = (pd ^. pdPath) ++ [name]
      curs    = cpred
                $ (pd ^. pdCursor)
                $/ (pd ^. pdAxis) >=> (laxElement name)
  for curs $ \cur -> do
    let newdata = ParserData
                  { _pdCursor = cur
                  , _pdAxis   = pure
                  , _pdPath   = newpath
                  }
    local (const newdata) p

-- | Find all elements with gievn name in current element and run
-- parser inside of this elements.
inElems :: Text -> DomParser a -> DomParser [a]
inElems = inElemsPred id

nonEmptyInElems :: Text -> DomParser a -> DomParser (NonEmpty a)
nonEmptyInElems name parser
    = (getCompose <$> inElemsPred (Compose . NE.nonEmpty) name parser)
  >>= maybe (throwParserError PENotFound) return

-- | Try to find element with given name and run parser inside of
-- it. If not found return Nothing
maybeInElem :: Text -> DomParser a -> DomParser (Maybe a)
maybeInElem = inElemsPred listToMaybe

-- | Run parser within axis context. Expected to not use directly.
inAxis :: [Text]                -- ^ Path suffix to append to path before run parser
       -> Axis                  -- ^ Axis to append to context
       -> DomParser a           -- ^ Parser to run
       -> DomParser a
inAxis pathsuff axis parser = do
  local ( over pdAxis (>=> axis)
        . over pdPath (++ pathsuff) ) parser

-- | Given parser will match inside specific
inTags :: [Text]                -- ^ Sequence of tag names parser must
                                -- match inside
       -> DomParser a           -- ^ Parser to run
       -> DomParser a
inTags names p =
  let axis = foldr (&/) pure $ map laxElement names
  in inAxis names axis p

-- | Given parser will match tag in arbitrary deepness
inDescendants :: DomParser a -> DomParser a
inDescendants = inAxis ["*"] descendant

-- | Return the name of current cursor we staying in. Return 'Nothing'
-- if we are not staying on element node
tryCurrentName :: DomParser (Maybe Name)
tryCurrentName = do
  pd <- ask
  return $ nodeName $ node $ pd ^. pdCursor

-- | Return name of current element the parser in.
currentName :: DomParser Name
currentName = tryCurrentName >>= \case
  Nothing -> throwParserError PENotElement
  Just name -> return name

-- | Run predicate with current tag name. Parser fails if predicate
-- returned (Just msg) or node is not an element.
checkCurrentName :: (Name -> Maybe Text) -- ^ name checking predicate
                 -> DomParser ()
checkCurrentName predicate = do
  n <- currentName
  case predicate n of
    Nothing -> return ()
    Just msg -> throwParserError $ \p -> PEWrongTagName p msg

-- | Throw 'PEWrongTagName' if name of current element does not match
-- with given.
checkCurrentLaxName :: Text -> DomParser ()
checkCurrentLaxName name =
  let msg = [st|Expected tag name: #{name}|]
      predicate n = if (nameLocalName n == name)
                    then Nothing
                    else Just msg
  in checkCurrentName predicate

-- | Get concatenated text from current parser's node(s). If current
-- context have no @Content@ nodes then return Nothing.
tryCurrentContentText :: DomParser (Maybe Text)
tryCurrentContentText = do
  pd <- ask
  let txt = (pd ^. pdCursor) $/ (pd ^. pdAxis) >=> content
  return $ case txt of
    [] -> Nothing
    x  -> Just $ mconcat x

tryCurrentContent :: ContentParser a -> DomParser (Maybe a)
tryCurrentContent cparse = do
  tmay <- tryCurrentContentText
  for tmay $ \t -> case cparse t of
    Left err -> throwParserError
                $ \p -> PEWrongFormat p err
    Right a -> pure a

currentContent :: ContentParser a -> DomParser a
currentContent cparse
   = tryCurrentContent cparse
 >>= maybe (throwParserError PEContentNotFound) return

elemContent :: Text -> ContentParser a -> DomParser a
elemContent name cparse
  = inElem name
  $ currentContent cparse

nonEmptyElemsContent :: Text -> ContentParser a -> DomParser (NonEmpty a)
nonEmptyElemsContent name cparse
    = (NE.nonEmpty <$> elemsContent name cparse)
  >>= maybe (throwParserError PENotFound) return

elemsContent :: Text -> ContentParser a -> DomParser [a]
elemsContent name cparse
  = fmap catMaybes
  $ inElems name
  $ tryCurrentContent cparse

maybeElemContent :: Text -> ContentParser a -> DomParser (Maybe a)
maybeElemContent name cparse
  = fmap join
  $ maybeInElem name
  $ tryCurrentContent cparse

-- | Take attribute from current node (if it is an element). Throws
-- 'PENotFound' or 'PENotElement'
currentAttr :: Text -> DomParser Text
currentAttr aname = do
  pd <- ask
  let newpath = (pd ^. pdPath) ++ [mappend "attribute " aname]
  takeFirstElem newpath
    $ laxAttribute aname (pd ^. pdCursor)

tryCurrentAttr :: Text -> DomParser (Maybe Text)
tryCurrentAttr aname = do
  pd <- ask
  return $ listToMaybe $ laxAttribute aname $ pd ^. pdCursor

-- | Always successfully parses any DOM to @()@
unitFromDom :: DomParser ()
unitFromDom = pure ()

-- | Never parses successfully. It is just 'mzero'
voidFromDom :: DomParser Void
voidFromDom = mzero

-- | Does not strip content. Returns content unmodified.
textFromContent :: ContentParser Text
textFromContent = Right

-- | Does not strip content. Returns content unmodified.
stringFromContent :: ContentParser String
stringFromContent = Right . T.unpack

-- | Expects content to be a singe non-blank character. Blank characters
-- are stripped to parse pretty-printed XML files.
charFromContent :: ContentParser Char
charFromContent t = case T.unpack $ T.strip t of
  [a] -> Right a
  x -> let msg = [st|Tag sould contain exactly one char, but it contains: #{x}|]
       in Left msg

intFromContent :: ContentParser Int
intFromContent = fromContentR

integerFromContent :: ContentParser Integer
integerFromContent = fromContentR

doubleFromContent :: ContentParser Double
doubleFromContent = fromContentR

fixedFromContent :: (HasResolution a, Typeable a) => ContentParser (Fixed a)
fixedFromContent = fromContentR

-- | Expects content to be y, yes, t, true or 1 for True value. n, no,
-- f, false or 0 for False value. Case is not significant, blank
-- characters are striped.
boolFromContent :: ContentParser Bool
boolFromContent t =
  let
    lowt  = T.toLower $ T.strip t
    tvals = ["y", "yes", "t", "true", "1"]
    fvals = ["n", "no", "f", "false", "0"]
  in if | lowt `elem` tvals -> return True
        | lowt `elem` fvals -> return False
        | otherwise         ->
          let msg = [st|Could not read "#{t}" as Bool|]
          in Left msg

-- | Typeclass for structures which may be parsed from XML
-- DOM. Usually you should pass parsing function explicitly to
-- combinators like 'inElem', 'maybeInElem' or 'inTags' , but
-- sometimes you need term search. Especially when you try to parse
-- polymorphic types. Or you maybe generate parser with TH for your
-- types, so typeclass would be convenient also.
class FromDom a where
  fromDom :: DomParser a

instance FromDom () where
  fromDom = unitFromDom

instance FromDom (Union '[]) where
  fromDom = mzero

instance ( Typeable a, FromDom a, FromDom (Union as)
         , SubList as (a ': as) )
         => FromDom (Union (a ': as)) where
  fromDom = (liftUnion <$> (fromDom :: DomParser a))
        <|> (reUnion <$> (fromDom :: DomParser (Union as)))

-- | Usually you should pass 'ContentParser' to combinators like
-- 'elemContent' or 'maybeElemContent' explicitly. But sometimes you
-- need term search. Especially for code generated with TH.
class FromContent a where
  -- | Should return either error message (what was wrong) or parsed
  -- value
  fromContent :: ContentParser a

instance FromContent Text where
  fromContent t = Right t

instance FromContent String where
  fromContent t = pure $ T.unpack t

instance FromContent Char where
  fromContent = charFromContent

fromContentR :: forall a. (Read a, Typeable a) => Text -> Either Text a
fromContentR t = case readMaybe $ T.unpack $ T.strip t of
  Nothing ->
    let name = typeRep (Proxy :: Proxy a)
    in Left [st|Unreadable #{show name}: #{t}|]
  Just x  -> Right x

instance FromContent Int where
  fromContent = intFromContent

instance FromContent Integer where
  fromContent = integerFromContent

instance FromContent Double where
  fromContent = doubleFromContent

instance (HasResolution a, Typeable a) => FromContent (Fixed a) where
  fromContent = fixedFromContent

-- | This isntance might be not very obvious but anyway
instance FromContent Bool where
  fromContent = boolFromContent

-- | Helper newtype returning 'currentContent' for any type with
-- instance 'FromContent'
newtype CurrentContent a = CurrentContent
  { unCurrentContent :: a
  } deriving (Ord, Eq, Show, Generic)

instance (FromContent a) => FromDom (CurrentContent a) where
  fromDom = CurrentContent <$> currentContent fromContent

-- | Get children nodes from current parser's node.
currentNodes :: DomParser [Node]
currentNodes = do
  pd <- ask
  let curs = (pd ^. pdCursor) $/ (pd ^. pdAxis)
  return $ fmap node curs
