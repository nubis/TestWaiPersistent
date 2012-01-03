{- |
This module uses HXT to transverse an HTML document using CSS selectors.

The most important function here is 'findBySelector', it takes a CSS query and
a string containing the HTML to look into,
and it returns a list of the HTML fragments that matched the given query.

You will usually just import it like:

@
import Text.XML.HXT.TransversingCSS (findBySelector)
@

Only a subset of the CSS spec is currently supported:

 * By tag name: /table td a/
 
 * By class names: /.container .content/

 * By Id: /#oneId/

 * By attribute: /[hasIt]/, /[exact=match]/, /[contains*=text]/, /[starts^=with]/, /[ends$=with]/
 
 * Union: /a, span, p/
 
 * Immediate children: /div > p/ 

 * Get jiggy with it: /div[data-attr=yeah] > .mon, .foo.bar div, #oneThing/

-}

module Text.XML.HXT.TransversingCSS (
  findBySelector,
  Html,
  Query,
  -- * For HXT hackers
  -- | These functions expose some low level details that you can blissfully ignore.
  parseQuery,
  runQuery,
  queryToArrow,
  Selector(..),
  SelectorGroup(..)

  )
where

import Text.XML.HXT.Core
import qualified Data.List as DL
import qualified Data.Set as DS
import Text.ParserCombinators.Parsec
import Control.Monad

type Html = String
type Query = String
 
-- | Perform a css 'Query' on 'Html'. Returns Either
--
-- * Left: Query parse error.
--
-- * Right: List of matching Html fragments.
findBySelector :: Html-> Query -> Either ParseError [Html]
findBySelector html query = fmap (runQuery html) (parseQuery query)

-- Run a compiled query on Html, returning a list of matching Html fragments.
runQuery :: Html -> [[SelectorGroup]] -> [Html]
runQuery html query =
  runLA (hread >>> (queryToArrow query) >>> xshow this) html

-- | Transform a compiled query into the HXT arrow that finally transverses the Html
queryToArrow :: ArrowXml a => [[SelectorGroup]] -> a XmlTree XmlTree
queryToArrow commaSeparated = 
  DL.foldl uniteCommaSeparated none commaSeparated

uniteCommaSeparated accum selectorGroups =
  accum <+> (DL.foldl sequenceSelectorGroups this selectorGroups)

sequenceSelectorGroups accum (DirectChildren sels) =
  accum >>> getChildren >>> (DL.foldl applySelectors this $ sels)
sequenceSelectorGroups accum (DeepChildren sels) =
  accum >>> getChildren >>> multi (DL.foldl applySelectors this $ sels)

applySelectors accum selector = accum >>> (toArrow selector)

toArrow selector = case selector of
  ById v -> hasAttrValue "id" (==v)
  ByClass v -> hasAttrValue "class" ((DL.elem v) . words)
  ByTagName v -> hasName v
  ByAttrExists n -> hasAttr n
  ByAttrEquals n v -> hasAttrValue n (==v)
  ByAttrContains n v -> hasAttrValue n (DL.isInfixOf v)
  ByAttrStarts n v -> hasAttrValue n (DL.isPrefixOf v)
  ByAttrEnds n v -> hasAttrValue n (DL.isSuffixOf v)

-- | Parses a query into an intermediate format which is easy to feed to HXT
--
-- * The top-level lists represent the top level comma separated queries.
--
-- * SelectorGroup is a group of qualifiers which are separated
--   with spaces or > like these three: /table.main.odd tr.even > td.big/
--
-- * A SelectorGroup as a list of Selector items, following the above example
--   the selectors in the group are: /table/, /.main/ and /.odd/ 
parseQuery :: String -> Either ParseError [[SelectorGroup]]
parseQuery = parse cssQuery "" 

data SelectorGroup 
  = DirectChildren [Selector]
  | DeepChildren [Selector]
  deriving Show

data Selector
  = ById String
  | ByClass String
  | ByTagName String
  | ByAttrExists String
  | ByAttrEquals String String
  | ByAttrContains String String
  | ByAttrStarts String String
  | ByAttrEnds String String
  deriving Show

-- Below this line is the Parsec parser for css queries.
cssQuery = sepBy rules (char ',' >> (optional (char ' ')))
--rules = sepBy selectors (oneOf " ")
rules = many $ directChildren <|> deepChildren

directChildren = do
  char '>'
  char ' '
  sels <- selectors
  optional $ char ' '
  return $ DirectChildren sels

deepChildren = do 
  sels <- selectors
  optional $ char ' '
  return $ DeepChildren sels
  
selectors = many1 $ parseId
  <|> parseClass
  <|> parseTag
  <|> parseAttr

parseId = do
  char '#'
  x <- many $ noneOf ",#.[ >"
  return $ ById x

parseClass = do
  char '.'
  x <- many $ noneOf ",#.[ >"
  return $ ByClass x

parseTag = do
  x <- many1 $ noneOf ",#.[ >"
  return $ ByTagName x

parseAttr = do
  char '['
  name <- many $ noneOf ",#.=$^*]"
  (parseAttrExists name)
    <|> (parseAttrWith "=" ByAttrEquals name)
    <|> (parseAttrWith "*=" ByAttrContains name)
    <|> (parseAttrWith "^=" ByAttrStarts name)
    <|> (parseAttrWith "$=" ByAttrEnds name)

parseAttrExists attrname = do
  char ']'
  return $ ByAttrExists attrname

parseAttrWith sign constructor name = do
  string sign
  value <- many $ noneOf ",#.]"
  char ']'
  return $ constructor name value

