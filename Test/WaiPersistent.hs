{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-|
Test.WaiPersistent is a pragmatic framework for testing web applications built
using wai and persistent.

By pragmatic I may also mean 'dirty'. It's main goal is to encourage integration
and system testing of web applications by making everything /easy to test/. 

Your tests are like browser sessions that keep track of cookies and the last
visited page. You can perform assertions on the content of HTML responses,
using css selectors to explore the document more easily.

You can also easily build requests using forms present in the current page.
This is very useful for testing web applications built in yesod for example,
were your forms may have field names generated by the framework or a randomly
generated '_nonce' field.

Your database is also directly available so you can use runDB to set up
backend pre-conditions, or to assert that your session is having the desired effect.

This is the helloworld and kitchen sink. In this case for testing a yesod app.
  
>  import Yesod
>  import Yesod.Static
>  import qualified MySite.Settings as Settings
>  import MySite.Models
>
>  main :: IO a
>  main = do
>    cfg <- (loadConfig Test) >>= either fail return
>    st <-  static Settings.staticDir
>    Settings.withConnectionPool (connStr cfg) $ \cnPool -> do
>      -- ... Perhaps some code here to truncate your test database?
>      app <- toWaiApp $ S4M st cfg 
>      runTests app cnPool $ mySuite
>
>  mySuite = do
>    describe "Basic navigation and assertions" $ do
>      it "Gets a page that has a form, with auto generated fields and nonce" $ do
>        doGet_ "url/of/page/with/form" -- Load a page
>        statusIs 200 -- Assert the status was success
>
>        bodyContains "Hello Person" -- Assert any part of the document contains some text.
>        
>        -- Perform css queries and assertions.
>        htmlCount "form .main" 1 -- It matches 1 element
>        htmlAllContain "h1#mainTitle" "Sign Up Now!" -- All matches have some text
>
>        -- Performs the post using the current page to extract field values:
>        doPost "url/to/post/to" $ do
>          addNonce -- Add the _nonce field with the currently shown value
>
>          -- Lookup field by the text on the labels pointing to them.
>          byLabel "Email:" "gustavo@cerati.com"
>          byLabel "Password:" "secret"
>          byLabel "Confirm:" "secret"
>
>      it "Sends another form, this one has a file" $ do
>        doPost "url/to/post/file/to" $ do
>          -- You can add files this easy, you still have to provide the mime type manually though.
>          addFile "file_field_name" "path/to/local/file" "image/jpeg"
>          
>          -- And of course you can add any field if you know it's name
>          byName "answer" "42"
>
>        statusIs 302
>
>    describe "Db access, still very raw" $ do
>      it "rubs the lotion on it's skin or else it gets the hose again" $ do
>        msgs <- testDB $ do (selectList [] [] :: SqlPersist IO [(Key SqlPersist Message, Message)])
>        assertEqual "One Message in the DB" 1 (DL.length msgs)

-}

module Test.WaiPersistent (
  -- * Declaring and running your test suite
  runTests, describe, it,

  -- * Making requests
  -- | To make a request you need to point to an url and pass in some parameters.
  --
  -- To build your parameters you will use the RequestBuilder monad that lets you
  -- add values, add files, lookup fields by label and find the current
  -- nonce value and add it to your request too.
  doPost, doPost_, doGet, doGet_, doRequest,
  byName, byLabel, addFile, addNonce, addNonce_,

  -- * Running database queries
  testDB,

  -- * Assertions
  assertEqual, statusIs, bodyContains, htmlAllContain, htmlCount,

  -- * Utils for debugging tests
  printBody, printMatches,

  -- * Utils for building your own assertions
  -- | Please consider generalizing and contributing the assertions you write.
  htmlQuery, parseHTML

)

where

import qualified Test.Hspec.Core as Core
import qualified Test.Hspec.Runner as Runner
import qualified Data.List as DL
import qualified Data.Maybe as DY
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Test.HUnit as HUnit
import qualified Test.Hspec.HUnit ()
import qualified Network.HTTP.Types as H
import qualified Network.Socket.Internal as Sock
import qualified Data.Ascii as Ascii
import Text.XML.HXT.Core
import Network.Wai
import Network.Wai.Test
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.State (get, put, execStateT, StateT)
import "monads-tf" Control.Monad.Trans
import System.IO
import Text.XML.HXT.TransversingCSS
import Database.Persist.GenericSql
import System.FilePath

-- | The state used in 'describe' to build a list of specs
data SpecsData = SpecsData
  { specsApp :: Application
  , specsConn :: ConnectionPool
  , specsList :: [Core.Spec]
  }

-- | The specs state monad is where 'describe' runs.
type Specs = StateT SpecsData IO ()

-- | The state used in a single test case defined using 'it'
data OneSpecData = OneSpecData 
  { oneSpecApp :: Application
  , oneSpecConn :: ConnectionPool
  , oneSpecCookie :: CookieValue
  , oneSpecResponse :: Maybe SResponse
  }

-- | The OneSpec state monad is where 'it' runs.
type OneSpec = StateT OneSpecData IO

data RequestBuilderData = RequestBuilderData
  { builderParts :: [RequestPart]
  , builderLastResponse :: Maybe SResponse
  }

-- | Request parts let us discern regular key/values from files sent in the request.
data RequestPart
  = ReqPlainPart String String
  | ReqFilePart String FilePath BSL8.ByteString String

-- | The RequestBuilder state monad constructs an url encoded string of arguments
-- to send with your requests. Some of the functions that run on it use the current
-- response to analize the forms that the server is expecting to receive.
type RequestBuilder = StateT RequestBuilderData IO

-- | Both the OneSpec and RequestBuilder monads hold a response that can be analized,
-- by making them instances of this class we can have general methods that work on
-- the last received response.
class HoldsResponse a where
  readResponse :: a -> Maybe SResponse
instance HoldsResponse OneSpecData where
  readResponse = oneSpecResponse
instance HoldsResponse RequestBuilderData where
  readResponse = builderLastResponse
  
type CookieValue = H.Ascii

-- | Runs your test suite, using you wai 'Application' and 'ConnectionPool' for performing 
-- the database queries in your tests.
--
-- You application may already have your connection pool but you need to pass another one
-- separately here.
-- 
-- Look at the examples directory on this package to get an idea of the (small) amount of
-- boilerplate code you'll need to write before calling this.
runTests :: Application -> ConnectionPool -> Specs -> IO a
runTests app connection specs = do
  (SpecsData _ _ specs) <- execStateT specs (SpecsData app connection [])
  Runner.hspecX specs

-- | Start describing a Tests suite keeping cookies and a reference to the tested 'Application'
-- and 'ConnectionPool'
describe :: String -> Specs -> Specs
describe label action = do
  sData@(SpecsData app conn specs) <- get
  SpecsData app conn specs <- liftIO $ execStateT action sData
  put $ SpecsData app conn (Core.describe label [specs])

-- | Describe a single test that keeps cookies, and a reference to the last response.
it :: String -> OneSpec () -> Specs
it label action = do
  SpecsData app conn specs <- get
  let spec = Core.it label $ do
        execStateT action $ OneSpecData app conn "" Nothing
        return ()
  put $ SpecsData app conn (specs++spec)

-- Performs a given action using the last response.
withResponse :: HoldsResponse a => b -> (SResponse -> StateT a IO b) -> StateT a IO b
withResponse e f = maybe err f =<< fmap readResponse get
 where
  err = do
    liftIO $ HUnit.assertFailure "There was no response, you should make a request"
    return e

-- | Use HXT to parse a value from an html tag.
-- Check for usage examples in this module's source.
parseHTML :: String -> LA XmlTree a -> [a]
parseHTML html p = runLA (hread >>> p ) html

-- | Query the last response using css selectors, returns a list of matched fragments
htmlQuery :: HoldsResponse a => Query -> StateT a IO [Html]
htmlQuery query = withResponse [] $ \ res ->
  case findBySelector (BSL8.unpack $ simpleBody res) query of
    Left err -> do 
      liftIO $ HUnit.assertFailure $ query ++ " did not parse: " ++ (show err)
      return []
    Right matches -> return matches

-- | Asserts that the two given values are equal.
assertEqual :: (Eq a) => String -> a -> a -> OneSpec ()
assertEqual msg a b = liftIO $ HUnit.assertBool msg (a == b)

-- | Assert the last response status is as expected.
statusIs :: HoldsResponse a => Int -> StateT a IO ()
statusIs number = withResponse () $ \ SResponse { simpleStatus = s } ->
  liftIO $ flip HUnit.assertBool (H.statusCode s == number) $ concat
    [ "Expected status was ", show number
    , " but received status was ", show $ H.statusCode s
    ]

-- | Assert the last response has the given text. The check is performed using the response
-- body in full text form.
bodyContains :: HoldsResponse a => String -> StateT a IO ()
bodyContains txt = withResponse () $ \ res ->
  liftIO $ HUnit.assertBool ("Expected body to contain " ++ txt) $ (simpleBody res) `contains` txt
contains :: BSL8.ByteString -> String -> Bool
contains a b = DL.isInfixOf b (BSL8.unpack a)

-- | Queries the html using a css selector, and all matched elements must contain
-- the given string.
htmlAllContain :: HoldsResponse a => Query -> String -> StateT a IO ()
htmlAllContain query search = do
  matches <- htmlQuery query
  case matches of
    [] -> liftIO $ HUnit.assertFailure $ "Nothing matched css query: "++query
    _ -> liftIO $ HUnit.assertBool ("Not all "++query++" contain "++search) $
          DL.all (DL.isInfixOf search) matches

-- | Performs a css query on the last response and asserts the matched elements
-- are as many as expected.
htmlCount :: HoldsResponse a => Query -> Int -> StateT a IO ()
htmlCount query count = do
  matches <- fmap DL.length $ htmlQuery query
  liftIO $ flip HUnit.assertBool (matches == count)
    ("Expected "++(show count)++" elements to match "++query++", found "++(show matches))

-- | Outputs the last response body to stderr (So it doesn't get captured by HSpec)
printBody :: HoldsResponse a => StateT a IO ()
printBody = withResponse () $ \ SResponse { simpleBody = b } -> 
  liftIO $ hPutStrLn stderr $ BSL8.unpack b

-- | Performs a CSS query and print the matches to stderr.
printMatches :: HoldsResponse a => Query -> StateT a IO ()
printMatches query = do
  matches <- htmlQuery query
  liftIO $ hPutStrLn stderr $ show matches

-- | Add a parameter with the given name and value.
byName :: String -> String -> RequestBuilder ()
byName name value = do
  RequestBuilderData parts r <- get
  put $ RequestBuilderData ((ReqPlainPart name value):parts) r

-- | Add a file to be posted with the current request
--
-- Adding a file will automatically change your request content-type to be multipart/form-data
addFile :: String -> FilePath -> String -> RequestBuilder ()
addFile name path mimetype = do
  RequestBuilderData parts r <- get
  contents <- liftIO $ BSL8.readFile path
  put $ RequestBuilderData ((ReqFilePart name path contents mimetype):parts) r

-- | Some frameworks like Yesod cat auto generate field ids, so you are never sure what
-- the argument name should be for each one of your args when constructing
-- your requests. What you do know is the /label/ of the field. This looks up a label
-- and adds a parameter for the field name that label is pointing to.
--
-- If the label or field it points to are not found its treated as a faild Hspec assertion.
byLabel :: String -> String -> RequestBuilder ()
byLabel label value = withResponse () $ \ res -> do
  let
    body = BSL8.unpack $ simpleBody res
    mfor = parseHTML body $ deep $
      hasName "label" >>> filterA (getChildren >>> hasText (DL.isInfixOf label)) >>> getAttrValue "for"

  case mfor of
    for:[] -> do
      let mname = parseHTML body $ deep $ hasAttrValue "id" (==for) >>> getAttrValue "name"
      case mname of
        "":_ -> liftIO $ HUnit.assertFailure $
          "Label "++label++" resolved to id "++for++" which was not found. "
        name:_ -> byName name value
        _ -> liftIO $ HUnit.assertFailure $ "More than one input with id " ++ for
    [] -> liftIO $ HUnit.assertFailure $ "No label contained: "++label
    _ -> liftIO $ HUnit.assertFailure $ "More than one label contained "++label

-- | Useful for yesod testing: Lookup a _nonce form field and add it's value to the params
-- being built. Receives a selector that should point to the form containing the desired nonce.
addNonce_ :: String -> RequestBuilder ()
addNonce_ scope = do
  matches <- htmlQuery $ scope ++ "input[name=_nonce][type=hidden][value]"
  case matches of
    [] -> liftIO $ HUnit.assertFailure $ "No nonce found in the current page"
    element:[] -> byName "_nonce" $ head $ parseHTML element $ getAttrValue "value"
    _ -> liftIO $ HUnit.assertFailure $ "More than one nonce found in the page"

-- | For responses that display a single form, lookup the current Nonce on the page and
-- add it to the params being built
addNonce :: RequestBuilder ()
addNonce = addNonce_ ""

-- | Perform a POST request to url, using params
doPost :: BS8.ByteString -> RequestBuilder () -> OneSpec ()
doPost url paramsBuild = do
  doRequest "POST" url paramsBuild

-- | Perform a POST request without params
doPost_ :: BS8.ByteString -> OneSpec ()
doPost_ = flip doPost $ return ()
 
-- | Perform a GET request to url, using params
doGet :: BS8.ByteString -> RequestBuilder () -> OneSpec ()
doGet url paramsBuild = doRequest "GET" url paramsBuild

-- | Perform a GET request without params
doGet_ :: BS8.ByteString -> OneSpec ()
doGet_ = flip doGet $ return ()

-- | General interface to performing requests, letting you specify the request method and extra headers.
doRequest :: H.Method -> BS8.ByteString -> RequestBuilder a -> OneSpec ()
doRequest method url paramsBuild = do
  OneSpecData app conn cookie mRes <- get
  RequestBuilderData parts _ <- liftIO $ execStateT paramsBuild $ RequestBuilderData [] mRes
  let request = if DL.any isFile parts
          then makeMultipart cookie parts
          else makeSinglepart cookie parts

  response <- liftIO $ runSession (srequest request) app
  let cookie' = DY.fromMaybe cookie $ fmap snd $ DL.find (("Set-Cookie"==) . fst) $ simpleHeaders response
  put $ OneSpecData app conn cookie' (Just response)
 where
  isFile (ReqFilePart _ _ _ _) = True
  isFile _ = False

  -- For building the multi-part requests
  boundary :: String
  boundary = "*******noneedtomakethisrandom"
  separator = BS8.concat ["--", BS8.pack boundary, "\r\n"]
  makeMultipart cookie parts =
    flip SRequest (BSL8.fromChunks [multiPartBody parts]) $ mkRequest
      [ ("Cookie", cookie)
      , ("Content-Type", BS8.pack $ "multipart/form-data; boundary=" ++ boundary)]
  multiPartBody parts = BS8.concat $ separator : [BS8.concat [multipartPart p, separator] | p <- parts]
  multipartPart (ReqPlainPart k v) = BS8.concat
    [ "Content-Disposition: form-data; "
    , "name=\"", (BS8.pack k), "\"\r\n\r\n"
    , (BS8.pack v), "\r\n"]
  multipartPart (ReqFilePart k v bytes mime) = BS8.concat
    [ "Content-Disposition: form-data; "
    , "name=\"", BS8.pack k, "\"; "
    , "filename=\"", BS8.pack v, "\"\r\n"
    , "Content-Type: ", BS8.pack mime, "\r\n\r\n"
    , BS8.concat $ BSL8.toChunks bytes, "\r\n"]

  unsafeStringToByteString :: String -> BSL8.ByteString 
  unsafeStringToByteString = BSL8.fromChunks . (: []) . Ascii.toByteString . Ascii.unsafeFromString

  -- For building the regular non-multipart requests
  makeSinglepart cookie parts =
    SRequest (mkRequest [("Cookie",cookie), ("Content-Type", "application/x-www-form-urlencoded")]) $
      BSL8.pack $ DL.concat $ DL.intersperse "&" $ map singlepartPart parts
  singlepartPart (ReqFilePart _ _ _ _) = ""
  singlepartPart (ReqPlainPart k v) = concat [k,"=",v]

  -- General request making 
  mkRequest headers = defaultRequest
    { requestMethod = method
    , remoteHost = Sock.SockAddrInet 1 2
    , requestHeaders = headers
    , rawPathInfo = url
    , pathInfo = T.split (== '/') $ TE.decodeUtf8 url
    }
  
-- | Run a persistent db query. For asserting on the results of performed actions
-- or setting up pre-conditions. At the moment this part is still very raw.
testDB :: SqlPersist IO a -> OneSpec a
testDB query = do
  OneSpecData _ pool _ _ <- get
  liftIO $ runSqlPool query pool
