{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Status as Status
import           Network.HTTP.Types.Status (status200)
import           Control.Exception.Base (bracket_)
import qualified Data.ByteString.Lazy.Char8 as C8LBS
import qualified Data.ByteString.Char8 as C8BS
import qualified Data.ByteString as DBS
import           Data.ByteString (ByteString, isInfixOf)
import qualified Data.Maybe as Maybe
import qualified Network.HTTP.Types.Header (HeaderName)
import qualified System.Console.ANSI as ANSI
import qualified Network.HTTP.Simple as HTTPs
import qualified Network.HTTP.Client as HTTPc
import qualified Network.HTTP.Client.TLS as HTTPtls
import           Data.Char (toUpper)
import           Control.Exception (try)
import qualified Data.Set as Set
import qualified System.Environment
import qualified Data.Text

data ReqRes = ReqRes Wai.Request C8BS.ByteString (HTTPc.Response C8LBS.ByteString)

instance Show ReqRes where
  show (ReqRes req body res) =
    unlines [
      "Request:"
    , "  Verb: " ++ x (Wai.requestMethod req)
    , "  Path:" ++ x (Wai.rawPathInfo req)
    , "  Headers:"
    , concatMap header2string (Wai.requestHeaders req)
    , "  Body:"
    , x body
    , ""
    , ""
    , "Response:"
    , "  Status: " ++ show (HTTPc.responseStatus res)
    , "  Headers: " ++ concatMap header2string (HTTPc.responseHeaders res)
    , "  Body:"
    , C8LBS.unpack (HTTPc.responseBody res)
    ]
    where x = C8BS.unpack
          header2string h = (show h) ++ ", \n"


main :: IO ()
main = Warp.run 8080 app

app :: Wai.Application
app req respond = (responding respond req)

allowedRequestHeaders :: Set.Set Network.HTTP.Types.Header.HeaderName
allowedRequestHeaders = Set.fromList [
  "Connection",
  "Content-length",
  "Content-type",
  "Depth",
  "Brief",
  "Accept",
  "Prefer",
  "User-agent",
  "Accept-language",
  "Accept-encoding"
  ]

disallowedResponseHeaders :: Set.Set Network.HTTP.Types.Header.HeaderName
disallowedResponseHeaders = Set.fromList [
  "Content-Encoding"
  ]

forwardedRequestHeaders :: (Network.HTTP.Types.Header.HeaderName,a) -> Bool
forwardedRequestHeaders (l,_) = Set.member l allowedRequestHeaders

forwardedResponseHeaders :: (Network.HTTP.Types.Header.HeaderName,a) -> Bool
forwardedResponseHeaders (l,_) = not $ Set.member l disallowedResponseHeaders

strip = Data.Text.unpack . Data.Text.strip . Data.Text.pack

gmailResponding responder req = do

  token <- readFile "gmail-access-token"
  let header = ("Authorization", C8BS.pack $ "Bearer " ++ (strip token))

  let method = Wai.requestMethod req
  let path = Wai.rawPathInfo req
  let headers = filter forwardedRequestHeaders $ Wai.requestHeaders req
  body <- (Wai.requestBody req)

  let gmailRequest =
          HTTPs.setRequestMethod method
        $ HTTPs.setRequestPath path
        $ HTTPs.setRequestIgnoreStatus
        $ HTTPs.setRequestSecure True
        $ HTTPs.setRequestHost "www.googleapis.com"
        $ HTTPs.setRequestPort 443
        $ HTTPs.setRequestQueryString []
        $ HTTPs.setRequestHeaders (header:headers)
        $ HTTPs.setRequestBodyLBS (C8LBS.fromStrict body)
        $ HTTPs.defaultRequest

  manager <- HTTPc.newManager HTTPtls.tlsManagerSettings
  gmailResponse <- HTTPc.httpLbs gmailRequest manager

  let responseCode = HTTPc.responseStatus gmailResponse
  let responseHeaders = filter forwardedResponseHeaders $ HTTPc.responseHeaders gmailResponse
  let responseBody = HTTPc.responseBody gmailResponse

  let response = Wai.responseLBS responseCode responseHeaders responseBody

  print $ ReqRes req body gmailResponse
  responder response




responding :: (Wai.Response -> IO a) -> Wai.Request -> IO a
responding responder req = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  putStrLn "\n\n"
  putStrLn "Request: "
  putStrLn $ show req
  ANSI.setSGR [ANSI.Reset]

  let method = C8BS.unpack $ Wai.requestMethod req
  let path = C8BS.unpack $ Wai.rawPathInfo req
  body <- (Wai.requestBody req)

  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
  putStrLn method
  putStrLn path
  putStrLn $ show body

  (code, headers, content) <- respond method path body

  let response = Wai.responseLBS code headers (C8LBS.pack content)

  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  putStrLn "\n\nResponse: "
  putStrLn $ show code
  putStrLn $ show headers
  putStrLn $ show content
  ANSI.setSGR [ANSI.Reset]

  responder response




type BS = DBS.ByteString
type LBS = C8LBS.ByteString

s404 = (Status.status404, [], "")
c200 r = (Status.status200, [], r)
h200 hs = (Status.status200, hs, "")

respond :: String -> String -> BS -> IO (Status.Status, [(Network.HTTP.Types.Header.HeaderName, ByteString)], String)
respond "PROPFIND" "/.well-known/carddav" body
 | isInfixOf "current-user-principal" body =
   return $ c200 response0
 | otherwise =
   return s404
respond "OPTIONS" "/principals/users/cdaboo/" _ =
   return $ h200 response1Headers
respond "PROPFIND" "/principals/users/cdaboo/" body
 | isInfixOf "addressbook-home-set" body =
   return $ c200 response2
 | otherwise =
   return s404

respond _ _ _ = return s404


xmlResponse str = unlines [
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
 , "<d:multistatus xmlns:d=\"DAV:\">"
 , "  <d:response>"
 , str
 , "  </d:response>"
 , "</d:multistatus>"
 ]

response0 = xmlResponse $ unlines [
   "<d:href>/carddav/v1/principals/spamdavtest@gmail.com/lists/default/</d:href>"
 , "<d:propstat>"
 , "  <d:status>HTTP/1.1 200 OK</d:status>"
 , "  <d:prop>"
 , "   <d:principal-URL>"
 , "    <d:href>/carddav/v1/principals/spamdavtest@gmail.com/</d:href>"
 , "   </d:principal-URL>"
 , "   <d:current-user-principal>"
 , "    <d:href>/carddav/v1/principals/spamdavtest@gmail.com</d:href>"
 , "   </d:current-user-principal>"
 , "   <d:resourcetype>"
 , "    <d:collection/>"
 , "    <card:addressbook/>"
 , "   </d:resourcetype>"
 , "  </d:prop>"
 , "</d:propstat>"
 ]

response1Headers :: [(Network.HTTP.Types.Header.HeaderName, ByteString)]
response1Headers = [
   ("Allow", "OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, COPY, MOVE")
 , ("Allow", "MKCOL, PROPFIND, PROPPATCH, LOCK, UNLOCK, REPORT, ACL")
 , ("DAV", "1, 2, 3, access-control, addressbook")
 , ("DAV", "extended-mkcol")
 ]

response2 = xmlResponse $ unlines [
   "<C:addressbook-home-set xmlns:D=\"DAV:\""
 , "        xmlns:C=\"urn:ietf:params:xml:ns:carddav\">"
 , "  <D:href>/bernard/addresses/</D:href>"
 , "</C:addressbook-home-set>"
 ]

--Verb, URL, HEADERS, BODY ->
