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
import qualified Text.XML.HaXml.ParseLazy as XMLParse
import qualified Text.XML.HaXml.Types as XML
import qualified Network.HTTP.Types.Header (HeaderName)
import qualified System.Console.ANSI as ANSI

main :: IO ()
main = do
  putStrLn "starting"
  Warp.run 8080 app

app :: Wai.Application
app req respond = bracket_
    allocating
    cleaning
    (responding respond req)

allocating = return ()
cleaning = return ()

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
c200 r = (Status.status200, [], response0)
h200 hs = (Status.status200, hs, "")

respond :: String -> String -> BS -> IO (Status.Status, [(Network.HTTP.Types.Header.HeaderName, ByteString)], String)
respond "PROPFIND" "/" body
 | isInfixOf "current-user-principal" body =
   return $ c200 response0
 | otherwise =
   return s404
respond "OPTIONS" "/principals/users/cdaboo/" _ =
   return $ h200 response1Headers
respond "PROPFIND" "/principals/users/cdaboo" body
 | isInfixOf "addressbook-home-set" body =
   return $ c200 response2
 | otherwise =
   return s404

respond _ _ _ = return s404



response0 = unlines [
   "<d:multistatus xmlns:d=\"DAV:\">"
 , "  <d:response>"
 , "      <d:href>/</d:href>"
 , "      <d:propstat>"
 , "          <d:prop>"
 , "              <d:current-user-principal>"
 , "                  <d:href>/principals/users/cdaboo/</d:href>"
 , "              </d:current-user-principal>"
 , "          </d:prop>"
 , "          <d:status>HTTP/1.1 200 OK</d:status>"
 , "      </d:propstat>"
 , "  </d:response>"
 , "</d:multistatus>"
 ]

response1Headers :: [(Network.HTTP.Types.Header.HeaderName, ByteString)]
response1Headers = [
   ("Allow", "OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, COPY, MOVE")
 , ("Allow", "MKCOL, PROPFIND, PROPPATCH, LOCK, UNLOCK, REPORT, ACL")
 , ("DAV", "1, 2, 3, access-control, addressbook")
 , ("DAV", "extended-mkcol")
 ]


response2 = unlines [
   "<C:addressbook-home-set xmlns:D=\"DAV:\""
 , "        xmlns:C=\"urn:ietf:params:xml:ns:carddav\">"
 , "       <D:href>/bernard/addresses/</D:href>"
 , "     </C:addressbook-home-set>"
 ]
