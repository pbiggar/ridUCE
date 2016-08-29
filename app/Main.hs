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

main :: IO ()
main = Warp.run 8080 app

app :: Wai.Application
app req respond = bracket_
    allocating
    cleaning
    (responding respond req)

allocating = return ()
cleaning = return ()

responding :: (Wai.Response -> IO a) -> Wai.Request -> IO a
responding responder req = do
  let method = C8BS.unpack $ Wai.requestMethod req
  let path = C8BS.unpack $ Wai.rawPathInfo req
  body <- (Wai.requestBody req)
  putStrLn "Starting: "
  putStrLn $ show req
  putStrLn method
  putStrLn path
  putStrLn $ show body

  (code, response) <- respond method path body
  putStrLn $ show code

  responder $ Wai.responseLBS code [] (C8LBS.pack response)

type BS = DBS.ByteString
type LBS = C8LBS.ByteString

respond :: String -> String -> BS -> IO (Status.Status, String)
respond "PROPFIND" "/" body
 | isInfixOf "current-user-principal" body = return (status200, response0)
 | otherwise = return (Status.status404, "")
respond _ _ _ = return (Status.status404, "")

response0 = unlines [
   "<d:multistatus xmlns:d=\"DAV:\">"
 , "  <d:response>"
 , "      <d:href>/</d:href>"
 , "      <d:propstat>"
 , "          <d:prop>"
 , "              <d:current-user-principal>"
 , "                  <d:href>/principals/users/spamdav/</d:href>"
 , "              </d:current-user-principal>"
 , "          </d:prop>"
 , "          <d:status>HTTP/1.1 200 OK</d:status>"
 , "      </d:propstat>"
 , "  </d:response>"
 , "</d:multistatus>"
 ]
