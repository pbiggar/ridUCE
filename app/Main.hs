{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Status as Status
import           Network.HTTP.Types.Status (status200)
import qualified Data.ByteString.Lazy.Char8 as C8LBS
import qualified Data.ByteString.Char8 as C8BS
import qualified Data.ByteString as DBS
import           Data.ByteString (ByteString, isInfixOf)
import qualified Data.Maybe as Maybe
import qualified Network.HTTP.Types.Header (HeaderName)
import qualified System.Console.ANSI as ANSI
import           Data.Char (toUpper)

main :: IO ()
main = Warp.run 8080 app

app :: Wai.Application
app req respond = (responding respond req)


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

respond "OPTIONS" "/carddav/v1/principals/spamdavtest@gmail.com/" _ =
   return $ h200 response1Headers

respond "PROPFIND" "/carddav/v1/principals/spamdavtest@gmail.com/" body
 | isInfixOf "addressbook-home-set" body =
   return $ c200 response2
 | otherwise =
   return s404

respond "PROPFIND" "/carddav/v1/principals/spamdavtest@gmail.com/lists/" _ =
  -- lots of things
  return $ c200 response3

respond "PROPFIND" "/carddav/v1/principals/spamdavtest@gmail.com/lists/default/" body
 | isInfixOf "getctag" body =
   -- getctag, sync-token
   return $ c200 response4

 | isInfixOf "getetag" body =
   -- just getetag
   return $ c200 response5

 | otherwise =
   return s404


respond "REPORT" "/carddav/v1/principals/spamdavtest@gmail.com/lists/default/" _
 = return $ c200 response6



respond _ _ _ = return s404



xml = (++) "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

multistatus str = xml $ unlines [
   "<d:multistatus xmlns:d=\"DAV:\" xmlns:cal=\"urn:ietf:params:xml:ns:caldav\" xmlns:card=\"urn:ietf:params:xml:ns:carddav\" xmlns:cs=\"http://calendarserver.org/ns/\" xmlns:d=\"DAV:\" xmlns:ical=\"http://apple.com/ns/ical/\">"
 , "  <d:response>"
 , str
 , "  </d:response>"
 , "</d:multistatus>"
 ]

response0 = multistatus $ unlines [
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
   ("Allow", "UNLOCK,MKCOL,PROPFIND,LOCK,PROPPATCH,OPTIONS,ACL,PUT,GET,REPORT,POST")
 , ("DAV", "1, 2, 3, access-control, addressbook")
 , ("DAV", "extended-mkcol")
 , ("Content-Type","text/plain")
 , ("Content-Length","0")
 ]

response2 = multistatus $ unlines [
   " <d:response>"
 , " <d:href>/carddav/v1/principals/spamdavtest@gmail.com/</d:href>"
 , " <d:propstat>"
 , "  <d:status>HTTP/1.1 200 OK</d:status>"
 , "  <d:prop>"
 , "   <d:displayname>Principal</d:displayname>"
 , "   <card:addressbook-home-set>"
 , "    <d:href>/carddav/v1/principals/spamdavtest@gmail.com/lists/</d:href>"
 , "   </card:addressbook-home-set>"
 , "   <d:principal-URL>"
 , "    <d:href>/carddav/v1/principals/spamdavtest@gmail.com/</d:href>"
 , "   </d:principal-URL>"
 , "   <d:principal-collection-set/>"
 , "  </d:prop>"
 , " </d:propstat>"
 , " <d:propstat>"
 , "  <d:status>HTTP/1.1 404 Not Found</d:status>"
 , "  <d:prop>"
 , "   <card:directory-gateway/>"
 , "   <cs:email-address-set/>"
 , "   <d:resource-id/>"
 , "  </d:prop>"
 , " </d:propstat>"
 , "</d:response>"
 ]

response3 = multistatus $ unlines [
   "<d:response>"
 , "<d:href>/carddav/v1/principals/spamdavtest@gmail.com/lists/</d:href>"
 , "<d:propstat>"
 , " <d:status>HTTP/1.1 200 OK</d:status>"
 , " <d:prop>"
 , "  <d:displayname>Homeset</d:displayname>"
 , "  <d:current-user-privilege-set>"
 , "   <d:privilege>"
 , "    <d:read/>"
 , "   </d:privilege>"
 , "   <d:privilege>"
 , "    <d:read-acl/>"
 , "   </d:privilege>"
 , "   <d:privilege>"
 , "    <d:read-current-user-privilege-set/>"
 , "   </d:privilege>"
 , "   <d:privilege>"
 , "    <d:unlock/>"
 , "   </d:privilege>"
 , "   <d:privilege>"
 , "    <d:write/>"
 , "   </d:privilege>"
 , "   <d:privilege>"
 , "    <d:write-acl/>"
 , "   </d:privilege>"
 , "  </d:current-user-privilege-set>"
 , "  <d:add-member>"
 , "   <d:href>/carddav/v1/principals/spamdavtest@gmail.com/lists/default/</d:href>"
 , "  </d:add-member>"
 , "  <d:supported-report-set>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <d:principal-search-property-set/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <card:addressbook-query/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <d:expand-property/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <d:principal-property-search/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <d:acl-principal-prop-set/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <card:addressbook-multiget/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <d:principal-match/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "  </d:supported-report-set>"
 , "  <d:resourcetype>"
 , "   <d:collection/>"
 , "  </d:resourcetype>"
 , " </d:prop>"
 , "</d:propstat>"
 , "<d:propstat>"
 , " <d:status>HTTP/1.1 404 Not Found</d:status>"
 , " <d:prop>"
 , "  <ns1:bulk-requests xmlns:ns1=\"http://me.com/_namespace/\"/>"
 , "  <card:max-image-size/>"
 , "  <card:max-resource-size/>"
 , "  <cs:me-card/>"
 , "  <d:owner/>"
 , "  <cs:push-transports/>"
 , "  <cs:pushkey/>"
 , "  <d:quota-available-bytes/>"
 , "  <d:quota-used-bytes/>"
 , "  <d:resource-id/>"
 , "  <d:sync-token/>"
 , " </d:prop>"
 , "</d:propstat>"
 , "</d:response>"
 , "<d:response>"
 , "<d:href>/carddav/v1/principals/spamdavtest@gmail.com/lists/default/</d:href>"
 , "<d:propstat>"
 , " <d:status>HTTP/1.1 200 OK</d:status>"
 , " <d:prop>"
 , "  <d:displayname>Address Book</d:displayname>"
 , "  <d:current-user-privilege-set>"
 , "   <d:privilege>"
 , "    <d:read/>"
 , "   </d:privilege>"
 , "   <d:privilege>"
 , "    <d:read-acl/>"
 , "   </d:privilege>"
 , "   <d:privilege>"
 , "    <d:read-current-user-privilege-set/>"
 , "   </d:privilege>"
 , "   <d:privilege>"
 , "    <d:unlock/>"
 , "   </d:privilege>"
 , "   <d:privilege>"
 , "    <d:write/>"
 , "   </d:privilege>"
 , "   <d:privilege>"
 , "    <d:write-acl/>"
 , "   </d:privilege>"
 , "  </d:current-user-privilege-set>"
 , "  <d:add-member>"
 , "   <d:href>/carddav/v1/principals/spamdavtest@gmail.com/lists/default/</d:href>"
 , "  </d:add-member>"
 , "  <d:supported-report-set>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <d:principal-search-property-set/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <card:addressbook-query/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <d:expand-property/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <d:principal-property-search/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <d:acl-principal-prop-set/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <card:addressbook-multiget/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <d:principal-match/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "   <d:supported-report>"
 , "    <d:report>"
 , "     <d:sync-collection/>"
 , "    </d:report>"
 , "   </d:supported-report>"
 , "  </d:supported-report-set>"
 , "  <d:resourcetype>"
 , "   <d:collection/>"
 , "   <card:addressbook/>"
 , "  </d:resourcetype>"
 , " </d:prop>"
 , "</d:propstat>"
 , "<d:propstat>"
 , " <d:status>HTTP/1.1 404 Not Found</d:status>"
 , " <d:prop>"
 , "  <ns2:bulk-requests xmlns:ns2=\"http://me.com/_namespace/\"/>"
 , "  <card:max-image-size/>"
 , "  <card:max-resource-size/>"
 , "  <cs:me-card/>"
 , "  <d:owner/>"
 , "  <cs:push-transports/>"
 , "  <cs:pushkey/>"
 , "  <d:quota-available-bytes/>"
 , "  <d:quota-used-bytes/>"
 , "  <d:resource-id/>"
 , "  <d:sync-token/>"
 , " </d:prop>"
 , "</d:propstat>"
 , "</d:response>"
 ]

response4 = multistatus $ unlines [
   "<d:response>"
 , " <d:href>/carddav/v1/principals/spamdavtest@gmail.com/lists/default/</d:href>"
 , " <d:propstat>"
 , "  <d:status>HTTP/1.1 200 OK</d:status>"
 , "  <d:prop>"
 , "   <d:sync-token>https://www.googleapis.com/carddav/v1/synctoken/0801100118F2C3CEC4FFEBCE02</d:sync-token>"
 , "   <cs:getctag>\"b518c1db61f1e286.1\"</cs:getctag>"
 , "  </d:prop>"
 , " </d:propstat>"
 , "</d:response>"
 ]

response5 = multistatus $ unlines [
   "<d:response>"
 , "<d:href>/carddav/v1/principals/spamdavtest@gmail.com/lists/default/</d:href>"
 , "<d:propstat>"
 , " <d:status>HTTP/1.1 200 OK</d:status>"
 , "</d:propstat>"
 , "<d:propstat>"
 , " <d:status>HTTP/1.1 404 Not Found</d:status>"
 , " <d:prop>"
 , "  <d:getetag/>"
 , " </d:prop>"
 , "</d:propstat>"
 , "</d:response>"
 , "<d:response>"
 , "<d:href>/carddav/v1/principals/spamdavtest@gmail.com/lists/default/430baa120dd56282</d:href>"
 , "<d:propstat>"
 , " <d:status>HTTP/1.1 200 OK</d:status>"
 , " <d:prop>"
 , "  <d:getetag>\"2016-08-31T06:48:15.818-07:00\"</d:getetag>"
 , " </d:prop>"
 , "</d:propstat>"
 , "</d:response>"
 ]

response6 = multistatus $ unlines [
   " <d:response>"
 , "  <d:href>/carddav/v1/principals/spamdavtest@gmail.com/lists/default/430baa120dd56282</d:href>"
 , "  <d:propstat>"
 , "   <d:status>HTTP/1.1 200 OK</d:status>"
 , "   <d:prop>"
 , "    <d:getetag>\"2016-08-31T06:48:15.818-07:00\"</d:getetag>"
 , "    <card:address-data>BEGIN:VCARD"
 , "VERSION:3.0"
 , "N:Daly;Johnny;;;"
 , "FN:Johnny Daly"
 , "NICKNAME:Johno"
 , "ORG:Fake Company Inc"
 , "REV:2016-08-31T13:48:15Z"
 , "UID:430baa120dd56282"
 , "item2.TEL:415 123 1234"
 , "item1.EMAIL;TYPE=PREF:john@daly.com"
 , "item1.X-ABLabel:"
 , "item2.X-ABLabel:"
 , "END:VCARD"
 , "</card:address-data>"
 , "   </d:prop>"
 , "  </d:propstat>"
 , " </d:response>"
 ]
