module Shell.Api (
  apiOptions,
  apiGet,
  apiGetParams,
  apiGetParamsList,
  apiPost,
  apiPostParams,
  apiPut,
  apiDelete
) where

import           Control.Monad

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types     hiding (Options)
import           Data.ByteString.Lazy
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Network.HTTP.Client  as C
import           Network.Wreq



lnUrl = "http://localhost:3000/"



apiOptions :: Options
-- apiOptions = defaults
apiOptions = defaults & header "Authorization" .~ ["1"]



apiGet url = go
  where
  handler e@(C.StatusCodeException s _ _) = print "fail"
  handler e = print "fail"
  go = do
    r <- getWith apiOptions (lnUrl <> url)
    return $ r ^. responseBody



apiGetParams url k v = go
  where
  handler e@(C.StatusCodeException s _ _) = print "fail"
  handler e = print "fail"
  go = do
    r <- getWith (apiOptions & param k .~ [v]) (lnUrl <> url)
    return $ r ^. responseBody



apiGetParamsList url kv = go
  where
  handler e@(C.StatusCodeException s _ _) = print "fail"
  handler e = print "fail"
  opts = Prelude.foldl (\acc (k, v) -> acc & param k .~ [v]) apiOptions kv
  go = do
    r <- getWith opts (lnUrl <> url)
    return $ r ^. responseBody



apiPost url body = go
  where
  handler e@(C.StatusCodeException s _ _) = print "fail"
  handler e = print "fail"
  go = do
    r <- postWith apiOptions (lnUrl <> url) (toJSON body)
    return $ r ^. responseBody



apiPostParams url body k v = go
  where
  handler e@(C.StatusCodeException s _ _) = print "fail"
  handler e = print "fail"
  go = do
    r <- postWith (apiOptions & param k .~ [v]) (lnUrl <> url) (toJSON body)
    return $ r ^. responseBody



apiPut url body = go
  where
  handler e@(C.StatusCodeException s _ _) = print "fail"
  handler e = print "fail"
  go = do
    r <- putWith apiOptions (lnUrl <> url) (toJSON body)
    return $ r ^. responseBody



apiDelete url = go
  where
  handler e@(C.StatusCodeException s _ _) = print "fail"
  handler e = print "fail"
  go = do
    deleteWith apiOptions (lnUrl <> url)
    return ()
