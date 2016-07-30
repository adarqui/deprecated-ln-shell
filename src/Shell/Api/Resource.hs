{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Shell.Api.Resource (
  apiResourceSubshell
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Shell.Api
import           Shell.Types

import           System.Console.Shell
import           System.Console.Shell.Backend.Haskeline
import           System.Console.Shell.ShellMonad

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Monoid                            ((<>))
import qualified Data.Text                              as T
import qualified Network.HTTP.Client                    as C
import           Network.Wreq

import           LN.T.Count
import           LN.T.Leuron
import           LN.T.Resource
import           LN.T.Visibility

import           Text.PrettyPrint.GenericPretty


defaultBackend = haskelineBackend



data ApiResourceState = ApiResourceState {
  resourceId :: Maybe Integer,
  resource   :: ResourceRequest
} deriving (Eq, Ord, Show, Read, Generic)

instance Out ApiResourceState



initialApiResourceState :: ApiResourceState
initialApiResourceState =
  ApiResourceState {
    resourceId = Nothing,
    resource   = defaultResourceRequest
  }



apiResourceDesc :: ShellDescription ApiResourceState
apiResourceDesc = initialShellDescription
  { prompt         = \_ -> return ">> "
  , commandStyle   = OnlyCommands
  , shellCommands  = apiResourceCommands
--  , historyEnabled = False
  , historyFile    = Just "/tmp/api.resources.hist"
  , historyEnabled = True
  }



apiResourceShell :: ApiResourceState -> IO ApiResourceState
apiResourceShell st = runShell apiResourceDesc defaultBackend st



apiResourceSubshell :: IO (Subshell ShellState ApiResourceState)
apiResourceSubshell = simpleSubshell (const $ return initialApiResourceState) apiResourceDesc



apiResourceCommands :: [ShellCommand ApiResourceState]
apiResourceCommands =
  [ exitCommand "q"
  , exitCommand "quit"
  , helpCommand "h"
  , helpCommand "help"
  , cmd "gets" _gets "Get all Resource records"
  , cmd "get"  _get  "Get a Resource record"
  , cmd "post" _post "Create a Resource record"
  , cmd "put"  _put  "Modify a Resource record"
  , cmd "del"  _del  "Delete a Resource record"
  , cmd "ls" listResources "List Resources"
  , cmd "cd" setResource "Set Resource"
  , cmd "title" setTitle "Set Title"
  , cmd "desc" setDesc "Set Description"
  , cmd "public" setPublic "Set Public"
  , cmd "private" setPrivate "Set Private"
  , cmd "url" setURL "Set URL"
  , cmd "isbn" setISBN "Set ISBN"
  , cmd "isbn10" setISBN10 "Set ISBN10"
  , cmd "isbn13" setISBN13 "Set ISBN13"
  , cmd "new" _post "Create a resource"
  , cmd "save" _put "Save current resource"
  , cmd "count" countResources "Return the resource count"
  , cmd "st" state "Print state"
  ]



evaluate :: String -> Sh ApiResourceState ()
evaluate s = do
  return ()



_gets :: Sh ApiResourceState ()
_gets = do
  resources <- liftIO $ apiGet "resources"
  liftIO $ pp (eitherDecode resources :: Either String ResourceResponses)



_get :: Integer -> Sh ApiResourceState ()
_get resource_id = do
  r <- liftIO $ apiGet ("resources" <> "/" <> show resource_id)
  let mresource = decode r :: Maybe ResourceRequest
  case mresource of
    Nothing -> shellPutStrLn "Error"
    Just resource@ResourceRequest{..} -> do
      modifyShellSt (\st -> st { resourceId = Just resource_id, resource = resource })



_post :: String -> String -> Sh ApiResourceState ()
_post title desc = do
  modifyShellSt (\st -> st {
    resource = (resource st) { resourceRequestTitle = T.pack title, resourceRequestDescription = T.pack desc }
  })
  st <- getShellSt
  resource <- liftIO $ apiPost "resources" (resource st)
  liftIO $ pp (decode resource :: Maybe ResourceResponse)



_put :: Sh ApiResourceState ()
_put = do
  st <- getShellSt
  case (resourceId st) of
    Nothing -> shellPutStrLn "No resource"
    Just resource_id -> do
      void $ liftIO $ apiPut ("resources/" <> show (resource_id)) (resource st)



_del :: Integer -> Sh ApiResourceState ()
_del tid = do
  void $ liftIO $ apiDelete ("resources" <> "/" <> show tid)
  shellPutStrLn "deleted"



listResources :: Sh ApiResourceState ()
listResources = do
  r <- liftIO $ apiGet "resources"
  let resources = decode r :: Maybe ResourceResponses
  case resources of
    Nothing -> return ()
    Just responses -> do
      mapM_ (\resource -> do
        shellPutStrLn $
          show (resourceResponseId resource) <> ": " <> (T.unpack $ resourceResponseTitle resource))
        (resourceResponses responses)



setResource :: Integer -> Sh ApiResourceState ()
setResource = _get



setTitle :: String -> Sh ApiResourceState ()
setTitle title = do
  modifyShellSt (\st -> st { resource = (resource st) { resourceRequestTitle = T.pack title }})



setDesc :: String -> Sh ApiResourceState ()
setDesc desc = do
  modifyShellSt (\st -> st { resource = (resource st) { resourceRequestDescription = T.pack desc }})



setPublic :: Sh ApiResourceState ()
setPublic = do
  modifyShellSt (\st -> st { resource = (resource st) { resourceRequestVisibility = Public }})



setPrivate :: Sh ApiResourceState ()
setPrivate = do
  modifyShellSt (\st -> st { resource = (resource st) { resourceRequestVisibility = Private }})



setURL :: String -> Sh ApiResourceState ()
setURL url = do
  modifyShellSt (\st -> st { resource = (resource st) { resourceRequestSource = URL (T.pack url) }})



setISBN :: String -> Sh ApiResourceState ()
setISBN isbn = do
  modifyShellSt (\st -> st { resource = (resource st) { resourceRequestSource = ISBN (T.pack isbn) }})



setISBN10 :: String -> Sh ApiResourceState ()
setISBN10 isbn10 = do
  modifyShellSt (\st -> st { resource = (resource st) { resourceRequestSource = URL (T.pack isbn10) }})


setISBN13 :: String -> Sh ApiResourceState ()
setISBN13 isbn13 = do
  modifyShellSt (\st -> st { resource = (resource st) { resourceRequestSource = URL (T.pack isbn13) }})



countResources :: Sh ApiResourceState ()
countResources = do
  r <- liftIO $ apiGet "resources/other/count"
  case (eitherDecode r :: Either String CountResponse) of
    Left err -> shellPutStrLn err
    Right count -> liftIO $ pp count



state :: Sh ApiResourceState ()
state = do
  st <- getShellSt
  liftIO $ pp st



runApiResource :: IO ()
runApiResource = void $ apiResourceShell initialApiResourceState
