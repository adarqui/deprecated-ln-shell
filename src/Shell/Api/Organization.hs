{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Shell.Api.Organization (
  apiOrganizationSubshell
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
import           LN.T.Organization
import           LN.T.Visibility

import           Text.PrettyPrint.GenericPretty


defaultBackend = haskelineBackend



data ApiOrganizationState = ApiOrganizationState {
  organizationId :: Maybe Integer,
  organization   :: OrganizationRequest
} deriving (Eq, Ord, Show, Read, Generic)

instance Out ApiOrganizationState



initialApiOrganizationState :: ApiOrganizationState
initialApiOrganizationState =
  ApiOrganizationState {
    organizationId = Nothing,
    organization   = defaultOrganizationRequest
  }



apiOrganizationDesc :: ShellDescription ApiOrganizationState
apiOrganizationDesc = initialShellDescription
  { prompt         = \_ -> return ">> "
  , commandStyle   = OnlyCommands
  , shellCommands  = apiOrganizationCommands
--  , historyEnabled = False
  , historyFile    = Just "/tmp/api.organizations.hist"
  , historyEnabled = True
  }



apiOrganizationShell :: ApiOrganizationState -> IO ApiOrganizationState
apiOrganizationShell st = runShell apiOrganizationDesc defaultBackend st



apiOrganizationSubshell :: IO (Subshell ShellState ApiOrganizationState)
apiOrganizationSubshell = simpleSubshell (const $ return initialApiOrganizationState) apiOrganizationDesc



apiOrganizationCommands :: [ShellCommand ApiOrganizationState]
apiOrganizationCommands =
  [ exitCommand "q"
  , exitCommand "quit"
  , helpCommand "h"
  , helpCommand "help"
  , cmd "gets" _gets "Get all Organization records"
  , cmd "get"  _get  "Get a Organization record"
  , cmd "post" _post "Create a Organization record"
  , cmd "put"  _put  "Modify a Organization record"
  , cmd "del"  _del  "Delete a Organization record"
  , cmd "ls" listOrganizations "List Organizations"
  , cmd "cd" setOrganization "Set Organization"
  , cmd "name" setName "Set Name"
  , cmd "desc" setDesc "Set Description"
  , cmd "nodesc" noDesc "No Description"
  , cmd "company" setCompany "Set Company"
  , cmd "location" setLocation "Set Location"
  , cmd "email" setEmail "Set Email"
  , cmd "billing" setBillingEmail "Set Billing Email"
  , cmd "new" _post "Create a organization"
  , cmd "save" _put "Save current organization"
  , cmd "count" countOrganizations "Return the organization count"
  , cmd "st" state "Print state"
  ]



evaluate :: String -> Sh ApiOrganizationState ()
evaluate s = do
  return ()



_gets :: Sh ApiOrganizationState ()
_gets = do
  organizations <- liftIO $ apiGet "organizations"
  liftIO $ pp (eitherDecode organizations :: Either String OrganizationResponses)



_get :: Integer -> Sh ApiOrganizationState ()
_get organization_id = do
  r <- liftIO $ apiGet ("orgs" <> "/" <> show organization_id)
  let morganization = decode r :: Maybe OrganizationRequest
  case morganization of
    Nothing -> shellPutStrLn "Error"
    Just organization@OrganizationRequest{..} -> do
      modifyShellSt (\st -> st { organizationId = Just organization_id, organization = organization })



_post :: String -> Sh ApiOrganizationState ()
_post name = do
  modifyShellSt (\st -> st {
    organization = (organization st) { organizationRequestName = T.pack name }
  })
  st <- getShellSt
  organization <- liftIO $ apiPost "orgs" (organization st)
  liftIO $ pp (decode organization :: Maybe OrganizationResponse)



_put :: Sh ApiOrganizationState ()
_put = do
  st <- getShellSt
  case (organizationId st) of
    Nothing -> shellPutStrLn "No organization"
    Just organization_id -> do
      void $ liftIO $ apiPut ("orgs/" <> show (organization_id)) (organization st)



_del :: Integer -> Sh ApiOrganizationState ()
_del tid = do
  void $ liftIO $ apiDelete ("orgs" <> "/" <> show tid)
  shellPutStrLn "deleted"



listOrganizations :: Sh ApiOrganizationState ()
listOrganizations = do
  r <- liftIO $ apiGet "orgs"
  let organizations = decode r :: Maybe OrganizationResponses
  case organizations of
    Nothing -> return ()
    Just responses -> do
      mapM_ (\organization -> do
        shellPutStrLn $
          show (organizationResponseId organization) <> ": " <> (T.unpack $ organizationResponseName organization))
        (organizationResponses responses)



setOrganization :: Integer -> Sh ApiOrganizationState ()
setOrganization = _get



setName :: String -> Sh ApiOrganizationState ()
setName name = do
  modifyShellSt (\st -> st { organization = (organization st) { organizationRequestName = T.pack name }})



setDesc :: String -> Sh ApiOrganizationState ()
setDesc desc = do
  modifyShellSt (\st -> st { organization = (organization st) { organizationRequestDescription = Just $ T.pack desc }})



noDesc :: Sh ApiOrganizationState ()
noDesc = do
  modifyShellSt (\st -> st { organization = (organization st) { organizationRequestDescription = Nothing }})



setCompany :: String -> Sh ApiOrganizationState ()
setCompany company = do
  modifyShellSt (\st -> st { organization = (organization st) { organizationRequestCompany = T.pack company }})



setLocation :: String -> Sh ApiOrganizationState ()
setLocation location = do
  modifyShellSt (\st -> st { organization = (organization st) { organizationRequestLocation = T.pack location }})



setEmail :: String -> Sh ApiOrganizationState ()
setEmail email = do
  modifyShellSt (\st -> st { organization = (organization st) { organizationRequestEmail = T.pack email }})



setBillingEmail :: String -> Sh ApiOrganizationState ()
setBillingEmail billing_email = do
  modifyShellSt (\st -> st { organization = (organization st) { organizationRequestBillingEmail = T.pack billing_email }})



-- setPublic :: Sh ApiOrganizationState ()
-- setPublic = do
--   modifyShellSt (\st -> st { organization = (organization st) { organizationRequestVisibility = Public }})



-- setPrivate :: Sh ApiOrganizationState ()
-- setPrivate = do
--   modifyShellSt (\st -> st { organization = (organization st) { organizationRequestVisibility = Private }})



countOrganizations :: Sh ApiOrganizationState ()
countOrganizations = do
  r <- liftIO $ apiGet "orgs/other/count"
  case (eitherDecode r :: Either String CountResponse) of
    Left err -> shellPutStrLn err
    Right count -> liftIO $ pp count



state :: Sh ApiOrganizationState ()
state = do
  st <- getShellSt
  liftIO $ pp st



runApiOrganization :: IO ()
runApiOrganization = void $ apiOrganizationShell initialApiOrganizationState
