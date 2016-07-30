{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Shell.Api.Team (
  apiTeamSubshell
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
import           LN.T.Team
import           LN.T.Visibility

import           Text.PrettyPrint.GenericPretty


defaultBackend = haskelineBackend



data ApiTeamState = ApiTeamState {
  teamId :: Maybe Integer,
  team   :: TeamRequest,
  org    :: Maybe OrganizationResponse,
  orgId  :: Maybe Integer
} deriving (Eq, Ord, Show, Read, Generic)

instance Out ApiTeamState



initialApiTeamState :: ApiTeamState
initialApiTeamState =
  ApiTeamState {
    teamId = Nothing,
    team   = defaultTeamRequest,
    org    = Nothing,
    orgId  = Nothing
  }



apiTeamDesc :: ShellDescription ApiTeamState
apiTeamDesc = initialShellDescription
  { prompt         = \_ -> return ">> "
  , commandStyle   = OnlyCommands
  , shellCommands  = apiTeamCommands
--  , historyEnabled = False
  , historyFile    = Just "/tmp/api.teams.hist"
  , historyEnabled = True
  }



apiTeamShell :: ApiTeamState -> IO ApiTeamState
apiTeamShell st = runShell apiTeamDesc defaultBackend st



apiTeamSubshell :: IO (Subshell ShellState ApiTeamState)
apiTeamSubshell = simpleSubshell (const $ return initialApiTeamState) apiTeamDesc



apiTeamCommands :: [ShellCommand ApiTeamState]
apiTeamCommands =
  [ exitCommand "q"
  , exitCommand "quit"
  , helpCommand "h"
  , helpCommand "help"
  , cmd "gets" _gets "Get all Team records"
  , cmd "get"  _get  "Get a Team record"
  , cmd "post" _post "Create a Team record"
  , cmd "put"  _put  "Modify a Team record"
  , cmd "del"  _del  "Delete a Team record"
  , cmd "ls" listTeams "List Teams"
  , cmd "cd" setTeam "Set Team"
  , cmd "name" setName "Set Name"
  , cmd "desc" setDesc "Set Description"
  , cmd "nodesc" noDesc "No Description"
  , cmd "new" _post "Create a team"
  , cmd "save" _put "Save current team"
  , cmd "count" countTeams "Return the team count"
  , cmd "org" setOrganization "Set Organization"
  , cmd "st" state "Print state"
  ]



evaluate :: String -> Sh ApiTeamState ()
evaluate s = do
  return ()



_gets :: Sh ApiTeamState ()
_gets = do
  teams <- liftIO $ apiGet "teams"
  liftIO $ pp (eitherDecode teams :: Either String TeamResponses)



_get :: Integer -> Sh ApiTeamState ()
_get team_id = do
  r <- liftIO $ apiGet ("teams" <> "/" <> show team_id)
  let mteam = decode r :: Maybe TeamRequest
  case mteam of
    Nothing -> shellPutStrLn "Error"
    Just team@TeamRequest{..} -> do
      modifyShellSt (\st -> st { teamId = Just team_id, team = team })



_post :: String -> Sh ApiTeamState ()
_post name = do
  st <- getShellSt
  case (orgId st) of
    Nothing -> shellPutStrLn "Set Org Id"
    Just org_id -> do
      r <- liftIO $ apiPostParams "teams" (team st) "org" (T.pack $ show org_id)
      case (eitherDecode r :: Either String TeamRequest) of
        Left err -> shellPutStrLn err
        Right team -> do
          modifyShellSt (\st -> st { team = team })
          liftIO $ pp team

  {-
  modifyShellSt (\st -> st {
    team = (team st) { teamRequestName = T.pack name }
  })
  st <- getShellSt
  team <- liftIO $ apiPost "teams" (team st)
  liftIO $ pp (decode team :: Maybe TeamResponse)
  -}



_put :: Sh ApiTeamState ()
_put = do
  st <- getShellSt
  case (teamId st) of
    Nothing -> shellPutStrLn "No team"
    Just team_id -> do
      void $ liftIO $ apiPut ("teams/" <> show (team_id)) (team st)



_del :: Integer -> Sh ApiTeamState ()
_del tid = do
  void $ liftIO $ apiDelete ("teams" <> "/" <> show tid)
  shellPutStrLn "deleted"



listTeams :: Sh ApiTeamState ()
listTeams = do
  st <- getShellSt
  case (orgId st) of
    Nothing -> shellPutStrLn "Set Organization Id"
    Just org_id -> do
      r <- liftIO $ apiGetParams "/teams" "org" (T.pack $ show org_id)
      let teams = eitherDecode r :: Either String TeamResponses
      case teams of
        Left err -> shellPutStrLn err
        Right responses -> do
          mapM_ (\team -> do
            shellPutStrLn $
              show (teamResponseId team) <> ": " <> (T.unpack $ teamResponseName team))
            (teamResponses responses)



setTeam :: Integer -> Sh ApiTeamState ()
setTeam = _get



setName :: String -> Sh ApiTeamState ()
setName name = do
  modifyShellSt (\st -> st { team = (team st) { teamRequestName = T.pack name }})



setDesc :: String -> Sh ApiTeamState ()
setDesc desc = do
  modifyShellSt (\st -> st { team = (team st) { teamRequestDescription = Just $ T.pack desc }})



noDesc :: Sh ApiTeamState ()
noDesc = do
  modifyShellSt (\st -> st { team = (team st) { teamRequestDescription = Nothing }})



-- setPublic :: Sh ApiTeamState ()
-- setPublic = do
--   modifyShellSt (\st -> st { team = (team st) { teamRequestVisibility = Public }})



-- setPrivate :: Sh ApiTeamState ()
-- setPrivate = do
--   modifyShellSt (\st -> st { team = (team st) { teamRequestVisibility = Private }})



countTeams :: Sh ApiTeamState ()
countTeams = do
  r <- liftIO $ apiGet "teams/other/count"
  case (eitherDecode r :: Either String CountResponse) of
    Left err -> shellPutStrLn err
    Right count -> liftIO $ pp count



setOrganization :: Integer -> Sh ApiTeamState ()
setOrganization org_id = do
  r <- liftIO $ apiGet ("/orgs/" <> show org_id)
  case (decode r :: Maybe OrganizationResponse) of
    Nothing -> shellPutStrLn "error"
    Just org -> do
      shellPutStrLn "org set"
      modifyShellSt (\st -> st{ org = Just org, orgId = Just org_id })



state :: Sh ApiTeamState ()
state = do
  st <- getShellSt
  liftIO $ pp st



runApiTeam :: IO ()
runApiTeam = void $ apiTeamShell initialApiTeamState
