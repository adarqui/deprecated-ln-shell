module Shell.Main (
  runMain
) where

import           Control.Monad
import           Shell.Api.Bucket
import           Shell.Api.Leuron
import           Shell.Api.Organization
import           Shell.Api.RBrowse
import           Shell.Api.Reminder
import           Shell.Api.Resource
import           Shell.Api.Team
import           Shell.Api.Test
import           Shell.Types

import           System.Console.Shell
import           System.Console.Shell.Backend.Haskeline
import           System.Console.Shell.ShellMonad

import           Control.Monad.IO.Class



defaultBackend = haskelineBackend



initialShellState = ShellState {
  shAuthToken = Nothing,
  shToggle = False,
  shName = "hi",
  shHist = Just "/tmp/hist"
}



completion :: ShellState -> String -> IO [String]
completion st prefix =
  return []



lnShell :: ShellState -> IO ShellState
lnShell st = do
  let
    desc =
       (mkShellDescription commands evaluate)
       { defaultCompletions = Just completion
       , historyFile        = shHist st
       , historyEnabled     = True
       , greetingText       = Just "Greetings!\n"
       , secondaryPrompt    = Just $ \_ -> return "] "
       , commandStyle       = OnlyCommands
       }
  runShell desc defaultBackend st



commands :: [ShellCommand ShellState]
commands =
  [ exitCommand "quit"
  , exitCommand "exit"
  , exitCommand "q"
  , helpCommand "help"
  , helpCommand "h"
  , toggle "toggle" "Toggle the boolean" shToggle (\x st -> st { shToggle = x })
  , cmd "auth" auth "Set the authorization token"
  , cmd "bucket" apiBucket "Bucket API"
  , cmd "leuron" apiLeuron "Leuron API"
  , cmd "orgs" apiOrganization "Organization API"
  , cmd "rbrowse" apiRBrowse "RBrowse"
  , cmd "reminder" apiReminder "Reminder API"
  , cmd "resource" apiResource "Resource API"
  , cmd "teams" apiTeam "Teams API"
  , cmd "test" apiTest "Test API"
  , cmd "nop" nop "no op"
  , cmd "state" state "Print the state"
  ]



evaluate :: String -> Sh ShellState ()
evaluate s = do
  st <- getShellSt
  shellPutStrLn $ show st
  return ()



auth :: String -> Sh ShellState ()
auth token = do
  modifyShellSt (\st -> st{shAuthToken = Just token})
  shellPutStrLn token



apiBucket :: Sh ShellState ()
apiBucket = do
  subsh <- liftIO $ apiBucketSubshell
  shellSpecial (ExecSubshell subsh)



apiLeuron :: Sh ShellState ()
apiLeuron = do
  subsh <- liftIO $ apiLeuronSubshell
  shellSpecial (ExecSubshell subsh)



apiOrganization :: Sh ShellState ()
apiOrganization = do
  subsh <- liftIO $ apiOrganizationSubshell
  shellSpecial (ExecSubshell subsh)



apiRBrowse :: Sh ShellState ()
apiRBrowse = do
  subsh <- liftIO $ apiRBrowseSubshell
  shellSpecial (ExecSubshell subsh)



apiReminder :: Sh ShellState ()
apiReminder = do
  subsh <- liftIO $ apiReminderSubshell
  shellSpecial (ExecSubshell subsh)



apiResource :: Sh ShellState ()
apiResource = do
  subsh <- liftIO $ apiResourceSubshell
  shellSpecial (ExecSubshell subsh)



apiTeam :: Sh ShellState ()
apiTeam = do
  subsh <- liftIO $ apiTeamSubshell
  shellSpecial (ExecSubshell subsh)



apiTest :: Sh ShellState ()
apiTest = do
  subsh <- liftIO $ apiTestSubshell
  shellSpecial (ExecSubshell subsh)



nop :: Sh ShellState ()
nop = do
  shellPutStrLn "nop"



state :: Sh ShellState ()
state = do
  st <- getShellSt
  shellPutStrLn $ show st



runMain :: IO ()
runMain = do
  void $ lnShell initialShellState
