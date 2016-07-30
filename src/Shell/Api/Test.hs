{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Shell.Api.Test (
  apiTestSubshell
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

import           LN.T.Test
import           LN.T.Test.JSON

import           Text.PrettyPrint.GenericPretty


defaultBackend = haskelineBackend


data ApiTestState = ApiTestState {
} deriving (Eq, Ord, Show, Generic)

instance Out ApiTestState



initialApiTestState :: ApiTestState
initialApiTestState = ApiTestState { }



apiTestDesc :: ShellDescription ApiTestState
apiTestDesc = initialShellDescription
  { prompt         = \_ -> return ">> "
  , commandStyle   = OnlyCommands
  , shellCommands  = apiTestCommands
--  , historyEnabled = False
  , historyFile    = Just "/tmp/api.tests.hist"
  , historyEnabled = True
  }



apiTestShell :: ApiTestState -> IO ApiTestState
apiTestShell st = runShell apiTestDesc defaultBackend st



apiTestSubshell :: IO (Subshell ShellState ApiTestState)
apiTestSubshell = simpleSubshell (const $ return initialApiTestState) apiTestDesc



apiTestCommands :: [ShellCommand ApiTestState]
apiTestCommands =
  [ exitCommand "q"
  , exitCommand "quit"
  , helpCommand "h"
  , helpCommand "help"
  , cmd "gets" _gets "Get all Test records"
  , cmd "get"  _get  "Get a Test record"
  , cmd "post" _post "Create a Test record"
  , cmd "put"  _put  "Modify a Test record"
  , cmd "del"  _del  "Delete a Test record"
  , cmd "st" state "Print state"
  ]



evaluate :: String -> Sh ApiTestState ()
evaluate s = do
  return ()



_gets :: Sh ApiTestState ()
_gets = do
  tests <- liftIO $ apiGet "tests"
  liftIO $ pp (decode tests :: Maybe TestResponses)



_get :: Integer -> Sh ApiTestState ()
_get tid = do
  test <- liftIO $ apiGet ("tests" <> "/" <> show tid)
  liftIO $ pp (decode test :: Maybe TestResponse)



_post :: String -> Sh ApiTestState ()
_post msg = do
  test <- liftIO $ apiPost "tests" (TestRequest (T.pack msg))
  liftIO $ pp (decode test :: Maybe TestResponse)



_put :: Integer -> String -> Sh ApiTestState ()
_put tid msg = do
  test <- liftIO $ apiPut ("tests" <> "/" <> show tid) (TestRequest (T.pack msg))
  liftIO $ pp (decode test :: Maybe TestResponse)




_del :: Integer -> Sh ApiTestState ()
_del tid = do
  void $ liftIO $ apiDelete ("tests" <> "/" <> show tid)
  shellPutStrLn "deleted"


state :: Sh ApiTestState ()
state = do
  st <- getShellSt
  liftIO $ pp st



runApiTest :: IO ()
runApiTest = void $ apiTestShell initialApiTestState
