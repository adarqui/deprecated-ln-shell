{-# LANGUAGE DeriveGeneric #-}

module Shell.Api.RBrowse (
  apiRBrowseSubshell
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Shell.Api
import           Shell.Leuron.Display
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

import           LN.T.Resource
import           LN.T.Leuron

import           Text.PrettyPrint.GenericPretty


defaultBackend = haskelineBackend



data ApiRBrowseState = ApiRBrowseState {
  resource     :: Maybe ResourceResponse,
  leuron       :: Maybe LeuronResponse,
  leuronOffset :: Int
} deriving (Eq, Ord, Show, Read, Generic)

instance Out ApiRBrowseState



initialApiRBrowseState :: ApiRBrowseState
initialApiRBrowseState =
  ApiRBrowseState {
    resource     = Nothing,
    leuron       = Nothing,
    leuronOffset = 1
  }



apiRBrowseDesc :: ShellDescription ApiRBrowseState
apiRBrowseDesc = initialShellDescription
  { prompt         = \_ -> return ">> "
  , commandStyle   = OnlyCommands
  , shellCommands  = apiRBrowseCommands
--  , historyEnabled = False
  , historyFile    = Just "/tmp/api.rbrowse.hist"
  , historyEnabled = True
  }



apiRBrowseShell :: ApiRBrowseState -> IO ApiRBrowseState
apiRBrowseShell st = runShell apiRBrowseDesc defaultBackend st



apiRBrowseSubshell :: IO (Subshell ShellState ApiRBrowseState)
apiRBrowseSubshell = simpleSubshell (const $ return initialApiRBrowseState) apiRBrowseDesc



apiRBrowseCommands :: [ShellCommand ApiRBrowseState]
apiRBrowseCommands =
  [ exitCommand "q"
  , exitCommand "quit"
  , helpCommand "h"
  , helpCommand "help"
  , cmd "cd" setResource "Set Resource Id"
  , cmd "n" nextLeuron "Next Leuron"
  , cmd "p" prevLeuron "Previous Leuron"
  , cmd "d" displayLeuron "Display Leuron"
  , cmd "st" state "Print state"
  ]



evaluate :: String -> Sh ApiRBrowseState ()
evaluate s = do
  return ()



setResource :: Integer -> Sh ApiRBrowseState ()
setResource resource_id = do
  r <- liftIO $ apiGet ("resources/" <> show resource_id)
  case (eitherDecode r :: Either String ResourceResponse) of
    Left err -> shellPutStrLn err
    Right resource -> modifyShellSt (\st -> st { resource = Just resource })



getResId = do
  st <- getShellSt
  case (resource st) of
    Nothing -> return $ T.pack "1"
    Just resource' -> return $ T.pack $ show (resourceResponseId resource')



getOffset = do
  st <- getShellSt
  return $ T.pack $ show (leuronOffset st)



nextLeuron :: Sh ApiRBrowseState ()
nextLeuron = do
  resource_id <- getResId
  offset <- getOffset
  r <- liftIO $ apiGetParamsList "leurons" [("offset", offset), ("limit", "1"), ("resource", resource_id)]
  case (eitherDecode r :: Either String LeuronResponses) of
    Left err -> shellPutStrLn err
    Right leurons -> case (leuronResponses leurons) of
                     [] -> shellPutStrLn "no more leurons"
                     [leuron] -> do
                       modifyShellSt (\st -> st {
                         leuronOffset = (leuronOffset st) + 1,
                         leuron = Just leuron
                       })
                       displayLeuron
                     _ -> shellPutStrLn "error: shouldnt have returned more than one leuron"
  return ()



prevLeuron :: Sh ApiRBrowseState ()
prevLeuron = do
  st <- getShellSt
  resource_id <- getResId
  case (leuronOffset st) of
    1 -> shellPutStrLn "empty"
    offset -> do
      r <- liftIO $ apiGetParamsList "leurons" [("offset", T.pack $ show (offset-1)), ("limit", "1"), ("resource", resource_id)]
      case (eitherDecode r :: Either String LeuronResponses) of
        Left err -> shellPutStrLn err
        Right leurons -> case (leuronResponses leurons) of
          [] -> shellPutStrLn "no more leurons"
          [leuron] -> do
            modifyShellSt (\st -> st { leuronOffset = offset - 1, leuron = Just leuron })
            displayLeuron
          _ -> shellPutStrLn "error: shouldnt have returned more than one leuron"
  return ()



displayLeuron :: Sh ApiRBrowseState ()
displayLeuron = do
  st <- getShellSt
  liftIO $ maybe (putStrLn "empty") (\leuron -> displayLeuronResponse leuron) (leuron st)



state :: Sh ApiRBrowseState ()
state = do
  st <- getShellSt
  liftIO $ pp st
--  shellPutStrLn $ show st



runApiRBrowse :: IO ()
runApiRBrowse = void $ apiRBrowseShell initialApiRBrowseState
