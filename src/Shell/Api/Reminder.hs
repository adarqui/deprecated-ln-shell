{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Shell.Api.Reminder (
  apiReminderSubshell
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
import           LN.T.Reminder
import           LN.T.Visibility

import           Text.PrettyPrint.GenericPretty


defaultBackend = haskelineBackend



data ApiReminderState = ApiReminderState {
  reminderId             :: Maybe Integer,
  reminder               :: ReminderRequest,
  reminderFolderId       :: Maybe Integer,
  reminderFolder         :: ReminderFolderRequest,
  reminderFolderResponse :: Maybe ReminderFolderResponse
} deriving (Eq, Ord, Show, Read, Generic)

instance Out ApiReminderState



initialApiReminderState :: ApiReminderState
initialApiReminderState =
  ApiReminderState {
    reminderId             = Nothing,
    reminder               = defaultReminderRequest,
    reminderFolderId       = Nothing,
    reminderFolder         = defaultReminderFolderRequest,
    reminderFolderResponse = Nothing
  }



apiReminderDesc :: ShellDescription ApiReminderState
apiReminderDesc = initialShellDescription
  { prompt         = \_ -> return ">> "
  , commandStyle   = OnlyCommands
  , shellCommands  = apiReminderCommands
--  , historyEnabled = False
  , historyFile    = Just "/tmp/api.reminders.hist"
  , historyEnabled = True
  }



apiReminderShell :: ApiReminderState -> IO ApiReminderState
apiReminderShell st = runShell apiReminderDesc defaultBackend st



apiReminderSubshell :: IO (Subshell ShellState ApiReminderState)
apiReminderSubshell = simpleSubshell (const $ return initialApiReminderState) apiReminderDesc



apiReminderCommands :: [ShellCommand ApiReminderState]
apiReminderCommands =
  [ exitCommand "q"
  , exitCommand "quit"
  , helpCommand "h"
  , helpCommand "help"
  , cmd "gets" _gets "Get all Reminder records"
  , cmd "get"  _get  "Get a Reminder record"
  , cmd "post" _post "Create a Reminder record"
  , cmd "put"  _put  "Modify a Reminder record"
  , cmd "del"  _del  "Delete a Reminder record"
  , cmd "list" listReminders "List Reminders"
--  , cmd "title" setTitle "Set Title"
--  , cmd "desc" setDesc "Set Description"
--  , cmd "public" setPublic "Set Public"
--  , cmd "private" setPrivate "Set Private"
  , cmd "ls" listFolders "List Folders"
  , cmd "mkdir" createFolder "Create a folder"
  , cmd "rmdir" removeFolder "Remove a folder"
  , cmd "cd" setFolder "Set current folder"
  , cmd "new" _post "Create a reminder"
  , cmd "save" _put "Save current reminder"
  , cmd "count" countReminders "Return the reminder count"
  , cmd "st" state "Print state"
  ]



evaluate :: String -> Sh ApiReminderState ()
evaluate s = do
  return ()



_gets :: Sh ApiReminderState ()
_gets = do
  reminders <- liftIO $ apiGet "reminders"
  liftIO $ pp (eitherDecode reminders :: Either String ReminderResponses)



_get :: Integer -> Sh ApiReminderState ()
_get reminder_id = do
  r <- liftIO $ apiGet ("reminders" <> "/" <> show reminder_id)
  let mreminder = decode r :: Maybe ReminderRequest
  case mreminder of
    Nothing -> shellPutStrLn "Error"
    Just reminder@ReminderRequest{..} -> do
      modifyShellSt (\st -> st { reminder = reminder })



_post :: String -> Sh ApiReminderState ()
_post reminder = do
  st <- getShellSt
  case (reminderFolderId st) of
    Nothing -> shellPutStrLn "cd to a folder"
    Just folder_id -> do
      r <- liftIO $ apiPostParams "reminders" (ReminderRequest $ reminder') "folder" (T.pack $ show folder_id)
      case (eitherDecode r :: Either String ReminderResponse) of
        Left err -> shellPutStrLn err
        Right reminder_response -> modifyShellSt (\st -> st { reminderId = Just (fromIntegral $ reminderResponseId reminder_response), reminder = ReminderRequest reminder' })
--      liftIO $ pp (decode reminder :: Maybe ReminderResponse)
  where
    reminder' = T.pack reminder



_put :: Sh ApiReminderState ()
_put = do
  st <- getShellSt
  case (reminderId st) of
    Nothing -> shellPutStrLn "No reminder"
    Just reminder_id -> do
      void $ liftIO $ apiPut ("reminders/" <> show (reminder_id)) (reminder st)



_del :: Integer -> Sh ApiReminderState ()
_del tid = do
  void $ liftIO $ apiDelete ("reminders" <> "/" <> show tid)
  shellPutStrLn "deleted"



listReminders :: Sh ApiReminderState ()
listReminders = do
  st <- getShellSt
  let
    act = case (reminderFolderId st) of
          Nothing -> apiGet "reminders"
          Just folder_id -> apiGetParams "reminders" "folder" (T.pack $ show folder_id)
  r <- liftIO act
  case (eitherDecode r :: Either String ReminderResponses) of
    Left err -> shellPutStrLn err
    Right responses -> do
      mapM_ (\reminder -> do
        shellPutStrLn $
          show (reminderResponseId reminder) <> " - " <> show (reminderResponseParentFolderId reminder) <> " : " <> (T.unpack $ reminderResponseData reminder))
        (reminderResponses responses)



listFolders :: Sh ApiReminderState ()
listFolders = do
  st <- getShellSt
  let
    act = case (reminderFolderId st) of
          Nothing -> apiGet "reminder_folders"
          Just folder_id -> apiGetParams "reminder_folders" "parent" (T.pack $ show folder_id)
  r <- liftIO act
  case (eitherDecode r :: Either String ReminderFolderResponses) of
    Left err -> shellPutStrLn err
    Right folders -> do
      mapM_ (\ReminderFolderResponse{..} ->
        shellPutStrLn $ show reminderFolderResponseId <> " - " <> show reminderFolderResponseParentFolderId <> " : " <> T.unpack reminderFolderResponseName)
        (reminderFolderResponses folders)



createFolder :: String -> String -> Sh ApiReminderState ()
createFolder name desc = do
  st <- getShellSt
  let
    reminder_folder_request = ReminderFolderRequest (T.pack name) (Just $ T.pack desc) Public
    act = case (reminderFolderId st) of
          Nothing -> apiPost "reminder_folders" reminder_folder_request
          Just folder_id -> apiPostParams "reminder_folders" reminder_folder_request "parent" (T.pack $ show folder_id)
  r <- liftIO act
  case (eitherDecode r :: Either String ReminderFolderRequest) of
   Left err -> shellPutStrLn err
   Right reminder_folder -> liftIO $ pp reminder_folder



removeFolder :: Integer -> Sh ApiReminderState ()
removeFolder folder_id = do
  void $ liftIO $ apiDelete ("reminder_folders/" <> show folder_id)
  shellPutStrLn "deleted"



-- | Allow user to 'cd ..' back to the parent folder id
-- This is accomplished via reminderFolderResponseParentFolderId
--
setFolder :: String -> Sh ApiReminderState ()
setFolder folder_id_s = do
  st <- getShellSt

  let
    folder_id = case folder_id_s of
      ".." -> case (reminderFolderResponse st) of
        Nothing -> 0
        Just res -> case (reminderFolderResponseParentFolderId res) of
          Nothing -> 0
          Just new_id -> fromIntegral new_id
      _    -> read folder_id_s :: Integer

  liftIO $ print folder_id
  r <- liftIO $ apiGet ("reminder_folders/" <> show folder_id)
  case (eitherDecode r :: Either String ReminderFolderRequest) of
    Left err -> shellPutStrLn err
    Right folder -> case (eitherDecode r :: Either String ReminderFolderResponse) of
      Left err -> shellPutStrLn err
      Right response ->
        modifyShellSt (\st -> st { reminderFolder = folder, reminderFolderId = Just folder_id, reminderFolderResponse = Just response })



{-
setName :: String -> Sh ApiReminderState ()
setName name = do
  modifyShellSt (\st -> st { reminder = (reminder st) { reminderRequestName = T.pack name }})
  -}



{-
setDesc :: String -> Sh ApiReminderState ()
setDesc desc = do
  modifyShellSt (\st -> st { reminder = (reminder st) { reminderRequestDescription = T.pack desc }})
-}



{-
setPublic :: Sh ApiReminderState ()
setPublic = do
  modifyShellSt (\st -> st { reminder = (reminder st) { reminderRequestVisibility = Public }})



setPrivate :: Sh ApiReminderState ()
setPrivate = do
  modifyShellSt (\st -> st { reminder = (reminder st) { reminderRequestVisibility = Private }})
  -}



countReminders :: Sh ApiReminderState ()
countReminders = do
  r <- liftIO $ apiGet "reminders/other/count"
  case (eitherDecode r :: Either String CountResponse) of
    Left err -> shellPutStrLn err
    Right count -> liftIO $ pp count



state :: Sh ApiReminderState ()
state = do
  st <- getShellSt
  liftIO $ pp st
--  shellPutStrLn $ show st



runApiReminder :: IO ()
runApiReminder = void $ apiReminderShell initialApiReminderState
