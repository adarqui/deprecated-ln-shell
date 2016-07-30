{-# LANGUAGE DeriveGeneric #-}

module Shell.Api.Leuron (
  apiLeuronSubshell
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Shell.Api
import           Shell.Editor (maybeEditor)
import           Shell.Types

import           System.Console.Shell
import           System.Console.Shell.Backend.Haskeline
import           System.Console.Shell.ShellMonad

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Monoid                            ((<>))
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Network.HTTP.Client                    as C
import           Network.Wreq

import           LN.T.Leuron
import           LN.T.Resource

import           Text.PrettyPrint.GenericPretty



defaultBackend = haskelineBackend



data ApiLeuronState = ApiLeuronState {
  resource   :: Maybe ResourceResponse,
  resourceId :: Maybe Integer,
  leuronId   :: Maybe Integer,
  leuron     :: LeuronRequest,
  leuronDef  :: LeuronRequest
} deriving (Eq, Ord, Show, Read, Generic)

instance Out ApiLeuronState



initialApiLeuronState :: ApiLeuronState
initialApiLeuronState =
  ApiLeuronState {
    resource   = Nothing,
    resourceId = Nothing,
    leuronId   = Nothing,
    leuron     = defaultLeuronRequest,
    leuronDef  = defaultLeuronRequest
  }



apiLeuronDesc :: ShellDescription ApiLeuronState
apiLeuronDesc = initialShellDescription
  { prompt         = \_ -> return ">> "
  , commandStyle   = OnlyCommands
  , shellCommands  = apiLeuronCommands
--  , historyEnabled = False
  , historyFile    = Just "/tmp/api.leurons.hist"
  , historyEnabled = True
  }



apiLeuronShell :: ApiLeuronState -> IO ApiLeuronState
apiLeuronShell st = runShell apiLeuronDesc defaultBackend st



apiLeuronSubshell :: IO (Subshell ShellState ApiLeuronState)
apiLeuronSubshell = simpleSubshell (const $ return initialApiLeuronState) apiLeuronDesc



apiLeuronCommands :: [ShellCommand ApiLeuronState]
apiLeuronCommands =
  [ exitCommand "q"
  , exitCommand "quit"
  , helpCommand "h"
  , helpCommand "help"
  , cmd "gets" _gets "Get all Leuron records"
  , cmd "get"  _get  "Get a Leuron record"
  , cmd "post" _post "Create a Leuron record"
  , cmd "save" _post "Create a Leuron record"
  , cmd "put"  _put  "Modify a Leuron record"
  , cmd "del"  _del  "Delete a Leuron record"
  , cmd "reset" resetLeuronDef "Reset leuron defaults"
  , cmd "cd" setResource "Set Resource Id"
  , cmd "ls" listLeurons "List Leurons under a Resource"
  , cmd "cat" setLeuron "Set Leuron"
  , cmd "rm" removeLeuron "Remove Leuron"
  , cmd "new" newLeuron "New Leuron"
  , cmd "fact" fact "Set Fact"
  , cmd "card" card "Set Card"
  , cmd "dcard" dcard "Set DCard"
  , cmd "syn" synonym "Synonym"
  , cmd "ant" antonym "Antonym"
  , cmd "acro" acronym "Acronym"
  , cmd "empty" empty "Set Empty"
  , cmd "title" setTitle "Set Title"
  , cmd "desc" setDesc "Set Description"
  , cmd "+cat" addCategory "Add a category to a leuron"
  , cmd "nocat" noCategories "No Categories"
  , cmd "+suball" addSubAllOf "Add a single SubsAllOf"
  , cmd "+subone" addSubOneOf "Add a single SubsOneOf"
  , cmd "+subsall" addSubsAllOf "Add a list of SubsAllOf"
  , cmd "+subsone" addSubsOneOf "Add a list of SubsOneOf"
  , cmd "nosubs" noSubs "No Subs"
  , cmd "inc" incPage "Increment page"
  , cmd "dec" decPage "Decrement page"
  , cmd "page" setPage "Set page"
  , cmd "nopage" noPage "No page"
  , cmd "section" setSection "Set section"
  , cmd "nosection" noSection "No section"
  , cmd "st" state "Print state"
  ]



evaluate :: String -> Sh ApiLeuronState ()
evaluate s = do
  return ()



_gets :: Sh ApiLeuronState ()
_gets = do
  leurons <- liftIO $ apiGet "leurons"
  liftIO $ print leurons
  liftIO $ pp (eitherDecode leurons :: Either String LeuronResponses)



_get :: Integer -> Sh ApiLeuronState ()
_get tid = do
  r <- liftIO $ apiGet ("leurons" <> "/" <> show tid)
  case (decode r :: Maybe LeuronRequest) of
    Nothing -> shellPutStrLn "error"
    Just leuron -> do
       modifyShellSt (\st -> st { leuronId = Just tid, leuron = leuron })



_post :: Sh ApiLeuronState ()
_post = do
  st <- getShellSt
  case (resourceId st) of
    Nothing -> shellPutStrLn "Set Resource Id"
    Just resource_id -> do
      r <- liftIO $ apiPostParams "leurons" (leuron st) "resource" (T.pack $ show resource_id)
      case (eitherDecode r :: Either String LeuronRequest) of
        Left err -> shellPutStrLn err
        Right leuron -> do
          modifyShellSt (\st -> st { leuron = leuron })
          case (eitherDecode r :: Either String LeuronResponse) of
            Left err -> shellPutStrLn err
            Right leuronResp -> do
              modifyShellSt (\st -> st { leuronId = Just $ fromIntegral $ leuronResponseId leuronResp })
              liftIO $ pp leuronResp



_put :: Sh ApiLeuronState ()
_put = do
  shellPutStrLn "put"
  st <- getShellSt
  case (leuronId st) of
    Nothing -> shellPutStrLn "Set Leuron Id"
    Just leuron_id -> do
      leuron <- liftIO $ apiPut ("leurons" <> "/" <> show leuron_id) (leuron st)
      liftIO $ pp (decode leuron :: Maybe LeuronResponse)



_del :: Integer -> Sh ApiLeuronState ()
_del tid = do
  void $ liftIO $ apiDelete ("leurons" <> "/" <> show tid)
  shellPutStrLn "deleted"



resetLeuronDef :: Sh ApiLeuronState ()
resetLeuronDef = do
  modifyShellSt (\st -> st { leuronDef = defaultLeuronRequest })



setResource :: Integer -> Sh ApiLeuronState ()
setResource resource_id = do
--  modifyShellSt (\st -> st{resourceId = Just resource_id})
  r <- liftIO $ apiGet ("/resources/" <> show resource_id)
  case (decode r :: Maybe ResourceResponse) of
    Nothing -> shellPutStrLn "error"
    Just resource -> do
      shellPutStrLn "resource set"
      modifyShellSt (\st -> st{ resource = Just resource, resourceId = Just resource_id })



listLeurons :: Sh ApiLeuronState ()
listLeurons = do
  st <- getShellSt
  case (resourceId st) of
    Nothing -> shellPutStrLn "Set ResourceId first"
    Just resource_id -> do
      r <- liftIO $ apiGetParams "/leurons" "resource" (T.pack $ show resource_id)
      let leurons = eitherDecode r :: Either String LeuronResponses
      case leurons of
        Left err -> shellPutStrLn err
        Right responses -> do
          mapM_ (\leuron -> do
            shellPutStrLn $
              (show $ leuronResponseId leuron) <> ": " <> (show $ leuronResponseData leuron))
            (leuronResponses responses)



setLeuron :: Integer -> Sh ApiLeuronState ()
setLeuron = _get



removeLeuron :: Integer -> Sh ApiLeuronState ()
removeLeuron = _del



newLeuron :: Sh ApiLeuronState ()
newLeuron = do
  modifyShellSt (\st -> st{leuron = (leuronDef st) })



fact :: String -> Sh ApiLeuronState ()
fact body' = do
  body <- maybeEditor body'
  modifyShellSt (\st -> st{leuron = (leuron st) { leuronRequestData = LnFact $ Fact body}})



card :: String -> String -> Sh ApiLeuronState ()
card front' back' = do
  front <- maybeEditor front'
  back <- maybeEditor back'
  modifyShellSt (\st -> st{leuron = (leuron st) { leuronRequestData = LnCard $ Card front back}})



dcard :: String -> String -> Sh ApiLeuronState ()
dcard front' back' = do
  front <- maybeEditor front'
  back <- maybeEditor back'
  modifyShellSt (\st -> st{leuron = (leuron st) { leuronRequestData = LnDCard $ DCard front back}})



synonym :: String -> String -> Sh ApiLeuronState ()
synonym a b = modifyShellSt (\st -> st {leuron = (leuron st) { leuronRequestData = LnSynonym $ Synonym (T.pack a) (T.pack b) }})



antonym :: String -> String -> Sh ApiLeuronState ()
antonym a b = modifyShellSt (\st -> st {leuron = (leuron st) { leuronRequestData = LnAntonym $ Antonym (T.pack a) (T.pack b) }})



acronym :: String -> String -> Sh ApiLeuronState ()
acronym a b = modifyShellSt (\st -> st {leuron = (leuron st) { leuronRequestData = LnAcronym $ Acronym (T.pack a) (T.pack b) }})



empty :: Sh ApiLeuronState ()
empty = do
  modifyShellSt (\st -> st{leuron = (leuron st) { leuronRequestData = LnEmpty }})



setTitle :: String -> Sh ApiLeuronState ()
setTitle title = do
  modifyShellSt (\st -> st{leuron = (leuron st) { leuronRequestTitle = Just $ T.pack title }})



noTitle :: Sh ApiLeuronState ()
noTitle = do
  modifyShellSt (\st -> st{leuron = (leuron st) { leuronRequestTitle = Nothing }})



setDesc :: String -> Sh ApiLeuronState ()
setDesc desc = do
  modifyShellSt (\st -> st{leuron = (leuron st) { leuronRequestDescription = Just $ T.pack desc }})



noDesc :: Sh ApiLeuronState ()
noDesc = do
  modifyShellSt (\st -> st{leuron = (leuron st) { leuronRequestDescription = Nothing }})



addCategory :: String -> Sh ApiLeuronState ()
addCategory category = do
  shellPutStrLn $ "adding " <> show cat
  modifyShellSt (\st ->
    st{
      leuron = (leuron st) {
        leuronRequestCategories = (leuronRequestCategories $ leuron st) ++ cat
      }
    })
  where
  cat = [read category]



noCategories :: Sh ApiLeuronState ()
noCategories = do
  modifyShellSt (\st ->
    st {
      leuron = (leuron st) { leuronRequestCategories = [] },
      leuronDef = (leuron st) { leuronRequestCategories = [] }
    })



addSubAllOf :: String -> Sh ApiLeuronState ()
addSubAllOf sub = do
  shellPutStrLn $ "adding " <> show sub'
  modifyShellSt (\st ->
    st{
      leuron = (leuron st) {
        leuronRequestSubstitutions = maybe (Just [sub']) (\subs -> Just (sub' : subs)) (leuronRequestSubstitutions $ leuron st)
      }
    })
  where
  sub' = SubsAllOf [T.pack sub]



addSubOneOf :: String -> Sh ApiLeuronState ()
addSubOneOf sub = do
  shellPutStrLn $ "adding " <> show sub'
  modifyShellSt (\st ->
    st{
      leuron = (leuron st) {
        leuronRequestSubstitutions = maybe (Just [sub']) (\subs -> Just (sub' : subs)) (leuronRequestSubstitutions $ leuron st)
      }
    })
  where
  sub' = SubsOneOf [T.pack sub]



addSubsAllOf :: String -> Sh ApiLeuronState ()
addSubsAllOf sub = do
  shellPutStrLn $ "adding " <> show sub'
  modifyShellSt (\st ->
    st{
      leuron = (leuron st) {
        leuronRequestSubstitutions = maybe (Just [sub']) (\subs -> Just (sub' : subs)) (leuronRequestSubstitutions $ leuron st)
      }
    })
  where
  sub' = SubsAllOf (read sub :: [T.Text])



addSubsOneOf :: String -> Sh ApiLeuronState ()
addSubsOneOf sub = do
  shellPutStrLn $ "adding " <> show sub'
  modifyShellSt (\st ->
    st{
      leuron = (leuron st) {
        leuronRequestSubstitutions = maybe (Just [sub']) (\subs -> Just (sub' : subs)) (leuronRequestSubstitutions $ leuron st)
      }
    })
  where
  sub' = SubsOneOf (read sub :: [T.Text])



noSubs :: Sh ApiLeuronState ()
noSubs = do
  modifyShellSt (\st ->
    st {
      leuron = (leuron st) { leuronRequestSubstitutions = Nothing }
    })



incPage :: Sh ApiLeuronState ()
incPage = incdecPage succ



decPage :: Sh ApiLeuronState ()
decPage = incdecPage pred



incdecPage :: (Integer -> Integer) -> Sh ApiLeuronState ()
incdecPage f = do
  st <- getShellSt
  let page = case (leuronRequestPage $ leuronDef st) of
             Nothing -> T.pack $ show 1
             Just page' -> T.pack $ show $ f (read (T.unpack page') :: Integer)

  modifyShellSt (\st ->
    st {
      leuronDef = (leuronDef st) { leuronRequestPage = Just page },
      leuron = (leuron st) { leuronRequestPage = Just page }
    })



setPage :: Integer -> Sh ApiLeuronState ()
setPage page = do
  modifyShellSt (\st ->
    st {
      leuronDef = (leuronDef st) { leuronRequestPage = Just page' },
      leuron = (leuron st) { leuronRequestPage = Just page' }
    })
  where
  page' = T.pack $ show page



noPage :: Sh ApiLeuronState ()
noPage = do
  modifyShellSt (\st ->
    st {
      leuronDef = (leuronDef st) { leuronRequestPage = Nothing },
      leuron = (leuron st) { leuronRequestPage = Nothing }
    })



setSection :: String -> Sh ApiLeuronState ()
setSection section = do
  modifyShellSt (\st ->
    st {
      leuronDef = (leuronDef st) { leuronRequestSection = Just section' },
      leuron = (leuron st) { leuronRequestSection = Just section' }
    })
  where
  section' = T.pack section



noSection :: Sh ApiLeuronState ()
noSection = do
  modifyShellSt (\st ->
    st {
      leuronDef = (leuronDef st) { leuronRequestSection = Nothing },
      leuron = (leuron st) { leuronRequestPage = Nothing }
    })



state :: Sh ApiLeuronState ()
state = do
  st <- getShellSt
  liftIO $ pp st



runApiLeuron :: IO ()
runApiLeuron = void $ apiLeuronShell initialApiLeuronState
