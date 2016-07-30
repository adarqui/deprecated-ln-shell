{-# LANGUAGE DeriveGeneric #-}

module Shell.Api.Bucket (
  apiBucketSubshell
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
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Network.HTTP.Client                    as C
import           Network.Wreq

import           LN.T.Bucket
import           LN.T.Bucket.JSON
import           LN.T.Leuron

import           Text.PrettyPrint.GenericPretty



defaultBackend = haskelineBackend



data ApiBucketState = ApiBucketState {
  bucketId    :: Maybe Integer,
  bucket      :: BucketRequest,
  bucketMode  :: DisplayMode,
  leuronHist  :: [Integer],
  leuron      :: Maybe LeuronResponse,
  training    :: TrainingRec
} deriving (Eq, Ord, Show, Read, Generic)

instance Out ApiBucketState



initialApiBucketState :: ApiBucketState
initialApiBucketState =
  ApiBucketState {
    bucketId    = Nothing,
    bucket      = defaultBucketRequest "empty",
    bucketMode  = DisplayModeRaw,
    leuronHist  = [],
    leuron      = Nothing,
    training    = defaultTrainingRec
  }



apiBucketDesc :: ShellDescription ApiBucketState
apiBucketDesc = initialShellDescription
  { prompt         = \_ -> return ">> "
  , commandStyle   = OnlyCommands
  , shellCommands  = apiBucketCommands
--  , historyEnabled = False
  , historyFile    = Just "/tmp/api.buckets.hist"
  , historyEnabled = True
  }



apiBucketShell :: ApiBucketState -> IO ApiBucketState
apiBucketShell st = runShell apiBucketDesc defaultBackend st



apiBucketSubshell :: IO (Subshell ShellState ApiBucketState)
apiBucketSubshell = simpleSubshell (const $ return initialApiBucketState) apiBucketDesc



apiBucketCommands :: [ShellCommand ApiBucketState]
apiBucketCommands =
  [ exitCommand "q"
  , exitCommand "quit"
  , helpCommand "h"
  , helpCommand "help"
  , cmd "gets" _gets "Get all Bucket records"
  , cmd "get"  _get  "Get a Bucket record"
  , cmd "post" _post "Create a Bucket record"
  , cmd "put"  _put  "Modify a Bucket record"
  , cmd "del"  _del  "Delete a Bucket record"
  , cmd "mkdir" createBucket "Create a bucket"
  , cmd "ls" listBuckets "List buckets"
  , cmd "cd" setBucket "Set bucket Id"
  , cmd "mode" setMode "Set Mode {raw, splits, subs, skim}"
  , cmd "lo" setLo "Set Low Score of the range [lo,hi]"
  , cmd "hi" setHi "Set High Score of the range [lo,hi]"
  , cmd "+leuron" addLeuron "Add a leuron to a bucket"
  , cmd "+resource" addResource "Add a resource to a bucket"
  , cmd "+cat" addCategory "Add a category to a bucket"
  , cmd "start" startTraining "Create training bucket"
  , cmd "s" skipLeuron "Skip Leuron"
  , cmd "k" knowLeuron "I know this Leuron"
  , cmd "d" dontKnowLeuron "I don't know this Leuron"
  , cmd "dc" dontCareLeuron "I don't care about this Leuron"
--  , cmd "." randomLeuron "Obtain a random leuron"
  , cmd "refresh" refresh "Refresh"
  , cmd "st" state "Print state"
  ]



evaluate :: String -> Sh ApiBucketState ()
evaluate s = do
  return ()



_gets :: Sh ApiBucketState ()
_gets = do
  buckets <- liftIO $ apiGet "buckets"
  liftIO $ print buckets
  liftIO $ pp (eitherDecode buckets :: Either String BucketResponses)



_get :: Integer -> Sh ApiBucketState ()
_get bucket_id = do
  r <- liftIO $ apiGet ("buckets" <> "/" <> show bucket_id)
  case (eitherDecode r :: Either String BucketRequest) of
    Left err -> shellPutStrLn err
    Right bucket -> modifyShellSt (\st -> st{bucketId = Just bucket_id, bucket = bucket})
  liftIO $ pp (decode r :: Maybe BucketResponse)



_post :: String -> String -> Sh ApiBucketState ()
_post name desc = do
  let
    br = BucketRequest {
      bucketRequestName = T.pack name,
      bucketRequestDescription = Just $ T.pack desc,
      bucketRequestScoreLo = -100,
      bucketRequestScoreHi = 2,
      bucketRequestLeurons = [],
      bucketRequestResources = [],
      bucketRequestCategories = [],
      bucketRequestFilters = []
    }
  bucket <- liftIO $ apiPost "buckets" br
  liftIO $ pp (decode bucket :: Maybe BucketResponse)



_put :: Sh ApiBucketState ()
_put = do
  st <- getShellSt
  case (bucketId st) of
    Nothing -> shellPutStrLn "need to be in a bucket first"
    Just bucket_id -> do
      r <- liftIO $ apiPut ("buckets" <> "/" <> show bucket_id) (bucket st)
      case (eitherDecode r :: Either String BucketRequest) of
        Left err -> shellPutStrLn err
        Right bucket -> _get bucket_id



_del :: Integer -> Sh ApiBucketState ()
_del tid = do
  void $ liftIO $ apiDelete ("buckets" <> "/" <> show tid)
  shellPutStrLn "deleted"



createBucket :: String -> String -> Sh ApiBucketState ()
createBucket = _post



listBuckets :: Sh ApiBucketState ()
listBuckets = do
  r <- liftIO $ apiGet "/buckets"
  let buckets = decode r :: Maybe BucketResponses
  case buckets of
    Nothing -> return ()
    Just responses -> do
      mapM_ (\bucket -> do
        shellPutStrLn $
          (show $ bucketResponseId bucket) <> ": " <> (show $ bucketResponseName bucket))
        (bucketResponses responses)



setBucket :: Integer -> Sh ApiBucketState ()
setBucket = _get



setMode :: String -> Sh ApiBucketState ()
setMode "raw"    = modifyShellSt (\st -> st { bucketMode = DisplayModeRaw })
setMode "subs"   = modifyShellSt (\st -> st { bucketMode = DisplayModeSubs })
setMode "splits" = modifyShellSt (\st -> st { bucketMode = DisplayModeSplits })
setMode "skim"   = modifyShellSt (\st -> st { bucketMode = DisplayModeSkim })
setMode _        = shellPutStrLn "unknown mode"



setLo :: Integer -> Sh ApiBucketState ()
setLo lo = do
  modifyShellSt (\st -> st {
    bucket = (bucket st) {
      bucketRequestScoreLo = fromIntegral lo
    }
  })
  return ()



setHi :: Integer -> Sh ApiBucketState ()
setHi hi = do
  modifyShellSt (\st -> st {
    bucket = (bucket st) {
      bucketRequestScoreHi = fromIntegral hi
    }
  })
  return ()



addLeuron :: Integer -> Sh ApiBucketState ()
addLeuron leuron_id = do
  st <- getShellSt
  case (bucketId st) of
    Nothing -> shellPutStrLn "Set bucket id first"
    Just bucket_id -> do
      void $ liftIO $ apiPost ("buckets/" <> show bucket_id <> "/leurons") leuron_id
      refresh



addResource :: Integer -> Sh ApiBucketState ()
addResource resource_id = do
  st <- getShellSt
  case (bucketId st) of
    Nothing -> shellPutStrLn "Set bucket id first"
    Just bucket_id -> do
      void $ liftIO $ apiPost ("buckets/" <> show bucket_id <> "/resources") resource_id
      refresh



addCategory :: String -> Sh ApiBucketState ()
addCategory category = do
  st <- getShellSt
  case (bucketId st) of
    Nothing -> shellPutStrLn "Set bucket id first"
    Just bucket_id -> do
      void $ liftIO $ apiPost ("buckets/" <> show bucket_id <> "/categories") category
      refresh



startTraining :: Sh ApiBucketState ()
startTraining = do
  st <- getShellSt
  case (bucketId st) of
    Nothing -> shellPutStrLn "Set bucket id first"
    Just bucket_id -> do
      void $ liftIO $ apiPost ("buckets/" <> show bucket_id <> "/training") ()
      randomLeuron



skipLeuron :: Sh ApiBucketState ()
skipLeuron = do
  st <- getShellSt
  case (leuron st, bucketId st) of
    (Just leuron', Just bucket_id) -> do
      void $ liftIO $ apiPut ("buckets/" <> show bucket_id <> "/" <> show (leuronResponseId leuron') <> "/skip") ()
    (_, _) -> shellPutStrLn "nothing to skip"
  randomLeuron



-- bleh, needs to take mode into account
knowLeuron :: Sh ApiBucketState ()
knowLeuron = do
  st <- getShellSt
  case (leuron st, bucketId st) of
    (Just leuron', Just bucket_id) -> void $ liftIO $ apiPut ("buckets/" <> show bucket_id <> "/" <> show (leuronResponseId leuron') <> "/raw/know") ()
    _ -> shellPutStrLn "nothing to know"
  randomLeuron



-- bleh, needs to take mode into account
dontKnowLeuron :: Sh ApiBucketState ()
dontKnowLeuron = do
  st <- getShellSt
  case (leuron st, bucketId st) of
    (Just leuron', Just bucket_id) -> void $ liftIO $ apiPut ("buckets/" <> show bucket_id <> "/" <> show (leuronResponseId leuron') <> "/raw/dontknow") ()
    _ -> shellPutStrLn "nothing to dont know"
  randomLeuron



dontCareLeuron :: Sh ApiBucketState ()
dontCareLeuron = do
  st <- getShellSt
  case (leuron st) of
    Nothing -> shellPutStrLn "nothing to dont care about"
    Just leuron' -> shellPutStrLn "dont care"
  randomLeuron



randomLeuron :: Sh ApiBucketState ()
randomLeuron = do
  st <- getShellSt
  case (bucketId st) of
    Nothing -> shellPutStrLn "Set bucket id first"
    Just bucket_id -> do
      result <- liftIO $ apiGet ("buckets/" <> show bucket_id <> "/training")
      let decode_result = eitherDecode result :: Either String LeuronResponse
      case decode_result of
        Left err -> liftIO $ print err
        Right leuron -> do
          training <- liftIO $ displayLeuronTraining (bucketMode st) leuron
          shellPutStrLn $ T.unpack $ trainingText training
--          liftIO $ displayLeuronResponse leuron
--          liftIO $ pp leuron
          modifyShellSt (\st -> st { leuron = Just leuron, training = training })



refresh :: Sh ApiBucketState ()
refresh = do
  st <- getShellSt
  case (bucketId st) of
    Nothing -> shellPutStrLn "nothing to refresh"
    Just bucket_id -> _get bucket_id



state :: Sh ApiBucketState ()
state = do
  st <- getShellSt
  liftIO $ pp st
--  shellPutStrLn $ show st



runApiBucket :: IO ()
runApiBucket = void $ apiBucketShell initialApiBucketState
