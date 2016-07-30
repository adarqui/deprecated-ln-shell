{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Shell.Leuron.Display (
  DisplayMode (..),
  displayLeuronRequest,
  displayLeuronResponse,
  prettyLeuronData,
  TrainingRec (..),
  displayLeuronTraining,
  defaultTrainingRec
) where

import           Data.List                      (intersperse)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           GHC.Generics                   (Generic)
import           Prelude                        hiding ((<$>))
import           Text.PrettyPrint.GenericPretty
import           Text.PrettyPrint.Leijen

import           LN.T.Leuron



data DisplayMode
  = DisplayModeSkim
  | DisplayModeRaw
  | DisplayModeSplits
  | DisplayModeSubs
  deriving (Eq, Ord, Show, Read, Generic, Out)



data TrainingRec = TrainingRec {
  trainingMode :: DisplayMode,
  trainingText :: Text,
  trainingAns  :: Text
} deriving (Eq, Ord, Show, Read, Generic, Out)



ind4 = indent 4
ind8 = indent 8
ttext = text . T.unpack



displayLeuronRequest :: LeuronRequest -> IO ()
displayLeuronRequest LeuronRequest{..} = do
  putDoc $ (prettyLeuronData leuronRequestData <> line)



displayLeuronResponse :: LeuronResponse -> IO ()
displayLeuronResponse LeuronResponse{..} = do
  putDoc $ ((text $ "id: " ++ show leuronResponseId) <> line <> prettyLeuronData leuronResponseData <> line)



prettyLeuronData :: LeuronData -> Doc
prettyLeuronData dat =
  case dat of
    LnFact (Fact body) ->
      (text "Fact") <$> (ind4 $ ttext body)
    LnFactList (FactList fact list) ->
      (text "FactList") <$> (ind4 $ ttext (T.concat $ intersperse "\n" list))
    LnCard (Card front back) ->
      (text "Card") <$>
        (ind4 $ text "Front") <$> (ind8 $ ttext front) <$>
        (ind4 $ text "Back") <$> (ind8 $ ttext back)
    LnDCard (DCard front back) ->
      (text "DCard") <$>
        (ind4 $ text "Front") <$> (ind8 $ ttext front) <$>
        (ind4 $ text "Back") <$> (ind8 $ ttext back)
    _ -> text "Not supported."


-- for training
--

displayLeuronTraining :: DisplayMode -> LeuronResponse -> IO TrainingRec
displayLeuronTraining DisplayModeSplits lr@LeuronResponse{..} =
  case leuronResponseSplits of
    Nothing -> displayLeuronTraining DisplayModeSubs lr
    Just splits ->
      return $ defaultTrainingRec {
        trainingMode = DisplayModeSplits,
        trainingText = T.pack $ show $ prettyLeuronData leuronResponseData
      }
displayLeuronTraining DisplayModeSubs lr@LeuronResponse{..} =
  case leuronResponseSubstitutions of
    Nothing -> displayLeuronTraining DisplayModeRaw lr
    Just subs ->
      return $ defaultTrainingRec {
        trainingMode = DisplayModeSubs,
        trainingText = T.pack $ show $ prettyLeuronData leuronResponseData
      }
displayLeuronTraining DisplayModeRaw LeuronResponse{..} =
  return $ defaultTrainingRec {
    trainingMode = DisplayModeRaw,
    trainingText = T.pack $ show $ prettyLeuronData leuronResponseData
  }


-- Order of training "precedence":
-- raw
-- subs
-- splits
-- subs
-- raw
--


defaultTrainingRec :: TrainingRec
defaultTrainingRec = TrainingRec {
  trainingMode = DisplayModeRaw,
  trainingText = "",
  trainingAns = ""
}
