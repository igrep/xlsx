{-# LANGUAGE OverloadedStrings #-}
module Sample where

import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import           Control.Lens
import           Text.XML.Lens
import           Text.HTML.DOM
import Control.Lens

import qualified Codec.Archive.Zip as Zip
import Codec.Xlsx.Parser
import           Codec.Xlsx.Types.SharedStringTable

xlsxName :: String
xlsxName = "ml-list.xlsx"

getSample :: IO Worksheet
getSample = do
  x <- toXlsx <$> L.readFile xlsxName
  let (Just s) = x ^? ixSheet "Sheet1"
  return s

getSsAt :: Int -> IO Element
getSsAt i = do
  input <- L.readFile "sharedStrings.xml"
  return $ parseLBS input ^?! root . el "sst" . indexing (plate . el "si") . index i


buildSst :: IO SharedStringTable
buildSst =
  getSharedStrings . Zip.toArchive <$> L.readFile xlsxName
