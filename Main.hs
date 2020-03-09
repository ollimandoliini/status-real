{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Web.Scotty
import Data.Maybe
import Data.List (find)
import System.Environment (lookupEnv)
import System.Directory (doesFileExist)

import Parser (PackageInfo(..), parsePackageInfo)
import Views

main :: IO ()
main = do
    port <- fmap (maybe 3000 read) (lookupEnv "PORT")
    file <- dataFile
    fileContent <- readFile file
    let pkgs = mapMaybe parsePackageInfo $ T.splitOn "\n\n" $ T.pack fileContent
    scotty port $ do
        get "/package/:packageName" $ do
            packageName <- param "packageName"
            case findPackage packageName pkgs of
                Just pkg -> html $ packageInfoView pkgs pkg
                Nothing -> html $ notFoundView packageName
        get "/" $
            html $ packageListView pkgs
    where
        findPackage packageName = find (\x -> name x == packageName)
        -- dataFile = if doesFileExist "/var/lib/dpkg/status.real" then "/var/lib/dpkg/status.real" else "data/status.real"  


dataFile :: IO String
dataFile = do
  fileExists <- doesFileExist "/var/lib/dpkg/status.real"
  if fileExists then return "/var/lib/dpkg/status.real" else return "app/data/status.real"


-- fff = do
--   response <- simpleHTTP $ getRequest "https://gist.githubusercontent.com/lauripiispanen/29735158335170c27297422a22b48caa/raw/61a0f1150f33a1f31510b8e3a70cbac970892b2f/status.real"
--   let body = fmap rspBody response
--   print response
--   print body