{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Web.Scotty
import Data.Maybe
import Data.List (find)
import System.Environment (lookupEnv)

import Parser (PackageInfo(..), parsePackageInfo)
import Views

main :: IO ()
main = do
    port <- fmap (maybe 3000 read) (lookupEnv "PORT")
    fileContent <- readFile "data/status.real"
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
