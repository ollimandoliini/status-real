{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Parser
    ( PackageInfo(..), getRowStartingWith, parseName, parseDescription, parseDependencies, parsePackageInfo
    ) where

import Data.List (find)
import qualified Data.Text as T

data PackageInfo = PackageInfo {  name :: T.Text
                                , description :: Maybe T.Text
                                , deps :: Maybe [T.Text]
                                } deriving Show

getRowStartingWith :: T.Text -> T.Text -> Maybe T.Text
getRowStartingWith keyword text =
    find (\x -> head (T.words x) == keyword) rows
    where
        rows = T.splitOn "\n" text

parseName :: T.Text -> Maybe T.Text
parseName txt =
    last . T.words <$> getRowStartingWith "Package:" txt


parseDescription :: T.Text -> Maybe T.Text
parseDescription txt =
    T.unlines <$> descriptionRows lines
    where
        lines = T.lines txt
        descriptionStarts = dropWhile (\x -> head (T.words x) /= "Description:") lines
        descriptionRows (_:_) = Just (removeFirstWord (head descriptionStarts) : takeWhile (\x -> T.head x == ' ') (tail descriptionStarts ))
        descriptionRows _ = Nothing
        removeFirstWord text = T.unwords $ tail $ T.words text


parseDependencies :: T.Text -> Maybe [T.Text]
parseDependencies text =
    map removeCommas . (filter (not . isVersionNumber) . tail <$> T.words) <$> row
    where
        row = getRowStartingWith "Depends:" text
        isVersionNumber word = T.any (== '(') word || T.any (== ')') word
        removeCommas = T.filter (/= ',')


parsePackageInfo :: T.Text -> Maybe PackageInfo
parsePackageInfo rows =
    case name of
        Just name' -> Just PackageInfo {name = name', description, deps}
        Nothing -> Nothing
    where
        name = parseName rows
        description = parseDescription rows
        deps = parseDependencies rows