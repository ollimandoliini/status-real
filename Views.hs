{-# LANGUAGE OverloadedStrings #-}
module Views
    where
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import Data.List (sort, find)
import Data.Maybe (fromMaybe)

import Parser (PackageInfo (..))


type Html = L.Text
type HtmlTemplate = T.Text

-- Renders link to package if package exists, otherwise package name only     
packageNameElement :: [PackageInfo] -> T.Text -> T.Text
packageNameElement packageList pkgName =
  case packageExists pkgName of
      Just pkg -> mconcat ["<li><a href=/package/", pkgName, ">", pkgName, "</a></li>"]  
      Nothing -> mconcat ["<li>", pkgName, "</li>"]  
  where
    packageExists y = find (\x -> name x == y) packageList

-- FRONTPAGE PACKAGE LISTING VIEW
packageListView :: [PackageInfo] -> Html
packageListView pkgs =
    L.fromStrict $ multiReplace ["${packageList}"] [linkElements] packageListTemplate
    where
        linkElements = mconcat $ map (packageNameElement pkgs) $ sort $ map name pkgs

packageListTemplate :: HtmlTemplate
packageListTemplate =
    "<html>\
        \<body>\
            \<h1>/var/lib/dpkg/status.real exploration UI</h1>\
            \<h2>Packages</h2>\
            \${packageList}\
        \</body>\
    \</html>"


-- INDIVIDUAL PACKAGE VIEW
packageInfoView :: [PackageInfo] -> PackageInfo -> Html
packageInfoView packageList pkg =
    L.fromStrict $ multiReplace ["${name}", "${description}", "${deps}",  "${reverseDeps}"] [name', desc, deps', reverseDeps] packageInfoTemplate
    where
        name' = name pkg
        desc = fromMaybe "" (description pkg)
        deps' = case deps pkg of
            Nothing -> ""
            Just deps'' -> mconcat $ map (packageNameElement packageList) deps''
        reverseDeps = mconcat $ map (packageNameElement packageList)  $ getReverseDeps packageList pkg

getReverseDeps :: [PackageInfo] -> PackageInfo -> [T.Text]
getReverseDeps packageList pkg =
    map name $ filter (isInDeps pkg) packageList

isInDeps :: PackageInfo -> PackageInfo -> Bool
isInDeps x y =
    case deps y of
        Nothing -> False
        Just deps' -> name x `elem` deps'

packageInfoTemplate :: HtmlTemplate
packageInfoTemplate =
    "<html>\
        \<body>\
            \<a href=/>Back</a>\
            \<h1>/var/lib/dpkg/status.real exploration UI</h1>\
            \<h2> ${name} </h2>\
            \<p> ${description} </p>\
            \<h3>Dependencies</h3>\
            \<ul>\
                \${deps}\
            \</ul>\
            \<h3>Reverse dependencies</h3>\
            \<ul>\
                \${reverseDeps}\
            \</ul>\
        \</body>\
    \</html>"

-- 'PACKAGE NOT FOUND' VIEW
notFoundView :: T.Text -> Html
notFoundView pkgName =
    L.fromStrict $ multiReplace ["${name}"] [pkgName] notFoundTemplate


notFoundTemplate :: HtmlTemplate
notFoundTemplate =
    "<html>\
        \<body>\
            \<a href=/>Back</a>\
                \<h1>/var/lib/dpkg/status.real exploration UI</h1>\
            \<p>Package '${name}' not found 😿</p>\
        \</body>\
    \</html>"



-- UTILS
-- Replaces the placeholders with substituents in the template
multiReplace :: [T.Text] -> [T.Text] -> HtmlTemplate -> T.Text
multiReplace placeholders substituents template =
    foldl (\acc (placeholder, sub) -> T.replace placeholder sub acc) template pairs
    where
        pairs = zip placeholders substituents