{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import System.Environment (lookupEnv)
main :: IO ()
main = do 
  port <- fmap (maybe 3000 read) (lookupEnv "PORT")
  scotty 3000 $ do
    get "/1" $
      html "1"
    get "/2" $
      html "2"
    get "/" $
      html "rest"
