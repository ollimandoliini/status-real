{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/1" $
    html "1"
  get "/2" $
    html "2"
  get "/" $
    html "rest"
