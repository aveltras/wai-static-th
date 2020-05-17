~~~haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Static.TH

mkStaticApp "src"

main :: IO ()
main = do

  print $ main_hs -- Main-8f7bcdcb75701aa06d4064149d1159dc.hs
  print $ network_wai_static_th_hs -- Network/Wai/Static/TH-b852dc8617e5f58f8b9c0724ba2e8455.hs

  let reloadOnChange = False

  run 3000 $ staticApp reloadOnChange
~~~
