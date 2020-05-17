# wai-static-th

Simple Template Haskell helper to manage your WAI application static assets.

## Usage

Call **mkStaticApp** passing the path to your assets directory relative to cabal file location.  

This will generate a function **staticApp** which takes a Bool indicating whether your files should be reloaded when they change. This should be **True** in development and **False** in production.

Functions returning the hashed names are also generated (see example below).

The following is an example using this library's source files.

~~~haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Static.TH

mkStaticApp "src"

main :: IO ()
main = do
  print $ network_wai_static_th_hs -- Network/Wai/Static/TH-b852dc8617e5f58f8b9c0724ba2e8455.hs
  let reloadOnChange = False
  run 3000 $ staticApp reloadOnChange
~~~
