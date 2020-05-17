{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Wai.Static.TH
  ( mkStaticApp
  ) where

import           Control.Monad              (forM)
import           Crypto.Hash                (Digest, MD5, hash)
import qualified Data.ByteString            as BS
import           Data.ByteString.Char8      (unpack)
import qualified Data.ByteString.Lazy       as BL
import           Data.ByteString.Lazy.Char8 (fromStrict)
import           Data.Char                  (toLower)
import           Data.FileEmbed             (bsToExp, getDir,
                                             makeRelativeToProject)
import           Data.String                (IsString)
import           Data.Text                  (pack)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (addDependentFile)
import           Network.HTTP.Types         (hContentType)
import           Network.HTTP.Types         (ok200)
import           Network.HTTP.Types.Header  (hCacheControl)
import           Network.HTTP.Types.Status  (notFound404)
import           Network.Mime               (defaultMimeLookup)
import           Network.Wai                (Application, rawPathInfo,
                                             responseLBS)
import           System.FilePath            (replaceBaseName, takeBaseName,
                                             (</>))

data Entry = Entry
  { entryContent      :: Exp
  , entryMimeType     :: String
  , entryPath         :: FilePath
  , entryPathRelative :: FilePath
  , entryHashedName   :: FilePath
  }

mkStaticApp :: FilePath -> Q [Dec]
mkStaticApp path = do
  directory <- makeRelativeToProject path
  files <- runIO . getDir $ directory
  entries <- forM files $ mkEntry directory
  pure $ (concat $ mkLink <$> entries) <> (mkApp (mkName "staticApp") entries)

mkEntry :: FilePath -> (FilePath, BS.ByteString) -> Q Entry
mkEntry directory (name, content) = do
  addDependentFile $ directory </> name
  contentExp <- bsToExp content
  pure $ Entry
    contentExp
    (unpack . defaultMimeLookup . pack $ name)
    (directory </> name)
    name
    (replaceBaseName name (takeBaseName name <> "-" <> (show (hash content :: Digest MD5))))

mkLink :: Entry -> [Dec]
mkLink Entry {..} =
  let funcName = mkName $ toFunChar <$> entryPathRelative
  in [ SigD funcName $ ForallT [] [AppT (ConT ''IsString) (VarT $ mkName "a")] (VarT $ mkName "a")
     , FunD funcName $ [ Clause [] (NormalB $ LitE $ StringL entryHashedName) [] ]
     ]

toFunChar :: Char -> Char
toFunChar = \case
  '.' -> '_'
  '-' -> '_'
  '/' -> '_'
  c -> toLower c

mkApp :: Name -> [Entry] -> [Dec]
mkApp appName entries =
  let mkClause (pat, fn) = Clause [ConP pat [], VarP $ mkName "request", VarP $ mkName "respond"] (NormalB $ CaseE (AppE (VarE 'rawPathInfo) (VarE $ mkName "request")) ((fn <$> entries) ++ [mkNoFileMatch])) []
  in [ SigD appName $ InfixT (ConT ''Bool) ''(->) (ConT ''Application)
     , FunD appName $ mkClause <$> [('True, mkFileMatchReload), ('False, mkFileMatchNoReload)]
     ]

mkMatch :: Pat -> (Exp -> Exp) -> Name -> Exp -> Exp -> Match
mkMatch filenameP contentE status headersE responseE =
  Match filenameP (NormalB $
    contentE $
    AppE (VarE $ mkName "respond")
         (AppE (AppE (AppE (VarE 'responseLBS)
                           (VarE status))
                     headersE)
               responseE)) []

mkNoFileMatch :: Match
mkNoFileMatch =
  mkMatch WildP id 'notFound404 (ConE '[]) (LitE $ StringL "Resource not found")

mkFileMatchHeaders :: String -> String ->  Exp
mkFileMatchHeaders mimeType cacheControl =
  ListE [ TupE [VarE 'hContentType, LitE $ StringL mimeType]
        , TupE [VarE 'hCacheControl, LitE $ StringL cacheControl]
        ]

mkFileMatchReload :: Entry -> Match
mkFileMatchReload Entry {..} =
  mkMatch (LitP $ StringL $ "/" <> entryHashedName)
          (UInfixE (AppE (VarE 'BL.readFile) (LitE $ StringL entryPath)) (VarE '(>>=)) . LamE [VarP $ mkName "lazyBS"])
          'ok200
          (mkFileMatchHeaders entryMimeType "no-store")
          (VarE $ mkName "lazyBS")

mkFileMatchNoReload :: Entry -> Match
mkFileMatchNoReload Entry {..} =
  mkMatch (LitP $ StringL $ "/" <> entryHashedName)
          id
          'ok200
          (mkFileMatchHeaders entryMimeType "public, max-age=31536000")
          (AppE (VarE 'fromStrict) entryContent)
