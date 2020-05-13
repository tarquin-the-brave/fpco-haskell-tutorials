{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import RIO.Process
import qualified Data.ByteString.Lazy.Char8 as C
import qualified RIO.List as L
import qualified RIO.ByteString.Lazy as BL

run :: RIO App ()
run = do
  path <- fmap (optionsPath . appOptions) ask
  out <- proc "git" ["ls-files", "-z", path] readProcessStdout_
  let files = C.split '\NUL' out
  let mdFiles = L.filter (BL.isSuffixOf (".md" :: BL.ByteString)) files
  _ <- mapM (logInfo . fromString . show) mdFiles
  return ()

