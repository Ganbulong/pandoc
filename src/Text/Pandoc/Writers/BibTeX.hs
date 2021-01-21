{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.BibTeX
   Copyright   : Copyright (C) 2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Writes a BibTeX or BibLaTeX bibliographies based on the
'references' metadata in a Pandoc document.
-}
module Text.Pandoc.Writers.BibTeX
  ( writeBibTeX
  , writeBibLaTeX
  )
where

import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Builder (setMeta, cite, str)
import Data.Text (Text)
import Data.Maybe (fromMaybe, mapMaybe)
import Citeproc (Lang(..), parseLang)
import Citeproc.Locale (getLocale)
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Class (PandocMonad, lookupEnv)
import Text.Pandoc.Citeproc.BibTeX as BibTeX
import Text.Pandoc.Citeproc.MetaValue (metaValueToReference)
import Control.Monad.Except (throwError)

-- | Write BibTeX based on the references metadata from a Pandoc document.
writeBibTeX :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeBibTeX = writeBibTeX' BibTeX.Bibtex

-- | Write BibLaTeX based on the references metadata from a Pandoc document.
writeBibLaTeX :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeBibLaTeX = writeBibTeX' BibTeX.Biblatex

writeBibTeX' :: PandocMonad m => Variant -> WriterOptions -> Pandoc -> m Text
writeBibTeX' variant opts (Pandoc meta _) = do
  lang <- maybe (Lang "en" (Just "US")) parseLang
             <$> lookupEnv "LANG"
  locale <- case getLocale lang of
               Left e  ->
                 case getLocale (Lang "en" (Just "US")) of
                   Right l -> return l
                   Left _  -> throwError $ PandocCiteprocError e
               Right l -> return l
  let refs = case lookupMeta "references" meta of
               Just (MetaList xs) -> mapMaybe metaValueToReference xs
               _ -> []
  return $ mconcat $
    map (BibTeX.writeBibtexString variant locale) refs

