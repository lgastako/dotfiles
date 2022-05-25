# -*- mode: snippet -*-
# name: new-module.hs
# key: nm
# --
{-# LANGUAGE NoImplicitPrelude #-}

module $1.`(file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`
  ( $2
  ) where

import $1.Prelude

$0
