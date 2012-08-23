{-# LANGUAGE EmptyDataDecls #-}

module Cgm.Data.Empty(
  onØ,
  Ø
  ) where

data Ø

onØ :: Ø -> a
onØ _ = undefined
