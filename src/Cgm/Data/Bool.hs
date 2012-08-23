module Cgm.Data.Bool(
  bool
  ) where
       
bool :: z -> z -> Bool -> z
bool f t c = if c then t else f
