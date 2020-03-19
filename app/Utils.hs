module Utils (zipWith') where

import Data.Sequence (Seq, (<|), ViewL((:<)), viewl, viewr)
import qualified Data.Sequence as S (Seq(Empty))

zipWith' :: (a -> a -> a) -> Seq a -> Seq a -> Seq a
zipWith' fn S.Empty xb = xb
zipWith' fn xa S.Empty = xa
zipWith' fn xa xb =
  let headA :< tailA = viewl xa
      headB :< tailB = viewl xb
  in  fn headA headB <| zipWith' fn tailA tailB
