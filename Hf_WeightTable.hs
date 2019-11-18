{-*
  * Master Parisien de Recherche en Informatique
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_WeightTable.hs,v 1.4 2011/04/27 04:42:57 denis7cordas Exp $
  *
  * Contents :
  *       Hf_WeightTable
  *
  *       Functions to character frequency computation  .
  *
  * History :
  *   DJ20191116 : Modify the deprecated Data.Map module by Data.Map.Strict and
  *                the deprecated insertWith' function by  insertWith
  *
  *   DJ20110406 : creation
  *
  * Note :
  *   ghc --make -fwarn-missing-signatures -Werror file.hs
  *
-}
module Hf_WeightTable (WeightTable, computeWeightTable)
  where
-- *****************************************************************************
import Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as Byte
-- *****************************************************************************
import Hf_Types
import Hf_Io
-- *****************************************************************************

-- -----------------------------------------------------------------------------
{-- Tables  :
    In ordrer to optimize the run-time we implement the list using Map module
    which is internally implemented with trees.
--}
-- table of symbols frequency
type WeightTable = Map.Map Symbol   Weight

-- -----------------------------------------------------------------------------
{-- calculateWeight : makes a table which associates for each symbol the number
    of his occurences in the source file given by the bytes flow.
    Avantage of using foldl' than code that uses explicit recursion. The foldl'
    that is similar to foldl, does not build up thunks. So stackoverflow error
    can be avoid.
--}
incrWeight
      :: WeightTable
      -> Symbol
      -> WeightTable
incrWeight wt symb = wt`seq`Map.insertWith (+) symb 1 wt
calculateWeight :: WeightTable -> Byte.ByteString -> WeightTable
calculateWeight wt symbl  =
          Byte.foldl' (\x y-> x`seq`y`seq`incrWeight x y ) wt symbl
-- -----------------------------------------------------------------------------
initWeightTable :: WeightTable
initWeightTable = (Map.fromList [(fromInteger n,0) |n <-[0..255]])
computeWeightTable :: Byte.ByteString-> [(Symbol,Weight)]
computeWeightTable contents = wt`seq`Map.toList $ aux1 contents wt
        where
          wt = initWeightTable
          aux1 :: Byte.ByteString-> WeightTable -> WeightTable
          aux1 c wt | Byte.null c = wt
          aux1 c wt =
                    let
                      (getByte , getContinuation) = Byte.splitAt packetSize c
                      wt_aux = getByte`seq`calculateWeight  wt getByte
                    in wt_aux`seq`getContinuation`seq`aux1 getContinuation wt_aux
