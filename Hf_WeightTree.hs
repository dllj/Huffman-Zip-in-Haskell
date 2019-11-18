{-*
  * Master Parisien de Recherche en Informatique
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_WeightTree.hs,v 1.5 2011/04/27 04:42:57 denis7cordas Exp $
  *
  * Contents :
  *       Hf_WeightTree
  *
  *       Functions related to Weight Tree   .
  *
  * History :
  *   DJ20191116 : substitute fold' by Data.List.fold' 
  *   DJ20110406 : creation
  *
  * Note :
  *   ghc --make -fwarn-missing-signatures -Werror file.hs
  *
-}
module Hf_WeightTree (WeightTree(..), makeWeightTree)
  where
-- *****************************************************************************
import Data.List
import Data.Map as Map
-- *****************************************************************************
import Hf_Types
import Hf_WeightTable
-- *****************************************************************************
-- -----------------------------------------------------------------------------
{-- Weight tree
--}
data WeightTree = Eof
                | Leaf Symbol Weight
                | Node Weight WeightTree WeightTree deriving (Show)

type ForestOfWeightTree = [WeightTree]
-- -----------------------------------------------------------------------------
--  weightOfNode : extract weight value from node of WeighTree
weightOfNode
      :: WeightTree
      -> Weight
weightOfNode wt = case wt of
                   Leaf s w -> w
                   Node w wt1 wt2 -> w
                   Eof -> 0
-- -----------------------------------------------------------------------------
{- insertInForest : Insert a new node (leaf or tree) in the forest conserving
    the increasing order of the weight nodes.
-}
insertInForest
      :: WeightTree
      -> ForestOfWeightTree
      -> ForestOfWeightTree
insertInForest wt [] = [wt]
insertInForest wt (fwt:fwts) =
    if w <= (weightOfNode fwt) then (wt:fwt:fwts)
    else (fwt:(insertInForest wt fwts))
    where
      w = weightOfNode wt
-- foldl (\fwt (s,w)-> insertInForest (Leaf s w) fwt) [] [(23,2),(45,1),(67,1)]
-- -----------------------------------------------------------------------------
{- combinateWeightTree : take two nodes (leaf or tree) and make an unique tree
    adding a root with the sum of the weight of his two subtrees.
    The greater is put on second child.
-}
combinateWeightTree
      :: WeightTree
      -> WeightTree
      -> WeightTree
combinateWeightTree wt1 wt2 =
      if w1 < w2 then
        Node (w1 + w2) wt1 wt2
      else
        Node (w1 + w2) wt2 wt1
      where
            w1 = weightOfNode wt1
            w2 = weightOfNode wt2
-- combinateWeightTree (Leaf 67 1) (Leaf 23 2)
-- -----------------------------------------------------------------------------
{- makeWeightTree : From the weights table of the symbols encountered in the
    source file, we make a weight tree like this :
       - first we create a forest including all couples (symbol, weight)
         contained in the Table using the insert function which conserves the
         increasing order of the forest. The couple with weight=0 are not
         inserted.
       - then call the agregate function which extracts the first and second
         element of the ordered forest and combinates it the inserts the
         resulting subtree in the forest and calls itself recursively on the
         new forest.
-}
makeWeightTree
      :: WeightTable
      -> WeightTree
makeWeightTree wTable =
    head (agregateForest
            (Data.List.foldl' (\fwt (s, w)->if w > 0 then  insertInForest (Leaf s w) fwt
                                  else fwt)
                            [Eof]
                            (Map.toList wTable)))
  where
    agregateForest :: ForestOfWeightTree -> ForestOfWeightTree
    agregateForest [wt] = [wt]
    agregateForest (wt1:wt2:fwt) =
        agregateForest (insertInForest (combinateWeightTree wt1 wt2) fwt)
-- makeWeightTree  (Map.fromList  [(23,2),(45,1),(67,1)])
