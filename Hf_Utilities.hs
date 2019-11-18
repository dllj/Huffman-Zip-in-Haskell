{-* 
  * Master Parisien de Recherche en Informatique 
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_Utilities.hs,v 1.1 2011/04/09 12:00:40 denis7cordas Exp $
  *
  * Contents : 
  *       Hf_Utilities
  * 
  *       Few little tools.
  *  
  * History :
  *   DJ20110406 : creation
  *
  * Note :
  *   ghc --make -fwarn-missing-signatures -Werror file.hs 
  *
-}
module Hf_Utilities (isPrefixList, isEqualList) 
  where 
-- *****************************************************************************
import Data.List
-- *****************************************************************************
-- -----------------------------------------------------------------------------
isEqualList :: Eq a => [a] -> [a] -> Bool
isEqualList l1 l2 = if length l1 == length l2 then isPrefixList l1 l2 else False
-- -----------------------------------------------------------------------------
isPrefixList :: Eq a => [a] -> [a] -> Bool
isPrefixList l1 l2 = foldl' (\x (a , b) -> x && (a == b)) True (zip l1 l2) 

