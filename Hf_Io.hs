{-* 
  * Master Parisien de Recherche en Informatique 
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_Io.hs,v 1.4 2011/04/09 12:00:08 denis7cordas Exp $
  *
  * Contents : 
  *       Hf_Io
  * 
  *       Parameters used to Files access.
  *  
  * History :
  *   DJ20110406 : creation
  *
  * Note :
  *   ghc --make -fwarn-missing-signatures -Werror file.hs 
  *
-}

module Hf_Io (readBufferSize, writeBufferSize) 
  where
-- *****************************************************************************
import Data.Int
-- *****************************************************************************
{- readBufferSize & writeBufferSize : in order to optimize hard disk access.
-}
readBufferSize :: Int64 
readBufferSize = 4096
writeBufferSize :: Int64 
writeBufferSize = 4096


