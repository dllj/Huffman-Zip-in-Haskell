{-* 
  * Master Parisien de Recherche en Informatique 
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_PrivateBytes.hs,v 1.3 2011/04/28 10:49:43 denis7cordas Exp $
  *
  * Contents : 
  *       Hf_PrivateBytes
  * 
  *       Module can be used to use the memory space reserved into the 
  *       zip file. Two blocks of 257 bytes can be used. The first byte of each 
  *       block gives the number of following bytes. The following functions has
  *       to be modified.  
  *  
  * History :
  *   DJ20110406 : creation
  *
  * Note :
  *   ghc --make -fwarn-missing-signatures -Werror file.hs 
  *
-}
module Hf_PrivateBytes (doSomethingWithFirstPrivateBytes, 
                        doSomethingWithLastPrivateBytes,
                        getFirstPrivatesBytes,
                        getLastPrivatesBytes) 
  where 
-- *****************************************************************************
import Debug.Trace
-- *****************************************************************************
import Hf_Types
-- *****************************************************************************
-- -----------------------------------------------------------------------------
doSomethingWithFirstPrivateBytes :: BitsFlow ->  ()
doSomethingWithFirstPrivateBytes bF = ()
getFirstPrivatesBytes :: BitsFlow
getFirstPrivatesBytes = (8,[0x0C,0x44,0x4A,0x55,0x4C,0x49,0x45,0x4E,
                        0x2D,0x32,0x30,0x31,0x31])
-- -----------------------------------------------------------------------------
doSomethingWithLastPrivateBytes :: BitsFlow -> ()
doSomethingWithLastPrivateBytes bF = ()
getLastPrivatesBytes :: BitsFlow
getLastPrivatesBytes = (8,[0])

