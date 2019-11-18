{-* 
  * Master Parisien de Recherche en Informatique 
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_Types.hs,v 1.7 2011/04/27 04:42:57 denis7cordas Exp $
  *
  * Contents : 
  *       Hf_Types
  * 
  *       The basic types used in Huffman Haskell project.
  *  
  * History :
  *   DJ20110406 : creation
  *
  * Note :
  *   ghc --make -fwarn-missing-signatures -Werror file.hs 
  *
-}

module Hf_Types (Symbol, SymbolsFlow, Weight, ByteFlow, BitsFlow, Bit, 
                  BitsSerie, 
                  bitsMasqH, 
                  bitsMasqL,
                  commonSymbolWithEof, 
                  emptyBitsFlow,
                  magicBytesInBitsFlow, 
                  packetSize) 
  where
-- *****************************************************************************
import Data.Word
import Data.Bits
import Data.Int
-- *****************************************************************************
-- -----------------------------------------------------------------------------
-- symbols are not algebric bytes - only 8 bits.
type Symbol       = Word8
type SymbolsFlow  = [Symbol]
type ByteFlow     = [Word8]
-- Symbol frequency is a number 
type Weight       = Integer
type BitsFlow     = (Int, ByteFlow)  -- Int = the number of bits used in the last 
                                -- byte in the list. This value is [1..8] 
type Bit          = Int   -- value possible calculed by (x .&. 1)  And bitwise operator
type BitsSerie    = [Bit] 

magicBytesInBitsFlow :: BitsFlow
magicBytesInBitsFlow = (8, [0X87, 0X4A, 0X1F,0X48])
emptyBitsFlow :: BitsFlow
emptyBitsFlow = (0, [])
commonSymbolWithEof :: Symbol
commonSymbolWithEof = 0xFF

-- Binary useful constants 
bitsMasqH :: Int -> Word8
-- met à 1 les n bits de poids forts
bitsMasqH n = shiftL 255 n
bitsMasqL :: Int -> Word8
-- met à 1 les n bits de poids faibles
bitsMasqL n = complement (bitsMasqH n)
-- -----------------------------------------------------------------------------
{- packetSize : Size of the internal lists receiving bytes from the file 
   contents. This value has been caculate by tests on huge set of data in order
   to optimize compression pass.    
-}
packetSize :: Int64
packetSize = 16

