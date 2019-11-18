{-* 
  * Master Parisien de Recherche en Informatique 
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_CodingTable.hs,v 1.11 2011/04/27 04:42:57 denis7cordas Exp $
  *
  * Contents : 
  *       Hf_CodingTable
  * 
  *       Functions to make and access the  Coding Table of Symbols   .
  *  
  * History :
  *   DJ20110406 : creation
  *
  * Note :
  *   ghc --make -fwarn-missing-signatures -Werror file.hs 
  *
-}
module Hf_CodingTable (CodingTable, Coding(..), 
                        convertBitsCodingInBitsFlow,
                        convertBitsFlowInBitsCoding,
                        convertBitsSerieInBitsCoding,
                        idxEofCode,
                        makeCodingTable
                       ) 
  where 
-- *****************************************************************************
import Data.Map as Map
import Data.Bits
import Data.Int
import Data.Word
import Data.List as List
import Debug.Trace
-- *****************************************************************************
import Hf_Types
import Hf_WeightTree
import Hf_BitsFlow
-- *****************************************************************************
-- -----------------------------------------------------------------------------
{- Definition of Symbol coding :
    Symbol's code is a list of bits. The lenght of this list is variable. 
    So symbol's code is a couple (n, l) where the first element is the number
    of bits of coding and the second one is the very list of bytes associated.
    There are 257 elements in this Coding table including the EOF symbol.
-}
data Coding   = BitsCoding Int ByteFlow deriving (Show)

-- table of symbols coding  
type CodingTable = Map.Map Int Coding
{- idxEofCode : It has to be an integer > 255
-}
idxEofCode :: Int
idxEofCode = 256
-- -----------------------------------------------------------------------------
{- convertBitsCodingInBitsFlow : Convert length in bits to # bits used in the 
   last byte of BitsFlow.
-}
convertBitsCodingInBitsFlow 
      :: Coding 
      -> BitsFlow
convertBitsCodingInBitsFlow (BitsCoding n bF) | (n`mod`8 == 0) = (8, bF)-- -- 
convertBitsCodingInBitsFlow (BitsCoding n bF) = (n`mod`8, bF)-- -- 
-----------------------------------------------------------------------------
{- convertBitsSerieInBitsCoding : Convert length in bits to # bits used in the 
   last byte of BitsFlow.
-}
convertBitsSerieInBitsCoding 
      :: BitsSerie 
      -> Coding
convertBitsSerieInBitsCoding  bS = 
  convertBitsFlowInBitsCoding
                     (agregateBitsSerieInBitsFlow emptyBitsFlow bS) 

-----------------------------------------------------------------------------
{- convertBitsFlowInBitsCoding : Convert length in bits to # bits used in the 
   last byte of BitsFlow.
-}
convertBitsFlowInBitsCoding 
      :: BitsFlow 
      -> Coding
convertBitsFlowInBitsCoding  (n , bF) = 
              -- trace (" n= "++ (show n) ++"-"++(show bF))
                     (BitsCoding (((List.length bF) -1 ) * 8 + n) bF) -----------------------------------------------------------------------------
boolToWord8 
      :: Bool 
      -> Int 
      -> Word8 
boolToWord8 b i = if b then (setBit 0 pos) else 0 where pos = (i `mod` 8)
-- -----------------------------------------------------------------------------
{-- delBitInCodeLastByte : del the last bit in coding.
--}
delBitInCodeLastByte 
      :: Coding 
      -> Coding
delBitInCodeLastByte  (BitsCoding n lb) 
                | (n == 0) = (BitsCoding n lb)
delBitInCodeLastByte  (BitsCoding n []) -- to avoid bad case using init 
                | (n > 0) = (BitsCoding (n-1) [])
delBitInCodeLastByte  (BitsCoding n ([bs])) 
                | (n > 0) = (BitsCoding (n - 1) ([bs]))
delBitInCodeLastByte  (BitsCoding n bs) 
                | (n > 0) = 
      if ((n-1) `mod` 8 /= 0 ) then 
        (BitsCoding (n - 1) bs)
      else
        (BitsCoding (n-1) (init bs))

-- -----------------------------------------------------------------------------
{-- addBitInCodeLastByte : add a new bit in the Last Byte used in the BitsCoding 
    of symbol. We assume that the last byte is used so at this step the value
    of the length of the coding given is not verified compliant with the length
    of byte list. 
    In order to optimize we consider in particular the case where the code
    contains only one byte. 
  -----------------------------------------------------------------------------
  list of parameters :
      - boolean value : the value of the bit to set.
      - source Code
  -----------------------------------------------------------------------------
  return : resulting Code
  
--}
addBitInCodeLastByte 
      :: Bool 
      -> Coding 
      -> Coding
addBitInCodeLastByte b (BitsCoding n _) 
                | (n == 0) = BitsCoding 1 [(boolToWord8 b 7)]
addBitInCodeLastByte b (BitsCoding n []) -- to avoid bad case using init 
                | (n > 0) = BitsCoding 1 [(boolToWord8 b 7)]
addBitInCodeLastByte b (BitsCoding n ([bs])) 
                | (n > 0) = 
      if n  < 8 then 
        (BitsCoding (n+1) ([(bs .|. (boolToWord8 b pos))]))
      else
        (BitsCoding (n+1) (bs:[(boolToWord8 b 7)]))
      where
        pos = (8 - ((n+1) `mod` 8))
addBitInCodeLastByte b (BitsCoding n (bs)) 
                | (n > 0) = 
      if n  < (8 * (length bs)) then 
        (BitsCoding (n+1) (h ++ [(fs .|. (boolToWord8 b pos))]))
      else
        (BitsCoding (n+1) (bs ++ [(boolToWord8 b 7)]))
      where
        fs  = List.last bs
        h   = List.init bs 
        pos = (8 - ((n+1) `mod` 8))
-- -----------------------------------------------------------------------------
{-- addBitInCodeFirstByte : add a new bit in the first Byte of the BitsCoding 
    of symbol. 
    It is the "reverse byte" of code which is used at this step. This can be
    useful in order to optimize large code construction.
  -----------------------------------------------------------------------------
  list of parameters :
      - boolean value : the value of the bit to set.
      - source Code
  -----------------------------------------------------------------------------
  return : resulting Code
  
--}
addBitInCodeFirstByte 
      :: Bool 
      -> Coding 
      -> Coding
addBitInCodeFirstByte b (BitsCoding n _) 
                | (n == 0) = BitsCoding 1 [(boolToWord8 b 7)]
addBitInCodeFirstByte b (BitsCoding n (fs:bs)) 
                | (n > 0) = 
      if n  < (8*(1+(length bs))) then 
        (BitsCoding (n+1) ((fs .|. (boolToWord8 b pos)):bs))
      else
        (BitsCoding (n+1) ((boolToWord8 b 7):fs:bs))
      where pos = (8 - ((n+1) `mod` 8))
-- -----------------------------------------------------------------------------
reverseCode 
      :: Coding 
      -> Coding
reverseCode (BitsCoding n bs) = (BitsCoding n (List.reverse bs))
-- -----------------------------------------------------------------------------
{-- makeCodingTable : From the WeightTree this function calculates the 
    CodingTable which associates for each Symbol a BitsCoding that is a bits 
    list. Eof is part of this table. Eof is known like idxEofCode.  
  -----------------------------------------------------------------------------
  list of parameters :
      - the weight tree
  -----------------------------------------------------------------------------
  return : resulting Coding table.
  
--}
makeCodingTable 
      :: WeightTree 
      -> CodingTable
makeCodingTable  wt = (makeCodingTable_aux wt (BitsCoding 0 []) Map.empty)
  where 
makeCodingTable_aux  :: WeightTree -> Coding -> CodingTable ->  CodingTable
makeCodingTable_aux wt code ctable = 
  case wt of
    Eof            -> Map.insert idxEofCode (reverseCode code) ctable
    Leaf s _       -> Map.insert (fromIntegral s) (reverseCode code) ctable    
    Node _ wt1 wt2 -> Map.fromList (
      (Map.toList (makeCodingTable_aux wt1 
                            (addBitInCodeFirstByte True code) ctable)) ++  
      (Map.toList (makeCodingTable_aux wt2 
                            (addBitInCodeFirstByte False  code) ctable)) 
      )

