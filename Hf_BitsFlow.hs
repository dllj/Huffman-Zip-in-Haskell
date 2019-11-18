{-* 
  * Master Parisien de Recherche en Informatique 
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_BitsFlow.hs,v 1.9 2011/04/27 04:42:57 denis7cordas Exp $
  *
  * Contents : 
  *       Hf_BitsFlow
  * 
  *       Functions related to  BitsFlow. 
  *  
  * History :
  *   DJ20110407: creation
  *
  * Note :
  *   ghc --make -fwarn-missing-signatures -Werror file.hs 
  *
-}
module Hf_BitsFlow (agregateBitsSerieInBitsFlow, 
                    concateBitsFlow, 
                    convertBitsSerieInInt32,
                    convert8BitsSerieInInt32,
                    explodeBitsFlowInBitsSerie,
                    getBitsFlowFromByteString,
                    getBytesFlowFromByteString,
                    getNextBits
                   ) 
  where 
-- *****************************************************************************
import Data.Bits
import Data.Word
import Data.Int
import Data.List as List
import qualified Data.ByteString.Lazy as Byte  
import Debug.Trace 
-- *****************************************************************************
import Hf_Types
--------------------------------------------------------------------------------
{-
   In order to uncompress the zip file we need to read the content bit by bit.
   What is exactly the reading of the file in this program ? How do we perform 
   a read bit by bit ? 
   We use the readFile lazy haskell function that in fact prepared the futur 
   real reading of the file. When it is needed by the program, a part of file is
   loaded in Ram and become available to the user in one "ByteString" structure 
   on which we can use some other related function like "ByteString.unpack", 
   "ByteString.foldl'" in order to access to the content byte by byte. The byte
   (unsigned char in C) in haskell is called Word8. But as we have seen the zip
   file structure implies a bit/bit reading in fact it is reading the file bits 
   by bits that the real size of each part (huffmantree or compressed data) is 
   discovered. To implement the bit reading we used a structure  called here
   "continuation" that is in fact a couple.The second element of the couple is 
   the reference of the "ByteString content" about which we were saying above 
   and the first is the list of the last bytes that have been unpacked from the
   previous content. The first element is a BitsFlow structure that is in fact 
   a couple (n, l) where l is the list of bytes and n is the number of the bits
   used in the last byte of the list. So, to access the n next bits we have to 
   see if exists unpacked bytes in the BitsFlow and if necessarly unpack enought
   bytes from the content and concate the unpacked byte to the current BitsFlow.
   Then we are able to transform the BitsFlows in the BitsSeries structure that
   is a list of integer each integer representing the value of the bit at this 
   index in hte list. In all this program the order of the bits increase from 
   left to right ie from the higher pârt of byte to the lower part.     
-}
--------------------------------------------------------------------------------
{- getNextBits : Provides the N next bits from the continuation defined by
   the followings data :
                BitSeries + BitsFlow + ByteString.
   Typically this function is called during the reading of the zip file, when 
   the huffman tree is loaded and when the data are uncompressed.
   Optimization done for the critic value N = 1.
  -----------------------------------------------------------------------------
  list of parameters :
      - continuation : tuple including BitsSeries, BitsFlow, ByteString to be
                       next read
      - nb  : number of bits wanted
  -----------------------------------------------------------------------------
  return : Nothing or Just
      - Bits list expected by the caller
      - continuation to be read from file
-}
getNextBits 
      :: (BitsSerie,BitsFlow, Byte.ByteString) 
      -> Int
      -> Maybe (BitsSerie, (BitsSerie,BitsFlow, Byte.ByteString))
getNextBits ([], (nbF, lbF), cont) nb     
        |(nb == 1) && (nbF > 0) = Just ([b],(ll ,emptyBitsFlow, cont))
                where (b:ll) = explodeBitsFlowInBitsSerie [] (nbF, lbF)       
getNextBits ([b], bF, cont) nb    | nb == 1 = Just ([b],([] ,bF, cont))
getNextBits ((b:ll), bF, cont) nb | nb == 1 = Just ([b],(ll ,bF, cont))
getNextBits (bS, bF, cont) nb | (List.length bS) - nb  < 0  =
        case getBitsFlowFromByteString (bF,cont) 
                        (fromIntegral (nb - (List.length bS))) of
          Nothing -> Nothing
          Just (bF1, (bF2,newcont)) -> Just (bb,(ll ,bF2, newcont))
                where (bb,ll) = List.splitAt nb 
                                    (bS ++ (explodeBitsFlowInBitsSerie [] bF1))       
getNextBits (bS, bF, cont) nb     | nb == 0 = Just ([], (bS, bF, cont))
getNextBits (bS, bF, cont) nb     | nb  > 1 = Just (bb,(ll ,bF, cont))
                where (bb,ll) = List.splitAt nb bS      --
-----------------------------------------------------------------------------
{- convertBitsSerieInInt : Convert length in bits to # bits used in the 
   last byte of BitsFlow.
  -----------------------------------------------------------------------------
  list of parameters :
      - BitsSeries to convert.
  -----------------------------------------------------------------------------
  return : the Int resulting from the conversion.
-}
convertBitsSerieInInt32 
      :: BitsSerie 
      -> Int
convertBitsSerieInInt32  bS = 
            foldl' (\x (y, pw) -> x + (y*2^pw)) 0 (zip (List.reverse h) [0..31])
      where (h, l) = List.splitAt 32 bS 
-----------------------------------------------------------------------------
{- convert8BitsSerieInInt : Convert 8 bits to # bits used in the 
   last byte of BitsFlow.
-}
convert8BitsSerieInInt32 
      :: BitsSerie 
      -> Int
convert8BitsSerieInInt32  bS =
              foldl' (\x (y,z) -> x + (y*z)) 0 (zip (List.reverse bS) 
                                                    [1,2,4,8,16,32,64,128])

-- ----------------------------------------------------------------------------
{- agregateBitsSerieInBitsFlow : This function prepare the BitsFlow, to be write 
   in file, from the BitsSeries which is a list of binary values.
   In order to optimize time we reverse the original BitsFlow before the scan
   of the BitsSeries and the agregation. At the end we reverse again the 
   BitsFlow to restitute the good order.  
   Optimize for bS = [0] or bS=[1]
  -----------------------------------------------------------------------------
  list of parameters :
      - bitsflow 
      - bitsSeries
  -----------------------------------------------------------------------------
  return : the resulting bitsflow  .
-}
agregateBitsSerieInBitsFlow 
      :: BitsFlow 
      -> BitsSerie 
      -> BitsFlow
agregateBitsSerieInBitsFlow (last, bF) [] =  (last, bF) 
agregateBitsSerieInBitsFlow (last, bF) bS = 
  let bF' = (last, List.reverse bF)
      (lst, bitsF) = foldl' (\x y -> agregate_aux x y) bF' bS
  in bF'`seq`(lst, bitsF)`seq`(lst, List.reverse bitsF)    
  where
    agregate_aux 
        :: BitsFlow 
        -> Bit
        -> BitsFlow
    agregate_aux  (_, [])    hs  = (1, [(addBit 0 0 hs)])
    agregate_aux  (0,  f)    hs  = (1, (addBit 0 0 hs):f)
    agregate_aux  (8,  f)    hs  = (1, (addBit 0 0 hs):f)                    
    agregate_aux  (l, hf:f)  hs  = (if (l + 1)`mod`8 == 0 then 8 
                                    else (l + 1)`mod`8, 
                                        (addBit hf (l `mod`8)  hs):f) 
    addBit byte pos value = if (testBit value 0) then 
                                byte `setBit` (7 - pos)
                              else
                                byte `clearBit` (7 - pos)
-- ----------------------------------------------------------------------------
{- explodeBitsFlowInBitsSerie : It's the reverse operation of the 
   agregateBitsSerieInBitsFlow. 
  -----------------------------------------------------------------------------
  list of parameters :
      - bitsSeries
      - bitsflow 
  -----------------------------------------------------------------------------
  return : the resulting bitsSeries.
-}
explodeBitsFlowInBitsSerie 
      :: BitsSerie 
      -> BitsFlow 
      -> BitsSerie
explodeBitsFlowInBitsSerie  bS (last, []) = bS
explodeBitsFlowInBitsSerie  bS (last, bF) = 
  let bS' = List.reverse bS 
      l = (length bF) - (if (last `mod` 8 == 0 ) then 0 else 1)
      (h, t) = splitAt l bF 
      bitsS = foldl' (\x y -> (splitBitsLower y 8 ) ++ x) bS' h
      bitslast = if( length t) >0 then (splitBitsLower (head t) last) else []
  in  bS ++ (List.reverse ( bitslast ++ bitsS ))
  where
    splitBitsLower :: Word8 -> Int -> BitsSerie
    splitBitsLower value significantBits = 
              foldl' (\x y -> if(testBit value (7-y)) then (1:x) else (0:x)) 
                              [] [n | n <- [0..7] , n < (significantBits) ]
-- ----------------------------------------------------------------------------
{- concateBitsFlow : Concate two given BitsFlows in one BitsFlow
  -----------------------------------------------------------------------------
  list of parameters :
      - 2 bitsflow to concate 
  -----------------------------------------------------------------------------
  return : the resulting bitsFlow.
-}
concateBitsFlow 
      :: BitsFlow 
      -> BitsFlow 
      -> BitsFlow 
concateBitsFlow (_,[]) bf2 = bf2
concateBitsFlow bf1 (_,[]) = bf1
concateBitsFlow (n1, lf1) (n2, lf2) 
      | n1 `mod` 8 == 0 = (n2, lf1 ++ lf2)
      | otherwise = concateBitsFlow_aux n1 rlf n2 lf2 
  where 
    rlf = List.reverse lf1 
    concateBitsFlow_aux :: Int -> ByteFlow -> Int -> ByteFlow -> BitsFlow 
    concateBitsFlow_aux n rlf ns [] = 
              let l = List.reverse rlf 
              in (n , l)  
    concateBitsFlow_aux n (b : rlf) ns (h:lf2) 
        | (length lf2 == 0) && (n + ns < 9) = 
              let l = List.reverse (((b .&. (bitsMasqH (8-n))) .|. 
                                     (shiftR (h.&. (bitsMasqH (8-ns))) n))
                                     :rlf)
              in ( n + ns, l)
        |  otherwise = 
              let nn = if (length lf2 > 0) then 8 else ns
                  x1 = ((b .&. (bitsMasqH (8 - n))) .|. (shiftR h n))
                  x2 = (shiftL h (8 - n) ) .&.(bitsMasqH (16 -(n+nn)))
              in concateBitsFlow_aux (nn -(8-n)) (x2:x1:rlf) ns lf2 
{- tests : 
1st case :  
*Hf_RWZipFile> concateBitsFlow (3,[170,160]) (4,[255,48])
(7,[170,191,230])
(0.02 secs, 1050920 bytes)
*Hf_RWZipFile> explodeBitsFlowInBitsSerie [] (7,[170,191,230])
[1,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1,1,1,1,0,0,1,1]
2nd case :
*Hf_RWZipFile> concateBitsFlow (3,[170,160]) (6,[255,58])
(6,[170,191,231,64])
(0.00 secs, 524912 bytes)
*Hf_RWZipFile> explodeBitsFlowInBitsSerie [] (6,[170,191,231,64])
[1,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1,1,1,1,0,0,1,1,1,0,1,0,0,0,0]
Limit case :

-}       
 -- ----------------------------------------------------------------------------
{- getBytesFlowFromByteString : Unpack bytes from the ByteString structure.
   
-}
getBytesFlowFromByteString 
      :: Byte.ByteString 
      -> Int64
      -> Maybe (ByteFlow, Byte.ByteString)
getBytesFlowFromByteString contents n | Byte.null contents = Nothing
getBytesFlowFromByteString contents n | n == 0 = Just ([], contents) 
getBytesFlowFromByteString contents n =
    let (getHead, getContinuation) = Byte.splitAt n  contents
    in getHead`seq`Just (Byte.unpack getHead, getContinuation)  
    
-- ----------------------------------------------------------------------------
{- getBitsFlowFromByteString : Unpack bytes and transform in BitsFlow. The 
   BitsFlow passed in parameter has to be contained the BitsFlow in used. Before
   unpack new bytes from the Bytestring the function first get the bits from
   the bitsFlow given by the caller.
   Optimization done for the critic value N = 1.
  -----------------------------------------------------------------------------
  list of parameters :
      - continuation : tuple including BitsSeries, BitsFlow, ByteString to be
                       next read
      - n : number of bits expected
  -----------------------------------------------------------------------------
  return : the resulting bitsFlow.
-}
getBitsFlowFromByteString 
      :: (BitsFlow, Byte.ByteString)
      -> Int64
      -> Maybe (BitsFlow, (BitsFlow, Byte.ByteString))
getBitsFlowFromByteString ((nbF,lbF), contents) n 
          | Byte.null contents && (nbF == 0) = Nothing
getBitsFlowFromByteString (bF, contents) n | n == 0 = 
                                      Just (bF, (emptyBitsFlow, contents)) 
getBitsFlowFromByteString ((nbF, lbF), contents) n  | n ==1 =
    if ((fromIntegral nn) > 0 ) || (((fromIntegral nn) == 0) && 
                                          (fromIntegral nbF) >= 1) then
      Just ((nbF, lbF), (emptyBitsFlow, contents))
    else
      Just (convertInBitsFlow,(emptyBitsFlow, getContinuation))
            where nn = List.length lbF -1
                  (getHead, getContinuation)= Byte.splitAt  packetSize contents
                  convertInBytes = Byte.unpack getHead
                  convertInBitsFlow = 
                          (if (List.length convertInBytes == 0) then 0 
                           else 8,convertInBytes)
getBitsFlowFromByteString ((nbF, lbF), contents) n | n > 1 =
    if ((fromIntegral nn) > nq ) || (((fromIntegral nn) == nq) && 
                                          (fromIntegral nbF) >= nr) then
      Just ((nbF, lbF), (emptyBitsFlow, contents))
    else
      Just (concatBf,(emptyBitsFlow, getContinuation))
            where nq = shiftR n 3 -- divide by 8
                  nr = n `mod` 8
                  nn = List.length lbF -1
                  nb_bytes_to_get = nq + 1
                  (getHead, getContinuation)  
                      = Byte.splitAt  (max packetSize nb_bytes_to_get) contents
                  convertInBytes = Byte.unpack getHead
                  convertInBitsFlow = 
                          (if (List.length convertInBytes == 0) then 0 
                           else 8,convertInBytes)
                  concatBf  = concateBitsFlow (nbF, lbF) convertInBitsFlow
