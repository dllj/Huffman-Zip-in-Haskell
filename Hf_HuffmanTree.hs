{-* 
  * Master Parisien de Recherche en Informatique 
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_HuffmanTree.hs,v 1.13 2011/04/27 04:42:57 denis7cordas Exp $
  *
  * Contents : 
  *       Hf_HuffmanTree
  * 
  *       Functions related to huffman Tree read and construction from the 
  *       Zip File  .
  *  
  * History :
  *   DJ20110406 : creation
  *
  * Note :
  *   ghc --make -fwarn-missing-signatures -Werror file.hs 
  *
-}
module Hf_HuffmanTree (HuffmanTree(..), readHuffmanTree) 
  where 
-- *****************************************************************************
import qualified Data.ByteString.Lazy as Byte  
import Data.List as List
import Debug.Trace
-- *****************************************************************************
import Hf_Types
import Hf_CodingTable
import Hf_BitsFlow
-- *****************************************************************************
-- -----------------------------------------------------------------------------
data HuffmanTree = HuffmanTreeEmpty
                 | LeafEof 
                 | Leaf Symbol   
                 | Node HuffmanTree HuffmanTree deriving (Show)
type HuffmanFlatTree = [(Int, Int)]
-- -----------------------------------------------------------------------------
{- makeHuffmantree : After the huffman tree read from the file and saved in the
   list of nodes called Flat tree, the function fold the elements of the list
   and create the nodes of the huffmanTree which will be used to uncompress
   the file.
   Particular node is created when the Eof symbol is found in the list. 
-}
makeHuffmantree
      :: HuffmanFlatTree 
      -> HuffmanTree   
makeHuffmantree []  = HuffmanTreeEmpty 
makeHuffmantree hFtree = fst (makeHuffmantree_aux hFtree HuffmanTreeEmpty) 
makeHuffmantree_aux 
      :: HuffmanFlatTree  
      -> HuffmanTree 
      -> (HuffmanTree, HuffmanFlatTree) 
makeHuffmantree_aux [] ht = (ht, []) 
makeHuffmantree_aux ((node_type,symbol):hft) ht 
      | node_type == 1  = 
         ((Node htchild_L htchild_R),newhft_R)
      | node_type == 0  = if symbol == idxEofCode then (LeafEof , hft)
                          else ((Leaf (fromIntegral symbol)), hft)  
      where (htchild_L, newhft_L) = makeHuffmantree_aux hft ht
            (htchild_R, newhft_R) = makeHuffmantree_aux newhft_L ht

-- -----------------------------------------------------------------------------
{- readHuffmanTree : We assume that the begining of huffman tree is aligned with
   the first hight bit of the first byte of contents. The read is done bit 
   by bit into a Bits Flow . 
-}
readHuffmanTree 
      ::  Byte.ByteString 
      -> (Bool, (BitsFlow, Byte.ByteString), HuffmanTree)   
readHuffmanTree contents = 
        (True, ((concateBitsFlow 
                (agregateBitsSerieInBitsFlow emptyBitsFlow bS) bF), cont), 
                                                        makeHuffmantree hFT)
  where
    (_,_,(bS, bF, cont), hFT) =  
            makeHuffmanFlatTree  (True, [],([], emptyBitsFlow, contents), []) 
 -----------------------------------------------------------------------------
{- makeHuffmanFlatTree : From the header of the zip file, this function read
   bit by bit using the getNextBits function. This function make a list of the
   nodes of the Huffman tree. This list is called the flat HuffmanTree. More 
   later this list has to be fold to make the real huffman tree.
-}
makeHuffmanFlatTree 
      :: (Bool,[Int], (BitsSerie,BitsFlow, Byte.ByteString) , HuffmanFlatTree) 
      -> (Bool,[Int], (BitsSerie,BitsFlow, Byte.ByteString) , HuffmanFlatTree)
makeHuffmanFlatTree (child,stack, continuation, hft) = 
        case getNextBits continuation 1 of
          Nothing -> error "EofNotExpected in makeHuffmanFlatTree"
          Just ([1], newContinuation) -> 
              makeHuffmanFlatTree 
                      (not child, (1:stack), newContinuation, hft ++ [(1,0)])
          Just ([0], newContinuation) ->
             if ( child) then 
                  if (List.length stack) == 0 then gs (child, stack,newContinuation, hft)
                  else makeHuffmanFlatTree (gs (not child,List.tail stack, newContinuation, hft))
             else  
                 if (List.length stack) == 0 then gs (child, stack,newContinuation, hft)
                 else makeHuffmanFlatTree (gs (not child,List.tail stack, newContinuation, hft))
  where
    gs :: (Bool,[Int], (BitsSerie,BitsFlow, Byte.ByteString) , HuffmanFlatTree) 
       -> (Bool,[Int], (BitsSerie,BitsFlow, Byte.ByteString) , HuffmanFlatTree)
    gs (child,stack,  continuation, hft) = 
        case getNextBits continuation 8 of
          Nothing -> error "EofNotExpected in makeHuffmanFlatTree (gs1) "
          Just (bS, newContinuation) -> 
              (child, stack, newCont, hft ++ [(0, code)])
                where code_aux  = convert8BitsSerieInInt32 bS
                      (code, newCont)= 
                        if code_aux == (fromIntegral commonSymbolWithEof) then 
                          case getNextBits newContinuation 1 of
                           Nothing -> error "EofNotExpected in makeHuffmanFlatTree (gs2) "
                           Just ([1], newCont_aux) -> --trace ("new cont aux ="++ (show newCont_aux))
                                                          (idxEofCode, newCont_aux)
                           Just ([0], newCont_aux) -> --trace ("new cont aux ="++ (show newCont_aux))
                                                          ((fromIntegral commonSymbolWithEof), newCont_aux)
                        else (code_aux, newContinuation) 

