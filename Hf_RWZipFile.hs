{-*
  * Master Parisien de Recherche en Informatique
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_RWZipFile.hs,v 1.18 2011/04/27 04:42:57 denis7cordas Exp $
  *
  * Contents :
  *       Hf_RWZipFile
  *
  *       Functions to read and write compressed file.
  *
  * History :
  *   DJ20110407: creation
  *
  * Note :
  *   ghc --make -fwarn-missing-signatures -Werror file.hs
  *
-}
module Hf_RWZipFile (writeHeaderOfFile, compressAndAppendFile, writeTailOfFile,
                    putInFileBitsFlow)
  where
-- *****************************************************************************
import Data.Bits
import Data.Char
import Data.Int
import Data.Word
import Data.List as List
import Data.Map as Map
import qualified Data.ByteString.Lazy as Byte
import System.IO
import Debug.Trace
-- *****************************************************************************
import Hf_Types
import Hf_WeightTree
import Hf_Io
import Hf_BitsFlow
import Hf_CodingTable
import Hf_PrivateBytes
-- *****************************************************************************

-- ----------------------------------------------------------------------------
{- writeHeaderOfFile : This function makes the header of file given by its
   handle. So the caller has to open inbinaryMode the file before the call.
   The header is made from the WeightTree and put into the file.
   Assume that the file does not exist. This has to be verified by the caller.
  -----------------------------------------------------------------------------
  list of parameters :
      - the handle of the target file
      - the Weight tree made before by the caller
  -----------------------------------------------------------------------------
  return : the BitsFlow continuation which has to be used by the next write in
  the file.
-}
writeHeaderOfFile
      :: Handle
      -> WeightTree
      -> IO BitsFlow
writeHeaderOfFile handle weightTree =
  (hSetBinaryMode handle True) >>
  (hSetBuffering handle (BlockBuffering (Just (fromIntegral writeBufferSize)))) >>
  ((writeBitsFlowInFile handle emptyBitsFlow magicBytesInBitsFlow) >>=
   (\lastByte -> writeOwnerBytesInFile handle lastByte >>=
   (\lastByte -> flushInFileBitsFlow handle  lastByte)
    )
  ) >>
  (writeHuffmanTreeInFile weightTree handle emptyBitsFlow)
-- ----------------------------------------------------------------------------
{- writeOwnerBytes : Write n free bytes in the header of the  file.  The first
   byte means the number of followings information bytes. In this version not
   any information byte is written.
-}
writeOwnerBytesInFile
      :: Handle
      -> BitsFlow
      -> IO BitsFlow
writeOwnerBytesInFile handle (last,bF) =
                                  putInFileBitsFlow handle getFirstPrivatesBytes
-- ----------------------------------------------------------------------------
{- flushInFileBitsFlow : Write all bytes included in the BitsFlow
   in the given in file.
-}
flushInFileBitsFlow
      :: Handle
      -> BitsFlow
      -> IO ()
flushInFileBitsFlow handle (last,bF) =
            putInFileBitsFlow handle (8, bF) >> return ()
-- ----------------------------------------------------------------------------
{- putInFileBitsFlow : Write the given BitsFlow in file handled. Different cases
   are analyzed due to the lenght of flow when it is not modulo 8.
   The flow of bits is converted in ByteString. The last byte when it cannot
   be written in the file is returned to the caller to be used at the next call.
-}
putInFileBitsFlow
      :: Handle
      -> BitsFlow
      -> IO BitsFlow
putInFileBitsFlow  handle (last, []) = return (last, [])
putInFileBitsFlow  handle (0, [a])    = return (0, [a])
putInFileBitsFlow  handle (last, [a])| (last < 8 )  = return  (last, [a])
putInFileBitsFlow  handle (last, bF) | (last == 8 ) =
    Byte.hPut handle (Byte.pack bF) >> return (0,[])
putInFileBitsFlow  handle (last, bF) | (last <  8) =
     Byte.hPut handle (Byte.pack (List.init bF))>> return (last, [List.last bF])
-- ----------------------------------------------------------------------------
{- writeBitsInFile : Write the given Bits in file handled. Sure only bytes can
   be written in file. So the serie of Bits is first is converted to list of
   word 8 then packed in one Byte string and passed to the hput haskell function
   The bits rest are saved in the BitsFlow structure and return  to the caller
   who is in charge to give again for its future call.

-}
writeBitsInFile
      :: Handle
      -> BitsFlow
      -> BitsSerie
      -> IO BitsFlow
writeBitsInFile handle accu bits =
    putInFileBitsFlow handle (agregateBitsSerieInBitsFlow accu bits)
-- ----------------------------------------------------------------------------
{- writeBitsFlowInFile : Write the given BitsFlow in file handled. The function
   begins by concatenate the bitsflow to the previous saved bitFlow given and
   maintained by the caller.
-}
writeBitsFlowInFile
      :: Handle
      -> BitsFlow
      -> BitsFlow
      -> IO BitsFlow
writeBitsFlowInFile handle accu bF =
    putInFileBitsFlow handle (concateBitsFlow  accu bF)

-- ----------------------------------------------------------------------------
{- makeBitsSeriesFromWt  : The function performs a deep first search of the
   weight tree given in parameter. Following the specification (JC) a BitsSerie
   is made and then written in the file.
-}
makeBitsSeriesFromWt
      :: WeightTree
      -> BitsSerie
      -> BitsSerie
makeBitsSeriesFromWt (Node w wt1 wt2) bS = --trace ("bs :[" ++ (foldl' (\x y-> x++","++(show y)) "" bS))
      (makeBitsSeriesFromWt wt2 (makeBitsSeriesFromWt wt1 (bS ++ [1])))
makeBitsSeriesFromWt  Eof bS = --trace ("bs :[" ++ (foldl' (\x y-> x++","++(show y)) "" bS)++"EOF")
      (explodeBitsFlowInBitsSerie (bS ++ [0]) (1,[commonSymbolWithEof,128]))
makeBitsSeriesFromWt (Leaf symb w) bS = --trace ("bs :[" ++ (foldl' (\x y-> x++","++(show y)) "" bS)++(show symb))
      (explodeBitsFlowInBitsSerie (bS ++ [0]) (code_aux symb))
  where
    code_aux symb = if (symb /= commonSymbolWithEof) then (8,[symb])
                    else (1,[commonSymbolWithEof,0])
-- ----------------------------------------------------------------------------
{- makeBitsFlowFromWt  : The function performs a deep first search of the
   weight tree given in parameter. Following the specification (JC) a BitsFlow
   is made and then written in the file.
-}
makeBitsFlowFromWt
      :: WeightTree
      -> BitsFlow
      -> BitsFlow
makeBitsFlowFromWt (Node w wt1 wt2) bF = --trace("-1-"++"(bf ="++show(bF)++")")
      (makeBitsFlowFromWt wt1
          (makeBitsFlowFromWt wt2 (agregateBitsSerieInBitsFlow bF [1])))
makeBitsFlowFromWt  Eof bF =  --trace ("-0-EOF"++"(bf ="++show(bF)++")")
      (concateBitsFlow  ((agregateBitsSerieInBitsFlow bF [0]))
                                          (1,[commonSymbolWithEof,128]))
makeBitsFlowFromWt (Leaf symb w) bF = --trace ("-0-"++(show symb)++"(bf ="++show(bF)++")")
      (concateBitsFlow  ((agregateBitsSerieInBitsFlow bF [0])) (code_aux symb))
  where
    code_aux symb = if (symb /= commonSymbolWithEof) then (8,[symb])
                    else (1,[commonSymbolWithEof,0])
-- ----------------------------------------------------------------------------
{- writeHuffmanTreeInFile  : The function performs a deep first search of the
   weight tree given in parameter. Following the specification (JC) a BitsSerie
   is made and then written in the file.
-}
writeHuffmanTreeInFile
      :: WeightTree
      -> Handle
      -> BitsFlow
      -> IO BitsFlow
writeHuffmanTreeInFile wt handle lastByte =
    putInFileBitsFlow handle (makeBitsFlowFromWt wt lastByte)
-- ----------------------------------------------------------------------------
{- compressAndAppendFile : This function open the target file in AppendMode, we
   know that file exists, and read the expanded information from the contents
   already accessible and zip using the codingTable previously computed and
   finally write the compressed information into the target file.
-}
compressAndAppendFile
      :: Byte.ByteString
      -> Handle
      -> CodingTable
      -> BitsFlow
      -> IO ()
compressAndAppendFile contents handle codingTable contextByte =
    (hSetBinaryMode handle True) >>
    (hSetBuffering handle(BlockBuffering(Just(fromIntegral writeBufferSize))))>>
    (compressAndWriteAllContentsByPacket handle codingTable contents contextByte >>=
        (\lastByte -> writeSpecialEof handle codingTable lastByte >>=
            (\lastByte -> flushInFileBitsFlow handle  lastByte)
        )
    )
-- -----------------------------------------------------------------------------
{- writeSpecialEof : This function read the code of Eof from the coding table
   and concate this value to the lasbyte given by the caller and finally put
   in the file.
   Note that Eof is NOT the Eof of the Zip file. It is just to help the reading
   of the zip file for the unzip operation.
-}
writeSpecialEof
      :: Handle
      -> CodingTable
      -> BitsFlow
      -> IO BitsFlow
writeSpecialEof handle codingTable  lastByte  =
  let
   codeEof =
     case (Map.lookup idxEofCode codingTable) of
      Nothing   -> error "Compress error : Unknown Eof in codes table"
      Just code ->  (convertBitsCodingInBitsFlow code)
  in codeEof`seq`(putInFileBitsFlow handle (concateBitsFlow lastByte codeEof))
-- ----------------------------------------------------------------------------
{- compressAndWriteAllContentsByPacket : This function read the contents by
   packet and compress the bytesString and store the contents in a ByteFlow the
   put this ByteFlow into the file.
-}
compressAndWriteAllContentsByPacket
      :: Handle
      -> CodingTable
      -> Byte.ByteString
      -> BitsFlow
      -> IO BitsFlow
compressAndWriteAllContentsByPacket handle codingTable contents lastByte
                      | Byte.null contents  = return lastByte
compressAndWriteAllContentsByPacket handle codingTable contents lastByte =
  let
    (getByte, getContinuation) =
                  Byte.splitAt packetSize contents
    bF = getByte`seq`
         compressByteStringInBitsFlow codingTable getByte lastByte
  in
  bF`seq`
  getContinuation`seq`
  (putInFileBitsFlow handle bF) >>=
  (\last_aux -> compressAndWriteAllContentsByPacket handle
                                  codingTable getContinuation   last_aux )
-- ----------------------------------------------------------------------------
{- compressByteStringInBitsFlow : This function compress the packet and concate
   the BitsFlow made with the lastByte of previous BitsFlow generated.
-}
compressByteStringInBitsFlow
      :: CodingTable
      -> Byte.ByteString
      -> BitsFlow
      -> BitsFlow
compressByteStringInBitsFlow codingTable packet lastByte =
  Byte.foldl' (\bF w  -> codeSymbol bF w) lastByte packet
  where
    codeSymbol bF w =
      case (Map.lookup (fromIntegral w) codingTable) of
        Nothing   -> error "Compress error : Unknown symbol in codes table"
        Just code -> (concateBitsFlow bF (convertBitsCodingInBitsFlow code))
-- ----------------------------------------------------------------------------
{- writeTailOfFile : This function write in append mode (m +1) arbitrary bytes.
   (0<= m <= 256). The first giving the number of following bytes.
-}
writeTailOfFile
      :: String
      -> IO ()
writeTailOfFile pathTo =
      (openBinaryFile pathTo AppendMode) >>=
        (\handle ->
          (hSetBinaryMode handle True) >>
          (hSetBuffering handle
            (BlockBuffering (Just (fromIntegral writeBufferSize)))) >>
          (writeBitsFlowInFile handle emptyBitsFlow
                                getLastPrivatesBytes)>> hClose handle
        )
