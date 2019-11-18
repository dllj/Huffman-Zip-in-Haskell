{-* 
  * Master Parisien de Recherche en Informatique 
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_ParseZipFile.hs,v 1.13 2011/04/28 10:49:43 denis7cordas Exp $
  *
  * Contents : 
  *       Hf_ParseZipFile
  * 
  *       Analyze of Zip file call functions to read Huffman tree and to 
  *       uncompress.
  *  
  * History :
  *   DJ20110406 : creation
  *
  * Note :
  *   ghc --make -fwarn-missing-signatures -Werror file.hs 
  *
-}
module Hf_ParseZipFile (analyzeZipFile, ParserState(..), ParserError(..)) 
  where 
-- *****************************************************************************
import Data.Tuple
import qualified Data.ByteString.Lazy as Byte  
import System.IO
import Debug.Trace
-- *****************************************************************************
import Hf_BitsFlow
import Hf_Types
import Hf_Utilities
import Hf_PrivateBytes
import Hf_HuffmanTree 
import Hf_RWZipFile
-- *****************************************************************************
-- -----------------------------------------------------------------------------
data ParserState = MagicBytes   | HeaderPrivateBytes | CodingOfHuffmanTree |
                   CompressPart | TailPrivateBytes   | Eof 
data ParserError = NoError | EofNotExpected | UnknownFileFormat 
-- -----------------------------------------------------------------------------
{- analyzeZipFile : For each part of the zipfile this function calls the read
   function related to the content expected in the expected order. If the read 
   has been done normally the function calls the next read operation according 
   to the next step called here "ParserState". The size in bytes of each part
   of the file is not known at the start. But what is exactly the reading of
   the file in this program ? How do we perform a read bit by bit ? We choose 
   to use the readFile lazy haskell function that in fact prepared the futur 
   real reading of the file. When it is needed by the program, a part of file is
   loaded in Ram and become available to the user in one "ByteString" structure 
   on which we can use some other related function like "ByteString.unpack", 
   "ByteString.foldl'" in order to access to the content byte by byte. The byte
   (unsigned char in C) in haskell is called Word8. But as we have seen the zip
   file structure implies a bit/bit reading in fact it is reading the file bits 
   by bits that the real size of each part (huffmantree or compressed data) is 
   discovered. To implement the bit reading we used a structure  called here
   "continuation" that is in fact a couple. Please refer to the comments 
   including in the Hf_BitsFlow module for more information. 
  -----------------------------------------------------------------------------
  list of parameters :
      - handle : handle of the binary file which has to received uncompressed 
                 data.
      - state : Indicates the step 
      - continuation : tuple including  BitsFlow, ByteString to be next read
      - huffTree : Huffman tree which is contructed by this function  
  -----------------------------------------------------------------------------
  return : 
      - ParserError NoEror or EofNotExpected  
-}
analyzeZipFile  
      :: Handle
      -> ParserState 
      -> (BitsFlow, Byte.ByteString)
      -> HuffmanTree
      -> IO ParserError
analyzeZipFile handle state (bF, contents) huffTree  = 
    case state of
      MagicBytes          -> 
        case readMagicBytes contents of 
          (NoError, continuation) -> 
               analyzeZipFile handle HeaderPrivateBytes continuation huffTree          
          (parserError, _)        -> return parserError 
      HeaderPrivateBytes  -> 
        case readPrivateBytes contents of 
          (NoError, continuation) -> 
               analyzeZipFile handle CodingOfHuffmanTree continuation huffTree   
          (parserError, _)        -> return parserError 
      CodingOfHuffmanTree         -> 
        case readHuffmanTree contents of 
          (True, continuation, gotHuffTree) -> 
               analyzeZipFile handle CompressPart continuation  gotHuffTree         
          (False, _,_)        -> return EofNotExpected
      CompressPart        -> 
                      unZip handle ([], bF, contents) huffTree huffTree >>= 
                (\(_,bF, contents) ->  analyzeZipFile handle TailPrivateBytes 
                                                       (bF, contents)  huffTree)         
      TailPrivateBytes    -> return NoError
      Eof                 -> return NoError 
-- -----------------------------------------------------------------------------
{- readMagicBytes :  Read the first byte of contents and compare to the 
   magic bytes of the file. This function returns not only  ParserError but
   also the continuation including the emptyBitsFlow and the folllowing of the
   contents in ByteString.
  -----------------------------------------------------------------------------
  list of parameters :
      - contents : ByteString read from ZipFile.
  -----------------------------------------------------------------------------
  return : 
      - ParserError NoEror or EofNotExpected  
      - continuation to be read from file
-}
readMagicBytes 
      :: Byte.ByteString 
      -> (ParserError, (BitsFlow, Byte.ByteString)) 
readMagicBytes contents =
  case (getBytesFlowFromByteString contents (fromIntegral (length magic)) ) of
    Nothing -> (EofNotExpected, (emptyBitsFlow, contents)) 
    Just (bytesFlow, continuation) -> 
          if isEqualList bytesFlow magic then (NoError, 
                                              (emptyBitsFlow, continuation))
          else (UnknownFileFormat, (emptyBitsFlow,contents)) 
  where
    magic = (snd magicBytesInBitsFlow)
-- -----------------------------------------------------------------------------
{- readPrivateBytes : Read the first byte of contents that contains the 
   number of following bytes to read. So only 255 bytes maximum are available.
   This function returns not only  ParserError but also the continutation 
   including the emptyBitsFlow and the folllowing of the contents in ByteString.
  -----------------------------------------------------------------------------
  list of parameters :
      - contents : ByteString read from ZipFile.
  -----------------------------------------------------------------------------
  return : 
      - ParserError NoEror or EofNotExpected  
      - continuation to be read from file
-}
readPrivateBytes 
      :: Byte.ByteString 
      -> (ParserError, (BitsFlow, Byte.ByteString)) 
readPrivateBytes contents =
  case (getBytesFlowFromByteString contents 1) of
    Nothing -> (EofNotExpected, (emptyBitsFlow, contents)) 
    Just (b:s, continuation) -> 
          if b == 0 then (NoError,(emptyBitsFlow, continuation)) 
          else case (getBytesFlowFromByteString contents ((fromIntegral b)+1)) of
                Nothing -> (EofNotExpected, (emptyBitsFlow, contents))
                Just (bF, continuation) -> 
                        doSomethingWithFirstPrivateBytes (8, bF)`seq`
                        (NoError, (emptyBitsFlow, continuation))

-- -----------------------------------------------------------------------------
{- unZip : This function read from the continuation the following of the bits 
   of the compressed data. Each bit read corresponds to the letter of one binary
   word labelling a particular path in the Huffman tree starting from the root
   of the tree until the leaf associated to the uncompressed ASCII symbol. This 
   latter is after immedialty write in the uncompressed file vie the Handle.
  -----------------------------------------------------------------------------
   list of parameters :
      - handle : handle of the binary file which has to received uncompressed 
                 data.
      - continuation : tuple including BitsSeries, BitsFlow, ByteString to be
                       next read
      - root  : It is form of pointer on the integer Huffman tree
      - ht    : It is the current node in the current path corresponding to the
                next ASCII symbol to be write.
  -----------------------------------------------------------------------------
   return : a continuation in monad IO 
  -----------------------------------------------------------------------------
   exception : In case of error the program stop displaying the related error 
               message. Errors which can occur are
                            - Unexpected End Of File
                            - Unexpected End Of Tree                    
-}
unZip :: Handle
      -> (BitsSerie,BitsFlow, Byte.ByteString)
      -> HuffmanTree
      -> HuffmanTree
      -> IO (BitsSerie,BitsFlow, Byte.ByteString)
unZip handle continuation root ht =  
   case (getNextBits continuation 1) of
     Nothing -> error "EofNotExpected  in Unzip function"
     Just ([1], newContinuation) -> 
            case ht of
              Node ht1 ht2 -> unZip handle newContinuation root ht2
              Leaf symb    -> putInFileBitsFlow handle (8, [symb]) >>  
                                         unZip handle continuation root root
              LeafEof      ->  return newContinuation 
              HuffmanTreeEmpty  ->  error "HuffTreeEmpty in Unzip function"
     Just ([0], newContinuation) -> 
            case ht of
              Node  ht1 ht2  -> unZip handle newContinuation root ht1
              Leaf symb      -> putInFileBitsFlow handle (8, [symb]) >>
                                         unZip handle continuation root root
              LeafEof       ->  return newContinuation 
              HuffmanTreeEmpty  ->  error "HuffTreeEmpty in Unzip function"

