{-*
  * Master Parisien de Recherche en Informatique
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_Uncompress.hs,v 1.6 2011/04/27 04:42:57 denis7cordas Exp $
  *
  * Contents :
  *       Hf_Uncompress
  *
  *       Functions to Unzip file   .
  *
  * History :
  *   DJ20110406 : creation
  *
  * Note :
  *                 ghc --make  -Werror Hf_Main.hs -fforce-recomp
  *                                     -caf-all -auto-all -prof -o hzip
  *
-}
module Hf_Uncompress (uncompress)
  where

-- *****************************************************************************
import System.IO
import qualified Data.ByteString.Lazy as Byte
-- *****************************************************************************
import Hf_RWZipFile
import Hf_ParseZipFile
import Hf_Types
import Hf_HuffmanTree
-- *****************************************************************************
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
uncompress
      :: FilePath
      -> FilePath
      -> IO ()
uncompress  pathFrom pathTo =
      Byte.readFile pathFrom >>=
      (\contents ->
-- -----------------------------------------------------------------------------
        (openBinaryFile pathTo WriteMode) >>=
          (\handle ->
              (analyzeZipFile handle MagicBytes
                                        (emptyBitsFlow, contents)
                                              HuffmanTreeEmpty )>>=
              (\parserError -> case parserError of
                  NoError -> return () -- putStrLn ("File Ok")
                  EofNotExpected -> putStrLn ("End of File not expected")
                  UnknownFileFormat -> putStrLn ("Unknown File Format")
               )>> hClose handle
          )
      )
