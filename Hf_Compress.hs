{-*
  * Master Parisien de Recherche en Informatique
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_Compress.hs,v 1.14 2011/04/27 04:42:57 denis7cordas Exp $
  *
  * Contents :
  *       Hf_Compress
  *
  *       Functions to zip file   .
  *
  * History :
  *   DJ20110406 : creation
  *
  * Note :
  *   ghc --make -fwarn-missing-signatures -Werror file.hs
  *
-}
module Hf_Compress (compress)
  where
-- *****************************************************************************
import Data.List
import Data.Map as Map
import qualified Data.ByteString.Lazy as Byte
import System.IO
-- *****************************************************************************
-- specific types and modules
import Hf_WeightTable
import Hf_WeightTree
import Hf_CodingTable
import Hf_RWZipFile
import Hf_BitsFlow
-- *****************************************************************************
-- -----------------------------------------------------------------------------
{- SymbolsCoding : Constructs the binary tree of prefix coding from the weight
   table of symbols read in source file. Constructs in same time the table of
   mapping between symbols and their coding in Bits serie.
-}
symbolsCoding
      :: WeightTable
      -> (CodingTable, WeightTree)
symbolsCoding weight = (makeCodingTable wt , wt)
                        where wt = makeWeightTree weight
-- symbolsCoding  (Map.fromList  [(23,2),(45,1),(67,1)])
-- -----------------------------------------------------------------------------
{- compress : Read the source file and write the target (zip) file in three
   parts :
              1) the header including the huffman tree
              2) the bits of the compressesd data
              3) the tail of the file (some bytes reserved).
  1) and to 2 are performed with the same handle of the target file. that is
  mandatory for the bit alignement reason.

-}
compress
      :: FilePath
      -> FilePath
      -> IO ()
compress  pathFrom pathTo =
      Byte.readFile pathFrom >>=
      (\contents ->
-- -----------------------------------------------------------------------------
          let weightTable = computeWeightTable contents
              (codingTable, weightTree)
                          = symbolsCoding (Map.fromList weightTable)
              ct          = Map.toList codingTable
          in
-- -----------------------------------------------------------------------------
      (openBinaryFile pathTo WriteMode) >>=
       (\handle ->
              writeHeaderOfFile handle weightTree >>=
-- The next read avoid a remanence of the memory used for contents. A new read
-- allows to use a constant memory space.
-- Here we cannot go out the lambda context because we have to conserve the
-- access to CodingTable.
              (\lastByte ->
                  Byte.readFile pathFrom >>=
                  (\contents2 ->
                    compressAndAppendFile contents2 handle codingTable lastByte
                  )
              )>>
              hClose handle
           )
       )
       >>
      writeTailOfFile pathTo
