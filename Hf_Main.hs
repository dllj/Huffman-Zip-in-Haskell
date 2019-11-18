{-*
  * Master Parisien de Recherche en Informatique
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_Main.hs,v 1.7 2011/04/28 09:00:14 denis7cordas Exp $
  *
  * Contents :
  *       Hf_Main
  *
  *       The main function of  Huffman Haskell project. This program named
  *       "hzip" is a direct implementation in Haskell of the Juliuz'S program
  *       writing in C. For more information go to the readme file.
  *       usage : hzip filepath in order to compress to filepath.hf
  *               hzip -d filepath.hf in order to uncompress to filepath
  *
  * History :
  *   DJ20110406 : creation
  * Note :
  *                 ghc --make -O -Werror Hf_Main.hs -fforce-recomp
  *                                     -caf-all -auto-all -prof -o hzip
  *
-}
-- *****************************************************************************
import System.Environment
import System.IO
import System.Directory
import Data.List
-- *****************************************************************************
import Hf_Compress
import Hf_Uncompress
import Hf_CarambarJokes
-- *****************************************************************************
-- -----------------------------------------------------------------------------
helpUsage
      :: String
helpUsage = "Usage: hzip [-h] [-j] [-d] FILE.\n"
              ++ "Compress or uncompress (with -d) FILE.\n"
              ++ "-j to display Carambar Jokes.\n"
              ++ "All Rights reserved for Sakorsy Land...\n"
              ++ "Please Help us.\n"
-- -----------------------------------------------------------------------------
{- verifFile : Check if source file exist and target file does not  exist
-}
verifFile
      ::  FilePath
      ->  FilePath
      ->  IO Int
verifFile fexist fdontexist =
            (doesFileExist fexist) >>=
              (\exist -> if exist then
                            (doesFileExist fdontexist) >>=
                              (\dontexist -> if dontexist then return 2
                                             else return 0
                              )
                         else return 1
             )
-- -----------------------------------------------------------------------------
{- pathToCompress and pathToUnCompress : make default File path using ".hf"
   suffix.
-}
pathToCompress
      :: FilePath
      -> FilePath
pathToCompress pathFrom  = pathFrom ++".hf"
pathToUnCompress
      :: FilePath
      -> FilePath
pathToUnCompress pathFrom = if (isSuffixOf ".hf" pathFrom) then
                               take ((length pathFrom) - 3) pathFrom
                            else ""
-- -----------------------------------------------------------------------------
main:: IO ()
main =
   getArgs >>=
      (\cmdline ->
         case cmdline of
          ("-h":_)  -> putStr helpUsage
          ("-j":_)  -> putCarambarJoke
          ("-d":pathFrom:ls) ->
                      (verifFile pathFrom (pathToUnCompress pathFrom)) >>=
               (\check ->
                    case (check) of
                       0 -> uncompress pathFrom (pathToUnCompress pathFrom)
                       1 -> putStr (pathFrom ++ "File not found.")
                       2 -> putStr ("Permission denied, "++
                                                (pathToUnCompress  pathFrom)
                                                  ++ " file already exist.")
                )
          (pathFrom:ls) -> (verifFile pathFrom (pathToCompress pathFrom)) >>=
               (\check ->
                    case (check) of
                       0 -> compress    pathFrom (pathToCompress  pathFrom)
                       1 -> putStr (pathFrom ++ "File not found.")
                       2 -> putStr ("Permission denied, "++
                                            (pathToCompress  pathFrom)
                                                  ++ " file already exist.")
               )
          otherwise -> putStr "For help, type: hzip -h\n"
      )
