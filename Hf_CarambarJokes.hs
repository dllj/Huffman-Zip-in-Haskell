{-* 
  * Master Parisien de Recherche en Informatique 
  * Université Denis Diderot Paris 7 - 2010 - 2011
  * Projet de Haskell
  * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
  *
  *   Project : Huffman Zip in Haskell
  *
  * $Id: Hf_CarambarJokes.hs,v 1.1 2011/04/28 09:00:35 denis7cordas Exp $
  *
  * Contents : 
  *       Hf_CarambarJokes
  *
  *       Sample to use Random function...
  *  
  * History :
  *   DJ20110406 : creation
  * Note :
  *                 ghc --make -O -Werror Hf_Main.hs -fforce-recomp 
  *                                     -caf-all -auto-all -prof -o hzip 
  *
-}
module Hf_CarambarJokes (putCarambarJoke) 
  where 
-- *****************************************************************************
import System.Random 
-- *****************************************************************************
joke :: Int -> String 
joke 0 =   "Deux fous se promenent. Soudain ils levent la tete pour regarder"
         ++"un avion.\n- Ah, je le reconnais dit l'un, c'est l'avion du"
         ++" president.\n- T'es fou, on aurait vu passer les motards\n"
joke 1 =   "Un fou fait des bons au-dessus d'un plat de spaghettis.\nIl "
         ++"explique : \"je saute un repas.\"\n"
joke 2 =   "Un fou telephone à un autre fou :\n" 
         ++"- Je suis bien au 01.02.03.04.05 ?\n" 
         ++"- Ah non ! Vous faites erreur, ici on n'a pas le telephone\n"
joke 3 =   "Un fou se tient tout en haut d'une grande echelle avec un livre. \n"
         ++"-Que fais-tu ?, lui demande un passant.\n" 
         ++"-Je fais des etudes superieures.\n"
joke 4 =   "- Il faut etre bon avec les animaux, explique le papa de Julien.\n"
         ++"- Oui papa, il ne faut pas les traiter comme des betes\n"
joke 5 =   "- Les gens devraient tout le temps dormir la fenetre ouverte.\n"
         ++"- Pourquoi, vous êtes medecin?\n" 
         ++"- Non, je suis cambrioleur.\n"
joke _ =   "- Les gens devraient tout le temps dormir la fenetre ouverte.\n"
         ++"- Pourquoi, vous etes medecin?\n" 
         ++"- Non, je suis cambrioleur.\n"

  --------------------------------------------------------------------------------
putCarambarJoke :: IO ()
putCarambarJoke = 
 getStdGen >>= 
  (\stg ->
     let (rand, _)  = (randomR (0,5) stg)
     in (putStr (joke rand)) 
  )
