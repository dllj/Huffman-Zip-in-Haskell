#
# * Master Parisien de Recherche en Informatique
# * Université Denis Diderot Paris 7 - 2010 - 2011
# * Projet de Haskell
# * Denis JULIEN  : julien@informatique.univ-paris-diderot.fr
# *
# *   Project : Huffman Zip in Haskell
# *
# * $Id: Makefile,v 1.1 2011/04/27 17:30:15 denis7cordas Exp $
# *
# * Contents :
# *       Make file of hzip programm
# *       for ghc version 8.2.1
# *
# * History :
# *   DJ20110427: creation
# *
# *
#

all : hzip

hzip : Hf_Main.hs
	ghc --make -fwarn-missing-signatures  -v -Werror -O -prof -fprof-auto Hf_Main.hs -o hzip


clean:
	@rm *.o *.hi hzip
