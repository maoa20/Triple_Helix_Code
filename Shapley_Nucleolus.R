library(GameTheory)

#------------------------------------------------------------------------#
#        Brazil                                                          #
#------------------------------------------------------------------------#

COALITIONS <- c(2.6042, 95.9833, 0.6287, 99.1175, 3.4360, 96.8484, 100)
Brazil_game <-DefineGame(3,COALITIONS)
summary(Brazil_game)
jogadores <- c("University", "Industry", "Government")
BRAZIL_SHAPLEY <- ShapleyValue(Brazil_game,jogadores)
BRAZIL_SHAPLEY

Brazil_nucleolus <- Nucleolus(Brazil_game)


Brazil_nucleolus


#------------------------------------------------------------------------#
#        Mexico                                                          #
#------------------------------------------------------------------------#

COALITIONS <- c(2.1833, 97.1324, 0.6837, 99.3157, 2.8670, 97.8167, 100)
Mexico_game <-DefineGame(3,COALITIONS)
summary(Mexico_game)
jogadores <- c("University", "Industry", "Government")
MEXICO_SHAPLEY <- ShapleyValue(Mexico_game,jogadores)
MEXICO_SHAPLEY

Mexico_nucleolus <- Nucleolus(Mexico_game)


Mexico_nucleolus

#------------------------------------------------------------------------#
#        Chile                                                           #
#------------------------------------------------------------------------#

COALITIONS <- c(2.1608, 96.7088, 0.3171, 99.3981, 2.5542, 97.2197, 100)
Chile_game <-DefineGame(3,COALITIONS)
summary(Chile_game)
jogadores <- c("University", "Industry", "Government")
CHILE_SHAPLEY <- ShapleyValue(Chile_game,jogadores)
CHILE_SHAPLEY

Chile_nucleolus <- Nucleolus(Chile_game)


Chile_nucleolus


#------------------------------------------------------------------------#
#        Argentina                                                       #
#------------------------------------------------------------------------#

COALITIONS <- c(0.6459, 97.8045, 0.3171, 99.2891, 0.9922, 98.3077, 100)
Argentina_game <-DefineGame(3,COALITIONS)
summary(Argentina_game)
jogadores <- c("University", "Industry", "Government")
ARGENTINA_SHAPLEY <- ShapleyValue(Argentina_game,jogadores)
ARGENTINA_SHAPLEY

Argentina_nucleolus <- Nucleolus(Argentina_game)


Argentina_nucleolus

#------------------------------------------------------------------------#
#        LATAM                                                           #
#------------------------------------------------------------------------#

COALITIONS <- c(2.30, 96.35, 0.61, 99.06, 3.04, 97.13, 100)
Latam_game <-DefineGame(3,COALITIONS)
summary(Latam_game)
jogadores <- c("University", "Industry", "Government")
LATAM_SHAPLEY <- ShapleyValue(Latam_game,jogadores)
LATAM_SHAPLEY

Latam_nucleolus <- Nucleolus(Latam_game)


Latam_nucleolus

#------------------------------------------------------------------------#
#        South Korea                                                     #
#------------------------------------------------------------------------#

COALITIONS <- c(5.17, 92.02, 1.15, 98.61, 6.36, 93.37, 100)
SK_game <-DefineGame(3,COALITIONS)
summary(SK_game)
jogadores <- c("University", "Industry", "Government")
SK_SHAPLEY <- ShapleyValue(SK_game,jogadores)
SK_SHAPLEY

SK_nucleolus <- Nucleolus(SK_game)


SK_nucleolus
