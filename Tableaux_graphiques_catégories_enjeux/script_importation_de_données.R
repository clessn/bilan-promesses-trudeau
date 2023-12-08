## Importation du csv du 2eme mandat
dfmandat2 <- read.csv("mandat2_df.csv", header = TRUE,
                      colClasses = c(Catégories = "character",
                                     Réalisée = "numeric",
                                     Partiellement.réalisée = "numeric",
                                     En.voie.de.réalisation = "numeric",
                                     En.suspens = "numeric",
                                     Rompue = "numeric"))

## Importation du csv du 3eme mandat
dfmandat3 <- read.csv("mandat3_df.csv", header = TRUE,
                      colClasses = c(Catégories = "character",
                                     Réalisée = "numeric",
                                     Partiellement.réalisée = "numeric",
                                     En.voie.de.réalisation = "numeric",
                                     En.suspens = "numeric",
                                     Rompue = "numeric"))
