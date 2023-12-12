library(tidyverse)

## Importation du csv

dfexcelchap1 <- read.xlsx("_SharedFolder_livre_promesses-trudeau/Chapitre 1/PolimètreTrudeau-Chapitre1.xlsx", 3) |>
  filter(Inclusion.Polimètre == TRUE)


dftest4 <- dfexcelchap1 |>
  group_by(Catégorie, Mandat) |>
  summarise(Value = n()) |>
  filter(Mandat %in% c("2", "3"))

dftest5 <- dfexcelchap1 |>
  group_by(Catégorie) |>
  summarise(Value = n())

colnames(dftest4) <- c("Catégories", "Mandat", "Value" )
colnames(dftest5) <- c("Catégories", "Value")

dfbind <- rbind(dftest4, dftest5)

## Réordonner les colomnes en facteurs, et les réordonner comme souhaité
dfbind$Mandat <- as.character(dfbind$Mandat)
dfbind$Mandat[is.na(dfbind$Mandat)] <- "2 et 3"

dfbind$Mandat <- factor(dfbind$Mandat, levels = c("2", "2 et 3", "3"))

## Création de la palette de couleur 

pourcentage_palette <- c("#CCCCCC", "#666666", "black")

## création du graphique

dfbind_percent <- dfbind %>%
  group_by(Mandat)  %>%
  mutate(Percentage = Value / sum(Value) * 100)

dftotalgraph <- ggplot(dfbind_percent, aes(x = Catégories, y = Percentage ,fill = Mandat)) +
                        geom_bar(stat = "identity", position = "dodge")  +
  geom_text(aes(label = paste0(round(Percentage), "%")), vjust = -0.5,
            position = position_dodge(0.9),
            size = 2.5) +
  scale_fill_manual(values = pourcentage_palette) +
                        labs(title = "Pourcentage de promesses par catégorie d’enjeu par mandat",
                              x = "Catégories d'enjeux",
                              y = "Pourcentage de l'ensemble des \n promesses du mandat par catégorie")+
  clessnverse::theme_clean_light(base_size = 15) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5), 
    axis.title.x = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 10, hjust = 0.7),
    axis.text = element_text(size = 10),            
    axis.text.x = element_text(angle = 65, hjust=0.9))

## Impression du ggplot
 print(dftotalgraph)

## Exportation 
 
 ggsave("pourcentage_mandat2_3.png", plot = dftotalgraph, width = 12, height = 6)
 
 