library(tidyverse)

## Importation du csv
dftotal <- read.csv("graphtotal.csv", header = TRUE,
                    colClasses = c(Catégories = "character",
                                   Mandat.2.et.3 = "numeric",
                                   Mandat.2 = "numeric",
                                   Mandat.3 = "numeric"))

colnames(dftotal) <- c("Catégories", "Mandats 2 et 3", "Mandat 2", "Mandat 3")


## changer le data frame en un long format
dftotal <- dftotal %>% gather(key = "Status", value = "Value", -Catégories)

## Réordonner les colomnes en facteurs, et les réordonner comme souhaité
dftotal$Status <- factor(dftotal$Status, levels = c("Mandat 2", "Mandats 2 et 3", "Mandat 3"))

## Création de la palette de couleur 

pourcentage_palette <- c("#CCCCCC", "#666666", "black")

## création du graphique

dftotalgraph <- ggplot(dftotal, aes(x = Catégories, y = Value ,fill = Status)) +
  geom_bar(stat = "identity", position = "dodge")  +
  geom_text(aes(label = paste0(Value, "%")), vjust = -0.5, position = position_dodge(0.9),
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
