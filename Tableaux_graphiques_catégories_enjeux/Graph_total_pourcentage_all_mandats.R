library(tidyverse)


dftotal <- read.csv("totalpourcentageallmandat.csv", header = TRUE,
                    colClasses = c(Catégories = "character",
                                   Mandat.2.et.3 = "numeric",
                                   Mandat.2 = "numeric",
                                   Mandat.3 = "numeric"))
dftotal <- dftotal %>% gather(key = "Status", value = "Value", -Catégories)


## Création de la palette de couleur 

pourcentage_palette <- c("#CCCCCC", "#666666", "black")

dftotalgraph <- ggplot(dftotal, aes(x = Catégories, y = Value ,fill = Status)) +
                        geom_bar(stat = "identity", position = "dodge")  +
  scale_fill_manual(values = verdict_palette) +
                        labs(title = "Pourcentages des nombres de promesses en fonction des catégories d'enjeux et du mandat",
                              x = "Catégories d'enjeux",
                              y = "Pourcentage")+
  clessnverse::theme_clean_light(base_size = 15) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5), 
    axis.title.x = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 8),            
    legend.position = "none",
    axis.text.x = element_text(angle = 80, hjust=1))
 print(dftotalgraph)
 
 