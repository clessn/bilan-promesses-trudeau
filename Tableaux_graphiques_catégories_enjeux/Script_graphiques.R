##
### Charger les packages
##
library(tidyverse)

##
### Importer les csv des deux mandats
##
dfmandat2 <- read.csv("mandat2_df.csv", header = TRUE,
                      colClasses = c(Catégories = "character",
                                     Réalisée = "numeric",
                                     Partiellement.réalisée = "numeric",
                                     En.voie.de.réalisation = "numeric",
                                     En.suspens = "numeric",
                                     Rompue = "numeric"))

dfmandat3 <- read.csv("mandat3_df.csv", header = TRUE,
                      colClasses = c(Catégories = "character",
                                     Réalisée = "numeric",
                                     Partiellement.réalisée = "numeric",
                                     En.voie.de.réalisation = "numeric",
                                     En.suspens = "numeric",
                                     Rompue = "numeric"))
##
### Renommer les colonnes
##
colnames(dfmandat3) <- c("Catégories", "Réalisée", "Partiellement réalisée",
                         "En voie de réalisation", "En suspens", "Rompue")
colnames(dfmandat2) <- c("Catégories", "Réalisée", "Partiellement réalisée",
                         "En voie de réalisation", "En suspens", "Rompue")

##
### Transformer les dataframes avec gather pour les rendres longs
##
dfmandat2 <- dfmandat2 %>% gather(key = "Status", value = "Value", -Catégories)
dfmandat3 <- dfmandat3 %>% gather(key = "Status", value = "Value", -Catégories)

##
### Transformer la variable Status en Facteur et ordonner comme souhaité
##
dfmandat2$Status <- factor(dfmandat2$Status, 
                           levels = c("Réalisée", "Partiellement réalisée",
                                      "En voie de réalisation", "En suspens", 
                                      "Rompue"))
dfmandat3$Status <- factor(dfmandat3$Status, 
                           levels = c("Réalisée", "Partiellement réalisée",
                                      "En voie de réalisation", "En suspens", 
                                      "Rompue"))

## Création de la palette de couleur 

verdict_palette <- c("#CCCCCC", "#999999", "#666666", "#333333", "black")

## graphique du 2eme mandat


graphmandat2 <- ggplot(dfmandat2, 
                       aes(x = Value , y = Catégories ,fill = Status)) +
  geom_bar(stat = "identity")  +
  geom_text(data = subset(dfmandat2, Value != 0), 
            aes(label = Value), position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  scale_fill_manual(values = verdict_palette) +
  labs(
    title = "Verdicts du second mandat selon les catégories d'enjeux",
    x = "Nombre de promesses",
    y = "Catégories d'enjeux",
    fill = "Legend Title") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5), 
    axis.title.x = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 8),            
    legend.position = "right")

ggsave("mandat2_plot.png", plot = graphmandat2, width = 12, height = 6
)
print(graphmandat2)


## graphique du 3eme mandat

graphmandat3 <- ggplot(dfmandat3, 
                       aes(x = Value , y = Catégories ,fill = Status)) +
  geom_bar(stat = "identity")  +
  geom_text(data = subset(dfmandat3, Value != 0), 
            aes(label = Value), position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  scale_fill_manual(values = verdict_palette) +
  labs(
    title = "Verdicts du troisième mandat selon les catégories d'enjeux",
    x = "Nombre de promesses",
    y = "Catégories d'enjeux",
    fill = "Legend Title") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5), 
    axis.title.x = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 8),            
    legend.position = "right")

ggsave("mandat3_plot.png", plot = graphmandat3, width = 12, height = 6
       )
print(graphmandat3)

