## loader tidyverse
library(tidyverse)

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

## transformation des dataframes avec gather pour les rendres longs
dfmandat2 <- dfmandat2 %>% gather(key = "Status", value = "Value", -Catégories)
dfmandat3 <- dfmandat3 %>% gather(key = "Status", value = "Value", -Catégories)

## Création de la palette de couleur 

verdict_palette <- c("#CCCCCC", "#999999", "#666666", "#333333", "black")

## graphique du 2eme mandat


graphmandat2 <- ggplot(dfmandat2, aes(x = Status , y = Value ,fill = Status)) +
  geom_bar(stat = "identity")  +
  scale_fill_manual(values = verdict_palette) +
  labs(
    title = "Verdicts du troisième mandat selon les catégories d'enjeux",
    x = NULL,
    y = NULL
  ) +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5), 
    axis.title.x = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 8),            
    legend.position = "none",
    axis.text.x = element_text(angle = 80, hjust=1)         
  ) + facet_wrap(~ Catégories)

ggsave("mandat2_plot.png", plot = graphmandat3, width = 12, height = 6
)
print(graphmandat2)


## graphique du 3eme mandat

graphmandat3 <- ggplot(dfmandat3, aes(x = Status , y = Value ,fill = Status)) +
  geom_bar(stat = "identity")  +
  scale_fill_manual(values = verdict_palette) +
  labs(
    title = "Verdicts du troisième mandat selon les catégories d'enjeux",
    x = NULL,
    y = NULL
  ) +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5), 
    axis.title.x = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 8),            
    legend.position = "none",
    axis.text.x = element_text(angle = 80, hjust=1)         
  ) + facet_wrap(~ Catégories)

ggsave("mandat3_plot.png", plot = graphmandat3, width = 12, height = 6
       )
print(graphmandat3)

