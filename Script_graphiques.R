##
### Charger les packages
##
library(tidyverse)
library(openxlsx)
library(dplyr)



dfexcelchap1 <- read.xlsx("_SharedFolder_livre_promesses-trudeau/Chapitre 1/PolimètreTrudeau-Chapitre1.xlsx", 3)
dftest2 <- dfexcelchap1 |>
  group_by(Catégorie, Mandat, Verdict,) |>
  summarise(Value = n())
colnames(dftest2) <- c("Catégories", "Mandat","Status", "Value" )


##
### Transformer la variable Status en Facteur et ordonner comme souhaité
##
dftest2$Status <- factor(dftest2$Status, 
                           levels = c("Réalisée", "Partiellement réalisée",
                                      "En voie de réalisation", "En suspens", 
                                      "Rompue"))

## Création de la palette de couleur 

verdict_palette1 <- c("#CCCCCC", "#999999", "#666666", "#333333", "black")
verdict_palette2 <- c("#999999", "#666666", "black")

## graphique du 2eme mandat

dftest2_percent <- dftest2 %>%
  filter(Mandat == 2) %>%
  group_by(Catégories) %>%
  mutate(Percentage = Value / sum(Value) * 100, 
         Value2 = sum(Value),
         Catégories2 = paste0(Catégories, " N = ", Value2))

graphmandat2 <- dftest2_percent |>
  ggplot(aes(x = Percentage , y = Catégories2 ,fill = Status)) +
  geom_bar(stat = "identity")  +
  geom_text(data = dftest2_percent, 
            aes(label = round(Percentage)), position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  scale_fill_manual(values = verdict_palette2) +
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

ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/mandat2_plot.png", plot = graphmandat2, width = 12, height = 6
)
print(graphmandat2)


## graphique du 3eme mandat

dftest3_percent <- dftest2 %>%
  filter(Mandat == 3) %>%
  group_by(Catégories) %>%
  mutate(Percentage = Value / sum(Value) * 100, 
         Value2 = sum(Value),
         Catégories2 = paste0(Catégories, " N = ", Value2))

dftest3_percent[,1] <- c("Identité et nationalisme", 3, NA, NA, NA, NA, NA)

graphmandat3 <- dftest3_percent |>
  ggplot(aes(x = Percentage , y = Catégories2 ,fill = Status)) +
  geom_bar(stat = "identity")  +
  geom_text(data = dftest3_percent, 
            aes(label = round(Percentage)), position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  scale_fill_manual(values = verdict_palette1) +
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

ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/mandat3_plot.png", plot = graphmandat3, width = 12, height = 6
)
print(graphmandat3)

