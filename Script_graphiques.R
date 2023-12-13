##
### Charger les packages
##
library(tidyverse)
library(openxlsx)
library(dplyr)



dfexcelchap1 <- read.xlsx("_SharedFolder_livre_promesses-trudeau/Chapitre 1/PolimètreTrudeau-Chapitre1.xlsx", 3)
dftest2 <- dfexcelchap1 |>
  group_by(`Catégorie./.Category`, Mandat, Verdict,) |>
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

verdict_palette1 <- c("#228B22", "#F3C349", "#FF8C00", "#444444", "#AE0101")
verdict_palette2 <- c("#228B22", "#F3C349", "#AE0101")

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
    x = "Pourcentage de promesses",
    y = "Catégories d'enjeux",
    fill = "Legend Title") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5), 
    axis.title.x = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 8),            
    legend.position = "right")

print(graphmandat2)
ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/mandat2_plot.png", plot = graphmandat2, width = 12, height = 6
)


## graphique du 3eme mandat

dftest3_percent <- dftest2 %>%
  filter(Mandat == 3) %>%
  group_by(Catégories) %>%
  mutate(Percentage = Value / sum(Value) * 100, 
         Value2 = sum(Value),
         Catégories2 = paste0(Catégories, " N = ", Value2))

#dftest3_percent[,1] <- c("Identité et nationalisme", 3, NA, NA, NA, NA, NA)

graphmandat3 <- dftest3_percent |>
  ggplot(aes(x = Percentage , y = Catégories2 ,fill = Status)) +
  geom_bar(stat = "identity")  +
  geom_text(data = dftest3_percent, 
            aes(label = round(Percentage)), position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  scale_fill_manual(values = verdict_palette1) +
  labs(
    title = "Verdicts du troisième mandat selon les catégories d'enjeux",
    x = "Pourcentage de promesses",
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

## ANGLAIS

dfexcelchap1 <- read.xlsx("_SharedFolder_livre_promesses-trudeau/Chapitre 1/PolimètreTrudeau-Chapitre1.xlsx", 3)
dfcategoverdiENG <- dfexcelchap1 |>
  group_by(`Catégorie./.Category`, Mandat, Verdict,) |>
  summarise(Value = n())
colnames(dfcategoverdiENG) <- c("Categories", "Term","Status", "Value" )

dfcategoverdiENG <- dfcategoverdiENG %>%
  mutate(Categories = case_when(
    Categories == "Affaires internationales et défense" ~ "International Affairs and Defense",
    Categories == "Arts et culture" ~ "Arts and Culture",
    Categories == "Économie et employabilité" ~ "Economy and Employability",
    Categories == "Éducation et recherche" ~ "Education and Research",
    Categories == "Environnement" ~ "Environment",
    Categories == "Familles" ~ "Families",
    Categories == "Gouvernement et démocratie" ~ "Government and Democracy",
    Categories == "Identité et nationalisme" ~ "Identity and Nationalism",
    Categories == "Loi et ordre" ~ "Law and Order",
    Categories == "Minorités" ~ "Minorities",
    Categories == "Régions et agriculture" ~ "Regions and Agriculture",
    Categories == "Santé et services sociaux" ~ "Health and Social Service",
    TRUE ~ Categories)) 

dfcategoverdiENG <- dfcategoverdiENG %>%
  mutate(Status = case_when(
    Status == "Partiellement réalisée" ~ "Partially kept",
    Status == "Rompue" ~ "Broken",
    Status == "Réalisée" ~ "Kept",
    Status == "En suspens" ~ "Not yet rated",
    Status == "En voie de réalisation" ~ "In progress",
    TRUE ~ Status)) 
##
### Transformer la variable Status en Facteur et ordonner comme souhaité
##
dfcategoverdiENG$Status <- factor(dfcategoverdiENG$Status, 
                         levels = c("Kept", "Partially kept",
                                    "In progress", "Not yet rated", 
                                    "Broken"))

## Création de la palette de couleur 

## graphique du 2eme mandat

dfcategoverdiENG_percent <- dfcategoverdiENG %>%
  filter(Term == 2) %>%
  group_by(Categories) %>%
  mutate(Percentage = Value / sum(Value) * 100, 
         Value2 = sum(Value),
         Categories2 = paste0(Categories, " (n = ", Value2, ")"))

graphmandat2ENG <- dfcategoverdiENG_percent |>
  ggplot(aes(x = Percentage, y = reorder(Categories2, desc(Categories2)),
             fill = Status)) +
  geom_bar(stat = "identity")  +
  geom_text(data = dfcategoverdiENG_percent, 
            aes(label = round(Percentage)),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  scale_fill_manual(values = verdict_palette2) +
  labs(
    #title = "Second-term verdicts by issue category",
    x = "Percent of promises",
    y = "Issue categories",
    fill = "Legend Title") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5), 
    axis.title.x = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 8),            
    legend.position = "right")

print(graphmandat2ENG)
ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/mandat2ENG_plot.png", plot = graphmandat2ENG, width = 12, height = 6
)


## graphique du 3eme mandat
dfcategoverdiENG_percent <- dfcategoverdiENG %>%
  filter(Term == 3) %>%
  group_by(Categories) %>%
  mutate(Percentage = Value / sum(Value) * 100, 
         Value2 = sum(Value),
         Categories2 = paste0(Categories, " (n = ", Value2, ")"))

graphmandat3ENG <- dfcategoverdiENG_percent |>
  ggplot(aes(x = Percentage, y = reorder(Categories2, desc(Categories2)),
             fill = Status)) +
  geom_bar(stat = "identity")  +
  geom_text(data = dfcategoverdiENG_percent, 
            aes(label = round(Percentage)), position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  scale_fill_manual(values = verdict_palette1) +
  labs(
    #title = "Third-term verdicts by issue category",
    x = "Percent of promises",
    y = "Issue categories",
    fill = "Legend Title") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5), 
    axis.title.x = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 8),            
    legend.position = "right")
print(graphmandat3ENG)

ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/mandat3ENG_plot.png", plot = graphmandat3ENG, width = 12, height = 6
)

