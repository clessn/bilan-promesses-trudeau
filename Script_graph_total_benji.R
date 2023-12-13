library(tidyverse)
library(dplyr)
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

ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/pourcentage_mandat2_3.png", plot = dftotalgraph, width = 12, height = 6)


## English version

## Importation du csv
dftotaleng <- read.csv("graphtotal.csv", header = TRUE,
                    colClasses = c(Catégories = "character",
                                   Mandat.2.et.3 = "numeric",
                                   Mandat.2 = "numeric",
                                   Mandat.3 = "numeric"))

colnames(dftotaleng) <- c("Categories", "Terms 2 & 3", "Term 2", "Term 3")

dftotaleng <- dftotaleng %>%
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
#
# À utiliser au besoin.
#
#row.names(dftotaleng$Categories) <- c(
#  "International Affairs and Defense", "Arts and Culture",
#  "Economy and Employability", "Education and Research", "Environment",
#  "Families", "Government and Democracy", "Identity and Nationalism",
#  "Law and Order", "Minorities", "Regions and Agriculture",
#  "Health and Social Services")


## changer le data frame en un long format
dftotaleng <- dftotaleng %>% gather(key = "Status", value = "Value", -Categories)

## Réordonner les colomnes en facteurs, et les réordonner comme souhaité
dftotaleng$Status <- factor(dftotaleng$Status, levels = c("Term 2", "Terms 2 & 3", "Term 3"))

## Création de la palette de couleur 

pourcentage_palette <- c("#CCCCCC", "#666666", "black")

## création du graphique

dftotalgrapheng <- ggplot(dftotaleng, aes(x = Categories, y = Value ,fill = Status)) +
  geom_bar(stat = "identity", position = "dodge")  +
  geom_text(aes(label = paste0(Value, "%")), vjust = -0.5, position = position_dodge(0.9),
            size = 2.5) +
  scale_fill_manual(values = pourcentage_palette) +
  labs(title = "Percentage of pledges by issue category by mandate",
       x = "Issue categories",
       y = "Percentage of total mandate promises by category")+
  clessnverse::theme_clean_light(base_size = 15) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5), 
    axis.title.x = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 10, hjust = 0.7),
    axis.text = element_text(size = 10),            
    axis.text.x = element_text(angle = 65, hjust=0.9))

## Impression du ggplot
print(dftotalgrapheng)

## Exportation 

ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/pourcentage_mandat2_3ENG.png", plot = dftotalgrapheng, width = 12, height = 6)
