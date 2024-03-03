library(tidyverse)
library(openxlsx)

dfexcelchap1 <- openxlsx::read.xlsx(
  "_SharedFolder_livre_promesses-trudeau/Chapitre 1/BDTrudeau-Chap1.xlsx",
  3) |>
  filter(`Inclusion.Polimètre./.Inclusion.Polimeter` == TRUE) |>
  filter(`Mandat./.Mandate` %in% c("2", "3"))

dftest4 <- dfexcelchap1 |>
  group_by(Nouvelle.catégorie, `Mandat./.Mandate`) |>
  summarise(Value = n())

dftest5 <- dfexcelchap1 |>
  group_by(Nouvelle.catégorie) |>
  summarise(Value = n())

dfbind <- rbind(dftest4, dftest5)
colnames(dfbind) <- c("Catégories", "Mandat", "Value" )

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

ggplot(dfbind_percent, aes(x = Catégories, y = Percentage, fill = Mandat)) +
  geom_bar(stat = "identity", position = "dodge")  +
  geom_text(aes(label = paste0(round(Percentage), "%")), vjust = -0.5,
            position = position_dodge(0.9),
            size = 2.5) +
  scale_fill_manual("Mandat", values = pourcentage_palette) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Catégorie d'enjeux",
       y = "% de promesses\nformulées par catégorie") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.7),
        axis.text.x = element_text(angle = 60, hjust = 0.95),
        legend.title = element_text())

ggsave(paste0("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/",
              "pourcentage_mandat2_3.png"), width = 9, height = 6)

##
## VERSION 2 APRÈS MODIFICATIONS (MANDAT 1,2 ET 3 PLUTÔT QUE 2, 3 ET 2 & 3)
##

dfexcelchap1 <- read.xlsx("_SharedFolder_livre_promesses-trudeau/Chapitre 1/BDTrudeau-Chap1.xlsx", 3) |>
  filter(`Inclusion.Polimètre./.Inclusion.Polimeter` == TRUE)

dftest41 <- dfexcelchap1 |>
  group_by(`Nouvelle.catégorie`, `Mandat./.Mandate`) |>
  summarise(Value = n()) |>
  filter(`Mandat./.Mandate` %in% c("1", "2", "3"))

colnames(dftest41) <- c("Catégories", "Mandat", "Value" )


## Réordonner les colomnes en facteurs, et les réordonner comme souhaité
dftest41$Mandat <- as.character(dftest41$Mandat)

dftest41$Mandat <- factor(dftest41$Mandat, levels = c("1", "2", "3"))

## Palette de couleur

pourcentage_palette <- c("#CCCCCC", "#666666", "black")

## création du graphique

dfbind_percent <- dftest41 %>%
  group_by(Mandat)  %>%
  mutate(Percentage = Value / sum(Value) * 100)

dftotalgraph <- ggplot(dfbind_percent, aes(x = Catégories, y = Percentage, fill = Mandat)) +
  geom_bar(stat = "identity", position = "dodge")  +
  geom_text(aes(label = paste0(round(Percentage), "%")), vjust = -0.5,
            position = position_dodge(0.9),
            size = 2.5) +
  scale_fill_manual("Mandat", values = pourcentage_palette) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Catégorie d'enjeux",
       y = "% de promesses\nformulées par catégorie") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(axis.title.x = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(hjust = 0.7, size = 12),
        axis.text.x = element_text(angle = 60, hjust = 0.95, size = 12),
        legend.title = element_text(size = 15))

## Impression du ggplot
print(dftotalgraph)

## Exportation 

ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/pourcentage_mandat_1_2_3.png", plot = dftotalgraph, width = 12, height = 6)
