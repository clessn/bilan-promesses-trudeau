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
dfcategoverdiENG <- dfexcelchap1 %>%
  mutate(Nouvelle.catégorie = case_when(
    Nouvelle.catégorie == "Affaires internationales et défense" ~ "International Affairs and Defense",
    Nouvelle.catégorie == "Culture et nationalisme" ~ "Culture and Nationalism",
    Nouvelle.catégorie == "Économie et travail" ~ "Economy and Labor",
    Nouvelle.catégorie == "Éducation" ~ "Education",
    Nouvelle.catégorie == "Environnement et énergie" ~ "Environment and Energy",
    Nouvelle.catégorie == "Technologie" ~ "Technology",
    Nouvelle.catégorie == "Gouvernements et gouvernance" ~ "Governments and Governance",
    Nouvelle.catégorie == "Identité et nationalisme" ~ "Identity and Nationalism",
    Nouvelle.catégorie == "Loi et crime" ~ "Law and Crime",
    Nouvelle.catégorie == "Droits, libertés, minorités et discrimination" ~ "Rights, Liberties, Minorities, and Discrimination",
    Nouvelle.catégorie == "Terres publiques et agriculture" ~ "Public Lands and Agriculture",
    Nouvelle.catégorie == "Santé et services sociaux" ~ "Health and Social Services",
    TRUE ~ Nouvelle.catégorie)) 
dftest41en <- dfcategoverdiENG |>
  group_by(Nouvelle.catégorie, `Mandat./.Mandate`) |>
  summarise(Value = n()) |>
  filter(`Mandat./.Mandate` %in% c("1", "2", "3"))

colnames(dftest41) <- c("Catégories", "Mandat", "Value" )
colnames(dftest41en) <- c("Categories", "Mandate", "Value" )

dftest41$Catégories <- gsub("et ", "\n et ", dftest41$Catégories)
dftest41en$Categories <- gsub("and ", "\nand ", dftest41en$Categories)

## Réordonner les colomnes en facteurs, et les réordonner comme souhaité
dftest41$Mandat <- as.character(dftest41$Mandat)
dftest41en$Mandate <- as.character(dftest41en$Mandate)

dftest41$Mandat <- factor(dftest41$Mandat, levels = c("1", "2", "3"))
dftest41en$Mandate <- factor(dftest41en$Mandate, levels = c("1", "2", "3"))

## Palette de couleur

pourcentage_palette <- c("#CCCCCC", "#666666", "black")

## création du graphique

dfbind_percent <- dftest41 %>%
  group_by(Mandat)  %>%
  mutate(Percentage = Value / sum(Value) * 100)
dfbind_percenten <- dftest41en %>%
  group_by(Mandate)  %>%
  mutate(Percentage = Value / sum(Value) * 100)

dftotalgraph <- ggplot(dfbind_percent, aes(x = Catégories, y = Percentage, fill = Mandat)) +
  geom_bar(stat = "identity", position = "dodge")  +
  geom_text(aes(label = paste0(round(Percentage), "%")), vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 3.5) +
  scale_fill_manual("Mandat", values = pourcentage_palette) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Catégorie d'enjeux",
       y = "% de promesses\nformulées par catégorie") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(axis.title.x = element_text(hjust = 0.5, size = 22),
        axis.title.y = element_text(hjust = 1, size = 22),
        axis.text.x = element_text(angle = 65, hjust = 0.95, size = 22),
        axis.text.y = element_text(size = 22),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22))

## Impression du ggplot
print(dftotalgraph)

## Exportation 

ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/pourcentage_mandat_1_2_3.png", plot = dftotalgraph, width = 12, height = 6)

dftotalgraphen <- ggplot(dfbind_percenten, aes(x = Categories, y = Percentage, fill = Mandate)) +
  geom_bar(stat = "identity", position = "dodge")  +
  geom_text(aes(label = paste0(round(Percentage), "%")), vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 4) +
  scale_fill_manual("Mandate", values = pourcentage_palette) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Issue category",
       y = "% of promises\nby category") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(axis.title.x = element_text(hjust = 0.5, size = 22),
        axis.title.y = element_text(hjust = 1, size = 22),
        axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5, size = 22),
        axis.text.y = element_text(size = 22),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22))
print(dftotalgraphen)
ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/pourcentage_mandat_1_2_3_en.png", plot = dftotalgraphen, width = 12, height = 8)


##
## VERSION 3 avec _SharedFolderpolimetre-dev/Catégories d'enjeux Polimètre/Polimètres.xlsx

dfexcelchap1 <- read.xlsx("_SharedFolder_livre_promesses-trudeau/Chapitre 1/Polimètres.xlsx") |>
  filter(`Inclusion` == TRUE) |>
  filter(grepl("Trudeau", gov, ignore.case = TRUE))

dftest42 <- dfexcelchap1 |>
  group_by(`Catégorie.Polimètre`, `mandat`) |>
  summarise(Value = n()) |>
  filter(`mandat` %in% c("1", "2", "3"))

colnames(dftest42) <- c("Catégories", "Mandat", "Value" )

dftest42$Catégories <- gsub("et ", "\n et ", dftest42$Catégories)

## Réordonner les colomnes en facteurs, et les réordonner comme souhaité
dftest42$Mandat <- as.character(dftest42$Mandat)

dftest42$Mandat <- factor(dftest42$Mandat, levels = c("1", "2", "3"))

## Palette de couleur

pourcentage_palette <- c("#CCCCCC", "#666666", "black")

## création du graphique

dfbind_percent <- dftest42 %>%
  group_by(Mandat)  %>%
  mutate(Percentage = Value / sum(Value) * 100)

dftotalgraph <- ggplot(dfbind_percent, aes(x = Catégories, y = Percentage, fill = Mandat)) +
  geom_bar(stat = "identity", position = "dodge")  +
  geom_text(aes(label = paste0(round(Percentage), "%")), vjust = -0.5,
            position = position_dodge(width = 0.9),
            size = 3.5) +
  scale_fill_manual("Mandat", values = pourcentage_palette) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Catégorie d'enjeux",
       y = "% de promesses\nformulées par catégorie") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(axis.title.x = element_text(hjust = 0.5, size = 22),
        axis.title.y = element_text(hjust = 1, size = 22),
        axis.text.x = element_text(angle = 65, hjust = 0.95, size = 22),
        axis.text.y = element_text(size = 22),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22))

## Impression du ggplot
print(dftotalgraph)

## Exportation 

ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/pourcentage_mandat_1_2_3_V2.png", plot = dftotalgraph, width = 12, height = 6)

