library(tidyverse)
library(openxlsx)

dfexcelchap1 <- openxlsx::read.xlsx(
  "_SharedFolder_livre_promesses-trudeau/Chapitre 1/BDTrudeau-Chap1.xlsx",
  3) |>
  filter(`Inclusion.Polimètre./.Inclusion.Polimeter` == TRUE) |>
  filter(`Mandat./.Mandate` %in% c("2", "3"))
dftest2 <- dfexcelchap1 |>
  group_by(Nouvelle.catégorie, `Mandat./.Mandate`, Verdict) |>
  summarise(Value = n())
colnames(dftest2) <- c("Catégories", "Mandat", "Status", "Value" )

##
### Transformer la variable Status en Facteur et ordonner comme souhaité
##
dftest2$Status[dftest2$Status == "Partiellement réalisée"] <-
  "Partiellement\nréalisée"
dftest2$Status[dftest2$Status == "En voie de réalisation"] <-
  "En voie de\nréalisation"
dftest2$Status <- factor(dftest2$Status, levels = c(
  "Réalisée", "Partiellement\nréalisée", "En voie de\nréalisation",
  "En suspens", "Rompue"))

## Création de la palette de couleur 

verdict_palette1 <- c("#228B22", "#F3C349", "#FF8C00", "#444444", "#AE0101")
verdict_palette2 <- c("#228B22", "#F3C349", "#AE0101")
verdict_palette3 <- c("#CCCCCC", "#AAAAAA", "#888888", "#555555", "black")
verdict_palette4 <- c("#CCCCCC", "#666666", "black")
verdict_palette5 <- c("black", "white", "white")
verdict_palette6 <- c("black", "black", "white", "white", "white")

## graphique du 2eme mandat

dftest2_percent <- dftest2 |>
  filter(Mandat == "2") |>
  group_by(Catégories) |>
  mutate(Value2 = sum(Value),
         Catégories2 = paste0(Catégories, " (N = ", Value2, ")")) |>
  mutate(Percentage = Value / sum(Value) * 100)

ggplot(dftest2_percent, aes(
  x = Percentage, y = reorder(Catégories2, desc(Catégories2)),
  fill = Status)) +
  geom_bar(stat = "identity")  +
  geom_text(data = dftest2_percent,
            aes(label = round(Percentage), color = Status),
            show.legend = FALSE,
            position = position_stack(vjust = 0.5),
            size = 4) +
  scale_fill_manual(values = verdict_palette4) +
  scale_color_manual(values = verdict_palette5) +
  labs(x = "Pourcentage de promesses",
       y = "Catégories d'enjeux",
       fill = "État de réalisation") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.title = element_text(),
        legend.position = "right")
        
ggsave(paste0("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/",
              "mandat2_plot.png"), width = 9, height = 6)

## graphique du 3eme mandat

dftest3_percent <- dftest2 |>
  filter(Mandat == "3") |>
  group_by(Catégories) |>
  mutate(Value2 = sum(Value),
         Catégories2 = paste0(Catégories, " (N = ", Value2, ")")) |>
  mutate(Percentage = Value / sum(Value) * 100)

ggplot(dftest3_percent, aes(
  x = Percentage, y = reorder(Catégories2, desc(Catégories2)),
  fill = Status)) +
  geom_bar(stat = "identity")  +
  geom_text(data = dftest3_percent,
            aes(label = round(Percentage), color = Status),
            show.legend = FALSE,
            position = position_stack(vjust = 0.5),
            size = 4) +
  scale_fill_manual(values = verdict_palette3) +
  scale_color_manual(values = verdict_palette6) +
  labs(x = "Pourcentage de promesses",
       y = "Catégories d'enjeux",
       fill = "État de réalisation") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.title = element_text(),
        legend.position = "right")
ggsave(paste0("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/",
              "mandat3_plot.png"), width = 9, height = 6)

## ANGLAIS

dfexcelchap1 <- read.xlsx("_SharedFolder_livre_promesses-trudeau/Chapitre 1/BDTrudeau-Chap1.xlsx", 3) |>
  filter(`Inclusion.Polimètre./.Inclusion.Polimeter` == TRUE)
dfcategoverdiENG <- dfexcelchap1 |>
  group_by(`Catégorie./.Category`, `Mandat./.Mandate`, Verdict,) |>
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
  mutate(Value2 = sum(Value)) |>
  mutate(Percentage = Value / sum(Value) * 100)
KeptPartiallyData <- dfcategoverdiENG_percent |>
  filter(Status %in% c("Kept", "Partially kept")) |>
  group_by(Categories, Term) |>
  summarise(Percentage = sum(Percentage))
KeptPartiallyData[12,1] <- "Identity and Nationalism"
KeptPartiallyData[12,2] <- "2"
KeptPartiallyData[12,3] <- 0
KeptPartiallyData <- arrange(KeptPartiallyData, Percentage)
KeptPartiallyData$id <- 1:nrow(KeptPartiallyData)
dfcategoverdiENG_percent$id <- mgsub::mgsub(
  string = dfcategoverdiENG_percent$Categories,
  pattern = KeptPartiallyData$Categories,
  replacement = KeptPartiallyData$id)
dfcategoverdiENG_percent$Categories2 <- paste0(
  dfcategoverdiENG_percent$Categories, " (N = ",
  dfcategoverdiENG_percent$Value2, ")")
graphmandat2ENG <- dfcategoverdiENG_percent |>
  ggplot(aes(x = Percentage, y = reorder(Categories2, as.numeric(id)),
             fill = Status)) +
  geom_bar(stat = "identity")  +
  geom_text(data = dfcategoverdiENG_percent, 
            aes(label = round(Percentage)),
            position = position_stack(vjust = 0.5),
            size = 4, color = "white") +
  scale_fill_manual(values = verdict_palette2) +
  labs(
    #title = "Second-term verdicts by issue category",
    x = "Percent of promises",
    y = "Issue categories",
    fill = "Legend Title") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5), 
    axis.title.x = element_text(size = 15, hjust = 0.5),
    axis.title.y = element_text(size = 15, hjust = 0.5),
    axis.text = element_text(size = 12),            
    legend.position = "right")

print(graphmandat2ENG)
ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/mandat2ENG_plot.png", plot = graphmandat2ENG, width = 12, height = 6
)


## graphique du 3eme mandat
dfcategoverdiENG_percent <- dfcategoverdiENG %>%
  filter(Term == 3) %>%
  group_by(Categories) %>%
  mutate(Value2 = sum(Value)) |>
  mutate(Percentage = Value / sum(Value) * 100)
KeptPartiallyData <- dfcategoverdiENG_percent |>
  filter(Status %in% c("Kept", "Partially kept")) |>
  group_by(Categories, Term) |>
  summarise(Percentage = sum(Percentage)) |>
  arrange(Percentage)
KeptPartiallyData$id <- 1:nrow(KeptPartiallyData)
dfcategoverdiENG_percent$id <- mgsub::mgsub(
  string = dfcategoverdiENG_percent$Categories,
  pattern = KeptPartiallyData$Categories,
  replacement = KeptPartiallyData$id)
dfcategoverdiENG_percent$Categories2 <- paste0(
  dfcategoverdiENG_percent$Categories, " (n = ",
  dfcategoverdiENG_percent$Value2, ")")
graphmandat3ENG <- dfcategoverdiENG_percent |>
  ggplot(aes(x = Percentage, y = reorder(Categories2, as.numeric(id)),
             fill = Status)) +
  geom_bar(stat = "identity")  +
  geom_text(data = dfcategoverdiENG_percent, 
            aes(label = round(Percentage)), position = position_stack(vjust = 0.5),
            size = 4, color = "white") +
  scale_fill_manual(values = verdict_palette1) +
  labs(
    #title = "Third-term verdicts by issue category",
    x = "Percent of promises",
    y = "Issue categories",
    fill = "Legend Title") +
  clessnverse::theme_clean_light(base_size = 15) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5), 
    axis.title.x = element_text(size = 15, hjust = 0.5),
    axis.title.y = element_text(size = 15, hjust = 0.5),
    axis.text = element_text(size = 12),            
    legend.position = "right")
print(graphmandat3ENG)

ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/mandat3ENG_plot.png", plot = graphmandat3ENG, width = 12, height = 6
)
