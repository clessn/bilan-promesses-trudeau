pacman::p_load(tidyverse, openxlsx, stringdist, gridExtra, ggtext)
Data <- openxlsx::read.xlsx(paste0(
  "../polimetre-dev/_SharedFolder_polimetre-fonctionnement/",
  "14. BD/BD_Polimètre.xlsx"), 2) |>
  select(X1, X2, X3, Réalisée, Partiellement.réalisée, Rompue, n, X4) |>
  rename(Endroit = X1, Législature = X2, Premier.ministre = X3)
Data <- Data[-c(11:22), ]
Data[1:2, ][c(2, 4:7)] <- c(
  44, 43, # mandat
  140, 79, # réalisée
  78, 99, # partiellement
  15, 165, # rompue
  354, 343) # total Trudeau II-III
Data[1, ][c(1, 3, 8)] <- c("CAN", "Trudeau", "Min") # Trudeau III
Data$Statut <- ifelse(Data$X4 == "Maj", "Majoritaire", "Minoritaire")
Data$Années <- c("2021-2025", "2019-2021", "2015-2019", "2011-2015",
                 "2008-2011", "2006-2008", "2004-2006", "2000-2004",
                 "1997-2000", "1993-1997")
Data$Année.de.début <- as.numeric(substr(Data$Années, 1, 4))
Data$Gouvernement <- paste0(Data$Premier.ministre, " ", Data$Années, " (n = ",
                            Data$n, ")")
Data$PourcentRéalisées <- 100 * Data$Réalisée / Data$n
Data$PourcentPartiellementRéalisées <- 100 * Data$Partiellement.réalisée /
  Data$n
Data$PourcentRompues <- 100 * Data$Rompue / Data$n
Data$PourcentAutre <- 100 * (
  Data$n - Data$Réalisée - Data$Partiellement.réalisée - Data$Rompue) / Data$n
Data$PourcentSuspens <- NA
Data$PourcentEnVoie <- NA
Data$PourcentSuspens[1] <- 100 * (34 / 354)
Data$PourcentEnVoie[1] <- 100 * (87 / 354)
DataFull <- filter(Data, Législature < 44)
mean(DataFull$PourcentRéalisées)
mean(DataFull$PourcentPartiellementRéalisées)
mean(DataFull$PourcentRompues)
GraphData <- pivot_longer(
  Data, cols = c(PourcentRéalisées, PourcentPartiellementRéalisées,
                 PourcentRompues, PourcentSuspens, PourcentEnVoie),
  names_to = "Verdict", values_to = "Pourcent")
GraphData$Verdict <- factor(
  GraphData$Verdict,
  levels = c("PourcentRéalisées", "PourcentPartiellementRéalisées",
             "PourcentEnVoie", "PourcentSuspens", "PourcentRompues"),
  labels = c("Réalisées", "Partiellement\nréalisées",
             "En voie de\nréalisation\n(Trudeau 44)",
             "En suspens\n(Trudeau 44)", "Rompues"))
GraphData$PourcentText <- str_replace_all(
  round(GraphData$Pourcent, 2), "\\.", ",")
GraphData$PercentText <- round(GraphData$Pourcent, 2)

bold.labels <- ifelse(levels(as.factor(GraphData$Gouvernement)) %in% c(
  "Trudeau 2015-2019 (n = 353)", "Trudeau 2019-2021 (n = 343)",
  "Trudeau 2021-2025 (n = 354)"), yes = "bold", no = "plain")
verdict_palette3 <- c("#CCCCCC", "#AAAAAA", "#888888", "#555555", "black")
verdict_palette6 <- c("black", "white", "white", "white", "white")

ggplot(GraphData, aes(x = reorder(Gouvernement, Année.de.début), y = Pourcent,
                      fill = Verdict)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = PourcentText, color = Verdict),
            show.legend = FALSE, position = position_fill(vjust = 0.5),
            size = 5) +
  scale_fill_manual(values = verdict_palette3) +
  scale_color_manual(values = verdict_palette6) +
  scale_x_discrete("") +
  scale_y_continuous("% des promesses\n",
                     labels = scales::percent_format(scale = 100)) +
  clessnverse::theme_clean_light(base_size = 17) +
  theme(axis.text.x = ggtext::element_markdown(hjust = 0.75, vjust = 0.75, angle = 90,
                                               face = bold.labels),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())
ggsave(paste0("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/",
              "VerdictsParMandat.png"), width = 9, height = 8)

GraphData$VerdictEN <- NA
GraphData$VerdictEN <- factor(
  GraphData$Verdict,
  levels = c("Réalisées", "Partiellement\nréalisées",
             "En voie de\nréalisation\n(Trudeau 44)",
             "En suspens\n(Trudeau 44)", "Rompues"),
  labels = c("Kept", "Partially kept", "In progress\n(Trudeau 44)",
             "Not yet rated\n(Trudeau 44)", "Broken"))
ggplot(GraphData, aes(x = reorder(Gouvernement, Année.de.début),
                      y = Pourcent, fill = VerdictEN)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = PercentText), position = position_fill(vjust = 0.5),
            size = 2.5, color = "white") +
  scale_fill_manual("\n\n\n\n\n\n\nVerdict", values = verdict_palette3) +
  scale_x_discrete("") +
  scale_y_continuous("% of promises\n",
                     labels = scales::percent_format(scale = 100)) +
  theme(axis.text.x = ggtext::element_markdown(hjust = 1, vjust = 0.5, angle = 90,
                                               face = bold.labels),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())
ggsave(paste0("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/",
              "VerdictsParMandat-EN.png"), width = 5.5, height = 4.25)

Promesses <- openxlsx::read.xlsx(
  "_SharedFolder_livre_promesses-trudeau/Chapitre 1/BDTrudeau-Chap1.xlsx",
  3) |> filter(`Inclusion.Polimètre./.Inclusion.Polimeter` == T)
Promises <- openxlsx::read.xlsx(paste0(
  "../polimetre-dev/_SharedFolder_polimetre-fonctionnement/",
  "14. BD/BD_Polimètre.xlsx"), 3)
Promises <- filter(Promises, Origine == "CAN")
mean(nchar(Promises$Libellé.en)[Promises$L == 35])
mean(nchar(Promises$Libellé.fr)[Promises$L == 35])
mean(nchar(Promises$Libellé.en)[Promises$L == 36])
mean(nchar(Promises$Libellé.fr)[Promises$L == 36])
mean(nchar(Promises$Libellé.en)[Promises$L == 37])
mean(nchar(Promises$Libellé.fr)[Promises$L == 37])
mean(nchar(Promises$Libellé.en)[Promises$L == 38])
mean(nchar(Promises$Libellé.fr)[Promises$L == 38])
mean(nchar(Promises$Libellé.en)[Promises$L == 39])
mean(nchar(Promises$Libellé.fr)[Promises$L == 39])
mean(nchar(Promises$Libellé.en)[Promises$L == 40])
mean(nchar(Promises$Libellé.fr)[Promises$L == 40])
mean(nchar(Promises$Libellé.en)[Promises$L == 41])
mean(nchar(Promises$Libellé.fr)[Promises$L == 41])
mean(nchar(Promesses$`Libellé.EN./.Label.EN`)[
  Promesses$`Mandat./.Mandate` == 1])
mean(nchar(Promesses$`Libellé.FR./.Label.FR`)[
  Promesses$`Mandat./.Mandate` == 1])
mean(nchar(Promesses$`Libellé.EN./.Label.EN`)[
  Promesses$`Mandat./.Mandate` == 2])
mean(nchar(Promesses$`Libellé.FR./.Label.FR`)[
  Promesses$`Mandat./.Mandate` == 2])
mean(nchar(Promesses$`Libellé.EN./.Label.EN`)[
  Promesses$`Mandat./.Mandate` == 3])
mean(nchar(Promesses$`Libellé.FR./.Label.FR`)[
  Promesses$`Mandat./.Mandate` == 3])
mean(nchar(Promises$Libellé.en), na.rm = T)
mean(nchar(Promises$Libellé.fr[nchar(Promises$Libellé.fr) > 3]), na.rm = T)
mean(nchar(Promesses$`Libellé.EN./.Label.EN`))
mean(nchar(Promesses$`Libellé.FR./.Label.FR`))
libelle <- tolower(Promesses$`Libellé.FR./.Label.FR`)
Promesses$autochtone <- str_detect(
  string = libelle,
  pattern = "autochton|premières? nation|inuit|metis|métis|dnudpa")
table(Promesses$autochtone, Promesses$`Mandat./.Mandate`)
label <- tolower(Promesses$`Libellé.EN./.Label.EN`)
Promesses$indigenous <- str_detect(
  string = label,
  pattern = "aborigin|indigen|first nation|inuit|métis|metis|undrip")
prop.table(table(Promesses$indigenous))
Promises$indigenous <- str_detect(
  string = tolower(Promises$Libellé.en),
  pattern = "aborigin|indigen|first nation|inuit|métis|metis|undrip")
prop.table(table(Promises$indigenous[Promises$L != 42]))
Promesses$provinc <- str_detect(
  string = label,
  pattern = "provinc")
prop.table(table(Promesses$provinc, Promesses$`Mandat./.Mandate`,
                 Promesses$`Inclusion.Polimètre./.Inclusion.Polimeter`),
           margin = 2)
Promises$provinc <- str_detect(
  string = tolower(Promises$Libellé.en),
  pattern = "provinc")
prop.table(table(Promises$provinc, Promises$L), margin = 2)
Promesses$collab <- str_detect(
  string = label,
  pattern = "work with|consult|collab|coop|co-op")
prop.table(table(Promesses$collab, Promesses$`Mandat./.Mandate`,
                 Promesses$`Inclusion.Polimètre./.Inclusion.Polimeter`),
           margin = 2)
Promises$collab <- str_detect(
  string = tolower(Promises$Libellé.en),
  pattern = "work with|consult|collab|coop|co-op")
prop.table(table(Promises$collab, Promises$L), margin = 2)
IndigenousData <- data.frame(
  en = label[Promesses$indigenous + Promesses$autochtone == 1],
  fr = libelle[Promesses$indigenous + Promesses$autochtone == 1])
Promesses$indigautoch <- 0
Promesses$indigautoch[Promesses$indigenous == TRUE |
                        Promesses$autochtone == TRUE] <- 1
table(Promesses$indigautoch, Promesses$`Mandat./.Mandate`)
PromessesAlt <- filter(Promesses, indigautoch != 1)
table(Promesses$Nouvelle.catégorie, Promesses$`Mandat./.Mandate`)
GSN <- transform(Promesses, n = nchar(as.character(`Libellé.FR./.Label.FR`)))
GSN2 <- GSN[with(GSN, order(n, Libellé.FR...Label.FR)), ]
Promesses$indigcollab[Promesses$indigenous == TRUE &
                        Promesses$collab == TRUE] <- 1
table(Promesses$indigcollab, Promesses$`Mandat./.Mandate`)
Promises$indigcollab[Promises$indigenous == TRUE &
                        Promises$collab == TRUE] <- 1
table(Promises$indigcollab, Promises$L)

# Tri Trudeau
AllPromises <- openxlsx::read.xlsx(paste0(
  "../polimetre-dev/_SharedFolder_polimetre-fonctionnement/6. Polimètre",
  " Fédéral (Trudeau-44)/",
  "3. Divers/Sélection des promesses étroites et importantes/TriFinal.xlsm")
)[1:577,]
AllPromisesRevised <- openxlsx::read.xlsx(paste0(
  "../polimetre-dev/_SharedFolder_polimetre-fonctionnement/6. Polimètre",
  " Fédéral (Trudeau-44)/PolimètreTrudeau.xlsx"), 2) |>
  filter(Mandat == 3)
matched_rows <- matrix(NA, nrow = nrow(AllPromisesRevised),
                       ncol = nrow(AllPromises))
for (i in 1:nrow(AllPromisesRevised)) {
  for (j in 1:nrow(AllPromises)) {
    if (stringdist::stringdistmatrix(AllPromisesRevised$Libellé.FR[i],
                                     AllPromises$`Libellé.(fr)`[j]) <= 30) {
      matched_rows[i, j] <- TRUE
    } else {
      matched_rows[i, j] <- FALSE
    }
  }
}
# Get indices of matched rows
matched_indices <- which(matched_rows, arr.ind = TRUE)
setdiff(1:570, matched_indices[, 1])
setdiff(1:577, matched_indices[, 2])

# corrélations
cor(AllPromises$`Importance.pour.la.société.0-10.MOYENNE`,
    AllPromises$`Large.0-.étroit.10.MOYENNE`)
cor(AllPromises$`Importance.pour.la.société.0-10.PONDÉRÉE`,
    AllPromises$`Large.0-.étroit.10.PONDÉRÉ`)
# la pondération diminue la corrélation

# distribution univariée avant et après pondération, étroitesse et importance
Plot1 <- ggplot(AllPromises, aes(
  x = `Importance.pour.la.société.0-10.MOYENNE`)) + geom_bar(width = 0.5) +
  xlab("Importance de la promesse pour la société (moyenne non pondérée)") +
  ylab("Nombre de promesses")
Plot2 <- ggplot(AllPromises, aes(x = `Large.0-.étroit.10.MOYENNE`)) +
  geom_bar(width = 0.5) +
  xlab("Degré d'étroitesse de la promesse (moyenne non pondérée)") +
  ylab("Nombre de promesses")
Plot3 <- ggplot(AllPromises, aes(
  x = `Importance.pour.la.société.0-10.PONDÉRÉE`)) + geom_histogram() +
  xlab("Importance de la promesse pour la société (moyenne pondérée)") +
  ylab("Nombre de promesses")
Plot4 <- ggplot(AllPromises, aes(x = `Large.0-.étroit.10.PONDÉRÉ`)) +
  geom_histogram() +
  xlab("Degré d'étroitesse de la promesse (moyenne pondérée)") +
  ylab("Nombre de promesses")
Plots <- gridExtra::grid.arrange(Plot1, Plot2, Plot3, Plot4)
#ggsave(paste0("_SharedFolder_polimetre-dev/graphs/",
#              "ImportanceEtroitessePromessesTrudeau.png"), width = 11,
#       height = 8.5, Plots)

# suppression de promesses, critère écart-type
PromisesToDeleteSD <- AllPromises |>
  filter(`Large.0-.étroit.10.PONDÉRÉ` <
           (mean(`Large.0-.étroit.10.PONDÉRÉ`) -
              sd(`Large.0-.étroit.10.PONDÉRÉ`) * 0.5) |
           `Importance.pour.la.société.0-10.PONDÉRÉE` < 
           (mean(`Importance.pour.la.société.0-10.PONDÉRÉE`) -
              sd(`Importance.pour.la.société.0-10.PONDÉRÉE`) * 0.5))
length(AllPromises$`Libellé.(fr)`) - length(PromisesToDeleteSD$`Libellé.(fr)`)
PromisesToDeleteByDomainSD <- data.frame(
  all = table(AllPromises$Domaine),
  to_delete = table(PromisesToDeleteSD$Domaine))
PromisesToDeleteByDomainSD$prop <- PromisesToDeleteByDomainSD$to_delete.Freq /
  PromisesToDeleteByDomainSD$all.Freq

# suppression de promesses, critère quantile
PromisesToDeleteQuantile <- AllPromises |>
  filter(`Large.0-.étroit.10.PONDÉRÉ` <
           quantile(`Large.0-.étroit.10.PONDÉRÉ`, 0.25) |
           `Importance.pour.la.société.0-10.PONDÉRÉE` < 
           quantile(`Importance.pour.la.société.0-10.PONDÉRÉE`, 0.25))
length(AllPromises$`Libellé.(fr)`) -
  length(PromisesToDeleteQuantile$`Libellé.(fr)`)
PromisesToDeleteByDomainQuantile <- data.frame(
  all = table(AllPromises$Domaine),
  to_delete = table(PromisesToDeleteQuantile$Domaine))
PromisesToDeleteByDomainQuantile$prop <-
  PromisesToDeleteByDomainQuantile$to_delete.Freq /
  PromisesToDeleteByDomainQuantile$all.Freq
PromisesToDeleteByDomainQuantile$to_delete.Var1 <- NULL
names(PromisesToDeleteByDomainQuantile) <- c(
  "Domaine", "Nombre actuel de promesses",
  "Nombre de promesses à faible importance ou très larges",
  "% de promesses à faible importance ou très larges")
#openxlsx::write.xlsx(
#  PromisesToDeleteByDomainQuantile,
#  "_SharedFolder_polimetre-dev/data/PromisesToDeleteByDomainQuantile.xlsx")
#openxlsx::write.xlsx(
#  PromisesToDeleteQuantile,
#  "_SharedFolder_polimetre-dev/data/PromisesToDeleteQuantile.xlsx")
