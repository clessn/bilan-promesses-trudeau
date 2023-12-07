library(tidyverse)
Data <- openxlsx::read.xlsx(paste0(
  "../polimetre-dev/_SharedFolder_polimetre-fonctionnement/",
  "14. BD/BD_Polimètre.xlsx"), 2) |>
  select(X1, X2, X3, Réalisée, Partiellement.réalisée, Rompue, n, X4) |>
  rename(Endroit = X1, Législature = X2, Premier.ministre = X3)
Data <- Data[-c(11:22), ]
Data[1:2, ][c(2, 4:7)] <- c(
  44, 43, 106, 79, 60, 99, 9, 165, 353, 343) # Trudeau II-III
Data[1, ][c(1, 3, 8)] <- c("CAN", "Trudeau", "Min") # Trudeau III
Data$Statut <- ifelse(Data$X4 == "Maj", "Majoritaire", "Minoritaire")
Data$Années <- c("2021-...", "2019-2021", "2015-2019", "2011-2015",
                 "2008-2011", "2006-2008", "2004-2006", "2000-2004",
                 "1997-2000", "1993-1997")
Data$Année.de.début <- as.numeric(substr(Data$Années, 1, 4))
Data$Gouvernement <- paste0(Data$Premier.ministre, " ", Data$Années, " (n = ",
                            Data$n, ")")
Data$Government <- paste0(Data$Premier.ministre, " ", Data$Années, " (n = ",
                          Data$n, ")")
Data$PourcentRéalisées <- 100 * Data$Réalisée / Data$n
Data$PourcentPartiellementRéalisées <- 100 * Data$Partiellement.réalisée /
  Data$n
Data$PourcentRompues <- 100 * Data$Rompue / Data$n
Data$PourcentAutre <- 100 * (
  Data$n - Data$Réalisée - Data$Partiellement.réalisée - Data$Rompue) / Data$n
DataFull <- filter(Data, Législature < 44)
mean(DataFull$PourcentRéalisées)
mean(DataFull$PourcentPartiellementRéalisées)
mean(DataFull$PourcentRompues)
GraphData <- pivot_longer(
  Data, cols = c(PourcentRéalisées, PourcentPartiellementRéalisées,
                 PourcentRompues, PourcentAutre),
  names_to = "Verdict", values_to = "Pourcent")
GraphData$Verdict <- factor(
  GraphData$Verdict,
  levels = c("PourcentRéalisées", "PourcentPartiellementRéalisées",
             "PourcentAutre", "PourcentRompues"),
  labels = c("Réalisées", "Partiellement\nréalisées",
             "En suspens/\nEn voie de\nréalisation\n(Trudeau 44)", "Rompues"))
GraphData$PourcentText <- str_replace_all(
  round(GraphData$Pourcent, 2), "\\.", ",")
ggplot(GraphData, aes(x = reorder(Gouvernement, Année.de.début), y = Pourcent,
                      fill = Verdict)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = PourcentText)) +
  scale_fill_grey("\n\n\n\n\n\n\nVerdict") +
  scale_x_discrete("") +
  scale_y_continuous("% des promesses") +
  theme(axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())
ggsave(paste0("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/",
              "VerdictsParMandat.png"))

Promesses <- openxlsx::read.xlsx(paste0(
  "_SharedFolder_livre_promesses-trudeau/Chapitre 1/PolimètreTrudeau-Chapitre",
  "1.xlsx"), 3)
Promises <- openxlsx::read.xlsx(paste0(
  "../polimetre-dev/_SharedFolder_polimetre-fonctionnement/",
  "14. BD/BD_Polimètre.xlsx"), 3)
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
mean(nchar(Promesses$Libellé.EN)[Promesses$Mandat == 1])
mean(nchar(Promesses$Libellé.FR)[Promesses$Mandat == 1])
mean(nchar(Promesses$Libellé.EN)[Promesses$Mandat == 2])
mean(nchar(Promesses$Libellé.FR)[Promesses$Mandat == 2])
mean(nchar(Promesses$Libellé.EN)[Promesses$Mandat == 3])
mean(nchar(Promesses$Libellé.FR)[Promesses$Mandat == 3])
mean(nchar(Promises$Libellé.en), na.rm = T)
mean(nchar(Promises$Libellé.fr), na.rm = T)
mean(nchar(Promesses$Libellé.EN))
mean(nchar(Promesses$Libellé.FR))

# Tri Trudeau
AllPromises <- openxlsx::read.xlsx(paste0(
  "../polimetre-dev/_SharedFolder_polimetre-fonctionnement/6. Polimètre",
  " Fédéral (Trudeau-44)/",
  "3. Divers/Sélection des promesses étroites et importantes/TriFinal.xlsm")
)[1:577,]
AllPromisesRevised <- openxlsx::read.xlsx(paste0(
  "../polimetre-dev/_SharedFolder_polimetre-fonctionnement/6. Polimètre",
  " Fédéral (Trudeau-44)/PolimètreTrudeau.xlsx"), 2) |>
  dplyr::filter(Mandat == 3)
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
  dplyr::filter(`Large.0-.étroit.10.PONDÉRÉ` <
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
  dplyr::filter(`Large.0-.étroit.10.PONDÉRÉ` <
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
