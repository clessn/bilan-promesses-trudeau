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
ggplot(GraphData, aes(x = reorder(Gouvernement, Année.de.début), y = Pourcent,
                      fill = Verdict)) +
  geom_bar(stat = "identity") +
  scale_fill_grey("\n\n\n\n\n\n\nVerdict") +
  #theme_classic() +
  scale_x_discrete("") +
  scale_y_continuous("% des promesses") +
  theme(axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())
ggsave(paste0("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/",
              "VerdictsParMandat.png"))

