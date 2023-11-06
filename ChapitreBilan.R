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
  xlab("") +
  ylab("% des promesses") +
  theme(axis.text.x = element_text(hjust = 1, angle = 90)) +
  scale_fill_manual("\n\n\n\n\n\n\nVerdict", values = c(
    "#228B22", "#F3C349", "#FF8C00", "#AE0101"))
ggsave(paste0("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/",
              "VerdictsParMandat.png"))

#### même graph mais après l'élection ####
Data2 <- Data[-13, ]
Data2$PourcentRéalisées[13] <- 100 * 138 / 251
Data2$PourcentPartiellementRéalisées[13] <- 100 * 61 / 251
Data2$PourcentRompues[13] <- 100 * 52 / 251
Data2$Statut <- stringr::str_replace_all(Data2$Statut, "Minoritaire",
                                         "Minoritaire (+ pâle)")
Data2$Gouvernement <- paste0(Data2$Premier.ministre, "\n", Data2$Années,
                             " (n = ", Data2$n, ")")
GraphData2 <- tidyr::pivot_longer(
  Data2, cols = c(PourcentRéalisées, PourcentPartiellementRéalisées,
                 PourcentRompues),
  names_to = "Verdict", values_to = "Pourcent")
GraphData2$Verdict <- factor(GraphData2$Verdict,
                            levels = c("PourcentRéalisées",
                                       "PourcentPartiellementRéalisées",
                                       "PourcentAutre",
                                       "PourcentRompues"),
                            labels = c("Réalisées", "Partiellement\nréalisées",
                                       "En suspens/\nEn voie de\nréalisation\n(Legault)",
                                       "Rompues"))
GraphData2$Endroit <- factor(GraphData2$Endroit, levels = c("QC", "CAN"),
                            labels = c("Québec", "Canada"))
sysfonts::font_add_google("Roboto", "roboto")
showtext::showtext_auto()
ggplot2::ggplot(GraphData2, ggplot2::aes(x = reorder(Gouvernement, Année.de.début),
                                        y = Pourcent, fill = Verdict)) +
  ggplot2::geom_bar(ggplot2::aes(alpha = Statut), stat = "identity") +
  ggplot2::xlab("") +
  ggplot2::ylab("% des promesses") +
  ggplot2::facet_wrap(~ Endroit, scales = "free_x") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, angle = 90)) +
  ggplot2::scale_fill_manual("Verdict", values = c(
    "#228B22", "#F3C349", "#AE0101")) +
  ggplot2::scale_alpha_discrete(range = c(1, 0.65)) +
  ggplot2::guides(fill = ggplot2::guide_legend(order = 1),
                  alpha = ggplot2::guide_legend(order = 2)) +
  clessnverse::theme_clean_dark(base_size = 15, base_margin = 5) +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90,
                                                   lineheight = 0.35),
                 legend.key.size = ggplot2::unit(0.2, "lines"),
                 plot.title = ggplot2::element_text(
                   lineheight = 0.35, margin = ggplot2::margin(-10, -20, 0, 0)),
                 plot.subtitle = ggplot2::element_text(lineheight = 0.35),
                 legend.text = ggplot2::element_text(lineheight = 0.35),
                 legend.box.margin = ggplot2::margin(-20, 0, 0, 0),
                 plot.background = ggplot2::element_rect(fill = "#494949",
                                                         colour = "#494949")) +
  ggplot2::ggtitle("Réalisation des promesses", subtitle =
                     paste0("Comment se comparent les gouvernements ",
                            "québécois et canadiens récents?"))
ggplot2::ggsave(paste0(
  "_SharedFolder_ouvrage-collectif_legault_caq/ChapitreBilan/",
  "Figure-VerdictsParMandat-QC-CA.png"), width = 1200, height = 675,
  units = "px")

#### Comparaison entre gouvernements ####
GraphTemporel <- data.frame(
  pourcent = c(22.7, 3.2, 43, 28.7, 2.4, 45.4, 11.2, 28.7, 10.8, 4,
               46.8, 15.8, NA, 25.9, 11.4, 58.9, 20.9, NA, 5.7,
               14.6,
               26.5, 10.3, 18.4, 30.1, 14.7, 36.8, 17.6, 22.8,
               10.3, 12.5,
               53.8, 23.8, NA, 18.1, 4.2, 61.5, 30.3, NA, 2.8,
               5.4,
               34.3, 2.1, NA, 60.8, 2.8, 72, 6.3, NA, 12.6,
               9.1),
  gouvernement = c(
    rep("Legault 2018-2022\n(QC, n = 251)", 10),
    rep("Couillard 2014-2018\n(QC, n = 158)", 10),
    rep("Ford 2018-2022\n(ON, n = 136)*", 10),
    rep("Trudeau 2015-2019\n(CA, n = 353)", 10),
    rep("Harper 2011-2015\n(CA, n = 143)", 10)),
  moment = rep(c(rep("17 mois après\nl'élection", 5),
                 rep("7 mois avant\nl'élection suivante", 5)), 5),
  verdict = rep(c("Réalisées", "Partiellement\nréalisées",
                  "En voie de\nréalisation", "En suspens", "Rompues")), 10)
GraphTemporel$moment <- factor(
  GraphTemporel$moment, levels = c("17 mois après\nl'élection",
                                   "7 mois avant\nl'élection suivante"))
GraphTemporel$verdict <- factor(
  GraphTemporel$verdict,
  levels = c("Réalisées", "Partiellement\nréalisées",
             "En voie de\nréalisation", "En suspens", "Rompues"))
GraphTemporel$gouvernement <- factor(
  GraphTemporel$gouvernement,
  levels = c("Legault 2018-2022\n(QC, n = 251)",
             "Couillard 2014-2018\n(QC, n = 158)",
             "Ford 2018-2022\n(ON, n = 136)*",
             "Trudeau 2015-2019\n(CA, n = 353)",
             "Harper 2011-2015\n(CA, n = 143)"))

library(tidyverse)
ggplot(GraphTemporel, aes(x = gouvernement, y = pourcent,
                          fill = verdict)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(vars(moment)) +
  theme(axis.text.x = element_text(hjust = 1, angle = 90),
        legend.position = "bottom") +
  scale_x_discrete("") +
  scale_y_continuous("% des promesses") +
  scale_fill_manual("Verdict", values = c(
    "#228B22", "#F3C349", "#FF8C00", "white", "#AE0101")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
ggsave(paste0(
  "_SharedFolder_ouvrage-collectif_legault_caq/ChapitreBilan/",
  "Figure-MomentsDuMandat.png"))

PIB <- data.frame(
  pib = c(8.9, 15.9, 5.3, 3.9, 3.3, 2.6, 9.4, 4.7))
PIB$government <- factor(c(
  "Parizeau/Bouchard 1994-1998 (97 promesses)",
  "Bouchard/Landry 1998-2003 (127 promesses)",
  "Charest 2003-2007 (106 promesses)",
  "Charest 2007-2008 (98 promesses)",
  "Charest 2008-2012 (62 promesses)",
  "Marois 2012-2014 (113 promesses)",
  "Couillard 2014-2018 (158 promesses)",
  "Legault 2018-2022 (251 promesses)"),
  levels = c(
    "Parizeau/Bouchard 1994-1998 (97 promesses)",
    "Bouchard/Landry 1998-2003 (127 promesses)",
    "Charest 2003-2007 (106 promesses)",
    "Charest 2007-2008 (98 promesses)",
    "Charest 2008-2012 (62 promesses)",
    "Marois 2012-2014 (113 promesses)",
    "Couillard 2014-2018 (158 promesses)",
    "Legault 2018-2022 (251 promesses)"),
  ordered = T)
ggplot(PIB, aes(x = government, y = pib)) +
  geom_bar(stat = "identity") +
  scale_x_discrete("Gouvernement") +
  scale_y_continuous(
    "Taux de croissance du PIB réel durant le mandat (%)",
    limits = c(0, 20)) +
  geom_text(aes(label = pib), size = 2.5, vjust = -1) +
  theme(axis.text.x = element_text(hjust = 1, angle = 90),
        axis.title.y = element_text(hjust = 1))
ggsave(paste0(
  "_SharedFolder_ouvrage-collectif_legault_caq/ChapitreBilan/",
  "PIB.png"))
