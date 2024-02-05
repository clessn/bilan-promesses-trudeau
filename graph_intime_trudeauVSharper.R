# Packages ----------------------------------------------------------------
library(ggplot2)
library(dplyr)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_livre_promesses-trudeau/Chapitre 1/data/output_graph_trudeauharper.rds")

graph <- Data %>% 
  group_by(mandate_id, date_limit) %>% 
  summarise(sum_realisee = sum(realisee),
            n = n()) %>% 
  mutate(day_in_mandate = rank(date_limit),
         prop = (sum_realisee / n) * 100,
         pm = substr(mandate_id, 1, nchar(mandate_id) - 1),
         mandate = substr(mandate_id, nchar(mandate_id), nchar(mandate_id)),
         mandate_id = factor(mandate_id, levels = c("harper1", "trudeau1", "harper2", "trudeau2", "harper3", "trudeau3")))


## Points à ajouter
# 9 mars 2020 pour COVID
## Guerre en Ukraine
DatesImportantes <- data.frame(
  date_limit = as.Date(c("2020-01-08",
                         "2020-01-20",
                         "2020-03-11",
                         "2022-02-24")),
  event = c("Avion civil ukrainien abattu en Iran",
            "Début du blocus autochtone anti-gazoduc",
            "Pandémie mondiale déclarée par l'OMS",
            "Début de l'invasion de l'Ukraine par la Russie")
) %>% 
  mutate(label = paste0(format(as.Date(date_limit), "%d %B %Y"), "\n", event)) %>% 
  left_join(graph, ., by = "date_limit") %>% 
  drop_na(event, label)


labels <- c("harper1" = "<span style='color:grey70'> Harper I (2006-2008)* </span>",
            "harper2" = "<span style='color:grey70'> Harper II (2008-2011)* </span>",
            "harper3" = "<span style='color:grey70'> Harper III (2011-2015)** </span>",
            "trudeau1" = "Trudeau I (2015-2019)**",
            "trudeau2" = "Trudeau II (2019-2021)*",
            "trudeau3" = "Trudeau III (2021-...)*")

ggplot(graph, aes(x = day_in_mandate, y = prop,
                  group = mandate_id, linetype = mandate_id,
                  color = mandate_id)) +
  geom_line(linewidth = 0.5) +
  clessnverse::theme_clean_light() +
  scale_color_manual(values = c("harper1" = "grey70", "harper2" = "grey70", "harper3" = "grey70",
                                "trudeau1" = "black", "trudeau2" = "black", "trudeau3" = "black"),
                     labels = labels) +
  scale_linetype_manual(values = c("harper1" = "dotted", "harper2" = "dashed", "harper3" = "solid",
                                   "trudeau1" = "dotted", "trudeau2" = "dashed", "trudeau3" = "solid"),
                        labels = labels) +
  ylab("Proportion des promesses réalisées, partiellement\nréalisées ou en voie de réalisation à ce jour (%)") +
  xlab("Jour dans le mandat") +
  labs(caption = "* = gouvernement minoritaire\n** = gouvernement majoritaire") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.text=ggtext::element_markdown(size=12))

ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/progression_mandats_harpertrudeau.png",
       width = 9, height = 6)
