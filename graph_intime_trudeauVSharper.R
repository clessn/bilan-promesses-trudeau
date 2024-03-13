# Packages ----------------------------------------------------------------
library(ggplot2)
library(dplyr)

# Data --------------------------------------------------------------------
Data <- readRDS("_SharedFolder_livre_promesses-trudeau/Chapitre 1/data/output_graph_trudeauharper.rds")

labels <- c("harper1" = "<span style='color:grey70'> Harper I (2006-2008)* </span>",
            "trudeau1" = "Trudeau I (2015-2019)**",
            "harper2" = "<span style='color:grey70'> Harper II (2008-2011)* </span>",
            "trudeau2" = "Trudeau II (2019-2021)*",
            "harper3" = "<span style='color:grey70'> Harper III (2011-2015)** </span>",
            "trudeau3" = "Trudeau III (2021-...)*")

graph <- Data %>% 
  group_by(mandate_id, date_limit) %>% 
  summarise(sum_realisee = sum(realisee),
            n = n()) %>% 
  mutate(day_in_mandate = rank(date_limit),
         prop = (sum_realisee / n) * 100,
         pm = substr(mandate_id, 1, nchar(mandate_id) - 1),
         mandate = substr(mandate_id, nchar(mandate_id), nchar(mandate_id)),
         mandate_id = factor(mandate_id, levels = names(labels)))

## Points à ajouter
# 9 mars 2020 pour COVID
# 15 septembre 2008 pour crise économique
DatesImportantes <- data.frame(
  date_limit = as.Date(c("2020-03-09")),
  event = c("Pandémie mondiale\ndéclarée par l'OMS")
) %>% 
  mutate(label = paste0(format(as.Date(date_limit), "%e %B %Y"), "\n", event)) %>% 
  left_join(graph, ., by = "date_limit") %>% 
  tidyr::drop_na(event, label) %>% 
  mutate(mandate_id = factor(mandate_id, levels = names(labels)))

ggplot(graph, aes(x = day_in_mandate, y = prop,
                  group = mandate_id, linetype = mandate_id,
                  color = mandate_id)) +
  geom_text(data = DatesImportantes,
            angle = 0, hjust = 0,
            y = 93, vjust = 0.5,
            aes(label = label, x = day_in_mandate - 40),
            size = 5, show.legend = FALSE) +
  geom_curve(data = DatesImportantes,
             aes(x = day_in_mandate - 40, xend = day_in_mandate - 2,
                 yend = prop), curvature = 0.2,
             y = 78, linetype = "solid", linewidth = 0.15,
             show.legend = FALSE) +
  geom_line(linewidth = 0.5) +
  geom_point(data = DatesImportantes, size = 2,
             alpha = 1, show.legend = FALSE,
             aes(y = prop - 0.5),
             shape = 21,
             fill = "grey90",
             stroke = 0.5) +
  geom_point(data = DatesImportantes,
             aes(y = prop - 0.5),
             alpha = 1, show.legend = FALSE,
             shape = 19,
             size = 0.25,
             color = "black") +
  clessnverse::theme_clean_light() +
  scale_color_manual(values = c("harper1" = "grey70", "harper2" = "grey70", "harper3" = "grey70",
                                "trudeau1" = "black", "trudeau2" = "black", "trudeau3" = "black"),
                     labels = labels, limits = names(labels)) +
  scale_linetype_manual(values = c("harper1" = "dotted", "harper2" = "dashed", "harper3" = "solid",
                                   "trudeau1" = "dotted", "trudeau2" = "dashed", "trudeau3" = "solid"),
                        labels = labels, limits = names(labels)) +
  ylab("Proportion des promesses réalisées,\npartiellement réalisées ou en voie\nde réalisation à ce jour (%)") +
  xlab("Jour dans le mandat") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100),
                     limits = c(0, 100)) +
  labs(caption = "* = gouvernement minoritaire\n** = gouvernement majoritaire\nLa crise financière mondiale de 2008 a commencé entre les deux premiers mandats de Stephen Harper.") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 16),
        axis.title.y = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        plot.caption = element_text(size = 11), 
        legend.text=ggtext::element_markdown(size=15))

ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/progression_mandats_harpertrudeau.png",
       width = 9, height = 6)


check <- graph %>% 
  group_by(mandate_id) %>% 
  filter(day_in_mandate == max(day_in_mandate))
