# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------

## Trudeau -----------------------------------------------------------------

mois_fr_to_num <- c(janvier = 1, février = 2, mars = 3, avril = 4, mai = 5, juin = 6, 
                    juillet = 7, août = 8, septembre = 9, octobre = 10, novembre = 11, décembre = 12)

## Load Trudeau II et III (only minority governments)
trudeau <- readxl::read_excel("_SharedFolder_livre_promesses-trudeau/Chapitre 1/BDTrudeau-Chap1.xlsx",
                              sheet = "Sources") %>% 
  mutate(Mandat = as.character(Mandat)) %>% 
  mutate(date = as.Date(ifelse(is.na(`Année source`), lubridate::as_date(`Date ajout`), as.Date(NA))),
         mois = ifelse(is.na(`Mois source`), 1, mois_fr_to_num[`Mois source`]),
         jour = ifelse(is.na(`Jour source`), 1, `Jour source`),
         date = as.Date(ifelse(is.na(date), lubridate::as_date(paste0(`Année source`, "-", mois, "-", jour)), as.Date(date))),
         mandate_id = paste0("trudeau", Mandat)) %>% 
  # select relevant columns
  select(mandate_id,
         pledge_id = `Numéro`,
         verdict = `Verdict (ou référence)`,
         date) %>% 
  ## keep verdicts only
  filter(verdict %in% c("En suspens",
                        "En voie de réalisation",
                        "Partiellement réalisée",
                        "Réalisée",
                        "Rompue"))

#length(unique(trudeau$pledge_id[trudeau$mandate_id == "trudeau3"]))

t <- readxl::read_excel("_SharedFolder_livre_promesses-trudeau/Chapitre 1/BDTrudeau-Chap1.xlsx",
                        sheet = "Promesses")

#### For trudeau III, need to add pledges with no source as "Rompue"
trudeauiii_nosources <- pull(readxl::read_excel("_SharedFolder_livre_promesses-trudeau/Chapitre 1/BDTrudeau-Chap1.xlsx",
                                                sheet = "Promesses") %>% 
                               filter(`Mandat / Mandate` == 3 &
                                        `Inclusion Polimètre / Inclusion Polimeter` == TRUE &
                                        !(`#` %in% trudeau$pledge_id[trudeau$mandate_id == "trudeau3"])),
                             `#`)

trudeauiii_nosourcesdf <- data.frame(
  mandate_id = "trudeau3",
  pledge_id = trudeauiii_nosources,
  verdict = "En suspens",
  date = as.Date("2021-11-22")
)

trudeau <- rbind(trudeau, trudeauiii_nosourcesdf)

## Harper ------------------------------------------------------------------

harper3940 <- readxl::read_excel("../polimetre-dev/_SharedFolder_polimetre-fonctionnement/14. BD/BD_Polimètre.xlsx",
                                 sheet = "Promesses_gouv") %>% 
  filter(Origine == "CAN" & L %in% c(39, 40)) %>% 
  mutate(mandate_id = paste0("harper", ifelse(L == 39, 1, 2))) %>% 
  rename(pledge_id = `#`,
         verdict = `Verdict Final`,
         date = `Date verdict final`) %>% 
  select(mandate_id,
         pledge_id,
         verdict,
         date)

harper41 <- readxl::read_excel("../polimetre-dev/_SharedFolder_polimetre-fonctionnement/5. Polimètres archivés/6. Polimètre Fédéral (41-Harper)/polimetre_harper-41.xlsx",
                               sheet = "Sources") %>% 
  mutate(mandate_id = "harper3",
         date = as.Date(ifelse(is.na(annee), as.character(status_changed_on), paste0(annee, "-", mois, "-", jour)))) %>% 
  # select relevant columns
  select(mandate_id,
         pledge_id = promesse,
         verdict = status,
         date)

harper <- rbind(harper3940, harper41)


# Merge both PMs ----------------------------------------------------------

data <- rbind(trudeau, harper) %>% 
  mutate(pledge_cross_id = paste0(mandate_id, pledge_id),
         realisee = case_when(
           verdict %in% c("Partiellement réalisée",
                          "Réalisée", "En voie de réalisation") ~ 1
         ),
         realisee = ifelse(is.na(realisee), 0, realisee))

table(data$verdict, data$mandate_id)
table(data$realisee, data$mandate_id)


# Next step: getting the status of multiple pledges in time ---------------

get_pledges_status <- function(data, pledges,
                               dates, status_column = "realisee") {
  # Créer une table de pledges et dates
  pledges_dates <- data.frame(pledge_cross_id = pledges, date_limit = as.Date(dates))
  # Rejoindre et filtrer
  t <- data %>%
    inner_join(pledges_dates, by = "pledge_cross_id",
               relationship = "many-to-many") %>%
    filter(date <= date_limit) %>%
    group_by(pledge_cross_id, date_limit) %>%
    filter(date == max(date)) %>% 
    distinct(., .keep_all = TRUE) %>% 
    select(last_verdict_date = date,
           pledge_cross_id, date_limit, all_of(status_column))
  pledges_infos <- data %>% 
    select(pledge_cross_id, mandate_id, pledge_id) %>% 
    distinct(.keep_all = TRUE)
  output <- left_join(pledges_dates, t, by = c("pledge_cross_id",
                                               "date_limit")) %>% 
    replace_na(list(realisee = 0)) %>% 
    left_join(., pledges_infos, by = "pledge_cross_id")
  return(output)
}

# Exemple d'utilisation :
# obtenir_verdicts_optimal(votre_dataframe, vecteur_pledges, vecteur_dates)

check <- get_pledges_status(data,
                            pledges = c("harper11.01", "harper21.01"),
                            dates = c("2006-09-13", "2011-01-01"))

# Create evolution of pledges verdicts ------------------------------------------

## For each mandate, get the range of dates of the mandate
harper1_dates <- seq(from = as.Date("2006-04-03"), to = as.Date("2008-09-07"), "days")
harper2_dates <- seq(from = as.Date("2008-11-18"), to = as.Date("2011-03-26"), "days")
harper3_dates <- seq(from = as.Date("2011-06-02"), to = as.Date("2015-08-02"), "days")
trudeau1_dates <- seq(from = as.Date("2015-12-03"), to = as.Date("2019-09-11"), "days")
trudeau2_dates <- seq(from = as.Date("2019-12-05"), to = as.Date("2021-08-15"), "days")
trudeau3_dates <- seq(from = as.Date("2021-11-22"), to = as.Date(Sys.time()), "days")

mandates <- unique(data$mandate_id)

## function to create skeletons
create_skeleton <- function(mandate_id){
  pledges <- unique(data %>%
                      filter({{mandate_id}} == mandate_id) %>%
                      pull(pledge_cross_id))
  dates <- eval(parse(text = paste0(mandate_id, "_dates")))
  skeleton <- expand.grid(
    pledge_cross_id = pledges,
    date_in_mandate = dates
  )
  return(skeleton)
}

for (i in 1:length(mandates)){
  skeletoni <- create_skeleton(mandates[i])
  if (i == 1){
    skeleton <- skeletoni
  } else {
    skeleton <- rbind(skeleton,
                      skeletoni)
  }
}

output <- get_pledges_status(data = data,
                             pledges = skeleton$pledge_cross_id,
                             dates = skeleton$date_in_mandate)

#saveRDS(output, "_SharedFolder_livre_promesses-trudeau/Chapitre 1/data/output_graph_trudeauharper.rds")
saveRDS(output, "_SharedFolder_livre_promesses-trudeau/Chapitre 1/data/output_graph_trudeauharper_cpsa.rds")

# Aggregate ---------------------------------------------------------------

graph <- output %>% 
  group_by(mandate_id, date_limit) %>% 
  summarise(sum_realisee = sum(realisee),
            n = n()) %>% 
  mutate(day_in_mandate = rank(date_limit),
         prop = (sum_realisee / n) * 100,
         pm = substr(mandate_id, 1, nchar(mandate_id) - 1))


# Graph -------------------------------------------------------------------

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


ggplot(graph, aes(x = day_in_mandate, y = prop,
                  group = mandate_id, linetype = mandate_id,
                  alpha = mandate_id)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~pm) +
  clessnverse::theme_clean_light() +
  ggrepel::geom_text_repel(data = DatesImportantes,
                           angle = 90, hjust = 0,
                           aes(label = label),
                           size = 2.5, nudge_y = 40,
                           segment.linetype = 2,
                           force = 25, direction = "x",
                           alpha = 1) +
  geom_point(data = DatesImportantes, size = 0.9,
             alpha = 1) +
  scale_linetype_manual(values = c("higgs" = "dotdash",
                                   "marois" = "dotted",
                                   "trudeau2" = "dashed",
                                   "trudeau3" = "solid"),
                        labels = c("higgs" = "Higgs 2018-2020",
                                   "marois" = "Marois 2012-2014",
                                   "trudeau2" = "Trudeau 2019-2021",
                                   "trudeau3" = "Trudeau 2021-...")) +
  scale_alpha_manual(values = c("higgs" = 0.3,
                                "marois" = 0.3,
                                "trudeau2" = 1,
                                "trudeau3" = 1),
                     labels = c("higgs" = "Higgs 2018-2020",
                                "marois" = "Marois 2012-2014",
                                "trudeau2" = "Trudeau 2019-2021",
                                "trudeau3" = "Trudeau 2021-...")) +
  guides(linetype = guide_legend(nrow = 2)) +
  scale_linewidth_continuous(range = c(0.7, 1.3)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600)) +
  guides(linewidth = "none") +
  ylab("Proportion des promesses réalisées, partiellement\nréalisées ou en voie de réalisation à ce jour (%)") +
  xlab("Jour dans le mandat") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))


ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/progression_mandats_minoritaires.png",
       width = 9, height = 6)

