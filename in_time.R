# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------

mois_fr_to_num <- c(janvier = 1, février = 2, mars = 3, avril = 4, mai = 5, juin = 6, 
                    juillet = 7, août = 8, septembre = 9, octobre = 10, novembre = 11, décembre = 12)

## Load Trudeau II et III (only minority governments)
trudeau <- readxl::read_excel("_SharedFolder_livre_promesses-trudeau/Chapitre 1/BDTrudeau-Chap1.xlsx",
                              sheet = "Sources") %>% 
  mutate(Mandat = as.character(Mandat)) %>% 
  filter(Mandat != 1) %>% 
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

## Load Higgs
higgs <- readxl::read_excel("../polimetre-dev/_SharedFolder_polimetre-fonctionnement/5. Polimètres archivés/8. Polimètre Nouveau-Brunswick (Higgs-59)/polimetre_higgs-59.xlsx",
                            sheet = "Sources") %>% 
  mutate(date = as.Date(ifelse(is.na(annee), lubridate::as_date(status_changed_on), as.Date(NA))),
         mois = ifelse(is.na(mois), 1, mois),
         jour = ifelse(is.na(jour), 1, jour),
         date = as.Date(ifelse(is.na(date), as.Date(paste0(annee, "-", mois, "-", jour)), date)),
         mandate_id = "higgs") %>% 
  select(mandate_id,
         pledge_id = promesse,
         verdict = status,
         date)

## Load Marois
marois <- readxl::read_excel("../polimetre-dev/_SharedFolder_polimetre-fonctionnement/5. Polimètres archivés/7. Polimètre Québec (40-Marois)/polimetre_marois-40.xlsx",
                             sheet = "Sources") %>% 
  mutate(date = as.Date(ifelse(is.na(annee), lubridate::as_date(status_changed_on), as.Date(NA))),
         mois = ifelse(is.na(mois), 1, mois),
         jour = ifelse(is.na(jour), 1, jour),
         date = as.Date(ifelse(is.na(date), lubridate::as_date(paste0(annee, "-", mois, "-", jour)), as.Date(date))),
         mandate_id = "marois") %>% 
  select(mandate_id,
         pledge_id = promesse,
         verdict = status,
         date)


data <- rbind(trudeau, higgs, marois) %>% 
  mutate(pledge_cross_id = paste0(mandate_id, pledge_id))

table(data$verdict, data$mandate_id)

#t <- data %>% 
#  filter(mandate_id == "trudeau3") %>% 
#  group_by(pledge_cross_id) %>% 
#  filter(date == max(date)) %>% 
#  mutate(realisee = case_when(
#              verdict %in% c("Partiellement réalisée",
#                    "Réalisée", "En voie de réalisation") ~ 1
#                  ),
#         realisee = ifelse(is.na(realisee), 0, realisee))
#
#table(t$verdict)

ggplot(data, aes(x = as.Date(date))) +
  geom_histogram() +
  facet_wrap(~mandate_id)

ggplot(data, aes(x = as.Date(date))) +
  geom_histogram(aes(group = mandate_id,
                     fill = mandate_id))

# Clean verdict variable --------------------------------------------------

data <- data %>% 
  mutate(realisee = case_when(
    verdict %in% c("Partiellement réalisée",
                   "Réalisée", "En voie de réalisation") ~ 1
  ),
  realisee = ifelse(is.na(realisee), 0, realisee))

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
     pledges = pledges,
     dates = dates)


# Create evolution of pledges verdicts ------------------------------------------

## For each mandate, get the range of dates of the mandate
marois_dates <- seq(from = as.Date("2012-09-19"), to = as.Date("2014-04-23"), "days")
higgs_dates <- seq(from = as.Date("2018-11-09"), to = as.Date("2020-08-17"), "days")
trudeau2_dates <- seq(from = as.Date("2019-12-05"), to = as.Date("2021-08-15"), "days")
trudeau3_dates <- seq(from = as.Date("2021-11-22"), to = as.Date(Sys.time()), "days")


## For each mandate, get all pledge_cross_ids
marois_pledges <- unique(data %>% 
  filter(mandate_id == "marois") %>% 
  pull(pledge_cross_id))
higgs_pledges <- unique(data %>% 
                           filter(mandate_id == "higgs") %>% 
                           pull(pledge_cross_id))
trudeau2_pledges <- unique(data %>% 
                          filter(mandate_id == "trudeau2") %>% 
                          pull(pledge_cross_id))

trudeau3_pledges <- unique(data %>% 
                             filter(mandate_id == "trudeau3") %>% 
                             pull(pledge_cross_id))

mandates <- unique(data$mandate_id)

## Create skeletons
skeletonmarois <- expand.grid(
  pledge_cross_id = marois_pledges,
  date_in_mandate = marois_dates
)

skeletonhiggs <- expand.grid(
  pledge_cross_id = higgs_pledges,
  date_in_mandate = higgs_dates
)

skeletontrudeau2 <- expand.grid(
  pledge_cross_id = trudeau2_pledges,
  date_in_mandate = trudeau2_dates
)

skeletontrudeau3 <- expand.grid(
  pledge_cross_id = trudeau3_pledges,
  date_in_mandate = trudeau3_dates
)


skeleton <- rbind(skeletonmarois,
                  skeletonhiggs,
                  skeletontrudeau2,
                  skeletontrudeau3)

output <- get_pledges_status(data = data,
                             pledges = skeleton$pledge_cross_id,
                             dates = skeleton$date_in_mandate)


# Agregate by mandate -----------------------------------------------------

graph <- output %>% 
  group_by(mandate_id, date_limit) %>% 
  summarise(sum_realisee = sum(realisee),
            n = n()) %>% 
  mutate(day_in_mandate = rank(date_limit),
         prop = (sum_realisee / n) * 100,
         trudeau_mandate = ifelse(mandate_id %in% c("trudeau3", "trudeau2"), 1, 0),
         mandate_id = factor(mandate_id, levels = c("marois", "trudeau2",
                                                   "higgs", "trudeau3")))

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
  drop_na(event, label) %>% 
  filter(mandate_id != "higgs")


ggplot(graph, aes(x = day_in_mandate, y = prop,
                  group = mandate_id, linetype = mandate_id,
                  alpha = mandate_id,
                  linewidth = trudeau_mandate)) +
  geom_line(linewidth = 0.5) +
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



### EN version
Sys.setlocale(category = "LC_TIME",
              locale = "en_CA.UTF-8")

DatesImportantes <- data.frame(
  date_limit = as.Date(c("2020-01-08",
                         "2020-01-20",
                         "2020-03-11",
                         "2022-02-24")),
  event = c("Ukrainian civilian plane\nshot down in Iran",
            "Start of Indigenous\nanti-pipeline blockade",
            "Global pandemic\ndeclared by WHO",
            "Start of the Russian\ninvasion of Ukraine")
) %>% 
  mutate(label = paste0("\n", format(as.Date(date_limit), "%B %d, %Y"), "\n", event, "\n")) %>% 
  left_join(graph, ., by = "date_limit") %>% 
  drop_na(event, label) %>% 
  filter(mandate_id != "higgs")


plot <- ggplot(graph, aes(x = day_in_mandate, y = prop,
                  group = mandate_id, linetype = mandate_id,
                  alpha = mandate_id,
                  linewidth = trudeau_mandate)) +
  geom_line(linewidth = 0.7) +
  clessnverse::theme_clean_light() +
  ggrepel::geom_text_repel(data = DatesImportantes,
                           angle = 90,
                           aes(label = label),
                           size = 4, nudge_y = 100,
                           hjust = 1,
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
  guides(linetype = guide_legend(nrow = 2,
                                 override.aes = list(linewidth = 2.25))) +
  scale_alpha_manual(values = c("higgs" = 0.3,
                                "marois" = 0.3,
                                "trudeau2" = 1,
                                "trudeau3" = 1),
                     labels = c("higgs" = "Higgs 2018-2020",
                                "marois" = "Marois 2012-2014",
                                "trudeau2" = "Trudeau 2019-2021",
                                "trudeau3" = "Trudeau 2021-...")) +
  scale_linewidth_continuous(range = c(0.7, 1.3)) +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600)) +
  scale_y_continuous(limits = c(0, 120), breaks = c(0, 25, 50, 75, 100)) +
  guides(linewidth = "none") +
  ylab("\nProportion of promises kept, partially\nkept, or in progress to date (%)\n") +
  xlab("\nDay in mandate\n") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14.5),
        axis.title.y = element_text(hjust = 0.5, size = 14.5),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.key.height = unit(1.15, "cm"),
        legend.key.width = unit(3, "cm"),
        legend.text = element_text(size = 14))


ggsave(plot = plot, filename = "_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/progression_mandats_minoritaires_EN_pres.png",
       width = 9, height = 6)




# Valid -------------------------------------------------------------------

trudeau2_mars2021 <- output %>%
  filter(mandate_id == "trudeau2" &
           date_limit == "2021-03-01")

writexl::write_xlsx(trudeau2_mars2021, "_SharedFolder_livre_promesses-trudeau/Chapitre 1/valid_in_time.xlsx")

