# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------

mois_fr_to_num <- c(janvier = 1, février = 2, mars = 3, avril = 4, mai = 5, juin = 6, 
                    juillet = 7, août = 8, septembre = 9, octobre = 10, novembre = 11, décembre = 12)

## Load Trudeau II et III (only minority governments)
trudeau <- readxl::read_excel("_SharedFolder_livre_promesses-trudeau/Chapitre 1/PolimètreTrudeau-Chapitre1.xlsx",
                              sheet = "Sources") %>% 
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

## Load Higgs
higgs <- readxl::read_excel("_SharedFolder_polimetre/5. Polimètres archivés/8. Polimètre Nouveau-Brunswick (Higgs-59)/polimetre_higgs-59.xlsx",
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
marois <- readxl::read_excel("_SharedFolder_polimetre/5. Polimètres archivés/7. Polimètre Québec (40-Marois)/polimetre_marois-40.xlsx",
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
                   "Réalisée") ~ 1
  ),
  realisee = ifelse(is.na(realisee), 0, realisee))


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
    group_by(pledge_cross_id) %>%
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

get_pledges_status(data,
     pledges = c("trudeau22.09.10",
                 "trudeau33.01.39",
                 "trudeau22.03.04",
                 "trudeau33.10.37",
                 "trudeau33.06.14"),
     dates = c("2021-05-05",
               "2022-05-05",
               "2021-08-15",
               "2022-08-08",
               "2022-10-12"))


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
         current_mandate = ifelse(mandate_id == "trudeau3", 1, 0))

# Graph -------------------------------------------------------------------

ggplot(graph, aes(x = day_in_mandate, y = prop,
                  group = mandate_id, linetype = mandate_id,
                  alpha = current_mandate,
                  linewidth = current_mandate)) +
  geom_line(alpha = 0.2, linewidth = 0.5) +
  stat_smooth(geom='line', color = "black", se=FALSE) +
  clessnverse::theme_clean_light() +
  scale_linetype_manual(values = c("higgs" = "dotdash",
                                   "marois" = "dotted",
                                   "trudeau2" = "dashed",
                                   "trudeau3" = "solid")) +
  scale_alpha_continuous(range = c(0.7, 1)) +
  scale_linewidth_continuous(range = c(0.7, 1.3)) +
  guides(alpha = "none",
         linewidth = "none") +
  ylab("Proportion des promesses\nréalisées à ce jour (%)") +
  xlab("Jour dans le mandat")
  

ggsave("_SharedFolder_livre_promesses-trudeau/Chapitre 1/graphs/progression_mandats_minoritaires.png",
       width = 9, height = 6)
