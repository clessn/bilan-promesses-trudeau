# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------

## Load Trudeau II et III (only minority governments)
trudeau <- readxl::read_excel("_SharedFolder_livre_promesses-trudeau/Chapitre 1/PolimètreTrudeau-Chapitre1.xlsx",
                              sheet = "Sources") %>% 
  filter(Mandat != 1) %>% 
  mutate(mandate_id = paste0("trudeau", Mandat)) %>% 
  # select relevant columns
  select(mandate_id,
         pledge_id = `Numéro`,
         verdict = `Verdict (ou référence)`,
         date = `Date source...7`) %>% 
  ## keep verdicts only
  filter(verdict %in% c("En suspens",
                        "En voie de réalisation",
                        "Partiellement réalisée",
                        "Réalisée",
                        "Rompue"))

## Load Higgs
higgs <- readxl::read_excel("_SharedFolder_polimetre/5. Polimètres archivés/8. Polimètre Nouveau-Brunswick (Higgs-59)/polimetre_higgs-59.xlsx",
                            sheet = "Sources") %>% 
  
  ### se fier à structuro pour fix annee mois jour
  
  mutate(mandate_id = "higgs") %>% 
  select(mandate_id,
         pledge_id = promesse,
         verdict = status,
         date = )

## Load Marois
marois <- readxl::read_excel("_SharedFolder_polimetre/5. Polimètres archivés/7. Polimètre Québec (40-Marois)/polimetre_marois-40.xlsx",
                             sheet = "Sources") %>% 
  mutate(mandate_id = "marois") %>% 
  select(mandate_id,
         pledge_id = promesse,
         verdict = status,
         date = status_changed_on)


data <- rbind(trudeau, higgs, marois) %>% 
  mutate(pledge_cross_id = paste0(mandate_id, pledge_id))


# Next step: getting the status of multiple pledges in time ---------------

test <- function(data, pledges, dates) {
  # Créer une table de pledges et dates
  pledges_dates <- data.frame(pledge_cross_id = pledges, date_limit = as.Date(dates))
  # Rejoindre et filtrer
  t <- data %>%
    inner_join(pledges_dates, by = "pledge_cross_id") %>%
    filter(date <= date_limit) %>%
    arrange(pledge_cross_id, desc(date)) %>%
    group_by(pledge_cross_id) %>%
    summarise(verdict = first(verdict))
}

# Exemple d'utilisation :
# obtenir_verdicts_optimal(votre_dataframe, vecteur_pledges, vecteur_dates)

test(data,
     pledges = c("trudeau22.09.10",
                 "trudeau33.01.39",
                 "trudeau22.03.04",
                 "trudeau33.10.37",
                 "trudeau33.06.14"),
     dates = c("2020-05-05",
               "2022-05-05",
               "2020-12-05",
               "2022-08-08",
               "2022-10-12"))




# regrouper en voie de réali et partiellement réalisée


# 

