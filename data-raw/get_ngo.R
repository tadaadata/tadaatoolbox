#### Cleaning up ngo ####
ngo <- readRDS("data-raw/d.ngo.rds")

ngo <- dplyr::select(ngo, -index, -zeng, -zdeutsch, -zmathe, -leist)

## factors
ngo$geschl   <- factor(ngo$geschl,   labels = c("M\u00e4nnlich", "Weiblich"))
ngo$jahrgang <- factor(ngo$jahrgang, labels = c("11", "12", "13"), ordered = TRUE)
ngo$hausauf  <- ifelse(ngo$hausauf  == 0, NA, ngo$hausauf)
ngo$abschalt <- ifelse(ngo$abschalt == 0, NA, ngo$abschalt)
ngo$abschalt <- factor(ngo$abschalt, labels = c("Ja", "Nein"))

## sjPlot value labels
ngo$geschl   <- sjlabelled::set_labels(ngo$geschl,   labels = c("M\u00e4nnlich", "Weiblich"))
ngo$abschalt <- sjlabelled::set_labels(ngo$abschalt, labels = c("Ja", "Nein"))
ngo$jahrgang <- sjlabelled::set_labels(ngo$jahrgang, labels = c("11", "12", "13"))
ngo$hausauf  <- sjlabelled::set_labels(ngo$hausauf,
                                       labels = c("gar nicht", "weniger als halbe Stunde",
                                                  "halbe Stunde bis Stunde", "1 bis 2 Stunden",
                                                  "2 bis 3 Stunden", "3 bis 4 Stunden",
                                                  "mehr als 4 Stunden"))

## Variable labels
ngo$geschl   <- sjlabelled::set_label(ngo$geschl,   "Geschlecht")
ngo$abschalt <- sjlabelled::set_label(ngo$abschalt, "Abschalten")
ngo$jahrgang <- sjlabelled::set_label(ngo$jahrgang, "Jahrgang")
ngo$hausauf  <- sjlabelled::set_label(ngo$hausauf,  "Hausaufgaben")

## Saving
ngo <- tibble::as_tibble(ngo)

devtools::use_data(ngo, overwrite = TRUE)
