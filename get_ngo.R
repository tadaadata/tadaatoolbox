#### Cleaning up ngo ####
ngo <- ryouready::d.ngo

## sjPlot value labels
ngo$geschl   <- sjmisc::set_labels(ngo$geschl,   c("M\u00e4nnlich", "Weiblich"))
ngo$abschalt <- sjmisc::set_labels(ngo$abschalt, c("Ja", "Nein"))
ngo$jahrgang <- sjmisc::set_labels(ngo$jahrgang, c("11", "12", "13"))
ngo$hausauf  <- sjmisc::set_labels(ngo$hausauf,  c("gar nicht", "weniger als halbe Stunde",
                                           "halbe Stunde bis Stunde", "1 bis 2 Stunden",
                                           "2 bis 3 Stunden", "3 bis 4 Stunden",
                                           "mehr als 4 Stunden"))

## factors
ngo$geschl   <- factor(ngo$geschl,   labels = c("M\u00e4nnlich", "Weiblich"))
ngo$jahrgang <- factor(ngo$jahrgang, labels = c("11", "12", "13"), ordered = TRUE)
ngo$hausauf  <- car::recode(ngo$hausauf,  "0 = NA")
ngo$abschalt <- car::recode(ngo$abschalt, "0 = NA")
ngo$abschalt <- factor(ngo$abschalt, labels = c("Ja", "Nein"))

## Variable labels
ngo$geschl   <- sjmisc::set_label(ngo$geschl, "Geschlecht")
ngo$abschalt <- sjmisc::set_label(ngo$abschalt, "Abschalten")
ngo$jahrgang <- sjmisc::set_label(ngo$jahrgang, "Jahrgang")
ngo$hausauf  <- sjmisc::set_label(ngo$hausauf, "Hausaufgaben")

## Saving
ngo <- dplyr::tbl_df(ngo)

devtools::use_data(ngo, overwrite = TRUE)
