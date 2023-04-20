# Capture soft dependencies for the entire pipeline here
future::plan
kableExtra::kbl
haven::read_dta
languageserver::run
labelled::labelled
GGally::wrap
ufs::dataShape
kfa::kfa
MBESS::ci.reliability
srvyr::as_survey_design

# Attach packages used in the entire pipeline here
library(targets)
library(tarchetypes)
library(blueprintr)
library(rcoder)
