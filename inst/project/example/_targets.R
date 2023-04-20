library(targets)
library(dplyr)
library(glue)
library(rlang)
source(here::here("inst/project/utils.R"))
source(here::here("inst/project/targets.R"))
source(here::here("inst/project/prelude.R"))

# Add tar_targets() to this list to define the pipeline
rlang::list2(
  # ------ 0. Loading data ------
  # ------ Lodaing operating -----
  # ------ 1. Preprocessing ------
  # ------ 2. Mapping file ------
  tar_file(
    item_mapping_file,
    here::here("inst/project/example/mapping/data-mapping.csv")
  ),
  tar_target(
    item_mapping,
    item_mapping_file %>%
      readr::read_csv(
        col_types = readr::cols(
          name_1 = readr::col_character(),
          description_1 = readr::col_character(),
          coding_1 = readr::col_character(),
          panel = readr::col_character(),
          homogenized_name = readr::col_character(),
          homogenized_coding = readr::col_character(),
          homogenized_description = readr::col_character()
        )
      ) %>%
      panelcleaner::panel_mapping(waves = 1)
  ),

  # ------ 3. Validation ------
  # ~~ Issues start here ~~
  # ------- Create excel ------
  #  ------ 4. Blueprints ------
  tar_proj_blueprints(),

  #  ------ 5. Exports ------
  # Reports ------
)
