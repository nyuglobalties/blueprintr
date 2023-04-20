#' Data processing prelude
#'
#' This script *needs* to be loaded at the top of each
#' project '_targets.R' file, along with a call to `library(targets)`
#'
#' For example:
#'
#' library(targets)
#' source(here::here("project/prelude.R"))
#'
#' # ... extra code + targets pipeline below
NULL

## ------ COMMON ------
# Load common R functions
src_files <- list.files(
  here::here("R"),
  pattern = "\\.[rR]$",
  full.names = TRUE
)

for (f in src_files) {
  source(f)
}

# Attach common packages
source(here::here("inst/project/packages.R"))

# Assign common global environment variables
source(here::here("inst/project/environment.R"))

# Enable interactive blueprintr macros so statements like
# `.TARGET("colombia_caregiver_all")` evaluate to the desired target
options(blueprintr.interactive_eval_macros = TRUE)

# As of blueprintr 0.2.0, the `metadata_file_path` parameter
# will rarely need to be manually specified
options(blueprintr.use_local_metadata_path = TRUE)

# Enable variable lineage
options(blueprintr.use_variable_uuids = TRUE)

## ------ PROJECT-SPECIFIC ------
# Check TAR_PROJECT
TAR_PROJECT <- Sys.getenv("TAR_PROJECT")

if (identical(TAR_PROJECT, "")) {
  stop0("No project specified. Use the 'TAR_PROJECT' env var to set one.")
}

# Load project-specific R functions
proj_r_dir <- here::here("R", TAR_PROJECT)

if (dir.exists(proj_r_dir)) {
  proj_src_files <- list.files(
    proj_r_dir,
    pattern = "\\.[rR]$",
    full.names = TRUE
  )

  for (f in proj_src_files) {
    source(f)
  }
}

# Attach project-specific packages
proj_packages <- here::here("inst/project", TAR_PROJECT, "packages.R")

if (file.exists(proj_packages)) {
  source(proj_packages)
}

# Assign project-specific global environment variables
proj_envs <- here::here("inst/project", TAR_PROJECT, "environment.R")

if (file.exists(proj_envs)) {
  source(proj_envs)
}

## ------ CLEANUP ------
rm(src_files)

rm(proj_r_dir)
rm(proj_packages)
rm(proj_envs)

if (exists("proj_src_files")) {
  rm(proj_src_files)
}
