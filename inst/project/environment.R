## Add globals from environment variables here
BOX_ROOT <- Sys.getenv("BOX_PATH") %if_empty_string% NULL
F_NIGHTLY <- as.logical(Sys.getenv("F_NIGHTLY") %if_empty_string% "FALSE")
