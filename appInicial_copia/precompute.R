# Precompute cache for the Shiny app
# Usage:
#   source("precompute.R")
# Optional env vars:
#   YELP_SAMPLE_FRAC, REVIEWS_SAMPLE_FRAC

message("Precomputing cache... This may take a while the first time.")
source("dataset.R", local = TRUE)
message("Done. Cache written to cache/preprocessed_v1.rds")
