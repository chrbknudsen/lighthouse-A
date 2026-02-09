# Her skal vi nok have noget ind, der sikrer at jeg har directories at 
# arbejde med lokalt.

repo_b_dir <- Sys.getenv("REPO_B_DIR", unset = NA_character_)
if (is.na(repo_b_dir) || repo_b_dir == "") {
  stop("REPO_B_DIR is not set")
}

# Eksempel: fil i lighthouse-B
input_path <- file.path(repo_b_dir, "data", "input.csv")

if (!file.exists(input_path)) {
  stop("File not found: ", input_path)
}

library(readr)
library(dplyr)

df <- read_csv(input_path, show_col_types = FALSE)

# ---- DINE TRANSFORMATIONER ----
df <- df |> mutate(processed_at = Sys.time())

# Skriv direkte tilbage til lighthouse-B
write_csv(df, input_path)
