library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(janitor)


# The url below points to an Excel file
# hosted on the book’s github repository
url <- "https://is.gd/1vvBAc"

raw_data <- tempfile(fileext = ".xlsx")

download.file(url, raw_data,
              method = "auto",
              mode = "wb")

sheets <- excel_sheets(raw_data)

read_clean <- function(..., sheet){
  read_excel(..., sheet = sheet) |>
    mutate(year = sheet)
}

raw_data <- map(
  sheets,
  ~read_clean(raw_data,
              skip = 10,
              sheet = .)
) |>
  bind_rows() |>
  clean_names()

raw_data <- raw_data |>
  rename(
    locality = commune,
    n_offers = nombre_doffres,
    average_price_nominal_euros = prix_moyen_annonce_en_courant,
    average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant,
    average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant
  ) |>
  mutate(locality = str_trim(locality)) |>
  select(year, locality, n_offers, starts_with("average"))


raw_data

# we can see how Luxembourg was spelled different ways
raw_data |>
  filter(grepl("Luxembourg", locality)) |>
  count(locality)

# also, we can look at Petange to see it has how many different spelling

raw_data |>
  filter(grepl("P.tange", locality)) |>
  count(locality)
# Petange has 2 different spelling type

# Now, we can write to correct both spelling mistakes

raw_data <- raw_data |>
  mutate(
    locality = ifelse(grepl("Luxembourg-Ville", locality),
                      "Luxembourg",
                      locality),
    locality = ifelse(grepl("P.tange", locality),
                      "Pétange",
                      locality)
  ) |>
  mutate(across(starts_with("average"),
                as.numeric))

# There seems Na values. Let's see

raw_data |>
  filter(is.na(average_price_nominal_euros))


# After checking our data, we can see that we need to remove some rows.

