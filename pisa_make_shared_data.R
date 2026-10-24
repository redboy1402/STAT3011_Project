# ------------------------------------------------------------
# Creates pisa_2006_2022_sample.csv: a random sample of
# students from the countries that appear in every cycle 2006-2022,
# cleaned and ready to load into Power BI / Python.
# ------------------------------------------------------------

#install.packages(c("learningtower", "tidyverse"))   # first time only
library(learningtower)
library(tidyverse)

#Settings
window_years <- c(2006, 2009, 2012, 2015, 2018, 2022)
n_per_group  <- 500      # rows kept per country per year
seed         <- 2026     # fixed seed so everyone gets the same sample
out_file     <- "pisa_2006_2022_sample.csv"

set.seed(seed)

#Load one year at a time, sample immediately (keeps memory low)
sample_year <- function(yr) {
  message("Loading ", yr, " ...")
  load_student(yr) |>
    filter(!grepl("^Q", country)) |> #Drop random unknown regions (QCN, QES, ...)
    group_by(country) |>
    slice_sample(n = n_per_group) |> #Keeps all rows if a country has fewer than n
    ungroup()
}

sampled <- map(window_years, sample_year) |> bind_rows()

#Keep only countries present in every cycle in the window
complete_countries <- sampled |>
  distinct(year, country) |>
  count(country, name = "cycles") |>
  filter(cycles == length(window_years)) |>
  pull(country)

pisa_sample <- sampled |>
  filter(country %in% complete_countries) |>
  arrange(year, country)

# ------------------------------------------------------------
# Cleaning / derived columns
# (done AFTER sampling so the seed gives the same students as before)
# ------------------------------------------------------------
pisa_sample <- pisa_sample |>
  mutate(
    year       = as.integer(as.character(year)),
    country    = as.character(country),
    school_id  = as.character(school_id),   # keep leading zeros
    student_id = as.character(student_id),
    
    # Unique row key (student_id alone repeats across schools)
    student_key = paste(year, country, school_id, student_id, sep = "-"),
    
    # 2022 split the lowest book band into "0" and "1-10"; merge back to match 2006-2018
    book = as.character(book),
    book = if_else(book %in% c("0", "1-10"), "0-10", book)
  ) |>
  # 4 rows have no gender; drop them so gender visuals have no "blank" category
  filter(!is.na(gender)) |>
  # ESCS quartile WITHIN each country-year (1 = most disadvantaged, 4 = most advantaged)
  group_by(year, country) |>
  mutate(escs_quartile = ntile(escs, 4)) |>   # NA escs stays NA
  ungroup() |>
  relocate(student_key, .after = student_id) |>
  relocate(escs_quartile, .after = escs)

#Checks
message("Countries kept: ", length(complete_countries), " (expected 48)")
print(count(pisa_sample, year))
print(count(pisa_sample, book))
print(count(pisa_sample, escs_quartile))
stopifnot(!anyDuplicated(pisa_sample$student_key))
message("learningtower version: ", as.character(packageVersion("learningtower")))

#Save -- na = "" writes missing values as empty cells, which Power BI and pandas read as null
write_csv(pisa_sample, out_file, na = "")
message("Saved ", nrow(pisa_sample), " rows to ", file.path(getwd(), out_file))

#load data with:
#pisa <- read_csv("pisa_2006_2022_sample.csv")                                         # R
#pisa = pd.read_csv("pisa_2006_2022_sample.csv", dtype={"school_id": str, "student_id": str})  # Python