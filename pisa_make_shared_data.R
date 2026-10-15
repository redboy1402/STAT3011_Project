# ------------------------------------------------------------
# Creates pisa_2006_2022_sample.csv: a random sample of
# students from the countries that appear in every cycle 2006-2022.
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

#Checks
message("Countries kept: ", length(complete_countries), " (expected 48)")
print(count(pisa_sample, year))
message("learningtower version: ", as.character(packageVersion("learningtower")))

#Save
write_csv(pisa_sample, out_file)
message("Saved ", nrow(pisa_sample), " rows to ", file.path(getwd(), out_file))

#load data with:
#pisa <- read_csv("pisa_2006_2022_sample.csv")