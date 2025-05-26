load("data/auckland.RData")
rm(list = setdiff(ls(), "df_providers_raw"))

library(stringdist)
library(stringr)
library(dplyr)
library(data.table)
library(readr)

source("combined/single_fuzzy_hybrid.R")
source("combined/group_matches.R")

# Normalization function
normalize_name <- function(name) {
  name %>%
    tolower() %>%
    str_trim() %>%
    str_replace_all("[^a-z ]", "")
}

# Step 1: Filter and normalize doctors
df_duplicate_doctors <- df_providers_raw %>%
  mutate(
    Provider_Name = normalize_name(trimws(Provider_Name)),
    healthpoint = ""
  ) %>%
  filter(
    !grepl("\\bANY\\b|\\bANY GP\\b|outsourced|HOSPITAL|CLINIC|URGENT|MEDICAL CENTER|MEDICAL CENTRE|
            RETIRED|\\bGP\\b|\\bNULL\\b|\\bAUCKLAND\\b|\\bWELLINGTON\\b|DOCTOR|NULL NULL|\\.|\\bUNIT\\b", surname, ignore.case = TRUE),
    !grepl("\\bANY\\b|\\bANY GP\\b|outsourced|HOSPITAL|CLINIC|URGENT|MEDICAL CENTER|MEDICAL CENTRE|
            RETIRED|\\bGP\\b|\\bNULL\\b|\\bAUCKLAND\\b|\\bWELLINGTON\\b|DOCTOR|NULL NULL|\\.|\\bUNIT\\b", firstnames, ignore.case = TRUE),
    Provider_Name != "",
    !is.na(Provider_Name)
  ) %>%
  rename(
    Clinics = clinicname,
    Emails = email,
    EDIs = hlemail
  ) %>%
  select(healthpoint, Provider_Name, Clinics, Emails, EDIs)


single_matches <- single_fuzzy_match_hybrid(df_duplicate_doctors, threshold = 0.5)
single_matches_small <- single_matches[composite_score >= 0.85]

grouped_matches <- group_duplicate_names(single_matches_small)

grouped_matches_full <- include_exact_duplicates(grouped_matches, df_duplicate_doctors)

grouped_detailed <- merge(grouped_matches_full,
                          df_duplicate_doctors,
                          by.x = "Name", by.y = "Provider_Name",
                          all.x = TRUE) %>%
  rename(
    Provider_Name = Name
  )


gp_list <- read_csv("data/doctor_list_ALL.csv", show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
  mutate(Name = trimws(Name)) %>%
  mutate(
    Provider_Name = gsub("^Dr\\s+", "", Name),
    Provider_Name = normalize_name(Provider_Name),
    surname = as.character(NA),
    firstnames = as.character(NA),
    healthpoint = "TRUE"
  ) %>%
  select(healthpoint, Provider_Name, Clinics, Addresses, Phones, Emails, EDIs)

# Build list of exact matches using all names in each group
matching_names <- unique(grouped_detailed$Provider_Name)

# Filter healthpoint list for matches
gp_list_filtered <- gp_list %>%
  filter(Provider_Name %in% matching_names)

# Add group_id to gp_list_filtered by finding first group_id match
gp_list_filtered_grouped <- gp_list_filtered %>%
  rowwise() %>%
  mutate(group_id = grouped_detailed$group_id[match(Provider_Name, grouped_detailed$Provider_Name)]) %>%
  ungroup()

# Combine and order with healthpoint first per name
combined_doctors <- bind_rows(
  gp_list_filtered_grouped %>% mutate(healthpoint = "TRUE") %>%
    select(group_id, healthpoint, Provider_Name, Clinics, Emails, Addresses, Phones, EDIs),
  grouped_detailed
) %>%
  arrange(group_id, desc(healthpoint))

rm(list = setdiff(ls(), "combined_doctors"))
save(combined_doctors, file = "data/combined_doctors.RData")



