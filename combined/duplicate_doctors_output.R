# Load in data from our SQL RProj
load("data/auckland.RData")
rm(list = setdiff(ls(), "df_providers_raw"))

# Load required libraries
library(stringdist)
library(stringr)
library(dplyr)
library(data.table)
library(readr)
library(DT)

# Get the single_fuzzy_match_hybrid function loaded
source("combined/single_fuzzy_hybrid.R")

# Load in the three functions; group_duplicate_names, include_exact_duplicates and review_matches
source("combined/group_matches.R")

# Load our known not matches (Or create)
if (file.exists("data/known_not_matches.csv")) {
  known_not_matches <- read_csv("data/known_not_matches.csv", show_col_types = FALSE)
} else {
  known_not_matches <- data.frame(Name1 = character(), Name2 = character())
}

# Normalization function for names
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
    status == 0,
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

# Run our big compare function and set different thresholds
single_matches <- single_fuzzy_match_hybrid(df_duplicate_doctors, threshold = 0.5)

# Save this DF
save(single_matches, file = "data/single_matches.RData")
#load("data/single_matches.RData")

# Remove known not-matches (in either order)
single_matches_filtered <- single_matches[composite_score >= 0.75]%>%
  filter(!(paste(Name1, Name2) %in% paste(known_not_matches$Name1, known_not_matches$Name2) |
             paste(Name2, Name1) %in% paste(known_not_matches$Name1, known_not_matches$Name2)))

save(single_matches_filtered, file = "data/single_matches_filtered.RData")

#load("data/single_matches_filtered.RData")

grouped_matches <- group_duplicate_names(single_matches_filtered)

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



