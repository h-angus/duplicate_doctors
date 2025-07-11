library(data.table)
library(igraph)

#' Group similar names using graph-based clustering
#'
#' @param matches A data.table of pairwise matches with columns 'Name1' and 'Name2'
#' @return A data.table with columns 'group_id' and 'Name', listing all unique names in each cluster
#' @examples
#' clustered <- group_duplicate_names(matches)
group_duplicate_names <- function(matches) {
  if (!"data.table" %in% class(matches)) {
    matches <- as.data.table(matches)
  }
  
  cat("\U0001F50E Building graph of matched names...\n")
  g <- graph_from_data_frame(matches[, .(Name1, Name2)], directed = FALSE)
  
  cat("\U0001F522 Finding connected components (clusters)...\n")
  comps <- components(g)
  
  cat("\U0001F4CA Mapping group IDs to names...\n")
  group_table <- data.table(Name = names(comps$membership),
                            group_id = comps$membership)
  
  return(group_table[order(group_id, Name)])
}

#' Extend grouped matches to include exact duplicates not in fuzzy clusters
#'
#' @param grouped_matches Output of group_duplicate_names (data.table with Name and group_id)
#' @param df_duplicate_doctors Original doctor data frame with Provider_Name
#' @return A data.table including original grouped matches plus new groups for exact duplicates
#' @examples
#' full_groups <- include_exact_duplicates(grouped_matches, df_duplicate_doctors)
include_exact_duplicates <- function(grouped_matches, df_duplicate_doctors) {
  library(dplyr)
  
  exact_dupes <- df_duplicate_doctors %>%
    group_by(Provider_Name) %>%
    filter(n() > 1) %>%
    distinct(Provider_Name) %>%
    mutate(exact_only = TRUE)
  
  grouped_names <- grouped_matches$Name
  new_exacts <- exact_dupes %>%
    filter(!(Provider_Name %in% grouped_names))
  
  next_group_id <- max(grouped_matches$group_id)
  new_groups <- new_exacts %>%
    ungroup() %>%
    mutate(group_id = row_number() + next_group_id) %>%
    select(group_id, Name = Provider_Name)
  
  grouped_matches_full <- bind_rows(grouped_matches, new_groups)
  return(grouped_matches_full)
}



review_matches <- function(run = FALSE,
                           match_df,
                           known_not_matches,
                           known_not_matches_file) {
  if (!run) return(match_df)
  
  cat("📋 Displaying possible matches for manual review...\n")
  print(
    DT::datatable(
      match_df %>% dplyr::arrange(dplyr::desc(composite_score)),
      rownames = TRUE,
      options = list(pageLength = 20, scrollX = TRUE)
    )
  )
  
  readline(prompt = "\n👀 Review complete? Press [Enter] to continue...")
  
  bad_indices <- readline(prompt = "🛑 Enter indices of BAD matches (e.g., c(1, 3, 5)): ")
  bad_indices <- tryCatch(eval(parse(text = bad_indices)), error = function(e) integer(0))
  
  if (length(bad_indices) == 0) {
    cat("✅ No bad matches entered. Proceeding.\n")
    return(match_df)
  }
  
  bad_matches <- match_df[bad_indices, c("Name1", "Name2")]
  
  updated_known <- dplyr::bind_rows(known_not_matches, bad_matches) %>%
    dplyr::distinct(Name1, Name2)
  
  readr::write_csv(updated_known, known_not_matches_file)
  
  cat("❌ Removed", nrow(bad_matches), "bad matches and updated known_not_matches.csv\n")
  
  # Return filtered match_df
  match_df[-bad_indices, ]
}


