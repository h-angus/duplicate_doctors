library(dplyr)
library(stringdist)
library(stringr)
library(data.table)

# Normalization function
normalize_name <- function(name) {
  name %>%
    tolower() %>%
    str_trim() %>%
    str_replace_all("[^a-z ]", "")
}

token_score_chunk <- function(chunk_df) {
  chunk_df[, token_score := 1 - stringdist(Name1, Name2, method = "jaccard", q = 2)]
  return(chunk_df)
}

single_fuzzy_match_hybrid <- function(df, threshold = 0.85) {
  start_time <- Sys.time()
  cat("\U0001F50D Starting hybrid fuzzy match...\n")
  
  # Step 1: Get unique names
  cat("\U0001F50D Extracting unique provider names...\n")
  unique_names <- df %>%
    distinct(Provider_Name) %>%
    pull(Provider_Name)
  
  n <- length(unique_names)
  cat(sprintf("\U0001F5C3 Comparing %d unique names (%d pairs)\n", n, n * (n - 1) / 2))
  
  # Precompute surname frequencies early
  cat("\U0001F50E Precomputing surname frequencies...\n")
  surname_lookup <- data.table(Name = unique_names)
  surname_lookup[, Surname := str_extract(Name, "\\b\\w+$")]
  surname_freq <- surname_lookup[, .N, by = Surname]
  setnames(surname_freq, "N", "surname_freq")
  setkey(surname_lookup, Surname)
  setkey(surname_freq, Surname)
  surname_lookup <- surname_freq[surname_lookup]
  setkey(surname_lookup, Name)
  
  # Step 2: Compute distance/similarity matrices
  cat("\U0001F522 Computing Jaro-Winkler matrix...\n")
  t_jw <- Sys.time()
  jw_matrix <- 1 - stringdistmatrix(unique_names, unique_names, method = "jw", useNames = TRUE)
  cat(sprintf("\u23F1 Jaro-Winkler done in %.2f sec\n", as.numeric(difftime(Sys.time(), t_jw, units = "secs"))))
  
  cat("\U0001F522 Computing OSA matrix...\n")
  t_osa <- Sys.time()
  osa_matrix <- stringdistmatrix(unique_names, unique_names, method = "osa", useNames = TRUE)
  cat(sprintf("\u23F1 OSA done in %.2f sec\n", as.numeric(difftime(Sys.time(), t_osa, units = "secs"))))
  
  # Convert matrices to long format
  cat("\U0001F4CA Converting distance matrices to long format...\n")
  jw_df <- as.data.table(as.table(jw_matrix))
  osa_df <- as.data.table(as.table(osa_matrix))
  setnames(jw_df, c("Name1", "Name2", "jw_score"))
  setnames(osa_df, c("Name1", "Name2", "osa_dist"))
  
  # Merge scores
  cat("\U0001F517 Merging similarity scores...\n")
  scores <- merge(jw_df, osa_df, by = c("Name1", "Name2"))
  scores <- scores[Name1 != Name2]
  
  # Compute scaled OSA similarity
  cat("\U0001F4A0 Calculating scaled OSA similarity...\n")
  scores[, max_len := pmax(nchar(Name1), nchar(Name2))]
  scores[, osa_score := 1 - (osa_dist / max_len)]
  
  # Token overlap in chunks (sequential)
  cat("\n\U0001F9EE Scoring token overlap sequentially...\n")
  t_token <- Sys.time()
  n_chunks <- 20
  chunk_size <- ceiling(nrow(scores) / n_chunks)
  chunk_indices <- split(1:nrow(scores), rep(1:n_chunks, each = chunk_size, length.out = nrow(scores)))
  
  token_chunks <- lapply(seq_along(chunk_indices), function(i) {
    rows <- chunk_indices[[i]]
    chunk <- scores[rows]
    chunk <- token_score_chunk(chunk)
    cat(sprintf("[%s] Chunk %d complete\n", format(Sys.time(), "%H:%M:%S"), i))
    chunk
  })
  
  scores <- rbindlist(token_chunks)
  cat(sprintf("\u2705 Token scoring done in %.2f sec\n", as.numeric(difftime(Sys.time(), t_token, units = "secs"))))
  
  # ❌ Remove very obvious non-matches before further scoring
  scores <- scores[token_score >= 0.2 & osa_score >= 0.2 & jw_score >= 0.2]
  
  # Surname frequency lookup for matches
  scores[, Surname1 := surname_lookup[J(Name1), Surname]]
  scores[, Surname2 := surname_lookup[J(Name2), Surname]]
  scores[, freq1 := surname_lookup[J(Name1), surname_freq]]
  scores[, freq2 := surname_lookup[J(Name2), surname_freq]]
  scores[, surname_freq := pmax(freq1, freq2, na.rm = TRUE)]
  scores[, surname_weight := pmin(1, log2(100 / (surname_freq + 1)))]
  
  # Extract first names and apply JW on first names
  scores[, First1 := word(Name1, 1)]
  scores[, First2 := word(Name2, 1)]
  scores[, jw_first := 1 - stringdist(First1, First2, method = "jw")]
  
  # Valid surname bonus only if first name JW is decent
  scores[, surname_bonus := ifelse(Surname1 == Surname2 & jw_first >= 0.6, 0.1, 0)]
  scores[, surname_bonus := surname_bonus * surname_weight]
  
  # Short name penalty for initials or very short first names
  scores[, short_name_penalty := ifelse(nchar(First1) < 3 | nchar(First2) < 3, 0.05, 0)]
  
  # Final composite score
  cat("\U0001F3AF Calculating final composite scores with surname weighting and penalties...\n")
  scores[, composite_score := 0.4 * jw_score + 0.4 * osa_score + 0.1 * token_score + surname_bonus - short_name_penalty]
  
  # Deduplicate symmetrical matches
  cat("\U0001F9F9 Removing symmetrical duplicates...\n")
  scores[, pair_id := paste(pmin(Name1, Name2), pmax(Name1, Name2), sep = "::")]
  scores <- unique(scores, by = "pair_id")
  
  # Filter by threshold
  cat("\U0001F4CF Filtering matches above threshold...\n")
  matches <- scores[composite_score >= threshold][order(-composite_score)]
  
  # Save results
  output_path <- "data/single_hybrid_matches_to_review.csv"
  fwrite(matches, output_path)
  
  cat(sprintf("\U0001F4C4 Saved %d hybrid matches to: %s\n", nrow(matches), output_path))
  cat(sprintf("\u2705 Total time: %.2f minutes\n", as.numeric(difftime(Sys.time(), start_time, units = "mins"))))
  
  return(matches)
}
