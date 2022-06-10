##deduplication of references

search_results_combined <- read_bibliography(
  filename = "Data/Combined.ris",
  return_df = TRUE
)

duplicates <- find_duplicates(
  data = search_results_combined,
  match_variable = "title",
  to_lower = TRUE,
  remove_punctuation = TRUE
)

deduplicated_refs <- extract_unique_references(
  search_results_combined,
  matches = duplicates
)

write_bibliography(
  deduplicated_refs,
  filename = "Other outputs/Deduplicated.ris",
  format = "ris"
)
