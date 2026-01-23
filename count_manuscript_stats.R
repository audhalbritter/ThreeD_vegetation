# Count words in abstract and keywords, and count figures/tables in manuscript

library(stringr)
library(yaml)

# Read main manuscript file
main_file <- "manuscript/main_manuscript.qmd"
main_content <- readLines(main_file, warn = FALSE)

# Extract YAML header
yaml_start <- which(main_content == "---")[1]
yaml_end <- which(main_content == "---")[2]
yaml_text <- paste(main_content[yaml_start:yaml_end], collapse = "\n")
yaml_data <- yaml::yaml.load(yaml_text)

# Count words in abstract
abstract <- yaml_data$abstract
if (!is.null(abstract)) {
  # Remove line breaks and extra whitespace
  abstract_clean <- str_replace_all(abstract, "\\s+", " ")
  abstract_words <- str_split(abstract_clean, "\\s+")[[1]]
  abstract_word_count <- length(abstract_words[abstract_words != ""])
} else {
  abstract_word_count <- 0
}

# Count words in keywords
keywords <- yaml_data$keywords
if (!is.null(keywords)) {
  keywords_text <- paste(keywords, collapse = " ")
  keywords_words <- str_split(keywords_text, "\\s+")[[1]]
  keywords_word_count <- length(keywords_words[keywords_words != ""])
} else {
  keywords_word_count <- 0
}

# Read included files
included_files <- c(
  "manuscript/introduction.qmd",
  "manuscript/methods.qmd",
  "manuscript/SEM_output.qmd",
  "manuscript/discussion.qmd"
)

all_content <- character()
for (file in included_files) {
  if (file.exists(file)) {
    all_content <- c(all_content, readLines(file, warn = FALSE))
  }
}

# Combine all content
full_text <- paste(all_content, collapse = "\n")

# Count figures
# Look for figure references: @fig-... or ```{r} blocks with fig-cap or label: fig-
figure_patterns <- c(
  "@fig-[^\\s\\]\\}]+",  # @fig-reference
  "#\\|\\s*label:\\s*fig-",  # label: fig- in code chunks
  "#\\|\\s*fig-cap:"  # fig-cap: in code chunks
)

figure_count <- 0
for (pattern in figure_patterns) {
  matches <- str_extract_all(full_text, pattern)[[1]]
  figure_count <- figure_count + length(matches)
}

# Remove duplicates by finding unique figure labels
fig_labels <- unique(c(
  str_extract_all(full_text, "@fig-([^\\s\\]\\}]+)", simplify = TRUE),
  str_extract_all(full_text, "label:\\s*fig-([^\\s\\n]+)", simplify = TRUE)
))
fig_labels <- fig_labels[!is.na(fig_labels) & fig_labels != ""]
# Clean up labels
fig_labels <- str_replace_all(fig_labels, "label:\\s*fig-", "fig-")
fig_labels <- str_replace_all(fig_labels, "@", "")
unique_figures <- length(unique(fig_labels))

# Count tables
# Look for table references: @tbl-... or table output
table_patterns <- c(
  "@tbl-[^\\s\\]\\}]+",  # @tbl-reference
  "#\\|\\s*label:\\s*tbl-",  # label: tbl- in code chunks
  "#\\|\\s*tbl-cap:"  # tbl-cap: in code chunks
)

table_count <- 0
for (pattern in table_patterns) {
  matches <- str_extract_all(full_text, pattern)[[1]]
  table_count <- table_count + length(matches)
}

# Remove duplicates by finding unique table labels
tbl_labels <- unique(c(
  str_extract_all(full_text, "@tbl-([^\\s\\]\\}]+)", simplify = TRUE),
  str_extract_all(full_text, "label:\\s*tbl-([^\\s\\n]+)", simplify = TRUE)
))
tbl_labels <- tbl_labels[!is.na(tbl_labels) & tbl_labels != ""]
# Clean up labels
tbl_labels <- str_replace_all(tbl_labels, "label:\\s*tbl-", "tbl-")
tbl_labels <- str_replace_all(tbl_labels, "@", "")
unique_tables <- length(unique(tbl_labels))

# Also count code chunks that might produce tables (gt, kable, etc.)
table_code_patterns <- c(
  "tar_read\\(.*table.*\\)",
  "gt\\(\\)",
  "kable\\(\\)",
  "knitr::kable"
)
table_code_count <- 0
for (pattern in table_code_patterns) {
  matches <- str_extract_all(full_text, pattern, ignore.case = TRUE)[[1]]
  table_code_count <- table_code_count + length(matches)
}

# Print results
cat("=== Manuscript Statistics ===\n\n")
cat("Abstract word count:", abstract_word_count, "\n")
cat("Keywords word count:", keywords_word_count, "\n")
cat("\n")
cat("Figures:\n")
cat("  Total figure references found:", figure_count, "\n")
cat("  Unique figures:", unique_figures, "\n")
cat("\n")
cat("Tables:\n")
cat("  Total table references found:", table_count, "\n")
cat("  Unique tables:", unique_tables, "\n")
cat("  Table code blocks:", table_code_count, "\n")
cat("\n")
cat("Total figures + tables:", unique_figures + unique_tables, "\n")
