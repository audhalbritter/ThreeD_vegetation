count_manuscript_stats <- function(manuscript_files = NULL) {
  library(stringr)
  library(dplyr)
  library(tibble)

  read_lines_safe <- function(path) {
    if (!file.exists(path)) return(character())
    readLines(path, warn = FALSE)
  }

  strip_yaml <- function(lines) {
    if (length(lines) < 3 || lines[1] != "---") return(lines)
    yaml_end <- which(lines[-1] == "---")[1]
    if (is.na(yaml_end)) return(lines)
    lines[-seq_len(yaml_end + 1)]
  }

  strip_code_chunks <- function(lines) {
    keep <- rep(TRUE, length(lines))
    in_chunk <- FALSE
    for (i in seq_along(lines)) {
      if (grepl("^```", lines[i])) {
        keep[i] <- FALSE
        in_chunk <- !in_chunk
        next
      }
      if (in_chunk) keep[i] <- FALSE
    }
    lines[keep]
  }

  clean_for_word_count <- function(text) {
    text |>
      str_replace_all("\\[[^\\]]*\\]\\([^\\)]*\\)", " ") |> # links
      str_replace_all("\\[@[^\\]]*\\]", " ") |>              # cite blocks
      str_replace_all("@[A-Za-z0-9:_-]+", " ") |>            # inline cites
      str_replace_all("\\{#[^\\}]+\\}", " ") |>              # ids
      str_replace_all("<[^>]+>", " ") |>                     # html tags
      str_replace_all("[*_`~]", " ") |>
      str_replace_all("\\s+", " ") |>
      str_trim()
  }

  count_words <- function(text) {
    if (!nzchar(text)) return(0L)
    tokens <- str_split(text, "\\s+")[[1]]
    length(tokens[tokens != ""])
  }

  # File layout for merged manuscript (excluding SI).
  # If manuscript_files is provided by targets, use it to ensure proper dependency tracking.
  default_files <- c(
    "manuscript/title_page.qmd",
    "manuscript/abstract.qmd",
    "manuscript/introduction.qmd",
    "manuscript/methods.qmd",
    "manuscript/SEM_output.qmd",
    "manuscript/discussion.qmd"
  )
  files <- if (is.null(manuscript_files)) default_files else manuscript_files
  title_file <- files[grepl("title_page\\.qmd$", files)][1]
  abstract_file <- files[grepl("abstract\\.qmd$", files)][1]
  main_text_files <- files[grepl("(introduction|methods|SEM_output|discussion)\\.qmd$", files)]
  all_ms_files <- c(title_file, abstract_file, main_text_files)

  # Running title characters
  title_lines <- read_lines_safe(title_file)
  running_title_line <- title_lines[str_detect(title_lines, "^\\*\\*Running title:\\*\\*")]
  running_title_text <- if (length(running_title_line) > 0) {
    str_replace(running_title_line[1], "^\\*\\*Running title:\\*\\*\\s*", "")
  } else {
    ""
  }
  # Ignore trailing helper text in parentheses, e.g. "(X out of 45 characters)"
  running_title_clean <- str_trim(str_remove(running_title_text, "\\s*\\([^\\)]*\\)\\s*$"))
  running_title_char_count <- as.integer(str_length(running_title_clean))

  # Abstract words
  abstract_lines <- read_lines_safe(abstract_file)
  abstract_body <- abstract_lines[!str_detect(abstract_lines, "^\\*\\*Abstract")]
  abstract_word_count <- count_words(clean_for_word_count(paste(abstract_body, collapse = " ")))

  # Main text words (excluding abstract, acknowledgements/references, figure/table legends, SI)
  cleaned_main_lines <- unlist(lapply(main_text_files, function(f) {
    lines <- read_lines_safe(f)
    lines <- strip_yaml(lines)

    # Remove acknowledgements/references and anything after them
    cut_idx <- which(str_detect(lines, "^##\\s+(Acknowledg(e)?ments|References)\\b"))
    if (length(cut_idx) > 0) {
      lines <- lines[seq_len(cut_idx[1] - 1)]
    }

    # Remove code chunks and legend lines
    lines <- strip_code_chunks(lines)
    lines <- lines[!str_detect(lines, "^#\\|\\s*(fig-cap|tbl-cap):")]
    lines <- lines[!str_detect(lines, "^!\\[")] # markdown image with legend text
    lines <- lines[!str_detect(lines, "^#+\\s")] # headings
    lines
  }))
  main_text_word_count <- count_words(clean_for_word_count(paste(cleaned_main_lines, collapse = " ")))

  # Reference count = unique citation keys used in merged manuscript (excluding SI)
  all_lines <- unlist(lapply(all_ms_files, read_lines_safe))
  all_text <- paste(all_lines, collapse = "\n")
  citation_tokens <- str_extract_all(all_text, "@[A-Za-z0-9][A-Za-z0-9:_-]*")[[1]]
  citation_keys <- unique(str_remove(citation_tokens, "^@"))
  citation_keys <- citation_keys[!str_detect(citation_keys, "^(fig|tbl|eq|sec)-")]
  n_references <- length(citation_keys)

  # Figures/Tables in main text = unique figure/table labels defined in main-text files
  main_lines_all <- paste(unlist(lapply(main_text_files, read_lines_safe)), collapse = "\n")
  fig_labels <- c(
    str_match_all(main_lines_all, "#\\|\\s*label:\\s*(fig-[A-Za-z0-9_-]+)")[[1]][, 2],
    str_match_all(main_lines_all, "\\{#(fig-[A-Za-z0-9_-]+)")[[1]][, 2]
  )
  tbl_labels <- c(
    str_match_all(main_lines_all, "#\\|\\s*label:\\s*(tbl-[A-Za-z0-9_-]+)")[[1]][, 2],
    str_match_all(main_lines_all, "\\{#(tbl-[A-Za-z0-9_-]+)")[[1]][, 2]
  )
  n_figures_main <- length(unique(fig_labels[!is.na(fig_labels) & fig_labels != ""]))
  n_tables_main <- length(unique(tbl_labels[!is.na(tbl_labels) & tbl_labels != ""]))

  tibble(
    running_title_characters = running_title_char_count,
    abstract_words = as.integer(abstract_word_count),
    main_text_words = as.integer(main_text_word_count),
    n_references = as.integer(n_references),
    n_figures_main_text = as.integer(n_figures_main),
    n_tables_main_text = as.integer(n_tables_main)
  )
}

if (sys.nframe() == 0) {
  print(count_manuscript_stats())
}
