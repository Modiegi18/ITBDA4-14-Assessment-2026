
# Question 1.2  MAPPER = Keyword search with Hadoop MapReduce 


#  1. Resolve the search keyword dynamically 
get_keyword <- function(args = commandArgs(trailingOnly = TRUE)) {
  kw <- if (length(args) >= 1 && nzchar(args[1])) args[1] else Sys.getenv("SEARCH_KEYWORD", "")
  if (!nzchar(kw) && interactive()) kw <- readline("Enter the keyword to search for: ")
  kw <- trimws(kw)
  if (!nzchar(kw)) stop("No keyword supplied. Use: Rscript Question1.2_Mapper.R <keyword>  or set SEARCH_KEYWORD")
  kw
}

get_match_mode <- function(args = commandArgs(trailingOnly = TRUE)) {
  mode <- if (length(args) >= 2) args[2] else Sys.getenv("MATCH_MODE", "word")
  match.arg(tolower(mode), c("word", "substring"))
}

# Escape regex special characters so a keyword like "c++" or "u.s." is literal
escape_regex <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

# Build a case-insensitive pattern. Word mode uses letter/digit look-arounds
# instead of \b so that multi-word keywords ("south africa") also work.
build_pattern <- function(keyword, mode = "word") {
  kw <- escape_regex(keyword)
  kw <- gsub("\\s+", "\\\\s+", kw)             # "south africa" -> "south\s+africa"
  if (mode == "word") paste0("(?i)(?<![[:alnum:]])", kw, "(?![[:alnum:]])")
  else                paste0("(?i)", kw)
}

# 2. The MAP function
# map_fn(doc, row, line, pattern) -> character vector of "doc\trow,col" records
map_fn <- function(doc, row, line, pattern) {
  hits <- gregexpr(pattern, line, perl = TRUE)[[1]]
  if (hits[1] == -1) return(character(0))       # keyword not on this line
  paste0(doc, "\t", row, ",", as.integer(hits))  # one record per occurrence
}

# 3. Streaming driver (runs only when called with Rscript) 
run_mapper <- function() {
  keyword <- get_keyword()
  pattern <- build_pattern(keyword, get_match_mode())

  # Name of the split's source file, supplied by Hadoop; fallback for local tests
  doc <- Sys.getenv("mapreduce_map_input_file",
                    Sys.getenv("map_input_file", Sys.getenv("DOC_NAME", "stdin")))
  doc <- basename(doc)

  con <- file("stdin", open = "r")
  on.exit(close(con))
  row <- 0L
  # Stream one line at a time: constant memory, as a real mapper should
  while (length(line <- readLines(con, n = 1L, warn = FALSE)) > 0) {
    Encoding(line) <- "UTF-8"                    # input files are UTF-8
    row <- row + 1L
    out <- map_fn(doc, row, line, pattern)
    if (length(out)) writeLines(out, stdout())
  }
}

# 4. RStudio demonstration (runs when you click Source in RStudio) 
# Applies the same map_fn() to every document in data/map_reduce_test_docs and
# prints the <key, value> pairs, so the mapper can be shown working on its own.
# Its output is saved to outputs/Q1/map_output.txt for Question1.3_Reducer.R.
demo_mapper <- function(keyword = NULL) {
  d <- normalizePath(getwd())
  while (!any(file.exists(file.path(d, c("ITDBA_Project.Rproj", "Question1.4_MapReduce.R")))) &&
         dirname(d) != d) d <- dirname(d)
  if (dirname(d) == d) d <- normalizePath(getwd())   # fallback: current folder
  docs <- list.files(if (dir.exists(file.path(d, "data", "map_reduce_test_docs"))) file.path(d, "data", "map_reduce_test_docs") else file.path(d, "map_reduce_test_docs"), "\\.txt$", full.names = TRUE)
  docs <- docs[order(as.integer(gsub("\\D", "", basename(docs))))]
  if (!length(docs)) stop("map_reduce_test_docs folder not found - use Session > Set Working Directory > To Source File Location")
  if (is.null(keyword)) keyword <- get_keyword(character(0))
  pattern <- build_pattern(keyword, Sys.getenv("MATCH_MODE", "word"))

  out <- unlist(lapply(docs, function(f) {
    lines <- readLines(f, encoding = "UTF-8", warn = FALSE)
    unlist(lapply(seq_along(lines), function(r) map_fn(basename(f), r, lines[r], pattern)))
  }))
  cat(sprintf("MAPPER output for '%s' (key <TAB> row,column): %d pairs\n", keyword, length(out)))
  cat(out, sep = "\n")
  dir.create(file.path(d, "outputs", "Q1"), recursive = TRUE, showWarnings = FALSE)
  writeLines(out, file.path(d, "outputs", "Q1", "map_output.txt"))
  cat("\nSaved to outputs/Q1/map_output.txt - now Source Question1.3_Reducer.R\n")
  invisible(out)
}

# In RStudio (interactive) -> demonstration; under Rscript / Hadoop -> streaming
if (interactive()) demo_mapper() else run_mapper()
