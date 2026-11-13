
# Question 1.3  REDUCER  =  Keyword search with Hadoop MapReduce 
#The REDUCE function 
# reduce_fn(key, values) -> one output line
#   key    : document name
#   values : character vector of "row,col" strings for that document
reduce_fn <- function(key, values) {
  rc  <- do.call(rbind, lapply(strsplit(values, ","), as.integer))
  rc  <- rc[order(rc[, 1], rc[, 2]), , drop = FALSE]      # order by row, then column
  loc <- paste0("(", rc[, 1], ",", rc[, 2], ")", collapse = " ")
  paste(key, nrow(rc), loc, sep = "\t")
}

# Streaming driver 
run_reducer <- function() {
  con <- file("stdin", open = "r")
  on.exit(close(con))
  current_key <- NULL
  buffer      <- character(0)

  flush <- function() {
    if (!is.null(current_key) && length(buffer))
      writeLines(reduce_fn(current_key, buffer), stdout())
  }

  while (length(line <- readLines(con, n = 1L, warn = FALSE)) > 0) {
    Encoding(line) <- "UTF-8"                    # input files are UTF-8
    if (!nzchar(line)) next
    kv    <- strsplit(line, "\t", fixed = TRUE)[[1]]
    key   <- trimws(kv[1])
    value <- trimws(kv[2])
    if (!identical(key, current_key)) {   # key changed -> emit previous group
      flush()
      current_key <- key
      buffer      <- character(0)
    }
    buffer <- c(buffer, value)
  }
  flush()                                 # emit the last group
}

# RStudio demonstration (runs when you click Source in RStudio)
# Reads the mapper's output (outputs/Q1/map_output.txt, created by sourcing
# Question1.2_Mapper.R), performs the shuffle & sort, then applies reduce_fn().
demo_reducer <- function() {
  d <- normalizePath(getwd())
  while (!any(file.exists(file.path(d, c("ITDBA_Project.Rproj", "Question1.4_MapReduce.R")))) &&
         dirname(d) != d) d <- dirname(d)
  if (dirname(d) == d) d <- normalizePath(getwd())   # fallback: current folder
  map_file <- file.path(d, "outputs", "Q1", "map_output.txt")
  if (!file.exists(map_file)) stop("Run (Source) Question1.2_Mapper.R first - it creates outputs/Q1/map_output.txt")
  pairs <- readLines(map_file, encoding = "UTF-8", warn = FALSE)
  pairs <- pairs[nzchar(pairs)]
  if (!length(pairs)) { cat("The mapper found no occurrences of the keyword.\n"); return(invisible()) }

  # SHUFFLE & SORT: group the values of each key, keys in document order
  keys   <- sub("\t.*", "", pairs)
  values <- sub("^[^\t]*\t", "", pairs)
  groups <- split(values, factor(keys, levels = unique(keys[order(as.integer(gsub("\\D", "", keys)))])))

  # REDUCE
  result <- vapply(names(groups), function(k) reduce_fn(k, groups[[k]]), character(1), USE.NAMES = FALSE)
  cat("REDUCER output (document <TAB> count <TAB> (row,column) locations):\n")
  cat(result, sep = "\n")
  writeLines(result, file.path(d, "outputs", "Q1", "part-00000"))
  cat("\nSaved to outputs/Q1/part-00000\n")
  invisible(result)
}

# In RStudio (interactive) -> demonstration; under Rscript / Hadoop -> streaming
if (interactive()) demo_reducer() else run_reducer()
