
# Question 1.4 Hadoop mapreduce configuration settings

#
# The keyword is typed at the prompt (RStudio) or given
# as an argument:  Rscript Question1.4_MapReduce.R africa local
#
# Uses: Question1.2_Mapper.R and Question1.3_Reducer.R 


proj_root <- function() {
  d <- normalizePath(getwd())
  while (!any(file.exists(file.path(d, c("ITDBA_Project.Rproj", "Question1.4_MapReduce.R")))) &&
         dirname(d) != d) d <- dirname(d)
  if (dirname(d) == d) d <- normalizePath(getwd())   # fallback: current folder
  d
}
ROOT      <- proj_root()
MAPPER    <- file.path(ROOT, "Question1.2_Mapper.R")
REDUCER   <- file.path(ROOT, "Question1.3_Reducer.R")
INPUT_DIR <- file.path(ROOT, "data", "map_reduce_test_docs")
if (!dir.exists(INPUT_DIR)) INPUT_DIR <- file.path(ROOT, "map_reduce_test_docs")   # folder without data/
OUT_DIR   <- file.path(ROOT, "outputs", "Q1")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# dynamic input = keyword and run mode 
args    <- commandArgs(trailingOnly = TRUE)
KEYWORD <- if (length(args) >= 1) args[1] else readline("Enter the keyword to search for: ")
KEYWORD <- trimws(gsub("[\"']", "", KEYWORD))      # ignore quote marks typed by the user
if (!nzchar(KEYWORD)) stop("A keyword is required")
RUN_MODE <- if (length(args) >= 2) args[2] else {
  m <- readline("Run mode - 'local' (demo on this PC) or 'hadoop' (cluster) [local]: ")
  if (nzchar(m)) m else "local"
}
RUN_MODE   <- tolower(trimws(gsub("[\"']", "", RUN_MODE)))  # ignore quote marks
if (!RUN_MODE %in% c("local", "hadoop")) stop("Run mode must be local or hadoop (type it without quotes)")
MATCH_MODE <- "word"   # "word" = whole word only; "substring" also matches "African"


# Part A = cluster configuration settings 

clean_hadoop_home <- gsub("\\\\", "/", gsub("[\"']", "", trimws(Sys.getenv("HADOOP_HOME"))))
if (nzchar(clean_hadoop_home)) Sys.setenv(HADOOP_HOME = clean_hadoop_home)
HADOOP_HOME_DIR <- if (nzchar(clean_hadoop_home)) clean_hadoop_home else "C:/hadoop-3.2.4"

cluster_config <- list(
  "core-site.xml" = c(
    "fs.defaultFS"                 = "hdfs://localhost:9000"),            # NameNode address
  "hdfs-site.xml" = c(
    "dfs.replication"              = "1",                                 # one DataNode -> one copy
    "dfs.namenode.name.dir"        = paste0("file:///", HADOOP_HOME_DIR, "/data/namenode"),
    "dfs.datanode.data.dir"        = paste0("file:///", HADOOP_HOME_DIR, "/data/datanode")),
  "mapred-site.xml" = c(
    "mapreduce.framework.name"     = "yarn",                              # run MapReduce on YARN
    "yarn.app.mapreduce.am.env"    = "HADOOP_MAPRED_HOME=${HADOOP_HOME}",
    "mapreduce.map.env"            = "HADOOP_MAPRED_HOME=${HADOOP_HOME}",
    "mapreduce.reduce.env"         = "HADOOP_MAPRED_HOME=${HADOOP_HOME}"),
  "yarn-site.xml" = c(
    "yarn.nodemanager.aux-services"                     = "mapreduce_shuffle", # shuffle & sort service
    "yarn.nodemanager.aux-services.mapreduce_shuffle.class" = "org.apache.hadoop.mapred.ShuffleHandler")
)

write_site_xml <- function(props, path) {
  body <- vapply(names(props), function(n) sprintf(
    "  <property>\n    <name>%s</name>\n    <value>%s</value>\n  </property>", n, props[[n]]), "")
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>', "<configuration>", body, "</configuration>"), path)
}
CONF_DIR <- file.path(OUT_DIR, "hadoop_config")
dir.create(CONF_DIR, showWarnings = FALSE)
for (f in names(cluster_config)) write_site_xml(cluster_config[[f]], file.path(CONF_DIR, f))
cat("Cluster configuration files written to", CONF_DIR, "\n")


# Part b = Job configuration settings 


job_config <- list(
  job_name    = paste0("KeywordSearch_", KEYWORD),
  mapper      = "Rscript Question1.2_Mapper.R",
  reducer     = "Rscript Question1.3_Reducer.R",
  hdfs_input  = "/keyword_search/input",
  hdfs_output = "/keyword_search/output",
  properties  = c(
    "mapreduce.job.name"                            = paste0("KeywordSearch_", KEYWORD),
    "mapreduce.job.reduces"                         = "1",          # one sorted output file
    "mapreduce.input.fileinputformat.split.minsize" = "268435456",  # 256 MB: 1 document = 1 split,
                                                                    # so row numbers stay correct
    "stream.map.output.field.separator"             = "\t",         # key <TAB> value
    "stream.num.map.output.key.fields"              = "1"),         # key = document name
  cmdenv      = c(SEARCH_KEYWORD = KEYWORD, MATCH_MODE = MATCH_MODE)  # dynamic keyword to every task
)
cat("\nJob configuration:\n"); str(job_config)


# Part C = Run the Job 

rscript <- file.path(R.home("bin"), "Rscript")
docs    <- list.files(INPUT_DIR, pattern = "\\.txt$", full.names = TRUE)
docs    <- docs[order(as.integer(gsub("\\D", "", basename(docs))))]

if (RUN_MODE == "local") {
  #  LOCAL DEMONSTRATION (no cluster needed) 
  cat(sprintf("\nSearching %d documents for '%s' (case-insensitive, %s mode)\n",
              length(docs), KEYWORD, MATCH_MODE))
  Sys.setenv(SEARCH_KEYWORD = KEYWORD, MATCH_MODE = MATCH_MODE)
  t0 <- Sys.time()

  # 1. MAP = one mapper process per input split (document), run in parallel
  library(parallel)
  run_mapper <- function(f, rscript, mapper) {
    Sys.setenv(mapreduce_map_input_file = basename(f))   # Hadoop sets this for each split
    system2(rscript, shQuote(mapper), stdin = f, stdout = TRUE)
  }
  cl <- makeCluster(min(4L, length(docs)))               # 4 simulated data nodes
  clusterExport(cl, c("KEYWORD", "MATCH_MODE"))
  clusterEvalQ(cl, Sys.setenv(SEARCH_KEYWORD = KEYWORD, MATCH_MODE = MATCH_MODE))
  map_out <- unlist(parLapply(cl, docs, run_mapper, rscript = rscript, mapper = MAPPER))
  stopCluster(cl)
  writeLines(map_out, file.path(OUT_DIR, "map_output.txt"))
  cat("\nMAP: emitted", length(map_out), "<key, value> pairs. First pairs:\n")
  cat(head(map_out, 6), sep = "\n")

  if (!length(map_out)) {
    cat("\nThe keyword was not found in any document.\n")
  } else {
    # 2. SHUFFLE & SORT - order by key so each document's values are together
    keys     <- sub("\t.*", "", map_out)
    shuffled <- map_out[order(as.integer(gsub("\\D", "", keys)))]
    sorted_f <- file.path(OUT_DIR, "shuffle_sorted.txt")
    writeLines(shuffled, sorted_f)
    cat("\nSHUFFLE & SORT: grouped into", length(unique(keys)), "keys\n")

    # 3. REDUCE = one reducer process reads the sorted stream
    result <- system2(rscript, shQuote(REDUCER), stdin = sorted_f, stdout = TRUE)
    writeLines(result, file.path(OUT_DIR, "part-00000"))
    cat(sprintf("REDUCE: finished in %.1f s\n", as.numeric(Sys.time() - t0, units = "secs")))

    cat("\n FINAL OUTPUT (part-00000) \n")
    cat("document\tcount\t(row,column) locations\n")
    cat(result, sep = "\n")

    # 4. Evidence table = every occurrence with its surrounding text
    rc <- do.call(rbind, strsplit(sub(".*\t", "", shuffled), ","))
    evidence <- data.frame(document = sub("\t.*", "", shuffled),
                           row = as.integer(rc[, 1]), column = as.integer(rc[, 2]))
    evidence$context <- mapply(function(d, r, c) {
      l <- readLines(file.path(INPUT_DIR, d), encoding = "UTF-8", warn = FALSE)[r]
      substr(l, max(1, c - 25), c + nchar(KEYWORD) + 25)
    }, evidence$document, evidence$row, evidence$column)
    write.csv(evidence, file.path(OUT_DIR, "keyword_locations.csv"), row.names = FALSE)
    cat("\nOccurrences with context:\n"); print(evidence, right = FALSE)

    counts <- table(factor(evidence$document, levels = basename(docs)))
    png(file.path(OUT_DIR, "keyword_counts_per_document.png"), width = 900, height = 550, res = 110)
    bp <- barplot(counts, col = "#2E86AB", border = NA, ylim = c(0, max(counts) * 1.2),
                  main = sprintf("Occurrences of '%s' per document (MapReduce output)", KEYWORD),
                  xlab = "Document", ylab = "Number of occurrences")
    text(bp, counts, labels = counts, pos = 3)
    dev.off()
    cat("\nEvidence saved in", OUT_DIR, "\n")
  }

} else {
  # SUBMIT TO THE HADOOP CLUSTER 
  # Start the cluster first (cmd as Administrator): start-dfs.cmd  and  start-yarn.cmd
  is_win <- .Platform$OS.type == "windows"

  #  Pre-flight check 1 = HADOOP_HOME
  hh <- clean_hadoop_home                                   # quotes already removed
  cat("HADOOP_HOME:", hh, "\n")
  if (!nzchar(hh) || !dir.exists(hh))
    stop("HADOOP_HOME is not set or does not exist. Set it to your Hadoop folder, e.g. C:/hadoop-3.1.0")
  bin <- function(x) file.path(hh, "bin", if (is_win) paste0(x, ".cmd") else x)
  if (!file.exists(bin("hadoop"))) stop("Cannot find ", bin("hadoop"), " - check HADOOP_HOME")

  #  Pre-flight check 2 = the Hadoop Streaming jar 
  jar <- Sys.glob(file.path(hh, "share", "hadoop", "tools", "lib", "hadoop-streaming-*.jar"))
  if (!length(jar)) jar <- list.files(hh, pattern = "^hadoop-streaming.*\\.jar$", recursive = TRUE, full.names = TRUE)
  jar <- jar[!grepl("sources|test", basename(jar))]
  if (!length(jar)) stop("hadoop-streaming-*.jar not found under ", hh,
                         ". It normally sits in share/hadoop/tools/lib/ - reinstall the full Hadoop binary.")
  jar <- normalizePath(jar[1], winslash = "/")
  cat("Streaming jar:", jar, "\n")

  # Pre-flight check 3 = Rscript must be on the system PATH (YARN tasks call it) 
  if (!nzchar(Sys.which("Rscript")))
    stop("Rscript is not on the system PATH. Add ", R.home("bin"),
         " to the Windows PATH (System > Environment Variables), restart the cluster and RStudio.")

  run <- function(cmd, a, fail_msg = NULL) {
    cat(">", basename(cmd), paste(a, collapse = " "), "\n")
    st <- system2(cmd, a)
    if (!is.null(fail_msg) && st != 0) stop(fail_msg)
    st
  }

  #  Pre-flight check 4: is HDFS running? 
  run(bin("hdfs"), c("dfs", "-ls", "/"),
      "HDFS is not running. Open cmd as Administrator and run start-dfs.cmd and start-yarn.cmd, then try again.")



  stage <- if (is_win) file.path(Sys.getenv("SystemDrive", "C:"), "hadoop_keyword_search") else file.path(tempdir(), "hadoop_keyword_search")
  dir.create(stage, showWarnings = FALSE, recursive = TRUE)
  file.copy(c(MAPPER, REDUCER), stage, overwrite = TRUE)
  file.copy(docs, stage, overwrite = TRUE)
  old_wd <- setwd(stage); on.exit(setwd(old_wd), add = TRUE)
  cat("Staging folder:", stage, "\n")

  # Load the corpus into HDFS (output folder must not exist before a job)
  run(bin("hdfs"), c("dfs", "-mkdir", "-p", job_config$hdfs_input), "Could not create the HDFS input folder")
  run(bin("hdfs"), c("dfs", "-put", "-f", basename(docs), job_config$hdfs_input), "Could not copy the documents into HDFS")
  run(bin("hdfs"), c("dfs", "-rm", "-r", "-f", job_config$hdfs_output))

  props <- job_config$properties[names(job_config$properties) != "stream.map.output.field.separator"]  # TAB is the default
  streaming_args <- c(
    "jar", shQuote(jar),
    unlist(lapply(names(props), function(p) c("-D", paste0(p, "=", props[[p]])))),
    "-files", "Question1.2_Mapper.R,Question1.3_Reducer.R",          # relative: no spaces
    unlist(lapply(names(job_config$cmdenv), function(e)
      c("-cmdenv", paste0(e, "=", job_config$cmdenv[[e]])))),
    "-input",   job_config$hdfs_input,
    "-output",  job_config$hdfs_output,
    "-mapper",  shQuote(job_config$mapper),
    "-reducer", shQuote(job_config$reducer))

  run(bin("hadoop"), streaming_args,
      "Job failed - open http://localhost:8088, click the job, then 'logs' to see the mapper/reducer error")
  result <- system2(bin("hdfs"), c("dfs", "-cat", paste0(job_config$hdfs_output, "/part-*")), stdout = TRUE)
  setwd(old_wd)
  writeLines(result, file.path(OUT_DIR, "hadoop_part-00000.txt"))
  cat("\n=== Hadoop output (part-00000) ===\n"); cat(result, sep = "\n")
}
