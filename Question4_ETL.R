# =============================================================================
# QUESTION 4  ~  ETL with R and PostgreSQL
#
# USE CASE: "What drives engagement on a tech-news aggregator?"
#   Source  : Hacker News front page (https://news.ycombinator.com/news)
# =============================================================================

pkgs <- c("httr", "rvest", "xml2", "dplyr", "tidyr", "stringr", "readr", "ggplot2", "DBI")
new  <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new)
invisible(suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE)))

proj_root <- function() {
  d <- normalizePath(getwd())
  while (!any(file.exists(file.path(d, c("ITDBA_Project.Rproj", "Question1.4_MapReduce.R")))) &&
         dirname(d) != d) d <- dirname(d)
  if (dirname(d) == d) d <- normalizePath(getwd())   # fallback: current folder
  d
}
ROOT     <- proj_root()
DATA_DIR <- file.path(ROOT, "data")
RAW_DIR  <- file.path(DATA_DIR, "q4_raw_html")
OUT_DIR  <- file.path(ROOT, "outputs", "Q4")
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Helpers: question headings in the Console, charts in the Plots pane ---
banner <- function(question, title) {                 # heading: start of a question
  line <- strrep("=", 78)
  cat("\n", line, "\n  ", question, "  -  ", title, "\n", line, "\n", sep = "")
}
step <- function(id, title) {                          # small heading: a step inside a question
  cat("\n~~~~ ", id, ": ", title, " ", strrep("~", max(3, 68 - nchar(id) - nchar(title))), "\n", sep = "")
}
end_banner <- function(question) {                     # marks the end of a question
  cat("\n", strrep("=", 30), " END OF ", question, " ", strrep("=", max(3, 39 - nchar(question))), "\n", sep = "")
}
show_gg <- function(p, file, width = 8, height = 5) {
  tryCatch(print(p), error = function(e)
    message("  (Plots pane too small - enlarge it or click Zoom; the chart is still saved)"))
  ggsave(file, p, width = width, height = height, dpi = 120, bg = "white")
  cat("  Chart shown in the Plots pane and saved as", basename(file), "\n")
}

BASE_URL    <- "https://news.ycombinator.com"
PAGES       <- 2                 # 30 stories per page
CRAWL_DELAY <- 30                # seconds, from robots.txt
USER_AGENT  <- "ITDBA-student-ETL/1.0 (academic project; R httr)"

# =============================================================================
# QUESTION 4.1  EXTRACT ~ web scraping + regex 
# =============================================================================
banner("QUESTION 4.1", "EXTRACT ~ scrape Hacker News + regex into a raw data frame")
cat("Use case: what drives engagement on a tech-news aggregator?\n",
    "Downloading", PAGES, "pages (30 s pause between pages, as robots.txt asks)...\n")
# ---- Fetch a page (cached so the analysis is reproducible) ----------------
fetch_page <- function(p) {
  cache <- file.path(RAW_DIR, sprintf("hn_news_p%d.html", p))
  if (file.exists(cache)) { message("cached: ", basename(cache)); return(read_html(cache)) }
  if (p > 1) Sys.sleep(CRAWL_DELAY)
  res <- GET(sprintf("%s/news?p=%d", BASE_URL, p), user_agent(USER_AGENT), timeout(30))
  stop_for_status(res)
  txt <- httr::content(res, "text", encoding = "UTF-8")
  writeLines(txt, cache, useBytes = TRUE)
  message("downloaded page ", p)
  read_html(txt)
}

# ---- Parse one page: every story is a <tr class="athing"> followed by a
#      <tr> holding the "subtext" (points, author, age, comments) -----------
txt_or_na <- function(node) if (inherits(node, "xml_missing")) NA_character_ else html_text(node)

parse_page <- function(page) {
  rows <- html_elements(page, "tr.athing")
  bind_rows(lapply(rows, function(r) {
    sub   <- xml_find_first(r, "./following-sibling::tr[1]")
    title <- html_element(r, "span.titleline > a")
    links <- html_elements(sub, "a")
    link_txt <- html_text(links)
    data.frame(
      item_id        = xml_attr(r, "id"),
      rank_raw       = txt_or_na(html_element(r, "span.rank")),            # "1."
      title_raw      = txt_or_na(title),
      url_raw        = html_attr(title, "href"),
      site_raw       = txt_or_na(html_element(r, "span.sitestr")),
      points_raw     = txt_or_na(html_element(sub, "span.score")),         # "123 points"
      author_raw     = txt_or_na(html_element(sub, "a.hnuser, a[href^='user?id=']")),  # class or profile link
      age_raw        = txt_or_na(html_element(sub, "span.age")),           # "3 hours ago"
      age_title_raw  = html_attr(html_element(sub, "span.age"), "title"),  # ISO timestamp + epoch
      comments_raw   = { m <- link_txt[grepl("comment|discuss", link_txt)]; if (length(m)) tail(m, 1) else NA_character_ },
      stringsAsFactors = FALSE)
  }))
}

step("4.1 (a)", "Download (or reuse cached) pages and parse the HTML")
raw <- bind_rows(lapply(seq_len(PAGES), function(p) parse_page(fetch_page(p))))
raw$scraped_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
stopifnot(nrow(raw) >= 20)

# -----------------------------------------------------------------------------
# QUESTION 4.1 (continued) REGEX ANALYSIS: derive structured attributes from the raw strings
# (no cleaning or imputation yet ~ that is Q4.2)
# -----------------------------------------------------------------------------
step("4.1 (b)", "Regex analysis: numbers, age, post type and domain from raw text")
extracted <- raw %>% mutate(
  rank      = as.integer(str_extract(rank_raw, "\\d+")),
  # numeric attribute 1 (existing): points -> NA when the field is absent
  points    = as.integer(str_extract(points_raw, "\\d+(?=\\s*points?)")),
  # numeric attribute 2 (existing): comments; "discuss" means no comments yet
  comments  = as.integer(str_extract(comments_raw, "\\d+(?=[\\s ]*comments?)")),
  # numeric attribute 3 (INFERRED): age in hours from "N minutes/hours/days ago"
  age_value = as.numeric(str_extract(age_raw, "\\d+")),
  age_unit  = str_extract(age_raw, "minute|hour|day|month|year"),
  age_hours = unname(age_value * c(minute = 1/60, hour = 1, day = 24, month = 720, year = 8760)[age_unit]),
  # text-derived categorical attributes
  post_type = case_when(
    str_detect(title_raw, regex("^\\s*show hn", ignore_case = TRUE)) ~ "Show HN",
    str_detect(title_raw, regex("^\\s*ask hn",  ignore_case = TRUE)) ~ "Ask HN",
    str_detect(title_raw, regex("^\\s*(tell|launch) hn", ignore_case = TRUE)) ~ "Tell/Launch HN",
    str_detect(title_raw, regex("\\bis hiring\\b|\\bhiring\\b|\\(YC [SWFX]\\d{2}\\)", ignore_case = TRUE)) & is.na(author_raw) ~ "Job",
    TRUE ~ "Story"),
  domain    = str_match(url_raw, "^https?://(?:www\\.)?([^/:?#]+)")[, 2],     # NA for relative links
  title_year= as.integer(str_match(title_raw, "\\((19|20)(\\d{2})\\)\\s*$")[, 1] %>% str_extract("\\d{4}")),
  has_pdf   = str_detect(title_raw, regex("\\[pdf\\]", ignore_case = TRUE))
) %>% select(-age_value, -age_unit)

step("4.1 (c)", "Data-quality report of the RAW extract")
cat("Records scraped:", nrow(extracted), "\n\n")
cat("Data-quality problems in the RAW extract:\n")
dq <- data.frame(
  issue = c("missing points", "missing author", "missing comment count",
            "'discuss' instead of a number", "no external domain (self/job post)",
            "non-breaking spaces / HTML entities in text", "titles with [pdf] or (year) tags",
            "age given in mixed units"),
  rows  = c(sum(is.na(extracted$points)), sum(is.na(extracted$author_raw)),
            sum(is.na(extracted$comments)), sum(grepl("discuss", extracted$comments_raw)),
            sum(is.na(extracted$domain)),
            sum(grepl(" |&amp;|&#", paste(extracted$title_raw, extracted$comments_raw))),
            sum(extracted$has_pdf | !is.na(extracted$title_year)),
            length(unique(str_extract(extracted$age_raw, "minute|hour|day")))))
print(dq)
str(extracted)

write_csv(extracted, file.path(DATA_DIR, "q4_hn_raw.csv"))
cat("\nRaw dataset saved to data/q4_hn_raw.csv\n")

# =============================================================================
# QUESTION 4.2  TRANSFORM ~ cleaning, denoising, missing-value imputation
# Reads the raw extract back from CSV (the staging file of the ETL pipeline)
# =============================================================================
end_banner("QUESTION 4.1")
banner("QUESTION 4.2", "TRANSFORM ~ cleaning, denoising and missing-value imputation")
raw <- read_csv(file.path(DATA_DIR, "q4_hn_raw.csv"), show_col_types = FALSE,
                col_types = cols(.default = col_character()))
num <- c("rank", "points", "comments", "age_hours", "title_year")
raw[num] <- lapply(raw[num], as.numeric)
cat("Raw rows:", nrow(raw), "\n")
missing_before <- colSums(is.na(raw[, c("title_raw", "author_raw", "domain", "points", "comments", "age_hours")]))

# ---- 4.2 (1) Text denoising ------------------------------------------------------
step("4.2 (1)", "Text denoising")
# html_text() already decodes most entities; catch any double-encoded leftovers
decode_entities <- function(x) x %>% str_replace_all("&amp;", "&") %>% str_replace_all("&quot;", "\"") %>%
  str_replace_all("&#x27;|&#39;", "'") %>% str_replace_all("&lt;", "<") %>% str_replace_all("&gt;", ">")

clean <- raw %>% mutate(
  title = title_raw %>%
    decode_entities() %>%                                       # &amp; -> &
    str_replace_all("[  -​ ]", " ") %>%     # exotic spaces
    str_replace_all("[‘’]", "'") %>% str_replace_all("[“”]", "\"") %>%
    str_replace_all("[–—]", "-") %>%
    str_remove("\\s*\\[(pdf|video)\\]\\s*$") %>%                # format tags
    str_remove("\\s*\\((19|20)\\d{2}\\)\\s*$") %>%              # "(2019)" kept in title_year
    str_remove(regex("^\\s*(show|ask|tell|launch)\\s+hn\\s*[:\\-]?\\s*", ignore_case = TRUE)) %>%
    str_squish(),
  author = str_squish(author_raw),
  domain = str_to_lower(str_remove(domain, "^www\\.")),
  post_type = factor(post_type)
)

# ---- 4.2 (2) Duplicates & invalid rows -------------------------------------------
step("4.2 (2)", "Duplicates & invalid rows")
n0 <- nrow(clean)
clean <- clean %>% distinct(item_id, .keep_all = TRUE) %>%        # same story on 2 pages
  filter(!is.na(title), nchar(title) > 0)
cat("Duplicates / empty titles removed:", n0 - nrow(clean), "\n")

# ---- 4.2 (3) Missing-value imputation --------------------------------------------
step("4.2 (3)", "Missing-value imputation")
clean <- clean %>% mutate(
  points_imputed   = is.na(points),
  comments_imputed = is.na(comments),
  # comments: rule-based - "discuss" link or no comment link means 0 comments
  comments = ifelse(is.na(comments), 0, comments),
  # domain: self posts (Ask/Tell HN, jobs) are hosted on HN itself
  domain   = ifelse(is.na(domain), "news.ycombinator.com", domain),
  # author: job adverts are posted by the platform, not a user
  author   = ifelse(is.na(author), ifelse(post_type == "Job", "hn_jobs", "unknown"), author),
  # age: fallback to the epoch in the timestamp attribute if the text was unreadable
  age_hours = ifelse(is.na(age_hours),
                     (as.numeric(as.POSIXct(scraped_at, tz = "UTC")) -
                        as.numeric(str_extract(age_title_raw, "\\d{9,}$"))) / 3600, age_hours)
)
# points: MEDIAN imputation within post_type (robust to the heavy right skew),
# falling back to the overall median when a whole group is missing (e.g. jobs)
overall_med <- median(clean$points, na.rm = TRUE)
clean <- clean %>% group_by(post_type) %>%
  mutate(points = ifelse(is.na(points), median(points, na.rm = TRUE), points)) %>% ungroup() %>%
  mutate(points = ifelse(is.na(points), overall_med, points))

# ---- 4.2 (4) Derived features & outlier flag -------------------------------------
step("4.2 (4)", "Derived features & outlier flag")
q <- quantile(clean$points, c(.25, .75)); iqr <- diff(q)
clean <- clean %>% mutate(
  points          = as.integer(round(points)),
  comments        = as.integer(comments),
  age_hours       = round(age_hours, 2),
  points_per_hour = round(points / pmax(age_hours, 1), 2),        # popularity velocity
  comments_per_pt = round(comments / pmax(points, 1), 3),         # discussion intensity
  title_words     = str_count(title, "\\S+"),
  points_outlier  = points > q[2] + 1.5 * iqr                     # flagged, not removed
)

final <- clean %>% transmute(
  item_id = as.integer(item_id), rank = as.integer(rank), title, post_type = as.character(post_type),
  domain, author, points, comments, age_hours, points_per_hour, comments_per_pt,
  title_words, title_year = as.integer(title_year), has_pdf = as.logical(has_pdf),
  points_imputed, comments_imputed, points_outlier,
  scraped_at = as.POSIXct(scraped_at, tz = "UTC"))

# ---- 4.2 (5) Quality report -------------------------------------------------------
step("4.2 (5)", "Quality report")
missing_after <- colSums(is.na(final[, c("title", "author", "domain", "points", "comments", "age_hours")]))
report <- data.frame(attribute = names(missing_before), missing_before = as.integer(missing_before),
                     missing_after = as.integer(missing_after))
cat("\nMissing values before vs after cleaning:\n"); print(report)
cat("\nImputed points:", sum(final$points_imputed), " | imputed comments:", sum(final$comments_imputed),
    " | outliers flagged:", sum(final$points_outlier), "\n")
cat("\nBefore / after examples:\n")
changed <- clean %>% filter(title_raw != title) %>% select(title_raw, title)
print(head(as.data.frame(changed), 6), right = FALSE)
print(head(final[, c("title", "post_type", "domain", "points", "comments", "age_hours")], 10))

p <- report %>% pivot_longer(-attribute, names_to = "stage", values_to = "n") %>%
  mutate(stage = factor(stage, c("missing_before", "missing_after"), c("Before cleaning", "After cleaning"))) %>%
  ggplot(aes(attribute, n, fill = stage)) + geom_col(position = "dodge") +
  scale_fill_manual(values = c("#C1121F", "#3D9970")) +
  labs(title = "Q4.2 - Missing values per attribute, before and after imputation",
       x = "Attribute", y = "Number of missing values", fill = NULL) + theme_minimal(base_size = 12)
show_gg(p, file.path(OUT_DIR, "Q4_2_missing_values.png"), 9, 5)

step("4.2 (6)", "Export the cleaned dataset to CSV")
write_csv(final, file.path(DATA_DIR, "q4_hn_clean.csv"), na = "")
cat("\nClean dataset exported: data/q4_hn_clean.csv (", nrow(final), "rows x", ncol(final), "cols )\n")

# =============================================================================
# QUESTION 4.3  LOAD into PostgreSQL + SQL analytical investigations
# Connection details come from environment variables or a password prompt 
# =============================================================================
end_banner("QUESTION 4.2")
banner("QUESTION 4.3", "LOAD into PostgreSQL + SQL analytical investigations")
cat("A password box will open: type your PostgreSQL (postgres) password.\n")
# ---- 4.3 (1) Connect ---------------------------------------------------------------
step("4.3 (1)", "Connect")
pg_password <- Sys.getenv("PGPASSWORD")
if (!nzchar(pg_password) && interactive())
  pg_password <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    rstudioapi::askForPassword("PostgreSQL password") else readline("PostgreSQL password: ")

driver <- if (requireNamespace("RPostgres", quietly = TRUE)) RPostgres::Postgres() else {
  if (!requireNamespace("RPostgreSQL", quietly = TRUE)) install.packages("RPostgres")
  if (requireNamespace("RPostgres", quietly = TRUE)) RPostgres::Postgres() else RPostgreSQL::PostgreSQL()
}
con <- dbConnect(driver,
                 dbname   = Sys.getenv("PGDATABASE", "itdba_etl"),
                 host     = Sys.getenv("PGHOST", "localhost"),
                 port     = as.integer(Sys.getenv("PGPORT", "5432")),
                 user     = Sys.getenv("PGUSER", "postgres"),
                 password = pg_password)
cat("Connected to:", dbGetQuery(con, "SELECT version()")[[1]], "\n")

# ---- 4.3 (2) Create the relational table ------------------------------------
step("4.3 (2)", "Create the relational table")
create_table_sql <- "
CREATE TABLE hn_stories (
    item_id          INTEGER PRIMARY KEY,
    rank             INTEGER,
    title            TEXT        NOT NULL,
    post_type        VARCHAR(20) NOT NULL,
    domain           VARCHAR(255),
    author           VARCHAR(100),
    points           INTEGER     CHECK (points >= 0),
    comments         INTEGER     CHECK (comments >= 0),
    age_hours        NUMERIC(8,2),
    points_per_hour  NUMERIC(10,2),
    comments_per_pt  NUMERIC(10,3),
    title_words      INTEGER,
    title_year       INTEGER,
    has_pdf          BOOLEAN,
    points_imputed   BOOLEAN,
    comments_imputed BOOLEAN,
    points_outlier   BOOLEAN,
    scraped_at       TIMESTAMP
)"
dbExecute(con, "DROP TABLE IF EXISTS hn_stories")
dbExecute(con, create_table_sql)
# (pgAdmin alternative: run the CREATE TABLE above, then right-click the table >
#  Import/Export Data > data/q4_hn_clean.csv, Format csv, Header on)

# ---- 4.3 (3) Import the cleaned CSV -------------------------------------------------
step("4.3 (3)", "Import the cleaned CSV")
clean <- read_csv(file.path(ROOT, "data", "q4_hn_clean.csv"), show_col_types = FALSE)
clean$scraped_at <- format(clean$scraped_at, "%Y-%m-%d %H:%M:%S")
invisible(dbWriteTable(con, "hn_stories", as.data.frame(clean), append = TRUE, row.names = FALSE))
cat("Rows loaded into hn_stories:", as.integer(dbGetQuery(con, "SELECT COUNT(*)::int AS n FROM hn_stories")$n), "\n")

# ---- 4.3 (4) Run the analytical SQL investigations ------------------------
step("4.3 (4)", "Run the analytical SQL investigations")
sql_queries <- list(
  # Investigation 1: do community posts (Show/Ask HN) attract more discussion
  # per vote than ordinary linked stories?
  "Investigation 1 - Engagement by post type" = "
    SELECT  post_type,
            COUNT(*)::int                              AS stories,
            ROUND(AVG(points), 1)                      AS avg_points,
            PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY points) AS median_points,
            ROUND(AVG(comments), 1)                    AS avg_comments,
            ROUND(SUM(comments)::numeric / NULLIF(SUM(points), 0), 3) AS comments_per_point,
            SUM(CASE WHEN points_imputed THEN 1 ELSE 0 END)::int AS imputed_rows
    FROM    hn_stories
    GROUP BY post_type
    ORDER BY avg_points DESC",

  # Investigation 2: how quickly do stories gain points, and does the front
  # page favour new or older items?
  "Investigation 2 - Freshness vs popularity" = "
    SELECT  CASE WHEN age_hours < 3  THEN '1: < 3 h'
                 WHEN age_hours < 6  THEN '2: 3-6 h'
                 WHEN age_hours < 12 THEN '3: 6-12 h'
                 ELSE                     '4: 12 h +' END AS age_bucket,
            COUNT(*)::int                     AS stories,
            ROUND(AVG(points), 1)             AS avg_points,
            ROUND(AVG(points_per_hour), 1)    AS avg_points_per_hour,
            ROUND(AVG(rank), 1)               AS avg_rank,
            ROUND(CORR(points, comments)::numeric, 2) AS corr_points_comments
    FROM    hn_stories
    WHERE   post_type <> 'Job'
    GROUP BY 1
    ORDER BY 1",

  # Investigation 3 (extra): which external sites reach the front page most?
  "Investigation 3 - Top sources" = "
    SELECT  domain,
            COUNT(*)::int                           AS stories,
            SUM(points)::int                        AS total_points,
            MAX(points)                             AS best_story_points,
            RANK() OVER (ORDER BY SUM(points) DESC) AS points_rank
    FROM    hn_stories
    WHERE   domain <> 'news.ycombinator.com'
    GROUP BY domain
    ORDER BY points_rank
    LIMIT 10")

results <- list()
for (name in names(sql_queries)) {
  res <- dbGetQuery(con, sql_queries[[name]])
  num <- vapply(res, is.numeric, logical(1)); res[num] <- lapply(res[num], function(x) round(as.numeric(x), 3))
  results[[name]] <- res
  cat("\n>>> ", name, "\n", sep = "")
  cat(sql_queries[[name]], "

")
  print(res, row.names = FALSE)
  write.csv(res, file.path(OUT_DIR, paste0(gsub("[^A-Za-z0-9]+", "_", name), ".csv")), row.names = FALSE)
}

# ---- 4.3 (5) Charts of the SQL results (for the report) -------------------------
step("4.3 (5)", "Charts of the SQL results (for the report)")
r1 <- results[[1]]
p1 <- ggplot(r1, aes(reorder(post_type, -avg_points), avg_points)) +
  geom_col(fill = "#2E86AB") +
  geom_text(aes(label = paste0(stories, " stories")), vjust = -0.4, size = 3.5) +
  labs(title = "Q4.3 Investigation 1: average points by post type (SQL GROUP BY)",
       x = "Post type", y = "Average points") + theme_minimal(base_size = 12)
show_gg(p1, file.path(OUT_DIR, "Q4_3_inv1_points_by_type.png"))

r2 <- results[[2]]
p2 <- ggplot(r2, aes(age_bucket, avg_points_per_hour, group = 1)) +
  geom_line(colour = "#E07A5F", linewidth = 1.2) + geom_point(size = 3, colour = "#E07A5F") +
  labs(title = "Q4.3 Investigation 2: points gained per hour, by story age (SQL CASE buckets)",
       x = "Age of story", y = "Average points per hour") + theme_minimal(base_size = 12)
show_gg(p2, file.path(OUT_DIR, "Q4_3_inv2_velocity_by_age.png"))

dbDisconnect(con)
cat("\nQ4.3 results saved to", OUT_DIR, "\n")
end_banner("QUESTION 4.3")
