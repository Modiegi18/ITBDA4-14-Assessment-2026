# =============================================================================
# QUESTION 3   WebText API: from a web link to text-mining insights
#   3.1 Fair-use web scraping -> clean text -> R data structure
#   3.2 Text preprocessing (denoising)
#   3.3 Text summarisation (3 techniques) + visualisations (5 charts)
#
# Website of choice: Department of Science, Technology and Innovation (DSTI)
#   https://www.dsti.gov.za  ->  news articles listed at /media/news
#

# =============================================================================

pkgs <- c("httr", "rvest", "xml2", "dplyr", "tidyr", "tidytext", "tm", "SnowballC",
          "textrank", "ggplot2", "wordcloud", "RColorBrewer", "stringr")
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
ROOT      <- proj_root()
OUT_DIR   <- file.path(ROOT, "outputs", "Q3")
CACHE_DIR <- file.path(ROOT, "data", "q3_web_cache")
dir.create(OUT_DIR,   recursive = TRUE, showWarnings = FALSE)
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Helper: question headings in the Console --------------------------------
banner <- function(question, title) {
  line <- strrep("=", 78)
  cat("\n", line, "\n  ", question, "  -  ", title, "\n", line, "\n", sep = "")
}

BASE_URL    <- "https://www.dsti.gov.za"
NEWS_PATH   <- "/media/news"
N_ARTICLES  <- 12      # articles to collect from the news listing
DELAY_SEC   <- 3       # pause between requests
N_SUMMARY   <- 6       # sentences in each extractive summary
USER_AGENT  <- "ITDBA-student-WebTextAPI/1.0 (academic text-mining project; R httr)"

# =============================================================================
# QUESTION 3.1  FAIR-USE WEB SCRAPING  ->  R DATA STRUCTURE
# =============================================================================

banner("QUESTION 3.1", "Fair-use web scraping of www.dsti.gov.za -> R data structure")
cat("Checking robots.txt, then downloading the news listing and articles",
    "(a few seconds between requests; pages are cached after the first run)...\n")

# ---- 3.1 (a) robots.txt check: rules for "User-agent: *" ------------------
ROBOTS <- NULL
robots_allowed <- function(url) {
  if (is.null(ROBOTS)) {
    res <- tryCatch(GET(paste0(BASE_URL, "/robots.txt"), user_agent(USER_AGENT), timeout(20)),
                    error = function(e) NULL)
    txt <- if (!is.null(res) && status_code(res) == 200) httr::content(res, "text", encoding = "UTF-8") else ""
    lines <- sub("#.*$", "", tolower(trimws(strsplit(txt, "\n")[[1]])))
    star <- FALSE; dis <- character(0); alw <- character(0)
    for (l in lines) {
      if (startsWith(l, "user-agent:")) star <- trimws(sub("user-agent:", "", l)) == "*"
      else if (star && startsWith(l, "disallow:")) dis <- c(dis, trimws(sub("disallow:", "", l)))
      else if (star && startsWith(l, "allow:"))    alw <- c(alw, trimws(sub("allow:", "", l)))
    }
    ROBOTS <<- list(disallow = dis[nzchar(dis)], allow = alw[nzchar(alw)])
    cat("robots.txt rules for '*': disallow =", if (length(ROBOTS$disallow)) paste(ROBOTS$disallow, collapse = ", ") else "(none)", "\n")
  }
  path <- tolower(sub("^https?://[^/]+", "", url))
  !any(startsWith(path, ROBOTS$disallow)) || any(startsWith(path, ROBOTS$allow))
}

# ---- 3.1 (b) polite, cached download ---------------------------------------
fetch_html <- function(url) {
  cache <- file.path(CACHE_DIR, paste0(gsub("[^A-Za-z0-9]+", "_", sub("^https?://(www\\.)?", "", url)), ".html"))
  if (file.exists(cache)) return(read_html(cache, encoding = "UTF-8"))
  if (!robots_allowed(url)) { warning("robots.txt disallows ", url); return(NULL) }
  Sys.sleep(DELAY_SEC)
  res <- tryCatch(GET(url, user_agent(USER_AGENT), timeout(30)), error = function(e) NULL)
  if (is.null(res) || status_code(res) != 200) { warning("Could not download ", url); return(NULL) }
  txt <- httr::content(res, "text", encoding = "UTF-8")
  writeLines(txt, cache, useBytes = TRUE)
  message("downloaded: ", url)
  read_html(txt)
}

# ---- 3.1 (c) collect article links from the news listing ------------------
listing <- fetch_html(paste0(BASE_URL, NEWS_PATH))
if (is.null(listing)) stop("The DSTI news page could not be downloaded - check your internet connection")
links <- html_attr(html_elements(listing, "a[href]"), "href")
links <- unique(sub("[?#].*$", "", links))
links <- links[grepl("^(https?://(www\\.)?dsti\\.gov\\.za)?/media/news/[A-Za-z0-9-]+/?$", links)]
links <- ifelse(startsWith(links, "http"), links, paste0(BASE_URL, links))
links <- head(unique(links), N_ARTICLES)
cat("Article links found on", NEWS_PATH, ":", length(links), "\n")
if (!length(links)) stop("No article links found - the site layout may have changed")

# ---- 3.1 (d) extract clean text from each article page --------------------
MONTHS <- "(January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
BOILERPLATE <- regex(paste(
  "javascript", "cookie", "copyright", "all rights reserved", "newsletter", "subscribe",
  "follow us", "share (this|on)", "loading latest", "chat with", "read more", "more articles",
  "min read", "privacy", "disclaimer", "paia", "^\\s*read\\s*\\|", sep = "|"), ignore_case = TRUE)

extract_article <- function(url) {
  page <- fetch_html(url)
  if (is.null(page)) return(NULL)
  # Remove non-content parts of the page BEFORE taking the text
  xml_remove(html_elements(page, "script, style, noscript, nav, header, footer, form, aside, button, svg, iframe"))
  title <- str_squish(html_text2(html_element(page, "h1")))
  if (is.na(title) || !nzchar(title)) title <- str_squish(html_text2(html_element(page, "title")))
  date  <- str_extract(html_text2(page), paste0("\\b\\d{1,2}\\s+", MONTHS, "\\s+20\\d{2}\\b"))

  # Body paragraphs: prefer <article>, then <main>, then the whole page
  container <- html_element(page, "article")
  if (inherits(container, "xml_missing")) container <- html_element(page, "main")
  if (inherits(container, "xml_missing")) container <- html_element(page, "body")
  paras <- str_squish(html_text2(html_elements(container, "p")))
  # Fallback for pages that use <div> instead of <p>: long text lines
  if (sum(nchar(paras) > 60) < 2) {
    lines <- str_squish(strsplit(html_text2(container), "\n")[[1]])
    paras <- lines[nchar(lines) > 80 & grepl("[.!?\"']$", lines)]
  }
  paras <- paras[nchar(paras) >= 40 & !str_detect(paras, BOILERPLATE) & paras != title]
  # Stop at the media-contact block ("Issued by ..." / "Enquiries ...")
  cut <- which(str_detect(paras, regex("^(issued by|enquiries|for (more information|media enquiries))", ignore_case = TRUE)))
  if (length(cut)) paras <- paras[seq_len(cut[1] - 1)]
  if (!length(paras)) return(NULL)
  data.frame(url = url, title = title, date = date, para_id = seq_along(paras),
             text = paras, stringsAsFactors = FALSE)
}

paragraphs <- bind_rows(lapply(links, extract_article))
if (!nrow(paragraphs)) stop("No article text could be extracted from the pages")
paragraphs <- paragraphs %>% distinct(url, text, .keep_all = TRUE)   # repeated blocks

articles <- paragraphs %>% group_by(url, title, date) %>%
  summarise(n_paragraphs = n(), n_words = sum(str_count(text, "\\S+")),
            text = paste(text, collapse = " "), .groups = "drop") %>%
  mutate(article_id = row_number()) %>% relocate(article_id)
paragraphs <- paragraphs %>% left_join(articles %>% select(url, article_id), by = "url")

# The R data structure returned by the "API"
webtext <- list(source = BASE_URL, retrieved = Sys.time(),
                articles = articles, paragraphs = paragraphs)
cat("\nArticles scraped:", nrow(articles), " | paragraphs:", nrow(paragraphs),
    " | words:", sum(articles$n_words), "\n\n")
print(as.data.frame(articles %>% select(article_id, date, n_words, title)), right = FALSE)
str(webtext, max.level = 1)
write.csv(articles, file.path(OUT_DIR, "Q3_1_dsti_articles.csv"), row.names = FALSE)

# =============================================================================
# QUESTION 3.2  TEXT PREPROCESSING
# =============================================================================
banner("QUESTION 3.2", "Text preprocessing (cleaning non-pertinent information)")
cat("Steps: sentence split, transliterate to ASCII, remove URLs / e-mails / phone numbers /\n",
    "       Rand amounts / bracketed acronyms, lower-case, remove numbers and punctuation,\n",
    "       squeeze white space, remove stop words and site words, drop tokens < 3 letters, stem\n", sep = "")
# Split into sentences (needed for summarisation); drop fragments
# Protect titles and abbreviations so "Prof. Blade Nzimande" is not cut at "Prof."
protect_abbrev <- function(x) gsub("\\b(Prof|Dr|Mr|Mrs|Ms|Adv|Hon|Rev|Gen|Col|Capt|Lt|St|No|Vol|Dept|Mt|Jan|Feb|Aug|Sept|Oct|Nov|Dec|vs|etc|e\\.g|i\\.e)\\.\\s",
                                   "\\1 ", x, perl = TRUE)
sentences <- paragraphs %>%
  mutate(text = protect_abbrev(text)) %>%
  unnest_tokens(sentence, text, token = "sentences", to_lower = FALSE) %>%
  mutate(n_words = str_count(sentence, "\\S+")) %>%
  filter(n_words >= 6) %>%                                       # drop fragments / headings
  distinct(sentence, .keep_all = TRUE) %>%                       # drop repeated sentences (captions, boilerplate)
  mutate(sentence_id = row_number())

denoise <- function(x) {
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = " ")          # curly quotes, accents, dashes
  x <- gsub("https?://\\S+|www\\.\\S+", " ", x)                  # URLs
  x <- gsub("\\S+@\\S+", " ", x)                                 # e-mail addresses
  x <- gsub("(\\+27|\\b0)\\s?\\d{2}[\\s-]?\\d{3}[\\s-]?\\d{4}", " ", x, perl = TRUE)  # SA phone numbers
  x <- gsub("\\bR\\s?\\d[\\d\\s,.]*(million|billion|bn|m)?\\b", " ", x, perl = TRUE)   # Rand amounts
  x <- gsub("\\([^)]{1,15}\\)", " ", x)                          # short bracketed acronyms (DSTI)
  x <- tolower(x)                                                # case folding
  x <- gsub("[0-9]+", " ", x)                                    # numbers and years
  x <- gsub("[^a-z\\s]", " ", x)                                 # punctuation and symbols
  str_squish(x)                                                  # white space
}
sentences$clean <- denoise(sentences$sentence)

data("stop_words", package = "tidytext")
web_noise <- c("said", "says", "say", "also", "will", "can", "one", "two", "us", "mr", "ms",
               "dr", "prof", "per", "cent", "including", "according", "however", "well", "today",
               "dsti", "gov", "za", "www", "click", "read", "issued", "enquiries", "media",
               "south", "africa", "african")                       # site- and corpus-wide words
STOPS <- unique(c(stop_words$word, stopwords("en"), web_noise))

tokens <- sentences %>% select(sentence_id, article_id, clean) %>%
  unnest_tokens(word, clean) %>%
  filter(!word %in% STOPS, nchar(word) >= 3) %>%                 # stop words, short tokens
  mutate(stem = wordStem(word, "en"))                            # stemming
stem_map <- tokens %>% count(stem, word) %>% group_by(stem) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% select(stem, term = word)
tokens <- tokens %>% left_join(stem_map, by = "stem")           # stem completion (readable)

prep_report <- data.frame(
  stage = c("Raw words scraped", "Sentences kept (6+ words)", "Tokens after denoising",
            "Tokens after stop-word / short-token removal", "Vocabulary (stemmed unique terms)"),
  value = c(sum(articles$n_words), nrow(sentences), sum(str_count(sentences$clean, "\\S+")),
            nrow(tokens), n_distinct(tokens$term)))
cat("\nPreprocessing report:\n"); print(prep_report)
cat("\nExample sentence before:\n ", sentences$sentence[1], "\nafter:\n ",
    paste(tokens$term[tokens$sentence_id == sentences$sentence_id[1]], collapse = " "), "\n")

# =============================================================================
# QUESTION 3.3  TEXT SUMMARISATION
# =============================================================================
banner("QUESTION 3.3", "Text summarisation (3 techniques) and visualisations (5 charts)")
term_freq <- tokens %>% count(term, sort = TRUE)

# ---- 3.3 Technique 1: Frequency-based extractive summary (Luhn) -----------
# score = sum of normalised word frequencies / sqrt(sentence length)
max_f <- max(term_freq$n)
freq_scores <- tokens %>% left_join(term_freq, by = "term") %>%
  group_by(sentence_id) %>% summarise(freq_score = sum(n / max_f) / sqrt(n()), .groups = "drop")
sentences <- sentences %>% left_join(freq_scores, by = "sentence_id") %>%
  mutate(freq_score = coalesce(freq_score, 0))
summary_freq <- sentences %>% slice_max(freq_score, n = N_SUMMARY, with_ties = FALSE) %>% arrange(sentence_id)

# ---- 3.3 Technique 2: TextRank (PageRank over a sentence-similarity graph)
tr <- textrank_sentences(
  data        = sentences %>% transmute(textrank_id = sentence_id, sentence),
  terminology = tokens %>% transmute(textrank_id = sentence_id, word = term))
sentences <- sentences %>% left_join(tr$sentences %>% select(textrank_id, textrank),
                                     by = c("sentence_id" = "textrank_id"))
summary_tr <- sentences %>% slice_max(textrank, n = N_SUMMARY, with_ties = FALSE) %>% arrange(sentence_id)

# One-sentence TextRank digest for every article
digest <- sentences %>% group_by(article_id) %>%
  slice_max(textrank, n = 1, with_ties = FALSE) %>% ungroup() %>%
  arrange(article_id) %>% select(article_id, title, key_sentence = sentence)

# ---- 3.3 Technique 3: keyword summary - TF-IDF per article + key phrases --
article_tfidf <- tokens %>% count(article_id, term) %>% bind_tf_idf(term, article_id, n) %>%
  group_by(article_id) %>% slice_max(tf_idf, n = 5, with_ties = FALSE) %>% ungroup()
kw <- textrank_keywords(tokens$term, relevant = rep(TRUE, nrow(tokens)), ngram_max = 3, sep = " ")
top_phrases <- kw$keywords %>% filter(ngram > 1, freq > 1) %>% head(15)

# ---- Print the summaries -------------------------------------------------------
show_summary <- function(df, name) {
  cat("\n================", name, "================\n")
  cat(paste0("- ", df$sentence), sep = "\n")
  cat(sprintf("(%d of %d sentences; %.1f%% of the words)\n", nrow(df), nrow(sentences),
              100 * sum(df$n_words) / sum(sentences$n_words)))
}
show_summary(summary_freq, "SUMMARY 1 - Frequency-based (Luhn)")
show_summary(summary_tr,   "SUMMARY 2 - TextRank")
overlap <- intersect(summary_freq$sentence_id, summary_tr$sentence_id)
cat("\nSentences chosen by both techniques:", length(overlap), "of", N_SUMMARY, "\n")
cat("\nOne-line TextRank digest per article:\n"); print(as.data.frame(digest), right = FALSE)
cat("\nTop key phrases (TextRank keywords):\n"); print(as.data.frame(top_phrases[, c("keyword", "freq")]))

writeLines(c("Source: DSTI news, https://www.dsti.gov.za/media/news", "",
             "FREQUENCY-BASED SUMMARY", summary_freq$sentence, "",
             "TEXTRANK SUMMARY", summary_tr$sentence, "",
             "ARTICLE DIGEST", paste0(digest$title, ": ", digest$key_sentence)),
           file.path(OUT_DIR, "Q3_3_summaries.txt"))
write.csv(sentences %>% select(sentence_id, article_id, freq_score, textrank, sentence),
          file.path(OUT_DIR, "Q3_3_sentence_scores.csv"), row.names = FALSE)

# ---- 3.3 Visualisations ----------------------------------------------------------
theme_set(theme_minimal(base_size = 12))
cat("\n--- QUESTION 3.3 visualisations (shown in the Plots pane and saved to outputs/Q3) ---\n")
# Show a ggplot chart in the Plots pane AND save it as a PNG file
save_plot <- function(p, f, w = 10, h = 6) {
  tryCatch(print(p), error = function(e)
    message("  (Plots pane too small - enlarge it or click Zoom; the chart is still saved)"))
  ggsave(file.path(OUT_DIR, f), p, width = w, height = h, dpi = 120, bg = "white")
  cat("  Chart shown in the Plots pane and saved as", f, "\n")
}

# V1 - Top 20 terms
p1 <- ggplot(head(term_freq, 20), aes(n, reorder(term, n))) +
  geom_col(fill = "#2E86AB") + geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  expand_limits(x = max(term_freq$n) * 1.1) +
  labs(title = "Q3.3 Chart 1 - Top 20 terms in DSTI news articles (after preprocessing)",
       subtitle = sprintf("%d articles from dsti.gov.za/media/news", nrow(articles)),
       x = "Frequency (occurrences)", y = "Term")
save_plot(p1, "Q3_V1_term_frequency.png")

# V2 - Word cloud
draw_wc <- function() {
op <- par(mar = c(0, 0, 2, 0)); on.exit(par(op))
suppressWarnings(wordcloud(term_freq$term, term_freq$n, max.words = 100, random.order = FALSE, rot.per = 0.15,
          scale = c(4, 0.6), colors = brewer.pal(8, "Dark2")))
title(main = "Q3.3 Chart 2 - Word cloud of DSTI news content", sub = "Word size proportional to frequency")
}
tryCatch(draw_wc(), error = function(e)
  message("  (Plots pane too small - enlarge it or click Zoom; the chart is still saved)"))
png(file.path(OUT_DIR, "Q3_V2_wordcloud.png"), width = 1100, height = 900, res = 130); draw_wc(); dev.off()
cat("  Chart shown in the Plots pane and saved as Q3_V2_wordcloud.png\n")

# V3 - Sentence scores: Luhn vs TextRank, with the summary sentences highlighted
sentences <- sentences %>% mutate(selected = case_when(
  sentence_id %in% overlap ~ "Both summaries",
  sentence_id %in% summary_freq$sentence_id ~ "Frequency summary only",
  sentence_id %in% summary_tr$sentence_id   ~ "TextRank summary only",
  TRUE ~ "Not selected"))
sel_cols <- c("Both summaries" = "#C1121F", "Frequency summary only" = "#F2A541",
              "TextRank summary only" = "#2E86AB", "Not selected" = "grey75")
p3 <- ggplot(sentences, aes(freq_score, textrank, colour = selected)) +
  geom_point(aes(size = selected != "Not selected"), alpha = 0.75) +
  scale_size_manual(values = c(`FALSE` = 1.5, `TRUE` = 3.5), guide = "none") +
  scale_colour_manual(values = sel_cols) +
  labs(title = "Q3.3 Chart 3 - Sentence importance under the two summarisation techniques",
       subtitle = sprintf("Pearson r = %.2f; coloured points are the %d-sentence summaries",
                          cor(sentences$freq_score, sentences$textrank), N_SUMMARY),
       x = "Frequency-based (Luhn) score", y = "TextRank score", colour = "Selected in")
save_plot(p3, "Q3_V3_sentence_scores_comparison.png")

# V4 - Distinctive terms per article (TF-IDF keyword summary)
article_tfidf <- article_tfidf %>%
  left_join(articles %>% transmute(article_id, label = paste0(article_id, ". ", str_trunc(title, 40))),
            by = "article_id") %>%
  mutate(label = factor(label, levels = unique(label[order(article_id)])))
p4 <- ggplot(article_tfidf, aes(tf_idf, reorder_within(term, tf_idf, label), fill = factor(article_id))) +
  geom_col(show.legend = FALSE) + scale_y_reordered() +
  facet_wrap(~ label, scales = "free_y", labeller = label_wrap_gen(28)) +
  labs(title = "Q3.3 Chart 4 - Most distinctive terms per article (TF-IDF keyword summary)", x = "TF-IDF", y = "Term")
save_plot(p4, "Q3_V4_tfidf_by_article.png", w = 14, h = 10)

# V5 - Top key phrases
p5 <- ggplot(top_phrases, aes(freq, reorder(keyword, freq))) +
  geom_col(fill = "#3D9970") +
  labs(title = "Q3.3 Chart 5 - Most frequent key phrases (TextRank keyword extraction)",
       x = "Frequency", y = "Key phrase")
save_plot(p5, "Q3_V5_key_phrases.png")

cat("\nAll Q3 outputs saved to", OUT_DIR, "\n")
