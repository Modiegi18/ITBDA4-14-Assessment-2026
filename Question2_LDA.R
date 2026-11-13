
# QUESTION 2  = Topic modelling of South African news with LDA

pkgs <- c("tm", "SnowballC", "topicmodels", "tidytext", "dplyr", "tidyr",
          "ggplot2", "wordcloud", "RColorBrewer", "reshape2")
new  <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

proj_root <- function() {
  d <- normalizePath(getwd())
  while (!any(file.exists(file.path(d, c("ITDBA_Project.Rproj", "Question1.4_MapReduce.R")))) &&
         dirname(d) != d) d <- dirname(d)
  if (dirname(d) == d) d <- normalizePath(getwd())   # fallback: current folder
  d
}
ROOT     <- proj_root()
NEWS_DIR <- file.path(ROOT, "data", "sa_news")
if (!dir.exists(NEWS_DIR)) NEWS_DIR <- file.path(ROOT, "sa_news")             # folder without data/
OUT_DIR  <- file.path(ROOT, "outputs", "Q2")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Helpers: question headings in the Console, charts in the Plots pane ---
banner <- function(question, title) {
  line <- strrep("=", 78)
  cat("\n", line, "\n  ", question, "  -  ", title, "\n", line, "\n", sep = "")
}
# Draw a chart in RStudio's Plots pane AND save the same chart as a PNG file
show_and_save <- function(draw, file, width = 1000, height = 650, res = 110) {
  tryCatch(draw(), error = function(e)
    message("  (Plots pane too small to draw this chart - enlarge the pane or click Zoom; ",
            "the chart is still saved to ", basename(file), ")"))
  png(file, width = width, height = height, res = res); draw(); dev.off()
  cat("  Chart shown in the Plots pane and saved as", basename(file), "\n")
}
show_gg <- function(p, file, width, height) {
  print(p)                                                   # Plots pane
  ggsave(file, p, width = width, height = height, dpi = 120, bg = "white")
  cat("  Chart shown in the Plots pane and saved as", basename(file), "\n")
}

# Parameters (change here) 
N_TOPICS <- 5     # n : number of topics
TOP_K    <- 10    # k : top words reported per topic
SEED     <- 1234  # reproducible LDA


# QUESTION 2.1  LOAD INTO A DATA FRAME  (col_1 = file_name, col_2 = content)

banner("QUESTION 2.1", "Load the news files into a data frame (file_name, content)")
files <- list.files(NEWS_DIR, pattern = "\\.txt$", full.names = TRUE)
files <- files[order(as.integer(gsub("\\D", "", basename(files))))]   # news_1 .. news_10

news_df <- data.frame(
  file_name = basename(files),
  content   = vapply(files, function(f)
                paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = " "),
                character(1)),
  stringsAsFactors = FALSE, row.names = NULL)

# Keep the headline (line 1) separately - useful to label documents later
news_df$headline <- vapply(files, function(f) readLines(f, n = 1, encoding = "UTF-8", warn = FALSE), "")
cat("Loaded", nrow(news_df), "documents into news_df. Structure:\n"); str(news_df[, c("file_name", "content")], nchar.max = 60)
cat("\nHeadline of each document:\n"); print(news_df[, c("file_name", "headline")])

#  QUESTION 2.1 (continued): denoising / preprocessing 

banner("QUESTION 2.1 (continued)", "Preprocessing / data denoising")
cat("Steps: transliterate to ASCII, remove datelines, URLs, e-mails, Rand amounts and brackets,\n",
    "      lower-case, remove numbers/punctuation, squeeze white space, remove stop words,\n",
    "      remove tokens < 3 letters, stem (with stem completion), drop words used only once\n", sep = "")
# Domain-specific noise words: reporting verbs and filler that occur in every
# news article but carry no topical meaning
news_noise <- c("said", "says", "say", "also", "will", "can", "one", "two", "three",
                "year", "years", "new", "mr", "ms", "dr", "per", "cent", "according",
                "including", "however", "well", "just", "like", "may", "must",
                "added", "noted", "explained", "made", "make", "get", "go", "going",
                "want", "us", "week", "day", "today", "time", "monday", "tuesday",
                "wednesday", "thursday", "friday", "saturday", "sunday",
                "continue", "continued", "ensure", "expected", "need", "needs",
                # source / site artefacts and corpus-wide terms (every article is
                # South African government news, so these do not separate topics)
                "sanews", "gov", "za", "south", "africa", "african", "africans")

clean_text <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ") # curly quotes, accents -> ASCII
  x <- gsub("(Mon|Tues|Wednes|Thurs|Fri|Satur|Sun)day,\\s+\\w+\\s+\\d{1,2},\\s+\\d{4}", " ", x) # datelines
  x <- gsub("https?://\\S+|www\\.\\S+", " ", x)              # URLs
  x <- gsub("\\S+@\\S+", " ", x)                             # e-mail addresses
  x <- gsub("\\bR\\s?\\d[\\d\\s,.]*(million|billion|m|bn)?", " ", x, perl = TRUE) # Rand amounts
  x <- gsub("\\[|\\]", " ", x)                               # editorial brackets [..]
  x <- tolower(x)                                            # case folding
  x <- gsub("[^a-z\\s]", " ", x)                             # numbers, punctuation, symbols
  x <- gsub("\\s+", " ", x)                                  # collapse white space
  trimws(x)                                                  # trailing / leading spaces
}
news_df$clean <- clean_text(news_df$content)

# Tokenise -> remove stop words -> remove short tokens -> stem
data("stop_words", package = "tidytext")
all_stops <- unique(c(stop_words$word, stopwords("en"), news_noise))

tokens <- news_df %>%
  select(file_name, clean) %>%
  unnest_tokens(word, clean) %>%
  filter(!word %in% all_stops, nchar(word) >= 3) %>%
  mutate(stem = wordStem(word, language = "en"))

# Stem completion: show each stem as its most frequent original word
# (e.g. "coalit" -> "coalition") so topic words stay readable
stem_map <- tokens %>% count(stem, word) %>% group_by(stem) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% select(stem, term = word)
tokens <- tokens %>% left_join(stem_map, by = "stem")

# Rare-term filter: drop terms that appear only once in the whole corpus
term_freq <- tokens %>% count(term)
tokens    <- tokens %>% semi_join(filter(term_freq, n >= 2), by = "term")

news_df$clean_tokens <- vapply(news_df$file_name, function(f)
  paste(tokens$term[tokens$file_name == f], collapse = " "), "")

cat("\nPreprocessing summary (words per document):\n")
prep_summary <- data.frame(
  file_name     = news_df$file_name,
  raw_words     = lengths(strsplit(news_df$content, "\\s+")),
  clean_tokens  = lengths(strsplit(news_df$clean_tokens, "\\s+")))
print(prep_summary)
write.csv(news_df[, c("file_name", "content", "clean_tokens")],
          file.path(OUT_DIR, "Q2_1_news_dataframe_clean.csv"), row.names = FALSE)

# Document-Term Matrix
dtm <- tokens %>% count(file_name, term) %>% cast_dtm(file_name, term, n)
dtm <- dtm[match(news_df$file_name, rownames(dtm)), ]
cat("\nDTM:", nrow(dtm), "documents x", ncol(dtm), "terms\n")


# QUESTION 2.2 =  LATENT DIRICHLET ALLOCATION

banner("QUESTION 2.2", sprintf("LDA: top n = %d topics, top k = %d words, word distribution per topic", N_TOPICS, TOP_K))
# (a) Supporting evidence for choosing n: model log-likelihood for n = 2..8
cat("Fitting LDA for n = 2 to 8 topics to choose n (takes about a minute)...\n")
k_grid <- 2:8
ll <- sapply(k_grid, function(k)
  as.numeric(logLik(LDA(dtm, k = k, method = "Gibbs",
                        control = list(seed = SEED, burnin = 500, iter = 1000,
                                       alpha = 0.5, delta = 0.1)))))
print(data.frame(n_topics = k_grid, log_likelihood = round(ll, 1)))
draw_ll <- function() {
  plot(k_grid, ll, type = "b", pch = 19, col = "#2E86AB", lwd = 2,
       main = "Q2.2 - LDA model fit by number of topics (Gibbs sampling)",
       xlab = "Number of topics (n)", ylab = "Log-likelihood (higher = better fit)")
  abline(v = N_TOPICS, lty = 2, col = "grey40")
  text(N_TOPICS, min(ll), paste("chosen n =", N_TOPICS), pos = 4, cex = 0.85)
}
show_and_save(draw_ll, file.path(OUT_DIR, "Q2_2_choose_n_topics.png"), 900, 550)

# (b) Fit the final model
lda_model <- LDA(dtm, k = N_TOPICS, method = "Gibbs",
                 control = list(seed = SEED, burnin = 1000, iter = 2000, thin = 100,
                                alpha = 0.5, delta = 0.1))

# (c) Word distribution per topic: beta = P(word | topic)
beta <- tidy(lda_model, matrix = "beta")

top_terms <- beta %>% group_by(topic) %>%
  slice_max(beta, n = TOP_K, with_ties = FALSE) %>%
  mutate(rank = row_number(), cum_share = cumsum(beta)) %>% ungroup() %>%
  arrange(topic, rank)

# Give each topic a short label from its top 3 words
topic_labels <- top_terms %>% filter(rank <= 3) %>% group_by(topic) %>%
  summarise(label = paste0("Topic ", first(topic), ": ", paste(term, collapse = ", ")))
top_terms <- top_terms %>% left_join(topic_labels, by = "topic")

cat("\n================ TOP", TOP_K, "WORDS PER TOPIC (beta = P(word|topic)) ================\n")
wide <- top_terms %>% mutate(cell = sprintf("%s (%.3f)", term, beta)) %>%
  select(rank, topic, cell) %>% pivot_wider(names_from = topic, values_from = cell,
                                             names_prefix = "Topic_")
print(as.data.frame(wide), right = FALSE)
write.csv(top_terms, file.path(OUT_DIR, "Q2_2_top_terms_per_topic.csv"), row.names = FALSE)

# Summary statistics of each topic's word distribution
beta_stats <- beta %>% group_by(topic) %>%
  summarise(vocab_size      = n(),
            top_k_mass      = sum(sort(beta, decreasing = TRUE)[1:TOP_K]),  # share of topic captured by top k
            max_beta        = max(beta),
            entropy_bits    = -sum(beta * log2(beta)),                      # lower = more focused topic
            .groups = "drop") %>% left_join(topic_labels, by = "topic")
cat("\nWord-distribution statistics per topic:\n"); print(as.data.frame(beta_stats))

# (d) Topic distribution per document: gamma = P(topic | document)
gamma <- tidy(lda_model, matrix = "gamma") %>%
  mutate(document = factor(document, levels = news_df$file_name))
dominant <- gamma %>% group_by(document) %>% slice_max(gamma, n = 1, with_ties = FALSE) %>%
  ungroup() %>% left_join(news_df[, c("file_name", "headline")], by = c("document" = "file_name")) %>%
  left_join(topic_labels, by = "topic") %>% arrange(match(document, news_df$file_name))
cat("\nDominant topic per document:\n")
print(as.data.frame(dominant[, c("document", "topic", "gamma", "headline")]), right = FALSE)

prevalence <- gamma %>% group_by(topic) %>% summarise(prevalence = mean(gamma)) %>%
  left_join(topic_labels, by = "topic") %>% arrange(desc(prevalence))
cat("\nTopic prevalence across the corpus (mean gamma):\n"); print(as.data.frame(prevalence))
write.csv(gamma, file.path(OUT_DIR, "Q2_2_document_topic_gamma.csv"), row.names = FALSE)


# QUESTION 2.3 = VISUALISATIONS OF TOP KEYWORDS PER TOPIC

banner("QUESTION 2.3", "Visualisation of the top keywords in each topic (4 methods)")
pal <- c("#2E86AB", "#E07A5F", "#3D9970", "#9B5DE5", "#F2A541", "#5C6B73", "#C1121F", "#118AB2")

# Method 1 - faceted horizontal bar chart of beta
cat("\nQ2.3 Method 1: bar chart of the top words per topic (beta)\n")
p1 <- ggplot(top_terms, aes(x = beta, y = reorder_within(term, beta, label), fill = label)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ label, scales = "free_y", labeller = label_wrap_gen(28)) +
  scale_y_reordered() + scale_fill_manual(values = pal) +
  labs(title = sprintf("Q2.3 Method 1 - Top %d words per topic (LDA, n = %d topics)", TOP_K, N_TOPICS),
       subtitle = "Bar length = beta, the probability of the word given the topic",
       x = "beta  P(word | topic)", y = "Term") +
  theme_minimal(base_size = 12) + theme(strip.text = element_text(face = "bold"))
show_gg(p1, file.path(OUT_DIR, "Q2_3a_top_terms_barchart.png"), 11, 7)

# Method 2 - one word cloud per topic (size proportional to beta)
cat("\nQ2.3 Method 2: word cloud for each topic\n")
draw_wc <- function() {
  op <- par(mfrow = c(ceiling(N_TOPICS / 2), 2), mar = c(0, 0, 2, 0)); on.exit(par(op))
  for (t in seq_len(N_TOPICS)) {
    w <- beta %>% filter(topic == t) %>% slice_max(beta, n = 40, with_ties = FALSE)
    suppressWarnings(wordcloud(w$term, w$beta, scale = c(3.2, 0.6), random.order = FALSE, rot.per = 0,
                               colors = colorRampPalette(c("grey60", pal[t]))(6)))
    title(main = paste("Q2.3 Method 2 -", topic_labels$label[t]), cex.main = 0.9)
  }
}
show_and_save(draw_wc, file.path(OUT_DIR, "Q2_3b_wordclouds_per_topic.png"), 1400, 1100, 130)

# Method 3 - heat map of top words across all topics (shows word sharing)
cat("\nQ2.3 Method 3: heat map of the top words across all topics\n")
heat_terms <- unique(top_terms$term)
heat <- beta %>% filter(term %in% heat_terms) %>%
  left_join(topic_labels, by = "topic")
p3 <- ggplot(heat, aes(x = factor(topic), y = reorder(term, beta, max), fill = beta)) +
  geom_tile(colour = "white") +
  scale_fill_gradient(low = "#F7FBFF", high = "#08306B", name = "beta") +
  labs(title = "Q2.3 Method 3 - Word distribution across topics (heat map)",
       subtitle = "Each top word's probability in every topic: dark = characteristic of that topic",
       x = "Topic", y = "Term") +
  theme_minimal(base_size = 11)
show_gg(p3, file.path(OUT_DIR, "Q2_3c_term_topic_heatmap.png"), 8, 10)

# Method 4 - document-topic composition (stacked bars of gamma)
cat("\nQ2.3 Method 4: topic mixture of each document (gamma)\n")
gamma_plot <- gamma %>% left_join(topic_labels, by = "topic")
gamma_plot$document <- factor(gamma_plot$document, levels = rev(news_df$file_name))
p4 <- ggplot(gamma_plot, aes(x = document, y = gamma, fill = label)) +
  geom_col() + coord_flip() + scale_fill_manual(values = pal) +
  labs(title = "Q2.3 Method 4 - Topic mixture of each news article",
       subtitle = "gamma = P(topic | document)",
       x = "Document", y = "Proportion of document (gamma)", fill = "Topic") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom", legend.direction = "vertical")
show_gg(p4, file.path(OUT_DIR, "Q2_3d_document_topic_mixture.png"), 10, 7)

cat("\nAll Q2 outputs saved to", OUT_DIR, "\n")
cat("Use the arrows at the top of the Plots pane to move between the 5 charts.\n")
