## Marina Pozhidaeva
## Master thesis: "Gender-based Violence and 'the Other': Computational Text Analysis of Framing Femicides in German Media"
## Final version (imrpoved visualisation; deleted LDA and STM with bigrams, since they weren't interpretable; added helper functions; extended the list of stopwords)

# -----------------------------
# 1. Install and load packages
# -----------------------------

pkgs <- c(
  "stringi", "stringr", "text2vec", "tidyr", "quanteda", "dplyr",
  "quanteda.textstats", "ggplot2", "stm", "readr", "tidytext",
  "purrr", "udpipe", "showtext", "scales", "DT", "topicmodels",
  "Matrix", "clue", "reshape2", "igraph", "gridExtra", "kableExtra",
  "knitr", "spacyr", "SnowballC", "curl", "patchwork", "tibble"
)

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)

invisible(lapply(pkgs, library, character.only = TRUE))

# -----------------------------------------------
# 2. Set the universal APA-style plotting theme
# -----------------------------------------------

theme_set(
  theme_classic(base_size = 12, base_family = "Arial") +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      axis.title = element_text(face = "plain", color = "black", size = 12),
      axis.text = element_text(color = "black", size = 11),
      strip.background = element_blank(),
      strip.text = element_text(face = "plain", color = "black", size = 11),
      legend.title = element_text(face = "plain", color = "black", size = 11),
      legend.text = element_text(color = "black", size = 10),
      legend.position = "right",
      panel.border = element_blank(),
      panel.grid = element_blank()
    )
)

apa_fills <- c(
  "German" = "grey35",
  "MiddleEastern" = "grey70"
)

label_num_apa <- scales::label_number(big.mark = ",")

# -----------------------------
# 3. Introduce helper function
# -----------------------------
## This function is introduced to improve the visualisation 

# Wrap long labels 
wrap_term <- function(x, width = 22) { stringr::str_wrap(x, width = width) }

# --------------------------
# 4. Load and combine data
# --------------------------

df1 <- read_csv("thesis1.csv", locale = locale(encoding = "UTF-8")) |>
  mutate(group = "MiddleEastern")

df2 <- read_csv("thesis2.csv", locale = locale(encoding = "UTF-8")) |>
  mutate(group = "German")

df_all <- bind_rows(df1, df2)

# --------------------
# 5. Preprocess data 
# --------------------

# Basic text cleaning
clean_basic <- function(x) {
  x |>
    stringr::str_replace_all("&amp;", " und ") |>
    stringr::str_replace_all("&lt;|&gt;", " ") |>
    stringr::str_replace_all("<[^>]+>", " ") |>
    stringr::str_replace_all("[\u00A0\u2007\u202F]", " ") |>
    stringr::str_replace_all("[‘’´`]", "'") |>
    stringr::str_replace_all("[“”]", "\"") |>
    stringr::str_squish()
}

# Remove URLs, emails and other noise
clean_noise <- function(x) {
  x |>
    stringr::str_replace_all("https?://\\S+|www\\.\\S+", " ") |>
    stringr::str_replace_all("\\b\\S+@\\S+\\b", " ") |>
    stringr::str_replace_all("\\b(dpa|afp|reuters|apa)\\b", " ") |>
    stringr::str_replace_all("[[:cntrl:]]", " ") |>
    stringr::str_squish()
}

# Text normalization (encoding, NFC, UTF-8, entity fixes, noise removal, lowercase)
df_all <- df_all |>
  mutate(
    full_text_raw = full_text,
    full_text = stri_encode(full_text, from = "", to = "UTF-8"),
    full_text = stri_trans_nfc(full_text),
    full_text = clean_basic(full_text),
    full_text = clean_noise(full_text),
    full_text = stri_trans_tolower(full_text, locale = "de_DE")
  )

# Build corpus
corp <- corpus(df_all, text_field = "full_text")

# Define stopwords
base_stop <- stopwords("de")
extra_stop <- c(
  "dass","sei","wurde","worden","werden","wird","beiden","sind","l","dänischenhagen","k","h","f","abdullah","nina","ursula","franziska","kai","jana","angela","achim","a","m","danishenhag","limburg","sein","hätten","oktober","sollen","humtrup","stadum","rendsburg","eckernförde","danischen","januar","donnerstag","märz","unklar","wenige","groß","kommen","erzählt","westensee","müssen","zufolge","drei","freiburgs","axt","ermittler","innen","seit","uhr","s","schleswig","satz","wegen","hussein","mehr","immer","irina","niddapark","mahdi","park","wohnung","thomas","peter","straße","flusses","ice","machmal","freiburger","48-jährigen","48-jährige","gezeigt","wisse","berliner","mittwochabend","mauloff","geben","sommer","art","bestimmt","daraufhin","17-jährige","konnte","lukas","genug","darin","hob","nehmen","ebenso","stunden","vier","p","eggers","ort","infrage","fragt","deutlich","tritt","knapp","ahmadiyya","richtig","angeblich","hessen","steilküste","gelegt","immerhin","gegeben","brief","baumgruppe","rande",
  "elke","breitenbach","eckart","berger","dieter","salomon","september","dezember","südkreuz","mindestens","rendsburg-eckernförde","dänischen","antonia","ernst","insel","korfu","ahmad","g","mirko","röder","de","maiziere","dietrich","oberwittler","bekannten","bereits","gericht","prozess","angeklagte","kam","sagen","teilte","zunächst","älteren","wort","darüber","sana","alten","kaiser-klan","29-jährigen","limburger","könnten","jungen","infolge","alt","yousuf","darmstadt","all","paar","montagmorgen","jahren","nathalie","lareeb","gibt","montag","wasser","uwe","insbesondere","kilometer","jedoch","deshalb","frankfurter","nie","monaten","ursprünglich","nächsten","dar","begonnen","endet","eigener","woche","gerade","ge","freitag","erklärt","weiß","beginn","raheel","spät","darauf","damals","gefahren","hält","brauereiviertel","sehe","dagegen","gegangen","einfach","schrieb","ersten","gehabt","müsse","mussten","her","shazia","jeweils","ende","beide","culmsee","schlaf","holzkirchen","halbe","hannover",
  "hartmut","pleines","hohenasperg","vw","golf","dreisam","vielleicht","19-jährige","19-jährigen","sefin","relativ","früh","metern","seien","gab","neuen","viele","dpa","lsw","komm","mach","badische","badischen","lareebs","demnach","wäre","bar","first","gemacht","wochen","abend","macht","machte","sieben","abermals","dann","khola","hübsch","könne","wolle","maria","donauwörth","29-jährige","gut","möchte","kamen","steht","wissen","imad","gute","23-jährige","bayern","sah","tatsächlich","stunde","jahre","später","jan","freiburg","geht","glathe","gesehen","ab","zog","kannten","deren","monate","nacht","bevor","gesagt","eher","regelmäßig","stehen","heraus","passiert","danach","weit","geäußert","lassen","ging","durfte","teilen","merkel","frühere","mal","anschließend","legt","essen","sogar","unten","weder","zufällig","23-jährigen","grünen","erfuhr","wiese","kaum","begrüßte","per","setzen","besetzen","nennt","sicht","blieb","euro","ebenfalls","hieß","maximal","weiterer","kopf","zahl","eben","47-jährige","fuß",
  "jörg","brommann","kieler","haftrichter","christine","schirrmacher","axel","kollbach","dienstag","älter","gesprochen","darmstädter","echo","shilans","anis","amri","süddeutsche","zeitung","schwarze","sonne","hells","angels","besonders","eng","khan","tun","namen","liege","weniger","somit","jahres","kenne","konnten","schließlich","zentimeter","langes","fanden","wirft","statt","lebt","wusste","saß","ahmadiyya-gemeinde","suche","fiel","entsprach","nordfriesland","mireille","sechs","gemeldet","sagte","maryam","mai","kiel","fast","erst","flensburg","kathrin","darunter","meldet","beschrieb","besonderen","beginnt","klar","zweiten","möglicherweise","geriet","wollten","gingen","denen","heißt","ei","meint","half","gras","herr","nahezu","gegenüber","zuvor","gehe","apple","uzi","zierliche","nahm","letzten","daher","stuttgart","lässt","tage","seelenberger","spur","erzählen","denken","jenseits","erstmals","möglichkeit","insgesamt","b","äußerte","ließ","solle","geladen","lange","liegen","dauern",
  "kugel","traf","i","boden","liegende","feldweges","nahe","näheres","bekannt","schuss","gehört","hümtrup","mordes","trifft","stellen","fest","angaben","laut","leben","heute","gestern","junge","jahr","ghazi","getötet","dabei","männer","gefunden","leiche","fall","koffer","gemeinde","freund","bild","entdeckt","alte","tat","humptrup","zehn","littenweiler","wer","meisten","zugegeben","jva","weiteren","fuhr","trotz","hanna","sagt","dachte","ja","schon","shilan","berlin","ten","dritten","fünf","früher","zwei","alter","august","schafflund","schenk","hamburg","wagishauser","hinaus","zumindest","bisher","rheinland-pfalz","bekommen","mögliche","sogenannten","treffen","inzwischen","14-jährige","gebe","bka","äußert","wem","bislang","scheint","vermutlich","äußern","bald","22-jährige","außerdem","sowohl","tag","derweil","kurz","47-jährigen","stadt","sehen","hamid","klienen","nachdem","sprach","zweit","betont","große","warum","wurden","marias","rund","südwesten","sprecher","ruhig","sorgte"
)

custom_stop <- unique(c(base_stop, extra_stop))

# Tokenize and remove stopwords
toks <- tokens(
  corp,
  remove_punct   = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)

toks <- tokens_remove(
  toks,
  pattern = c(custom_stop, "in-nen"),
  valuetype = "fixed",
  padding = FALSE
)

# Create dfm
dfm_all <- dfm(toks)

if (!"group" %in% names(docvars(dfm_all))) {
  docvars(dfm_all, "group") <- df_all$group
}
grp <- docvars(dfm_all, "group")

# --------------------------------
# 6. Absolute frequency analysis
# --------------------------------

# Top 20 terms overall
top_n <- 20
freq_all <- textstat_frequency(dfm_all, n = top_n)

# Top 20 terms plot
p_top_terms_apa <- freq_all %>%
  mutate(feature = wrap_term(feature, 20)) %>%
  ggplot(aes(x = frequency, y = forcats::fct_reorder(feature, frequency))) +
  geom_col(fill = "grey40", width = 0.7) +
  scale_x_log10(labels = label_num_apa) +
  labs(
    x = "Frequency",
    y = "Term"
  )

p_top_terms_apa

# Frequency analysis by group

freq_by_group <- textstat_frequency(dfm_all, groups = grp)

freq_top <- freq_by_group %>%
  group_by(group) %>%
  slice_max(frequency, n = top_n, with_ties = FALSE) %>%
  ungroup()

# Top 20 per group plot
p_top_by_group_apa <- freq_top %>%
  mutate(feature = wrap_term(feature, 20)) %>%
  ggplot(aes(
    x = frequency,
    y = tidytext::reorder_within(feature, frequency, group),
    fill = group
  )) +
  geom_col(width = 0.7, show.legend = FALSE) +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = apa_fills) +
  scale_x_log10(labels = label_num_apa) +
  facet_wrap(~ group, scales = "free_y") +
  labs(
    x = "Frequency",
    y = "Term"
  )

p_top_by_group_apa

# -----------------------------------
# 7. Keyness analysis with log2ratio
# -----------------------------------
freq <- textstat_frequency(dfm_all, groups = grp) %>%
  select(feature, group, frequency)

wide <- freq %>%
  pivot_wider(names_from = group, values_from = frequency, values_fill = 0) %>%
  rename(ME = MiddleEastern, GE = German) %>%
  mutate(
    total = ME + GE,
    log2ratio = log2((ME + 0.5) / (GE + 0.5))
  )

min_total <- 20
top_k <- 30

sel <- wide %>%
  filter(total >= min_total) %>%
  mutate(abs_lr = abs(log2ratio)) %>%
  slice_max(abs_lr, n = top_k) %>%
  arrange(log2ratio)

# Log2ratio plot
p_log2ratio_apa <- sel %>%
  mutate(
    feature = wrap_term(feature, 20),
    direction = ifelse(log2ratio > 0, "MiddleEastern", "German")
  ) %>%
  ggplot(aes(x = log2ratio, y = reorder(feature, log2ratio), shape = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_segment(
    aes(x = 0, xend = log2ratio, yend = feature),
    linewidth = 0.5,
    color = "grey50"
  ) +
  geom_point(size = 2.5, color = "black") +
  scale_shape_manual(values = c("German" = 16, "MiddleEastern" = 17)) +
  labs(
    x = "Log2 ratio (positive values indicate higher frequency in MiddleEastern articles)",
    y = "Term",
    shape = "Group"
  )

p_log2ratio_apa

# ------------------------------------
# 8. Keyness analysis with chi-square
# ------------------------------------

key_me <- textstat_keyness(
  dfm_all,
  target = docvars(dfm_all, "group") == "MiddleEastern"
)

key_plot_data <- key_me %>%
  slice_max(abs(chi2), n = 20) %>%
  mutate(
    direction = ifelse(chi2 > 0, "MiddleEastern", "German"),
    feature = reorder(wrap_term(feature, 20), chi2)
  )

# Keyness with chi-square plot
p_chi2_apa <- ggplot(key_plot_data, aes(x = chi2, y = feature, fill = direction)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = apa_fills) +
  labs(
    x = "Chi-square keyness",
    y = "Term",
    fill = "Group"
  )

p_chi2_apa

# ---------------------------------------------------------------------
# 9. How many times are the words "Femizid" and "Ehrenmord" mentioned?
# ---------------------------------------------------------------------

fem_tbl <- df_all %>%
  mutate(n_femizid = str_count(full_text, regex("\\bfemizid(en|e|es|s)?\\b", ignore_case = TRUE))) %>%
  group_by(group) %>%
  summarise(
    total_mentions = sum(n_femizid),
    n_docs_with = sum(n_femizid > 0),
    .groups = "drop"
  )

ehr_tbl <- df_all %>%
  mutate(n_ehrenmord = str_count(full_text, regex("\\behrenmord(en|e|es)?\\b", ignore_case = TRUE))) %>%
  group_by(group) %>%
  summarise(
    total_mentions = sum(n_ehrenmord),
    n_docs_with = sum(n_ehrenmord > 0),
    .groups = "drop"
  )

term_usage_tbl <- bind_rows(
  fem_tbl %>% mutate(term = "Femizid"),
  ehr_tbl %>% mutate(term = "Ehrenmord")
) %>%
  pivot_longer(
    cols = c(total_mentions, n_docs_with),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      total_mentions = "Total mentions",
      n_docs_with = "Documents with at least one mention"
    ),
    term = factor(term, levels = c("Femizid", "Ehrenmord"))
  )

# Term usage plot
p_term_usage_apa <- ggplot(term_usage_tbl, aes(x = group, y = value, fill = group)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = apa_fills) +
  facet_grid(metric ~ term, scales = "free_y") +
  labs(
    x = "Group",
    y = "Count"
  ) +
  theme(
    strip.text.y = element_text(angle = 0)
  )

p_term_usage_apa

# -----------------------------
# 10. Structural topic modeling
# -----------------------------

# Create dfm that is trimmed for topic modeling
dfm_uni <- dfm(toks)
dfm_uni <- dfm_subset(dfm_uni, rowSums(dfm_uni) > 0)
dfm_uni <- dfm_trim(dfm_uni, min_docfreq = 3, docfreq_type = "count")
dfm_uni <- dfm_trim(dfm_uni, max_docfreq = 0.9, docfreq_type = "prop")
dfm_uni <- dfm_subset(dfm_uni, rowSums(dfm_uni) > 0)

# Convert dfm to stm format
out_uni <- convert(dfm_uni, to = "stm")

# Find optimal K
set.seed(1234)
k_uni <- searchK(
  documents = out_uni$documents,
  vocab     = out_uni$vocab,
  K         = 4:20,
  data      = out_uni$meta,
  init.type = "Spectral"
)

plot(k_uni)

# Choose K after inspecting diagnostics
K_uni_final <- 4

# Fit stm
model_uni <- stm(
  documents = out_uni$documents,
  vocab     = out_uni$vocab,
  K         = K_uni_final,
  data      = out_uni$meta,
  init.type = "Spectral",
  seed      = 1234
)

labelTopics(model_uni, n = 10)
plot(model_uni, type = "summary", n = 10)
plot(model_uni, type = "labels", n = 10)

# Group effect
eff_uni <- estimateEffect(
  1:model_uni$settings$dim$K ~ group,
  stmobj      = model_uni,
  metadata    = out_uni$meta,
  uncertainty = "Global"
)

summary(eff_uni)

# Top words per topic
top_uni <- tidy(model_uni, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 10, with_ties = FALSE) |>
  ungroup()

# Top words per topic plot
p_top_uni_apa <- top_uni %>%
  mutate(term = wrap_term(term, 18)) %>%
  ggplot(aes(x = beta, y = tidytext::reorder_within(term, beta, topic))) +
  geom_col(fill = "grey45", width = 0.7) +
  tidytext::scale_y_reordered() +
  facet_wrap(~ topic, scales = "free_y") +
  labs(
    x = "Topic-word probability",
    y = "Term"
  )

p_top_uni_apa

# Topic prevalence summary from document-topic distribution 
make_topic_summary <- function(model_obj, meta_obj, group_var = "group") {
  topic_prev <- as.data.frame(model_obj$theta)
  colnames(topic_prev) <- paste0("Topic ", seq_len(ncol(topic_prev)))
  topic_prev[[group_var]] <- meta_obj[[group_var]]
  
  topic_prev %>%
    pivot_longer(
      cols = starts_with("Topic "),
      names_to = "topic",
      values_to = "gamma"
    ) %>%
    group_by(.data[[group_var]], topic) %>%
    summarise(
      mean_gamma = mean(gamma, na.rm = TRUE),
      se = sd(gamma, na.rm = TRUE) / sqrt(dplyr::n()),
      lower = mean_gamma - 1.96 * se,
      upper = mean_gamma + 1.96 * se,
      .groups = "drop"
    ) %>%
    rename(group = .data[[group_var]])
}

topic_summary_uni <- make_topic_summary(model_uni, out_uni$meta)

# Topic prevalence plot
p_topic_prev_uni <- ggplot(
  topic_summary_uni,
  aes(x = mean_gamma, y = topic, fill = group)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  scale_fill_manual(values = apa_fills) +
  labs(
    x = "Mean topic proportion",
    y = "Topic",
    fill = "Group"
  ) +
  theme(
    plot.margin = margin(5.5, 60, 5.5, 5.5)  # ← RIGHT margin bigger
  )

p_topic_prev_uni

# -------------------------
# 11. Collocation analysis
# -------------------------

# Collocations by group
## I decided to skip analyzing collocations overall, since it wasn't informative
toks_ME <- toks[grp == "MiddleEastern"]
toks_GE <- toks[grp == "German"]

## For the Middle Eastern collocations I added extra filter to remove hidden characters because one broken form appeared in the results
coll_ME <- textstat_collocations(toks_ME, size = 2:3, min_count = 4) %>%
  mutate(
    collocation = stringi::stri_trans_nfc(collocation),
    collocation = stringr::str_replace_all(collocation, "[\u00AD\u2010\u2011\u2012\u2013\u2014]", "-"),
    collocation = stringr::str_squish(collocation),
    n_words = stringr::str_count(collocation, " ") + 1
  ) %>%
  filter(n_words %in% c(2, 3)) %>%
  filter(!stringr::str_detect(collocation, "er-?mitt-?le-?r\\s+in-?nen"))

coll_GE <- textstat_collocations(toks_GE, size = 2:3, min_count = 4) %>%
  mutate(n_words = stringr::str_count(collocation, " ") + 1) %>%
  filter(n_words %in% c(2, 3))

# Only strong collocations
filter_strong <- function(tbl, min_lambda = 5, min_z = 3) {
  tbl %>%
    mutate(n_words = stringr::str_count(collocation, " ") + 1) %>%
    filter(n_words %in% c(2, 3), lambda > min_lambda, z > min_z) %>%
    arrange(desc(lambda))
}

coll_ME_strong  <- filter_strong(coll_ME)
coll_GE_strong  <- filter_strong(coll_GE)

# Top collocations for both groups plots

plot_top_coll_apa <- function(tbl, n = 20) { 
  tbl %>% 
    mutate(n_words = stringr::str_count(collocation, " ") + 1) %>% 
    filter(n_words %in% c(2, 3)) %>% 
    arrange(desc(lambda)) %>% 
    slice_head(n = n) %>% 
    mutate(collocation = wrap_term(collocation, 22)) %>% 
    ggplot(aes(x = lambda, y = forcats::fct_reorder(collocation, lambda))) +
    geom_col(fill = "grey45", width = 0.7) +
    labs(
      x = "Association strength (lambda)", 
      y = "Collocation"
    )
}

p_coll_me_apa  <- plot_top_coll_apa(coll_ME_strong, 20)
p_coll_ge_apa  <- plot_top_coll_apa(coll_GE_strong, 20)

p_coll_me_apa
p_coll_ge_apa

# -----------------------------
# 12. Named entity recognition
# -----------------------------

# Load German model
m  <- udpipe_download_model("german")
ud <- udpipe_load_model(m$file_model)

# Annotate texts
anno <- udpipe_annotate(
  ud,
  x      = df_all$full_text,
  doc_id = seq_len(nrow(df_all))
) |>
  as.data.frame()

# Nationality adjectives to track
nat_terms <- c("deutsch", "pakistanisch", "afghanisch", "irakisch", "kurdisch")

# Count by group
nat_counts <- anno |>
  filter(lemma %in% nat_terms) |>
  mutate(group = df_all$group[as.integer(doc_id)]) |>
  count(group, lemma, sort = TRUE)

nat_counts

# Nationality adjectives by group plot
p_nat_apa <- nat_counts %>%
  mutate(lemma = stringr::str_to_title(lemma)) %>%
  ggplot(aes(x = lemma, y = n, fill = group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = apa_fills) +
  labs(
    x = "Nationality adjective",
    y = "Count",
    fill = "Group"
  )

p_nat_apa

# ----------------
# 13. GloVe model
# ----------------

# Train GloVe
train_glove <- function(toks_group,
                        min_termfreq = 5,
                        min_docfreq  = 2,
                        window       = 5,
                        rank         = 50,
                        n_iter       = 20,
                        x_max        = 10,
                        seed         = 1234) {
  # Build and trim dfm
  dfm_g <- dfm(toks_group)
  dfm_g <- dfm_subset(dfm_g, rowSums(dfm_g) > 0)
  dfm_g <- dfm_trim(
    dfm_g,
    min_termfreq = min_termfreq,
    min_docfreq  = min_docfreq
  )
  dfm_g <- dfm_subset(dfm_g, rowSums(dfm_g) > 0)
  
  feats <- featnames(dfm_g)
  
  # Keep only retained features in tokens
  toks_g <- tokens_select(toks_group, pattern = feats, padding = FALSE)
  
  # Feature co-occurrence matrix
  fcm_g <- fcm(
    toks_g,
    context = "window",
    window  = window,
    count   = "weighted",
    weights = 1 / (1:window),
    tri     = TRUE
  )
  
  # Fit GloVe
  set.seed(seed)
  glove <- GlobalVectors$new(rank = rank, x_max = x_max)
  
  main <- glove$fit_transform(
    fcm_g,
    n_iter = n_iter,
    convergence_tol = 0.01
  )
  
  context <- glove$components
  word_vectors <- main + t(context)
  
  # Normalize to unit length for cosine similarity
  
  row_norms <- sqrt(rowSums(word_vectors^2))
  word_vectors <- word_vectors[row_norms > 0, , drop = FALSE]
  row_norms <- row_norms[row_norms > 0]
  word_vectors <- word_vectors / row_norms
  
  word_vectors
}

# Train embeddings
wordvec_ME <- train_glove(toks_ME, rank = 50, n_iter = 20, seed = 1234)
wordvec_GE <- train_glove(toks_GE, rank = 50, n_iter = 20, seed = 1234)

# Nearest neighbors as tidy tibble
nearest_words <- function(term, word_vectors, group_label, n = 10) {
  if (!term %in% rownames(word_vectors)) {
    message("Word not in vocabulary: ", term, " [", group_label, "]")
    return(NULL)
  }
  
  v <- word_vectors[term, , drop = FALSE]
  sim <- as.numeric(word_vectors %*% t(v))
  names(sim) <- rownames(word_vectors)
  
  # Remove the query word itself
  sim <- sim[names(sim) != term]
  out <- sort(sim, decreasing = TRUE)[1:n]
  
  tibble(
    group  = group_label,
    target = term,
    word   = names(out),
    cosine = as.numeric(out),
    rank   = seq_along(out)
  )
}

# Seed words
seed_terms <- c("mord", "familie", "ehre", "femizid")

# Show the neighboring words
glove_results <- bind_rows(
  lapply(seed_terms, nearest_words, word_vectors = wordvec_ME, group_label = "MiddleEastern", n = 10),
  lapply(seed_terms, nearest_words, word_vectors = wordvec_GE, group_label = "German", n = 10)
)

glove_results

# GloVe plot for the Middle Eastern group
p_glove_ME <- glove_results %>%
  group_by(group, target) %>%
  slice_max(cosine, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  filter(group == "MiddleEastern") %>%
  mutate(
    word = stringr::str_wrap(word, width = 14),
    target = factor(target, levels = c("familie", "mord", "ehre", "femizid"))
  ) %>%
  ggplot(aes(
    x = cosine,
    y = tidytext::reorder_within(word, cosine, target)
  )) +
  geom_col(fill = "grey50", width = 0.65) +
  tidytext::scale_y_reordered() +
  facet_wrap(~ target, scales = "free_y", ncol = 2) +
  labs(
    x = "Cosine similarity",
    y = "Nearest semantic neighbor"
  ) +
  theme(
    axis.text.y = element_text(size = 9)
  )

p_glove_ME

# GloVe plot for the German group
p_glove_GE <- glove_results %>%
  group_by(group, target) %>%
  slice_max(cosine, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  filter(group == "German") %>%
  mutate(
    word = stringr::str_wrap(word, width = 14),
    target = factor(target, levels = c("familie", "mord", "ehre", "femizid"))
  ) %>%
  ggplot(aes(
    x = cosine,
    y = tidytext::reorder_within(word, cosine, target)
  )) +
  geom_col(fill = "grey35", width = 0.65) +
  tidytext::scale_y_reordered() +
  facet_wrap(~ target, scales = "free_y", ncol = 2) +
  labs(
    x = "Cosine similarity",
    y = "Nearest semantic neighbor"
  ) +
  theme(
    axis.text.y = element_text(size = 9)
  )
    
p_glove_GE

install.packages("usethis")
library(usethis)
usethis::use_git_ignore(c(".RDataTmp", "Rplot.png", "*.udpipe"))
usethis::use_git()
usethis::use_git_remote(
  name = "origin",
  url = "https://github.com/marinapoz/Master-s-thesis.git"
)
