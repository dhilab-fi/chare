# =========================
# CARE CITATIONS PIPELINE
# Scopus (CSV) + WoS (Excel .xls/.xlsx) -> dedup by DOI -> plots
# =========================

library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(zoo)

# -------------------------
# Helpers
# -------------------------
norm_doi <- function(x) {
  x %>%
    tolower() %>%
    str_squish() %>%
    str_replace_all("^https?://(dx\\.)?doi\\.org/", "") %>%
    str_replace_all("^doi:\\s*", "") %>%
    na_if("") %>%
    na_if("na")
}

to_int <- function(x) suppressWarnings(as.integer(str_replace_all(as.character(x), "[^0-9]", "")))

current_year <- 2025

# -------------------------
# 1) Load Scopus (CSV)
# -------------------------
scopus_df <- read_csv("scopus.csv", col_types = cols(.default = col_character()))

scopus_min <- scopus_df %>%
  transmute(
    source = "scopus",
    doi = norm_doi(DOI),
    year = to_int(Year),
    cited_scopus = to_int(`Cited by`)
  ) %>%
  filter(!is.na(doi), !is.na(year))

# -------------------------
# 2) Load WoS (Excel)
# -------------------------
wos_df <- read_excel("wos.xls", col_types = "text")  # works also for .xlsx if you change filename

# If your WoS column names differ slightly, run: names(wos_df)
wos_min <- wos_df %>%
  transmute(
    source = "wos",
    doi = norm_doi(DOI),
    year = to_int(`Publication Year`),
    cited_wos_core = to_int(`Times Cited, WoS Core`),
    cited_wos_all  = to_int(`Times Cited, All Databases`)
  ) %>%
  filter(!is.na(doi), !is.na(year))

# -------------------------
# 3) Union + dedup (by DOI)
# -------------------------
union_df <- bind_rows(scopus_min, wos_min)

care_cit <- union_df %>%
  group_by(doi) %>%
  summarise(
    article_id = first(doi),
    year = first(na.omit(year)),
    cited_scopus   = if (all(is.na(cited_scopus)))   NA_integer_ else max(cited_scopus,   na.rm = TRUE),
    cited_wos_core = if (all(is.na(cited_wos_core))) NA_integer_ else max(cited_wos_core, na.rm = TRUE),
    cited_wos_all  = if (all(is.na(cited_wos_all)))  NA_integer_ else max(cited_wos_all,  na.rm = TRUE),
    citations_total = pmax(cited_scopus, cited_wos_core, cited_wos_all, na.rm = TRUE),
    sources = paste(sort(unique(source)), collapse = "+"),
    .groups = "drop"
  ) %>%
  mutate(
    citations_total = ifelse(is.infinite(citations_total), NA_integer_, citations_total)
  ) %>%
  select(article_id, year, citations_total, cited_scopus, cited_wos_core, cited_wos_all, sources)

# -------------------------
# 4) Basic filters + axis breaks
# -------------------------
care_cit2 <- care_cit %>%
  filter(!is.na(year), !is.na(citations_total), year <= current_year) %>%
  mutate(year = as.integer(year))

x_breaks <- seq(
  from = min(care_cit2$year, na.rm = TRUE),
  to   = max(care_cit2$year, na.rm = TRUE),
  by   = 5
)

# =========================
# P0) Total citation count per year (SUM)
# =========================
cit_year_sum <- care_cit2 %>%
  group_by(year) %>%
  summarise(
    n_papers = n(),
    total_citations = sum(citations_total, na.rm = TRUE),
    .groups = "drop"
  )

p0 <- ggplot(cit_year_sum, aes(x = year, y = total_citations)) +
  geom_col(fill = "#5B8DB8") +
  geom_line(aes(group = 1), linewidth = 1, color = "#1F3A5F") +
  geom_point(size = 2, color = "#1F3A5F") +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    x = "Year",
    y = "Total citation count (sum)",
    title = "Total citation count per year (≤ 2025)"
  ) +
  theme_minimal()

# =========================
# P1) Citation count per year (MEDIAN)
# =========================
cit_year_med <- care_cit2 %>%
  group_by(year) %>%
  summarise(
    n_papers = n(),
    median_citations = median(citations_total, na.rm = TRUE),
    .groups = "drop"
  )

p1 <- ggplot(cit_year_med, aes(x = year, y = median_citations)) +
  geom_col(fill = "#5B8DB8") +
  geom_line(aes(group = 1), linewidth = 1, color = "#1F3A5F") +
  geom_point(size = 2, color = "#1F3A5F") +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    x = "Year",
    y = "Median citation count",
    title = "Citation count per year (median, ≤ 2025)"
  ) +
  theme_minimal()

# =========================
# P2) Age-normalised (citations per year of age)
#     MEDIAN + IQR + 5-year rolling average
# =========================
care_cit2 <- care_cit2 %>%
  mutate(
    age_years = pmax(1L, current_year - year + 1L),
    cit_per_year = citations_total / age_years
  )

cit_year_age <- care_cit2 %>%
  group_by(year) %>%
  summarise(
    n_papers = n(),
    median_cit_per_year = median(cit_per_year, na.rm = TRUE),
    p25 = quantile(cit_per_year, 0.25, na.rm = TRUE),
    p75 = quantile(cit_per_year, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year) %>%
  mutate(
    roll5 = zoo::rollmean(median_cit_per_year, k = 5, fill = NA, align = "center")
  )

p2 <- ggplot(cit_year_age, aes(x = year)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#A7C4A0", alpha = 0.45) +
  geom_line(aes(y = median_cit_per_year), linewidth = 0.9, color = "#2F5D50") +
  geom_point(aes(y = median_cit_per_year), size = 2, color = "#2F5D50") +
  geom_line(aes(y = roll5), linewidth = 1.2, linetype = "dashed", color = "#1F2D3A") +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    x = "Year",
    y = "Citations per year (age-normalised, median)",
    title = "Age-normalised citation rate (median + IQR, 5-year rolling average)"
  ) +
  theme_minimal()

# =========================
# P2b) Age-normalised volume (SUM) + 5-year rolling average
# =========================
cit_year_age_sum <- care_cit2 %>%
  group_by(year) %>%
  summarise(
    n_papers = n(),
    total_cit_per_year = sum(cit_per_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year) %>%
  mutate(
    roll5 = zoo::rollmean(total_cit_per_year, k = 5, fill = NA, align = "center")
  )

p2b <- ggplot(cit_year_age_sum, aes(x = year)) +
  geom_col(aes(y = total_cit_per_year), fill = "#7A8DA6") +
  geom_line(aes(y = roll5), linewidth = 1, color = "#2F3E4E") +
  scale_x_continuous(
    breaks = seq(
      from = min(pub_year$year, na.rm = TRUE),
      to   = max(pub_year$year, na.rm = TRUE),
      by   = 5
    )
  ) +
  labs(
    x = "Year",
    y = "Total citations per year (age-normalised)",
    title = "Age-normalised Total Citation Rate (2020-'25)"
  ) +
  theme_minimal()

# =========================
# P3) Distribution of citations (log10)
# =========================
p3 <- ggplot(care_cit2, aes(x = citations_total)) +
  geom_histogram(bins = 40, fill = "#E3B23C", color = "white") +
  scale_x_log10() +
  labs(
    x = "Total citations (log scale)",
    y = "Number of papers",
    title = "Distribution of citation counts (log scale)"
  ) +
  theme_minimal()

# -------------------------
# Print plots
# -------------------------
p0
p1
p2
p2b
p3
