library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(zoo)

# ---- Load data ----
articles_ssh <- read_csv(
  "articles-ssh.csv",
  col_types = cols(.default = col_character())
)

# ---- Extract year ----
articles_ssh <- articles_ssh %>%
  mutate(year = str_extract(Date, "^\\d{4}")) %>%
  filter(!is.na(year))

# ---- Aggregate per year ----
pub_year <- articles_ssh %>%
  count(year) %>%
  mutate(year = as.integer(year)) %>%
  arrange(year) %>%
  complete(year = full_seq(year, 1), fill = list(n = 0)) %>%
  filter(year <= 2025)

# ---- Rolling average (5-year window, centered) ----
pub_year <- pub_year %>%
  mutate(
    n_roll5 = rollmean(n, k = 5, fill = NA, align = "center")
  )

# ---- Plot ----
plot <- ggplot(pub_year, aes(x = year, y = n)) +
  geom_col(fill = "#7A8DA6") +
  geom_line(aes(y = n_roll5), linewidth = 1, color = "#2F3E4E") +
  geom_text(
    aes(label = n),
    vjust = -0.3,
    size = 3
  ) +
  scale_x_continuous(
    breaks = seq(
      from = min(pub_year$year, na.rm = TRUE),
      to   = max(pub_year$year, na.rm = TRUE),
      by   = 5
    )
  ) +
  labs(
    x = "Year",
    y = "Number of publications",
    title = "Total Number of Scholarly Publications (2000-'25)"
  ) +
  theme_minimal()

plot
