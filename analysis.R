# Full analysis pipeline in R - replace 'your_dataset.csv' with your actual filename
# Requirements: tidyverse, lubridate, janitor, skimr, readr, ggplot2, scales, corrplot, rmarkdown
# Install packages if needed:
# install.packages(c("tidyverse","lubridate","janitor","skimr","corrplot","scales","readr","ggplot2","rmarkdown"))

library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(corrplot)
library(scales)

#---- Set working directory to the folder containing your dataset ----
# setwd("/path/to/your/folder")
# Replace the filename below with the exact name you uploaded to Kaggle
input_file <- "your_dataset.csv"

#---- Read data ----
df_raw <- read_csv(input_file, guess_max = 10000)
glimpse(df_raw)
write_csv(df_raw, "raw_preview.csv")

#---- Data cleaning helpers ----
clean_data <- function(df) {
  df <- df %>%
    clean_names() %>%          # make column names consistent
    mutate(across(where(is.character), ~na_if(.,""))) %>%
    mutate(across(where(is.character), str_squish)) %>%
    distinct()                 # drop exact duplicates
  return(df)
}

df <- clean_data(df_raw)

#---- Initial summary ----
skim(df) %>% print()
summary(df) %>% print()
write_csv(df, "cleaned_data.csv")

#---- Date parsing (common) ----
# If there's a date column named 'date' or similar, parse it:
date_cols <- names(df)[str_detect(names(df), "date|time|day", ignore_case = TRUE)]
if (length(date_cols) > 0) {
  for (c in date_cols) {
    newname <- paste0(c, "_dt")
    df <- df %>% mutate(!!newname := parse_date_time(.data[[c]], orders = c("ymd","dmy","mdy","Ymd HMS","dmY HMS"), tz = "UTC"))
  }
}

#---- Example EDA & visualizations ----
# Adjust column names below to match your dataset.
# 1) Distribution of a numeric variable
num_cols <- df %>% select(where(is.numeric)) %>% names()
if (length(num_cols) > 0) {
  p1 <- ggplot(df, aes(x = .data[[num_cols[1]]])) +
    geom_histogram(bins = 30) +
    labs(title = paste("Distribution of", num_cols[1]), x = num_cols[1], y = "Count") +
    theme_minimal()
  ggsave("plot_distribution.png", p1, width=8, height=5)
}

# 2) Time series (if a date column exists)
if (exists("date_cols") && length(date_cols) > 0) {
  # pick the first parsed date column that ended with _dt
  parsed_dates <- names(df)[str_detect(names(df), "_dt$")]
  if (length(parsed_dates) > 0 && length(num_cols) > 0) {
    ts <- df %>%
      mutate(date = as_date(.data[[parsed_dates[1]]])) %>%
      group_by(date) %>%
      summarise(metric = sum(.data[[num_cols[1]]], na.rm = TRUE)) %>%
      arrange(date)
    p2 <- ggplot(ts, aes(date, metric)) + geom_line() + labs(title = "Time series of metric", x = "Date", y = "Metric") + theme_minimal()
    ggsave("plot_timeseries.png", p2, width=10, height=4)
  }
}

# 3) Top categories bar chart
cat_cols <- df %>% select(where(is.character)) %>% names()
if (length(cat_cols) > 0) {
  top_col <- cat_cols[1]
  cat_summary <- df %>% count(.data[[top_col]], sort = TRUE) %>% slice_head(n=20)
  p3 <- ggplot(cat_summary, aes(reorder(.data[[top_col]], n), n)) + geom_col() + coord_flip() +
    labs(title = paste("Top values of", top_col), x = top_col, y = "Count") + theme_minimal()
  ggsave("plot_top_categories.png", p3, width=8, height=6)
  write_csv(cat_summary, "top_categories.csv")
}

#---- Correlation matrix for numeric features ----
if (length(num_cols) >= 2) {
  cor_mat <- df %>% select(all_of(num_cols)) %>% cor(use = "pairwise.complete.obs")
  png("correlation_matrix.png", width = 800, height = 800)
  corrplot(cor_mat, method = "color", tl.cex = 0.7)
  dev.off()
}

#---- Export cleaned data and checkpoints ----
write_csv(df, "cleaned_data_final.csv")

#---- Optional: produce RMarkdown report (requires rmarkdown) ----
# You can render an HTML report from an Rmd file if you create one.
# rmarkdown::render("analysis_report.Rmd", output_file = "analysis_report.html")

#---- Save workspace ----
save.image("analysis_workspace.RData")
cat("Done. Files produced: cleaned_data_final.csv, plot_*.png, top_categories.csv, analysis_workspace.RData\n")
