library("dplyr")
library("stringr")
library("ggplot2")
library("tidyverse")

spl_df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_df <- spl_df %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

books <- spl_df %>%
  filter(Title %in% c('"A" is for alibi / Sue Grafton.', "Headphones / Seattle Public Library."))

ggplot(data = books) +
  geom_line(mapping = aes(x = date, y = Checkouts, color = Title)) +
  labs(title = "Books Max And Min Checkouts Over Time", x = "Date", y = "Checkouts")
