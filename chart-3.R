library("dplyr")
library("stringr")
library("ggplot2")
library("tidyverse")
library("scales")

spl_df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_df <- spl_df %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

spl_extend <- spl_df %>%
  group_by(date) %>%
  summarize(checkoutss = sum(Checkouts, na.rm = T))

ggplot(data = spl_extend) +
  geom_line(mapping = aes(x = date, y = checkoutss, color = date)) +
  labs(title = "Books Checkouts Over Time", x = "Date", y = "Checkouts") +
  scale_y_continuous(labels = label_number_si())
