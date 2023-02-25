library("dplyr")
library("stringr")
library("ggplot2")
library("tidyverse")
library("plotly")

spl_df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_df <- spl_df %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

ave_checkout_title <- spl_df %>%
  filter(MaterialType %in% c("BOOK", "EBOOK", "VIDEO", "AUDIOBOOK", "SOUNDDISC", "VIDEODISC")) %>%
  group_by(date, MaterialType) %>%
  summarize(average_checkout = mean(Checkouts, na.rm = T))

ave_checkoutttt <- ggplot(data = ave_checkout_title) +
  geom_line(mapping = aes(
    x = date, y = average_checkout,
    color = MaterialType,
    text = paste("average checkout")
  )) +
  labs(
    title = "Books Average Checkouts Over Time - BOOKS, EBOOKS, VIDEO, AUDIOBOOK, SOUNDDISC, VIDEODISC",
    x = "Date",
    y = "Checkouts"
  )

ggplotly(ave_checkoutttt, tooltip = c("text"))
