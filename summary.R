library("dplyr")
library("stringr")
library("ggplot2")
library("tidyverse")

# Exercise 1: Load the data
# Download and unzip one or more of the SPL datasets and load here from a file path

spl_df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

#- What is the average number of checkouts for each item?
# show calculation content?
# 3.335852

ave_checkout <- spl_df %>%
  summarize(average_checkout = mean(Checkouts, na.rm = T))
paste("The average checkout is", ave_checkout)


#- what is the one that has most average checkouts? And how many 
# average monthly checkouts did it have?
# Headphones / Seattle Public library, 1627.308 checkouts per month.

ave_checkout_for_each_book <- spl_df %>%
  group_by(Title) %>%
  summarize(average_checkout = mean(Checkouts, na.rm = T))

most_checkout <- ave_checkout_for_each_book %>%
  filter(average_checkout == max(average_checkout, na.rm = T)) %>%
  pull(Title)

paste(most_checkout)

#- what is one of the items that has least average checkouts?

least_checkout <- ave_checkout_for_each_book %>%
  filter(average_checkout == min(average_checkout, na.rm = T))

print('"A" is for alibi / Sue Grafton.')


#- What is the month or year with the most checkouts for book 'Headphones / Seattle Public Library.'?
# It is 2023-01-01
spl_df <- spl_df %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

checkout_of_headphone <- spl_df %>%
  filter(str_detect(Title, "Headphones / Seattle Public Library."))

most_checkout_date <- checkout_of_headphone %>%
  filter(Checkouts == max(Checkouts, na.rm = T)) %>%
  pull(date)
paste(most_checkout_date)


#- What is the total checkouts for the book 'Headphones / Seattle Public Library.'?
# It's 21155 checkouts.
total_book_checkout <- spl_df %>%
  filter(str_detect(Title, "Headphones / Seattle Public Library.")) %>%
  summarize(Checkouts = sum(Checkouts, na.rm = T)) %>%
  pull(Checkouts)
paste(total_book_checkout)

#- What is the each-month checkout for 'Headphones / Seattle Public Library.' and '"A" is for alibi / Sue Grafton.'?


checkouts_per_month <- checkout_of_headphone %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))


checkout_of_a_is_for <- spl_df %>%
  filter(str_detect(Title, '"A" is for alibi / Sue Grafton.'))

checkouts_per_month1 <- checkout_of_a_is_for %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))
