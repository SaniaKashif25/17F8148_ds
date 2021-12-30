#import Libraries
library(tidyverse)
library(naniar)
library(visdat)


#import Data
Billionaires <- read.csv("forbes_billionaires.csv", header = TRUE, na.strings = c("NA","N/A",""))
head(Billionaires)


#summary
glimpse(Billionaires)


#full duplicates
sum(duplicated(Billionaires))


#partial duplicates
Billionaires %>%
  count(Name) %>%
  filter (n > 1)


#missing value(1)
sum(is.na(Billionaires))


#missing value(2)
miss_var_summary(Billionaires)


#missing value summary
Billionaires %>%
  mutate(miss_education = is.na(Education)) %>%
  group_by(miss_education) %>%
  select(-Name, -Country, -Source, -Residence, -Citizenship, -Status, -Education, -Self_made) %>%
  summarize_all(mean, na.rm = TRUE)


#missing Value Visualization
Billionaires %>%
  arrange(NetWorth) %>%
  vis_miss()


#partial duplicates and missing value treatment
Billionaires_cleaned <- na.omit(Billionaires) %>%
  distinct(Name, .keep_all = TRUE)

sum(is.na(Billionaires_cleaned))

Billionaires_cleaned %>%
  count(Name) %>%
  filter (n > 1)


#netWorth histogram
Billionaires_cleaned %>%
  ggplot(aes(x = NetWorth)) + geom_histogram(binwidth = 30, color = "red", fill = "white") + labs(title = "Net Worth")


#NetWorth Summary
summary(Billionaires_cleaned$NetWorth)


#Citizenship Summary
Billionaires_cleaned %>%
  count(Citizenship) %>%
  arrange(desc(n)) %>%
  mutate(Percentage = n / sum(n) * 100)


#Education Summary
Billionaires_cleaned %>%
  count(Education) %>%
  arrange(desc(n)) %>%
  mutate(Percentage = n / sum(n) * 100)


#Status Summary
Billionaires_cleaned %>%
  count(Status) %>%
  arrange(desc(n)) %>%
  mutate(Percentage = n / sum(n) * 100)


#Children Summary
Billionaires_cleaned %>%
  count(Children) %>%
  arrange(desc(n)) %>%
  mutate(Percentage = n / sum(n) * 100)


#Variables Correlation
cor(Billionaires_cleaned[, c("NetWorth", "Age", "Children")])


#Boxplot Visualization
NetWorthLabel = c("1 - 88.5", "88.6 - 177")
Billionaires_cleaned$NetWorth_Group = cut(Billionaires_cleaned$NetWorth, breaks = c(1, 88.5, Inf), labels = NetWorthLabel, right = FALSE)
ggplot(data = Billionaires_cleaned, mapping = aes(x = NetWorth_Group, y = Age)) + geom_boxplot(alpha = 0, color = "red", fill = "white") + geom_jitter(color = "blue", alpha = 0.5) + labs(title = "Forbes Billionaires 2021 by Age")

