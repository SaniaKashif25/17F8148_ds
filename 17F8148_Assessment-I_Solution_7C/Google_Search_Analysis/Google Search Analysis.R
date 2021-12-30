library(gtrendsR)
library(tidyverse)
res <- gtrends(c("Apple", "Microsoft"))

iot <- res$interest_over_time

iot2020 <- iot %>% 
  filter(date > as.Date("2020-01-01"))

iot2020 %>% 
  ggplot() + geom_line(aes(x = date,
                           y = hits,
                           color = keyword)) +
  theme_minimal() +
  labs(title = "Apple vs Microsoft - in 2020",
       subtitle = "Google Trends Report",
       caption = "Courtesy: gtrendsR package")
library(gtrendsR)
library(dplyr)
library(ggplot2)

res <- gtrends(c("Micro", "Soft"))

iot <- res$interest_over_time

iot2020 <- iot %>% 
  filter(date > as.Date("2020-01-01"))
## Warning in mask$eval_all_filter(dots, env_filter): Incompatible methods
## ("Ops.POSIXt", "Ops.Date") for ">"
iot2020 %>% 
  ggplot() + geom_line(aes(x = date,
                           y = hits,
                           color = keyword)) +
  theme_minimal() +
  labs(title = "Micro VS Soft - in 2020",
       subtitle = "Google Trends Report",
       caption = "Courtesy: gtrendsR package")