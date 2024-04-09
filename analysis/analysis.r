if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, knittr)

## SUMMARIZE THE RESULTS ##

# 1. distribution of plan counts by county over time
final.data = subset(final.data, snp != "Yes" & eghp != "Yes")

ggplot(final.data, aes(x = year, fill = plan_type)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Plan Counts by County Over Time",
       x = "Year",
       y = "Number of Plans") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plan_distribution.png")

# 2. bar graphs showing the distribution of star ratings in 2010, 2012, and 2015
stars = subset(star.ratings, year %in% c(2010, 2012, 2015))

ggplot(stars, aes(x = as.factor(overallrating_plan))) +
  geom_bar() +
  facet_wrap(~ year, scales = "free_x") +
  labs(title = "Distribution of Star Ratings in 2010, 2012, and 2015",
       x = "Star Rating",
       y = "Count") +
  theme_minimal()
ggsave("star_ratings.png")

# 3. average benchmark payment over time from 2010 through 2015
payment_years = subset(final.data, year >= 2010 & year <= 2015)

avg_payment = aggregate(avg_ffscost ~ year, data = payment_years, FUN = mean)

ggplot(avg_payment, aes(x = year, y = avg_ffscost)) +
  geom_line() +
  labs(title = "Average Benchmark Payment Over Time (2010-2015)",
       x = "Year",
       y = "Average Benchmark Payment") +
  theme_minimal()
ggsave("avg_payment")

# 4. average share of Medicare Advantage (relative to all Medicare eligibles) over time from 2010 through 2015.
# which plans do and do not fall under MA? 

## Estimate ATEs ##
# 5. Running variable
summary(lm(avg_enrollment~factor(Star_Rating), data=final.data))



