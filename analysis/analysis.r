if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

## SUMMARIZE THE RESULTS ##

# 1. distribution of plan counts by county over time
## group by year and county, agg by count
final.data = subset(final.data, snp != "Yes" & eghp != "Yes")

plan_count = final.data %>%
  group_by(county, year) %>%
  summarize(plan_count = n())

ggplot(plan_count, aes(x = as.factor(year), y = plan_count)) +
  geom_boxplot() +
  labs(title = "Distribution of Plan Counts by County Over Time",
       x = "Year",
       y = "Number of Plans") +
  theme_minimal() 
ggsave("plan_distribution_1.png")

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
ggsave("avg_payment.png")

# 4. average share of Medicare Advantage (relative to all Medicare eligibles) over time from 2010 through 2015.
ma_share = final.data %>% 
  group_by(year)%>%
  filter(year >= 2010 & year <= 2015) %>%
  summarize(ma_share = sum(avg_enrolled, na.rm= TRUE)/ sum(avg_eligibles, na.rm = TRUE))

  ggplot(ma_share, aes(x= year, y = ma_share ))+
  geom_line() +
  labs(title = "Average Share of Medicare Advantage")
ggsave("ma_share.png")


## Estimate ATEs ##

# 5. Running variable
final.data$rounded_star = round(final.data$Star_Rating * 2) / 2
star_counts = table(final.data$rounded_star)
print(star_counts)

# 6. Estimate of 3-star, 2.5-star, and 3.5-star ratings on enrollment
summary(lm(avg_enrollment~factor(Star_Rating), data=final.data))



