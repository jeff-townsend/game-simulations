library(tidyverse)
library(ggrepel)
library(ggthemes)
library(geomtextpath)

library(readxl)
results.import <- read_excel("Tracking/Timeguessr Data.xlsx",
                             sheet = "Results")
questions.import <-
  read_excel("Tracking/Timeguessr Data.xlsx",
             sheet = "Questions") %>%
  filter(!is.na(id))

questions <-
  questions.import %>%
  inner_join(results.import %>% select(-id), by = c("date", "question")) %>%
  group_by(id, date, question, country, city, description) %>%
  summarize(question_points = mean(points),
            question_year_points = mean(year_points),
            question_location_points = mean(location_points),
            question_years_off = mean(years_off),
            question_distance_away = mean(distance_away)) %>%
  ungroup()

results <-
  results.import %>%
  inner_join(questions %>% select(-id), by = c("date", "question")) %>%
  mutate(relative_points = points - question_points,
         relative_year_points = year_points - question_year_points,
         relative_location_points = location_points - question_location_points) %>%
  select(id, date, competitor, question, points,
         years_off, distance_away, year_points, location_points,
         relative_points, relative_year_points, relative_location_points)

daily.performance <-
  results %>%
  filter(date == as.Date("2024-01-20")) %>%
  group_by(competitor) %>%
  summarize(points = sum(points),
            points_per_question = mean(points),
            year_points = mean(year_points),
            location_points = mean(location_points),
            years_off = mean(years_off),
            distance_away = mean(distance_away))

# Today's Results
ggplot(daily.performance, aes(x = year_points, y = location_points)) +
  geom_point() +
  geom_text_repel(aes(label = competitor)) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dotted", color = "red2") +
  geom_textabline(label = "46,000", intercept = 9200, slope = -1,
                  linetype = "dashed", hjust = 0.45) +
  geom_textabline(label = "42,000", intercept = 8400, slope = -1,
                  linetype = "dashed", hjust = 0.5) +
  geom_textabline(label = "38,000", intercept = 7600, slope = -1,
                  linetype = "dashed") +
  geom_textabline(label = "34,000", intercept = 6800, slope = -1,
                  linetype = "dashed") +
  geom_textabline(label = "30,000", intercept = 6000, slope = -1,
                  linetype = "dashed") +
  scale_x_continuous(limits = c(2500, 5000), name = "Year Points") +
  scale_y_continuous(limits = c(2500, 5000), name = "Location Points") +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        axis.title = element_text()) +
  ggtitle("1/20 Scoring Breakdown")

question.average <- mean(questions$question_points)
overall.performance <-
  results %>%
  group_by(competitor) %>%
  summarize(points_per_day = mean(points)*5,
            points_per_question = mean(points),
            year_points = mean(year_points),
            location_points = mean(location_points),
            years_off = mean(years_off),
            distance_away = mean(distance_away),
            relative_points = (mean(relative_points)+question.average)*5)

# Performance
ggplot(overall.performance, aes(x = year_points, y = location_points)) +
  geom_point() +
  geom_text_repel(aes(label = competitor)) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dotted", color = "red2") +
  geom_textabline(label = "46,000", intercept = 9200, slope = -1,
                  linetype = "dashed", hjust = 0.45) +
  geom_textabline(label = "42,000", intercept = 8400, slope = -1,
                  linetype = "dashed", hjust = 0.5) +
  geom_textabline(label = "38,000", intercept = 7600, slope = -1,
                  linetype = "dashed") +
  geom_textabline(label = "34,000", intercept = 6800, slope = -1,
                  linetype = "dashed") +
  scale_x_continuous(limits = c(3000, 5000), name = "Year Points") +
  scale_y_continuous(limits = c(3000, 5000), name = "Location Points") +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        axis.title = element_text()) +
  ggtitle("Overall Performance Breakdown")
