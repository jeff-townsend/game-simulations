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
  group_by(date, question) %>%
  mutate(placement = rank(desc(points), ties.method = "average"),
         placement_points = rank(points, ties.method = "average"),
         participants = placement + placement_points - 1) %>%
  ungroup() %>%
  mutate(relative_points = points - question_points,
         relative_year_points = year_points - question_year_points,
         relative_location_points = location_points - question_location_points) %>%
  select(id, date, competitor, question, points,
         years_off, distance_away, year_points, location_points,
         placement, placement_points, participants,
         relative_points, relative_year_points, relative_location_points)

daily.performance <-
  results %>%
  filter(date == as.Date("2024-01-21")) %>%
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
  ggtitle("Today's Scoring Breakdown")

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
ggplot(overall.performance %>% filter(competitor != ""),
       aes(x = year_points, y = location_points)) +
  geom_point() +
  geom_text_repel(aes(label = competitor)) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dotted", color = "red2") +
  geom_textabline(label = "42,000", intercept = 8400, slope = -1,
                  linetype = "dashed", hjust = 0.5) +
  geom_textabline(label = "40,000", intercept = 8000, slope = -1,
                  linetype = "dashed") +
  geom_textabline(label = "38,000", intercept = 7600, slope = -1,
                  linetype = "dashed") +
  scale_x_continuous(limits = c(3500, 4500), name = "Year Points") +
  scale_y_continuous(limits = c(3500, 4500), name = "Location Points") +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        axis.title = element_text()) +
  ggtitle("Scoring Breakdown")

View(
  results %>%
    group_by(competitor) %>%
    summarize(location_4800 = mean(ifelse(location_points >= 4800, 1, 0)),
              location_4900 = mean(ifelse(location_points >= 4900, 1, 0)),
              location_5000 = mean(ifelse(location_points >= 4995, 1, 0)),
              year_within5 = mean(ifelse(years_off <= 5, 1, 0)),
              year_within2 = mean(ifelse(years_off <= 2, 1, 0)),
              year_exact = mean(ifelse(years_off == 0, 1, 0)),
              median_year = median(years_off),
              median_distance = median(distance_away),
              points_9000 = mean(ifelse(points >= 9000, 1, 0)),
              points_9500 = mean(ifelse(points >= 9500, 1, 0)),
              points_9800 = mean(ifelse(points >= 9800, 1, 0)),
              avg_points = mean(points),
              median_points = median(points),
              median_year_points = median(year_points),
              median_location_points = median(location_points))
)

ggplot(data = results %>% filter(competitor %in% c("Matt")),
       aes(x = location_points)) +
  geom_density(aes(fill = competitor),
                 alpha = 0.5) +
  theme_fivethirtyeight() +
  theme(legend.title = element_blank()) +
  ggtitle("Points Distribution")

View(results %>%
       filter(location_points >= 4700) %>%
       group_by(competitor) %>%
       summarize(questions = n(),
                 distance_away = mean(distance_away),
                 years_off = mean(years_off)))

# placement by question

placement <-
  results %>%
  group_by(competitor) %>%
  summarize(questions = n(),
            question_wins = sum(ifelse(placement <= 1.5, 1,0)),
            question_losses = sum(ifelse(placement_points <= 1.5, 1, 0)),
            placement_points = sum(placement_points),
            available_points = sum(participants)) %>%
  mutate(question_win_rate = question_wins / questions,
         placement_point_pct = placement_points / available_points)

# location point scoring

ggplot(results, aes(x = distance_away, y = location_points)) +
  geom_point()
