library(readxl)
library(tidyverse)

wordle.import <- read_excel("Data Analysis/wordle words.xlsx",
                            col_names = FALSE)
colnames(wordle.import) <- c("word")

wordle.list <-
  wordle.import %>%
  mutate(word = tolower(word),
         first_letter = substr(word, 1, 1),
         second_letter = substr(word, 2, 2),
         third_letter = substr(word, 3, 3),
         fourth_letter = substr(word, 4, 4),
         fifth_letter = substr(word, 5, 5))

letters.table <- data.frame(letter = c("q",
                                       "w",
                                       "e",
                                       "r",
                                       "t",
                                       "y",
                                       "u",
                                       "i",
                                       "o",
                                       "p",
                                       "a",
                                       "s",
                                       "d",
                                       "f",
                                       "g",
                                       "h",
                                       "j",
                                       "k",
                                       "l",
                                       "z",
                                       "x",
                                       "c",
                                       "v",
                                       "b",
                                       "n",
                                       "m"))

start.time <- Sys.time()

simulations <- 10000
simulation.ids <- data.frame(simulation_id = c(1:simulations))

letter.eligibility <-
  merge(letters.table, simulation.ids) %>%
  mutate(first_eligible = 1,
         second_eligible = 1,
         third_eligible = 1,
         fourth_eligible = 1,
         fifth_eligible = 1,
         is_yellow = 0)

## pick the words
correct.words <-
  sample_n(wordle.list, simulations, replace = TRUE) %>%
  mutate(simulation_id = c(1:simulations))

#### begin the game
## make first guess
first.guesses <-
  sample_n(wordle.list, simulations, replace = TRUE) %>%
  rename(word_guess = word,
         first_guess = first_letter,
         second_guess = second_letter,
         third_guess = third_letter,
         fourth_guess = fourth_letter,
         fifth_guess = fifth_letter) %>%
  mutate(simulation_id = c(1:simulations))

first.merged <-
  first.guesses %>%
  inner_join(correct.words, by = "simulation_id") %>%
  mutate(first_green = ifelse(first_guess == first_letter, 1, 0),
         second_green = ifelse(second_guess == second_letter, 1, 0),
         third_green = ifelse(third_guess == third_letter, 1, 0),
         fourth_green = ifelse(fourth_guess == fourth_letter, 1, 0),
         fifth_green = ifelse(fifth_guess == fifth_letter, 1, 0),
         first_yellow = ifelse(first_guess %in% c(second_letter, third_letter, fourth_letter, fifth_letter), 1, 0),
         second_yellow = ifelse(second_guess %in% c(first_letter, third_letter, fourth_letter, fifth_letter), 1, 0),
         third_yellow = ifelse(third_guess %in% c(second_letter, first_letter, fourth_letter, fifth_letter), 1, 0),
         fourth_yellow = ifelse(fourth_guess %in% c(second_letter, third_letter, first_letter, fifth_letter), 1, 0),
         fifth_yellow = ifelse(fifth_guess %in% c(second_letter, third_letter, fourth_letter, first_letter), 1, 0),
         first_grey = 1 - first_green - first_yellow,
         second_grey = 1 - second_green - second_yellow,
         third_grey = 1 - third_green - third_yellow,
         fourth_grey = 1 - fourth_green - fourth_yellow,
         fifth_grey = 1 - fifth_green - fifth_yellow,
         first_feedback = ifelse(first_green == 1, "Green", ifelse(first_yellow == 1, "Yellow", "Grey")),
         second_feedback = ifelse(second_green == 1, "Green", ifelse(second_yellow == 1, "Yellow", "Grey")),
         third_feedback = ifelse(third_green == 1, "Green", ifelse(third_yellow == 1, "Yellow", "Grey")),
         fourth_feedback = ifelse(fourth_green == 1, "Green", ifelse(fourth_yellow == 1, "Yellow", "Grey")),
         fifth_feedback = ifelse(fifth_green == 1, "Green", ifelse(fifth_yellow == 1, "Yellow", "Grey")))

first.relevant <-
  first.merged %>%
  select(simulation_id,
         first_guess, first_feedback,
         second_guess, second_feedback,
         third_guess, third_feedback,
         fourth_guess, fourth_feedback,
         fifth_guess, fifth_feedback)

first.feedback <-
  letter.eligibility %>%
  inner_join(first.relevant, by = "simulation_id") %>%
  ## update Grey feedback; if the feedback is Grey, then set every eligibility column to 0
  mutate(first_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, fifth_eligible)) %>%
  ## update Yellow feedback; if the feedback is Yellow, then set just that column to 0
  mutate(first_eligible = ifelse(letter == first_guess & first_feedback == "Yellow", 0, first_eligible),
         second_eligible = ifelse(letter == second_guess & second_feedback == "Yellow", 0, second_eligible),
         third_eligible = ifelse(letter == third_guess & third_feedback == "Yellow", 0, third_eligible),
         fourth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Yellow", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Yellow", 0, fifth_eligible)) %>%
  mutate(is_yellow = ifelse(letter == first_guess & first_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == second_guess & second_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == third_guess & third_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == fourth_guess & fourth_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == fifth_guess & fifth_feedback == "Yellow", 1, is_yellow)) %>%
  ## update Green feedback; if the feedback is Green and the letter is not the guess, set that column to 0
  mutate(first_eligible = ifelse(letter != first_guess & first_feedback == "Green", 0, first_eligible),
         second_eligible = ifelse(letter != second_guess & second_feedback == "Green", 0, second_eligible),
         third_eligible = ifelse(letter != third_guess & third_feedback == "Green", 0, third_eligible),
         fourth_eligible = ifelse(letter != fourth_guess & fourth_feedback == "Green", 0, fourth_eligible),
         fifth_eligible = ifelse(letter != fifth_guess & fifth_feedback == "Green", 0, fifth_eligible))

letter.eligibility <-
  first.feedback %>%
  select(simulation_id, letter, first_eligible, second_eligible, third_eligible, fourth_eligible, fifth_eligible, is_yellow)

## make second guess
second.possible <-
  wordle.list %>%
  inner_join(letter.eligibility %>% select(letter, first_eligible, simulation_id), by = c("first_letter" = "letter")) %>%
  inner_join(letter.eligibility %>% select(letter, second_eligible, simulation_id), by = c("second_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, third_eligible, simulation_id), by = c("third_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, fourth_eligible, simulation_id), by = c("fourth_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, fifth_eligible, simulation_id), by = c("fifth_letter" = "letter", "simulation_id")) %>%
  filter(first_eligible == 1,
         second_eligible == 1,
         third_eligible == 1,
         fourth_eligible == 1,
         fifth_eligible == 1) %>%
  distinct(simulation_id, word, first_letter, second_letter, third_letter, fourth_letter, fifth_letter) %>%
  rename(word_guess = word,
         first_guess = first_letter,
         second_guess = second_letter,
         third_guess = third_letter,
         fourth_guess = fourth_letter,
         fifth_guess = fifth_letter)

second.possible <-
  rbind(second.possible %>%
    left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("first_guess" = "letter", "simulation_id")) %>%
    mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, first_guess, NA)),
  second.possible %>%
    left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("second_guess" = "letter", "simulation_id")) %>%
    mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, second_guess, NA)),
  second.possible %>%
    left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("third_guess" = "letter", "simulation_id")) %>%
    mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, third_guess, NA)),
  second.possible %>%
    left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("fourth_guess" = "letter", "simulation_id")) %>%
    mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, fourth_guess, NA)),
  second.possible %>%
    left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("fifth_guess" = "letter", "simulation_id")) %>%
    mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, fifth_guess, NA))) %>%
  distinct(simulation_id, word_guess, first_guess, second_guess, third_guess, fourth_guess, fifth_guess, is_yellow) %>%
  mutate(has_yellow = ifelse(is.na(is_yellow), 0, 1)) %>%
  group_by(simulation_id, word_guess, first_guess, second_guess, third_guess, fourth_guess, fifth_guess) %>%
  summarize(yellows = sum(has_yellow)) %>%
  ungroup() %>%
  group_by(simulation_id) %>%
  mutate(max = max(yellows)) %>%
  filter(yellows == max) %>%
  select(-yellows, -max)

second.guesses <-
  second.possible %>%
  group_by(simulation_id) %>%
  slice_sample(n = 1)

second.merged <-
  second.guesses %>%
  inner_join(correct.words, by = "simulation_id") %>%
  mutate(first_green = ifelse(first_guess == first_letter, 1, 0),
         second_green = ifelse(second_guess == second_letter, 1, 0),
         third_green = ifelse(third_guess == third_letter, 1, 0),
         fourth_green = ifelse(fourth_guess == fourth_letter, 1, 0),
         fifth_green = ifelse(fifth_guess == fifth_letter, 1, 0),
         first_yellow = ifelse(first_guess %in% c(second_letter, third_letter, fourth_letter, fifth_letter), 1, 0),
         second_yellow = ifelse(second_guess %in% c(first_letter, third_letter, fourth_letter, fifth_letter), 1, 0),
         third_yellow = ifelse(third_guess %in% c(second_letter, first_letter, fourth_letter, fifth_letter), 1, 0),
         fourth_yellow = ifelse(fourth_guess %in% c(second_letter, third_letter, first_letter, fifth_letter), 1, 0),
         fifth_yellow = ifelse(fifth_guess %in% c(second_letter, third_letter, fourth_letter, first_letter), 1, 0),
         first_grey = 1 - first_green - first_yellow,
         second_grey = 1 - second_green - second_yellow,
         third_grey = 1 - third_green - third_yellow,
         fourth_grey = 1 - fourth_green - fourth_yellow,
         fifth_grey = 1 - fifth_green - fifth_yellow,
         first_feedback = ifelse(first_green == 1, "Green", ifelse(first_yellow == 1, "Yellow", "Grey")),
         second_feedback = ifelse(second_green == 1, "Green", ifelse(second_yellow == 1, "Yellow", "Grey")),
         third_feedback = ifelse(third_green == 1, "Green", ifelse(third_yellow == 1, "Yellow", "Grey")),
         fourth_feedback = ifelse(fourth_green == 1, "Green", ifelse(fourth_yellow == 1, "Yellow", "Grey")),
         fifth_feedback = ifelse(fifth_green == 1, "Green", ifelse(fifth_yellow == 1, "Yellow", "Grey")))

second.relevant <-
  second.merged %>%
  select(simulation_id,
         first_guess, first_feedback,
         second_guess, second_feedback,
         third_guess, third_feedback,
         fourth_guess, fourth_feedback,
         fifth_guess, fifth_feedback)

second.feedback <-
  letter.eligibility %>%
  inner_join(second.relevant, by = "simulation_id") %>%
  ## update Grey feedback; if the feedback is Grey, then set every eligibility column to 0
  mutate(first_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, fifth_eligible)) %>%
  ## update Yellow feedback; if the feedback is Yellow, then set just that column to 0
  mutate(first_eligible = ifelse(letter == first_guess & first_feedback == "Yellow", 0, first_eligible),
         second_eligible = ifelse(letter == second_guess & second_feedback == "Yellow", 0, second_eligible),
         third_eligible = ifelse(letter == third_guess & third_feedback == "Yellow", 0, third_eligible),
         fourth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Yellow", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Yellow", 0, fifth_eligible)) %>%
  mutate(is_yellow = ifelse(letter == first_guess & first_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == second_guess & second_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == third_guess & third_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == fourth_guess & fourth_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == fifth_guess & fifth_feedback == "Yellow", 1, is_yellow)) %>%
  ## update Green feedback; if the feedback is Green and the letter is not the guess, set that column to 0
  mutate(first_eligible = ifelse(letter != first_guess & first_feedback == "Green", 0, first_eligible),
         second_eligible = ifelse(letter != second_guess & second_feedback == "Green", 0, second_eligible),
         third_eligible = ifelse(letter != third_guess & third_feedback == "Green", 0, third_eligible),
         fourth_eligible = ifelse(letter != fourth_guess & fourth_feedback == "Green", 0, fourth_eligible),
         fifth_eligible = ifelse(letter != fifth_guess & fifth_feedback == "Green", 0, fifth_eligible)) %>%
  mutate(is_yellow = ifelse(letter == first_guess & first_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == second_guess & second_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == third_guess & third_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == fourth_guess & fourth_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == fifth_guess & fifth_feedback == "Green", 0, is_yellow))

letter.eligibility <-
  second.feedback %>%
  select(simulation_id, letter, first_eligible, second_eligible, third_eligible, fourth_eligible, fifth_eligible, is_yellow)

## make third guess
third.possible <-
  wordle.list %>%
  inner_join(letter.eligibility %>% select(letter, first_eligible, simulation_id), by = c("first_letter" = "letter")) %>%
  inner_join(letter.eligibility %>% select(letter, second_eligible, simulation_id), by = c("second_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, third_eligible, simulation_id), by = c("third_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, fourth_eligible, simulation_id), by = c("fourth_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, fifth_eligible, simulation_id), by = c("fifth_letter" = "letter", "simulation_id")) %>%
  filter(first_eligible == 1,
         second_eligible == 1,
         third_eligible == 1,
         fourth_eligible == 1,
         fifth_eligible == 1) %>%
  distinct(simulation_id, word, first_letter, second_letter, third_letter, fourth_letter, fifth_letter) %>%
  rename(word_guess = word,
         first_guess = first_letter,
         second_guess = second_letter,
         third_guess = third_letter,
         fourth_guess = fourth_letter,
         fifth_guess = fifth_letter)

third.possible <-
  rbind(third.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("first_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, first_guess, NA)),
        third.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("second_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, second_guess, NA)),
        third.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("third_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, third_guess, NA)),
        third.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("fourth_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, fourth_guess, NA)),
        third.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("fifth_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, fifth_guess, NA))) %>%
  distinct(simulation_id, word_guess, first_guess, second_guess, third_guess, fourth_guess, fifth_guess, is_yellow) %>%
  mutate(has_yellow = ifelse(is.na(is_yellow), 0, 1)) %>%
  group_by(simulation_id, word_guess, first_guess, second_guess, third_guess, fourth_guess, fifth_guess) %>%
  summarize(yellows = sum(has_yellow)) %>%
  ungroup() %>%
  group_by(simulation_id) %>%
  mutate(max = max(yellows)) %>%
  filter(yellows == max) %>%
  select(-yellows, -max)

third.guesses <-
  third.possible %>%
  group_by(simulation_id) %>%
  slice_sample(n = 1)

third.merged <-
  third.guesses %>%
  inner_join(correct.words, by = "simulation_id") %>%
  mutate(first_green = ifelse(first_guess == first_letter, 1, 0),
         second_green = ifelse(second_guess == second_letter, 1, 0),
         third_green = ifelse(third_guess == third_letter, 1, 0),
         fourth_green = ifelse(fourth_guess == fourth_letter, 1, 0),
         fifth_green = ifelse(fifth_guess == fifth_letter, 1, 0),
         first_yellow = ifelse(first_guess %in% c(second_letter, third_letter, fourth_letter, fifth_letter), 1, 0),
         second_yellow = ifelse(second_guess %in% c(first_letter, third_letter, fourth_letter, fifth_letter), 1, 0),
         third_yellow = ifelse(third_guess %in% c(second_letter, first_letter, fourth_letter, fifth_letter), 1, 0),
         fourth_yellow = ifelse(fourth_guess %in% c(second_letter, third_letter, first_letter, fifth_letter), 1, 0),
         fifth_yellow = ifelse(fifth_guess %in% c(second_letter, third_letter, fourth_letter, first_letter), 1, 0),
         first_grey = 1 - first_green - first_yellow,
         second_grey = 1 - second_green - second_yellow,
         third_grey = 1 - third_green - third_yellow,
         fourth_grey = 1 - fourth_green - fourth_yellow,
         fifth_grey = 1 - fifth_green - fifth_yellow,
         first_feedback = ifelse(first_green == 1, "Green", ifelse(first_yellow == 1, "Yellow", "Grey")),
         second_feedback = ifelse(second_green == 1, "Green", ifelse(second_yellow == 1, "Yellow", "Grey")),
         third_feedback = ifelse(third_green == 1, "Green", ifelse(third_yellow == 1, "Yellow", "Grey")),
         fourth_feedback = ifelse(fourth_green == 1, "Green", ifelse(fourth_yellow == 1, "Yellow", "Grey")),
         fifth_feedback = ifelse(fifth_green == 1, "Green", ifelse(fifth_yellow == 1, "Yellow", "Grey")))

third.relevant <-
  third.merged %>%
  select(simulation_id,
         first_guess, first_feedback,
         second_guess, second_feedback,
         third_guess, third_feedback,
         fourth_guess, fourth_feedback,
         fifth_guess, fifth_feedback)

third.feedback <-
  letter.eligibility %>%
  inner_join(third.relevant, by = "simulation_id") %>%
  ## update Grey feedback; if the feedback is Grey, then set every eligibility column to 0
  mutate(first_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, fifth_eligible)) %>%
  ## update Yellow feedback; if the feedback is Yellow, then set just that column to 0
  mutate(first_eligible = ifelse(letter == first_guess & first_feedback == "Yellow", 0, first_eligible),
         second_eligible = ifelse(letter == second_guess & second_feedback == "Yellow", 0, second_eligible),
         third_eligible = ifelse(letter == third_guess & third_feedback == "Yellow", 0, third_eligible),
         fourth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Yellow", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Yellow", 0, fifth_eligible)) %>%
  mutate(is_yellow = ifelse(letter == first_guess & first_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == second_guess & second_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == third_guess & third_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == fourth_guess & fourth_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == fifth_guess & fifth_feedback == "Yellow", 1, is_yellow)) %>%
  ## update Green feedback; if the feedback is Green and the letter is not the guess, set that column to 0
  mutate(first_eligible = ifelse(letter != first_guess & first_feedback == "Green", 0, first_eligible),
         second_eligible = ifelse(letter != second_guess & second_feedback == "Green", 0, second_eligible),
         third_eligible = ifelse(letter != third_guess & third_feedback == "Green", 0, third_eligible),
         fourth_eligible = ifelse(letter != fourth_guess & fourth_feedback == "Green", 0, fourth_eligible),
         fifth_eligible = ifelse(letter != fifth_guess & fifth_feedback == "Green", 0, fifth_eligible)) %>%
  mutate(is_yellow = ifelse(letter == first_guess & first_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == second_guess & second_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == third_guess & third_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == fourth_guess & fourth_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == fifth_guess & fifth_feedback == "Green", 0, is_yellow))

letter.eligibility <-
  third.feedback %>%
  select(simulation_id, letter, first_eligible, second_eligible, third_eligible, fourth_eligible, fifth_eligible, is_yellow)

## make fourth guess
fourth.possible <-
  wordle.list %>%
  inner_join(letter.eligibility %>% select(letter, first_eligible, simulation_id), by = c("first_letter" = "letter")) %>%
  inner_join(letter.eligibility %>% select(letter, second_eligible, simulation_id), by = c("second_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, third_eligible, simulation_id), by = c("third_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, fourth_eligible, simulation_id), by = c("fourth_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, fifth_eligible, simulation_id), by = c("fifth_letter" = "letter", "simulation_id")) %>%
  filter(first_eligible == 1,
         second_eligible == 1,
         third_eligible == 1,
         fourth_eligible == 1,
         fifth_eligible == 1) %>%
  distinct(simulation_id, word, first_letter, second_letter, third_letter, fourth_letter, fifth_letter) %>%
  rename(word_guess = word,
         first_guess = first_letter,
         second_guess = second_letter,
         third_guess = third_letter,
         fourth_guess = fourth_letter,
         fifth_guess = fifth_letter)

fourth.possible <-
  rbind(fourth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("first_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, first_guess, NA)),
        fourth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("second_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, second_guess, NA)),
        fourth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("third_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, third_guess, NA)),
        fourth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("fourth_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, fourth_guess, NA)),
        fourth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("fifth_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, fifth_guess, NA))) %>%
  distinct(simulation_id, word_guess, first_guess, second_guess, third_guess, fourth_guess, fifth_guess, is_yellow) %>%
  mutate(has_yellow = ifelse(is.na(is_yellow), 0, 1)) %>%
  group_by(simulation_id, word_guess, first_guess, second_guess, third_guess, fourth_guess, fifth_guess) %>%
  summarize(yellows = sum(has_yellow)) %>%
  ungroup() %>%
  group_by(simulation_id) %>%
  mutate(max = max(yellows)) %>%
  filter(yellows == max) %>%
  select(-yellows, -max)

fourth.guesses <-
  fourth.possible %>%
  group_by(simulation_id) %>%
  slice_sample(n = 1)

fourth.merged <-
  fourth.guesses %>%
  inner_join(correct.words, by = "simulation_id") %>%
  mutate(first_green = ifelse(first_guess == first_letter, 1, 0),
         second_green = ifelse(second_guess == second_letter, 1, 0),
         third_green = ifelse(third_guess == third_letter, 1, 0),
         fourth_green = ifelse(fourth_guess == fourth_letter, 1, 0),
         fifth_green = ifelse(fifth_guess == fifth_letter, 1, 0),
         first_yellow = ifelse(first_guess %in% c(second_letter, third_letter, fourth_letter, fifth_letter), 1, 0),
         second_yellow = ifelse(second_guess %in% c(first_letter, third_letter, fourth_letter, fifth_letter), 1, 0),
         third_yellow = ifelse(third_guess %in% c(second_letter, first_letter, fourth_letter, fifth_letter), 1, 0),
         fourth_yellow = ifelse(fourth_guess %in% c(second_letter, third_letter, first_letter, fifth_letter), 1, 0),
         fifth_yellow = ifelse(fifth_guess %in% c(second_letter, third_letter, fourth_letter, first_letter), 1, 0),
         first_grey = 1 - first_green - first_yellow,
         second_grey = 1 - second_green - second_yellow,
         third_grey = 1 - third_green - third_yellow,
         fourth_grey = 1 - fourth_green - fourth_yellow,
         fifth_grey = 1 - fifth_green - fifth_yellow,
         first_feedback = ifelse(first_green == 1, "Green", ifelse(first_yellow == 1, "Yellow", "Grey")),
         second_feedback = ifelse(second_green == 1, "Green", ifelse(second_yellow == 1, "Yellow", "Grey")),
         third_feedback = ifelse(third_green == 1, "Green", ifelse(third_yellow == 1, "Yellow", "Grey")),
         fourth_feedback = ifelse(fourth_green == 1, "Green", ifelse(fourth_yellow == 1, "Yellow", "Grey")),
         fifth_feedback = ifelse(fifth_green == 1, "Green", ifelse(fifth_yellow == 1, "Yellow", "Grey")))

fourth.relevant <-
  fourth.merged %>%
  select(simulation_id,
         first_guess, first_feedback,
         second_guess, second_feedback,
         third_guess, third_feedback,
         fourth_guess, fourth_feedback,
         fifth_guess, fifth_feedback)

fourth.feedback <-
  letter.eligibility %>%
  inner_join(fourth.relevant, by = "simulation_id") %>%
  ## update Grey feedback; if the feedback is Grey, then set every eligibility column to 0
  mutate(first_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, fifth_eligible)) %>%
  ## update Yellow feedback; if the feedback is Yellow, then set just that column to 0
  mutate(first_eligible = ifelse(letter == first_guess & first_feedback == "Yellow", 0, first_eligible),
         second_eligible = ifelse(letter == second_guess & second_feedback == "Yellow", 0, second_eligible),
         third_eligible = ifelse(letter == third_guess & third_feedback == "Yellow", 0, third_eligible),
         fourth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Yellow", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Yellow", 0, fifth_eligible)) %>%
  mutate(is_yellow = ifelse(letter == first_guess & first_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == second_guess & second_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == third_guess & third_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == fourth_guess & fourth_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == fifth_guess & fifth_feedback == "Yellow", 1, is_yellow)) %>%
  ## update Green feedback; if the feedback is Green and the letter is not the guess, set that column to 0
  mutate(first_eligible = ifelse(letter != first_guess & first_feedback == "Green", 0, first_eligible),
         second_eligible = ifelse(letter != second_guess & second_feedback == "Green", 0, second_eligible),
         third_eligible = ifelse(letter != third_guess & third_feedback == "Green", 0, third_eligible),
         fourth_eligible = ifelse(letter != fourth_guess & fourth_feedback == "Green", 0, fourth_eligible),
         fifth_eligible = ifelse(letter != fifth_guess & fifth_feedback == "Green", 0, fifth_eligible)) %>%
  mutate(is_yellow = ifelse(letter == first_guess & first_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == second_guess & second_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == third_guess & third_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == fourth_guess & fourth_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == fifth_guess & fifth_feedback == "Green", 0, is_yellow))

letter.eligibility <-
  fourth.feedback %>%
  select(simulation_id, letter, first_eligible, second_eligible, third_eligible, fourth_eligible, fifth_eligible, is_yellow)

## make fifth guess
fifth.possible <-
  wordle.list %>%
  inner_join(letter.eligibility %>% select(letter, first_eligible, simulation_id), by = c("first_letter" = "letter")) %>%
  inner_join(letter.eligibility %>% select(letter, second_eligible, simulation_id), by = c("second_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, third_eligible, simulation_id), by = c("third_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, fourth_eligible, simulation_id), by = c("fourth_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, fifth_eligible, simulation_id), by = c("fifth_letter" = "letter", "simulation_id")) %>%
  filter(first_eligible == 1,
         second_eligible == 1,
         third_eligible == 1,
         fourth_eligible == 1,
         fifth_eligible == 1) %>%
  distinct(simulation_id, word, first_letter, second_letter, third_letter, fourth_letter, fifth_letter) %>%
  rename(word_guess = word,
         first_guess = first_letter,
         second_guess = second_letter,
         third_guess = third_letter,
         fourth_guess = fourth_letter,
         fifth_guess = fifth_letter)

fifth.possible <-
  rbind(fifth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("first_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, first_guess, NA)),
        fifth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("second_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, second_guess, NA)),
        fifth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("third_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, third_guess, NA)),
        fifth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("fourth_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, fourth_guess, NA)),
        fifth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("fifth_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, fifth_guess, NA))) %>%
  distinct(simulation_id, word_guess, first_guess, second_guess, third_guess, fourth_guess, fifth_guess, is_yellow) %>%
  mutate(has_yellow = ifelse(is.na(is_yellow), 0, 1)) %>%
  group_by(simulation_id, word_guess, first_guess, second_guess, third_guess, fourth_guess, fifth_guess) %>%
  summarize(yellows = sum(has_yellow)) %>%
  ungroup() %>%
  group_by(simulation_id) %>%
  mutate(max = max(yellows)) %>%
  filter(yellows == max) %>%
  select(-yellows, -max)

fifth.guesses <-
  fifth.possible %>%
  group_by(simulation_id) %>%
  slice_sample(n = 1)

fifth.merged <-
  fifth.guesses %>%
  inner_join(correct.words, by = "simulation_id") %>%
  mutate(first_green = ifelse(first_guess == first_letter, 1, 0),
         second_green = ifelse(second_guess == second_letter, 1, 0),
         third_green = ifelse(third_guess == third_letter, 1, 0),
         fourth_green = ifelse(fourth_guess == fourth_letter, 1, 0),
         fifth_green = ifelse(fifth_guess == fifth_letter, 1, 0),
         first_yellow = ifelse(first_guess %in% c(second_letter, third_letter, fourth_letter, fifth_letter), 1, 0),
         second_yellow = ifelse(second_guess %in% c(first_letter, third_letter, fourth_letter, fifth_letter), 1, 0),
         third_yellow = ifelse(third_guess %in% c(second_letter, first_letter, fourth_letter, fifth_letter), 1, 0),
         fourth_yellow = ifelse(fourth_guess %in% c(second_letter, third_letter, first_letter, fifth_letter), 1, 0),
         fifth_yellow = ifelse(fifth_guess %in% c(second_letter, third_letter, fourth_letter, first_letter), 1, 0),
         first_grey = 1 - first_green - first_yellow,
         second_grey = 1 - second_green - second_yellow,
         third_grey = 1 - third_green - third_yellow,
         fourth_grey = 1 - fourth_green - fourth_yellow,
         fifth_grey = 1 - fifth_green - fifth_yellow,
         first_feedback = ifelse(first_green == 1, "Green", ifelse(first_yellow == 1, "Yellow", "Grey")),
         second_feedback = ifelse(second_green == 1, "Green", ifelse(second_yellow == 1, "Yellow", "Grey")),
         third_feedback = ifelse(third_green == 1, "Green", ifelse(third_yellow == 1, "Yellow", "Grey")),
         fourth_feedback = ifelse(fourth_green == 1, "Green", ifelse(fourth_yellow == 1, "Yellow", "Grey")),
         fifth_feedback = ifelse(fifth_green == 1, "Green", ifelse(fifth_yellow == 1, "Yellow", "Grey")))

fifth.relevant <-
  fifth.merged %>%
  select(simulation_id,
         first_guess, first_feedback,
         second_guess, second_feedback,
         third_guess, third_feedback,
         fourth_guess, fourth_feedback,
         fifth_guess, fifth_feedback)

fifth.feedback <-
  letter.eligibility %>%
  inner_join(fifth.relevant, by = "simulation_id") %>%
  ## update Grey feedback; if the feedback is Grey, then set every eligibility column to 0
  mutate(first_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == first_guess & first_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == second_guess & second_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == third_guess & third_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Grey", 0, fifth_eligible)) %>%
  mutate(first_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, first_eligible),
         second_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, second_eligible),
         third_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, third_eligible),
         fourth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Grey", 0, fifth_eligible)) %>%
  ## update Yellow feedback; if the feedback is Yellow, then set just that column to 0
  mutate(first_eligible = ifelse(letter == first_guess & first_feedback == "Yellow", 0, first_eligible),
         second_eligible = ifelse(letter == second_guess & second_feedback == "Yellow", 0, second_eligible),
         third_eligible = ifelse(letter == third_guess & third_feedback == "Yellow", 0, third_eligible),
         fourth_eligible = ifelse(letter == fourth_guess & fourth_feedback == "Yellow", 0, fourth_eligible),
         fifth_eligible = ifelse(letter == fifth_guess & fifth_feedback == "Yellow", 0, fifth_eligible)) %>%
  mutate(is_yellow = ifelse(letter == first_guess & first_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == second_guess & second_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == third_guess & third_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == fourth_guess & fourth_feedback == "Yellow", 1, is_yellow),
         is_yellow = ifelse(letter == fifth_guess & fifth_feedback == "Yellow", 1, is_yellow)) %>%
  ## update Green feedback; if the feedback is Green and the letter is not the guess, set that column to 0
  mutate(first_eligible = ifelse(letter != first_guess & first_feedback == "Green", 0, first_eligible),
         second_eligible = ifelse(letter != second_guess & second_feedback == "Green", 0, second_eligible),
         third_eligible = ifelse(letter != third_guess & third_feedback == "Green", 0, third_eligible),
         fourth_eligible = ifelse(letter != fourth_guess & fourth_feedback == "Green", 0, fourth_eligible),
         fifth_eligible = ifelse(letter != fifth_guess & fifth_feedback == "Green", 0, fifth_eligible)) %>%
  mutate(is_yellow = ifelse(letter == first_guess & first_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == second_guess & second_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == third_guess & third_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == fourth_guess & fourth_feedback == "Green", 0, is_yellow),
         is_yellow = ifelse(letter == fifth_guess & fifth_feedback == "Green", 0, is_yellow))

letter.eligibility <-
  fifth.feedback %>%
  select(simulation_id, letter, first_eligible, second_eligible, third_eligible, fourth_eligible, fifth_eligible, is_yellow)

## make sixth guess
sixth.possible <-
  wordle.list %>%
  inner_join(letter.eligibility %>% select(letter, first_eligible, simulation_id), by = c("first_letter" = "letter")) %>%
  inner_join(letter.eligibility %>% select(letter, second_eligible, simulation_id), by = c("second_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, third_eligible, simulation_id), by = c("third_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, fourth_eligible, simulation_id), by = c("fourth_letter" = "letter", "simulation_id")) %>%
  inner_join(letter.eligibility %>% select(letter, fifth_eligible, simulation_id), by = c("fifth_letter" = "letter", "simulation_id")) %>%
  filter(first_eligible == 1,
         second_eligible == 1,
         third_eligible == 1,
         fourth_eligible == 1,
         fifth_eligible == 1) %>%
  distinct(simulation_id, word, first_letter, second_letter, third_letter, fourth_letter, fifth_letter) %>%
  rename(word_guess = word,
         first_guess = first_letter,
         second_guess = second_letter,
         third_guess = third_letter,
         fourth_guess = fourth_letter,
         fifth_guess = fifth_letter)

sixth.possible <-
  rbind(sixth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("first_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, first_guess, NA)),
        sixth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("second_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, second_guess, NA)),
        sixth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("third_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, third_guess, NA)),
        sixth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("fourth_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, fourth_guess, NA)),
        sixth.possible  %>%
          left_join(letter.eligibility %>% filter(is_yellow == 1), by = c("fifth_guess" = "letter", "simulation_id")) %>%
          mutate(is_yellow = ifelse(is.na(is_yellow) == FALSE, fifth_guess, NA))) %>%
  distinct(simulation_id, word_guess, first_guess, second_guess, third_guess, fourth_guess, fifth_guess, is_yellow) %>%
  mutate(has_yellow = ifelse(is.na(is_yellow), 0, 1)) %>%
  group_by(simulation_id, word_guess, first_guess, second_guess, third_guess, fourth_guess, fifth_guess) %>%
  summarize(yellows = sum(has_yellow)) %>%
  ungroup() %>%
  group_by(simulation_id) %>%
  mutate(max = max(yellows)) %>%
  filter(yellows == max) %>%
  select(-yellows, -max)

sixth.guesses <-
  sixth.possible %>%
  group_by(simulation_id) %>%
  slice_sample(n = 1)

sixth.merged <-
  sixth.guesses %>%
  inner_join(correct.words, by = "simulation_id") %>%
  mutate(first_green = ifelse(first_guess == first_letter, 1, 0),
         second_green = ifelse(second_guess == second_letter, 1, 0),
         third_green = ifelse(third_guess == third_letter, 1, 0),
         fourth_green = ifelse(fourth_guess == fourth_letter, 1, 0),
         fifth_green = ifelse(fifth_guess == fifth_letter, 1, 0),
         first_yellow = ifelse(first_guess %in% c(second_letter, third_letter, fourth_letter, fifth_letter), 1, 0),
         second_yellow = ifelse(second_guess %in% c(first_letter, third_letter, fourth_letter, fifth_letter), 1, 0),
         third_yellow = ifelse(third_guess %in% c(second_letter, first_letter, fourth_letter, fifth_letter), 1, 0),
         fourth_yellow = ifelse(fourth_guess %in% c(second_letter, third_letter, first_letter, fifth_letter), 1, 0),
         fifth_yellow = ifelse(fifth_guess %in% c(second_letter, third_letter, fourth_letter, first_letter), 1, 0),
         first_grey = 1 - first_green - first_yellow,
         second_grey = 1 - second_green - second_yellow,
         third_grey = 1 - third_green - third_yellow,
         fourth_grey = 1 - fourth_green - fourth_yellow,
         fifth_grey = 1 - fifth_green - fifth_yellow,
         first_feedback = ifelse(first_green == 1, "Green", ifelse(first_yellow == 1, "Yellow", "Grey")),
         second_feedback = ifelse(second_green == 1, "Green", ifelse(second_yellow == 1, "Yellow", "Grey")),
         third_feedback = ifelse(third_green == 1, "Green", ifelse(third_yellow == 1, "Yellow", "Grey")),
         fourth_feedback = ifelse(fourth_green == 1, "Green", ifelse(fourth_yellow == 1, "Yellow", "Grey")),
         fifth_feedback = ifelse(fifth_green == 1, "Green", ifelse(fifth_yellow == 1, "Yellow", "Grey")))

sixth.relevant <-
  sixth.merged %>%
  select(simulation_id,
         first_guess, first_feedback,
         second_guess, second_feedback,
         third_guess, third_feedback,
         fourth_guess, fourth_feedback,
         fifth_guess, fifth_feedback)

game.results <-
  correct.words %>%
  select(simulation_id, word) %>%
  rename(correct_word = word) %>%
  mutate(first_possible = 2315) %>%
  inner_join(first.guesses %>% select(simulation_id, word_guess), by = "simulation_id") %>%
  rename(first_guess = word_guess) %>%
  mutate(first_solved = ifelse(first_guess == correct_word, 1, 0)) %>%
  inner_join(second.possible %>% select(simulation_id, word_guess), by = "simulation_id") %>%
  select(-word_guess) %>%
  group_by_all() %>%
  summarize(second_possible = n()) %>%
  ungroup() %>%
  inner_join(second.guesses %>% select(simulation_id, word_guess), by = "simulation_id") %>%
  rename(second_guess = word_guess) %>%
  mutate(second_solved = ifelse(second_guess == correct_word, 1, 0)) %>%
  inner_join(third.possible %>% select(simulation_id, word_guess), by = "simulation_id") %>%
  select(-word_guess) %>%
  group_by_all() %>%
  summarize(third_possible = n()) %>%
  ungroup() %>%
  inner_join(third.guesses %>% select(simulation_id, word_guess), by = "simulation_id") %>%
  rename(third_guess = word_guess) %>%
  mutate(third_solved = ifelse(third_guess == correct_word, 1, 0)) %>%
  inner_join(fourth.possible %>% select(simulation_id, word_guess), by = "simulation_id") %>%
  select(-word_guess) %>%
  group_by_all() %>%
  summarize(fourth_possible = n()) %>%
  ungroup() %>%
  inner_join(fourth.guesses %>% select(simulation_id, word_guess), by = "simulation_id") %>%
  rename(fourth_guess = word_guess) %>%
  mutate(fourth_solved = ifelse(fourth_guess == correct_word, 1, 0)) %>%
  inner_join(fifth.possible %>% select(simulation_id, word_guess), by = "simulation_id") %>%
  select(-word_guess) %>%
  group_by_all() %>%
  summarize(fifth_possible = n()) %>%
  ungroup() %>%
  inner_join(fifth.guesses %>% select(simulation_id, word_guess), by = "simulation_id") %>%
  rename(fifth_guess = word_guess) %>%
  mutate(fifth_solved = ifelse(fifth_guess == correct_word, 1, 0)) %>%
  inner_join(sixth.possible %>% select(simulation_id, word_guess), by = "simulation_id") %>%
  select(-word_guess) %>%
  group_by_all() %>%
  summarize(sixth_possible = n()) %>%
  ungroup() %>%
  inner_join(sixth.guesses %>% select(simulation_id, word_guess), by = "simulation_id") %>%
  rename(sixth_guess = word_guess) %>%
  mutate(sixth_solved = ifelse(sixth_guess == correct_word, 1, 0)) %>%
  mutate(solved = ifelse(sixth_solved == 1, 1, 0),
         guesses = 7 - (first_solved + second_solved + third_solved + fourth_solved + fifth_solved + sixth_solved))

end.time <- Sys.time()
runtime <- end.time - start.time
runtime

# all.games <- rbind(all.games, game.results)

# write.csv(all.games, file = "Data Analysis/wordle simulation results.csv")

## evaluate performance
summary.stats <-
  game.results %>%
  summarize(solve_rate = mean(solved),
            solve_in_one = mean(first_solved),
            solve_in_two = mean(second_solved),
            solve_in_three = mean(third_solved),
            solve_in_four = mean(fourth_solved),
            solve_in_five = mean(fifth_solved),
            solve_in_six = mean(sixth_solved),
            guesses = mean(guesses))

starter.performance <-
  game.results %>%
  inner_join(wordle.list, by = c("first_guess" = "word")) %>%
  group_by(first_guess) %>%
  summarize(freq = n(),
            solve_rate = mean(solved),
            guesses = mean(guesses),
            second_possible = mean(second_possible),
            third_possible = mean(third_possible)) %>%
  arrange(desc(freq))
