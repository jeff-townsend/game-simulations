library(tidyverse)

## map every possible game

games <- matrix(nrow = factorial(9), ncol = 9)
colnames(games) <- c("x1", "o1", "x2", "o2", "x3", "o3", "x4", "o4", "x5")
id <- 0

for(m1 in 1:9){
  for(m2 in 1:9){
    if(m2 != m1){
      for(m3 in 1:9){
        if(m3 != m1 && m3 != m2){
          for(m4 in 1:9){
            if(m4 != m1 && m4 != m2 && m4 != m3){
              for(m5 in 1:9){
                if(m5 != m1 && m5 != m2 && m5 != m3 && m5 != m4){
                  for(m6 in 1:9){
                    if(m6 != m1 && m6 != m2 && m6 != m3 && m6 != m4 && m6 != m5){
                      for(m7 in 1:9){
                        if(m7 != m1 && m7 != m2 && m7 != m3 && m7 != m4 && m7 != m5 && m7 != m6){
                          for(m8 in 1:9){
                            if(m8 != m1 && m8 != m2 && m8 != m3 && m8 != m4 && m8 != m5 && m8 != m6 && m8 != m7){
                              for(m9 in 1:9){
                                if(m9 != m1 && m9 != m2 && m9 != m3 && m9 != m4 && m9 != m5 && m9 != m6 && m9 != m7 && m9 != m8){
                                  id <- id + 1
                                  games[id,] <- c(m1, m2, m3, m4, m5, m6, m7, m8, m9)
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

games.df <- data.frame(games)

wins <- data.frame(moves = c(123, 132, 213, 231, 312, 321,
                            456, 465, 546, 564, 645, 654,
                            789, 798, 879, 897, 978, 987,
                            147, 174, 417, 471, 714, 741,
                            258, 285, 528, 582, 825, 852,
                            369, 396, 639, 693, 936, 963,
                            159, 195, 519, 591, 915, 951,
                            357, 375, 537, 573, 735, 753),
                   win = 1)

results <-
  games.df %>%
  mutate(x3.moves = as.numeric(paste0(x1, x2, x3))) %>%
  left_join(wins, by = c("x3.moves" = "moves")) %>%
  mutate(x3.win = ifelse(is.na(win), 0, 1)) %>%
  select(-win) %>%
  mutate(o3.moves = as.numeric(paste0(o1, o2, o3))) %>%
  left_join(wins, by = c("o3.moves" = "moves")) %>%
  mutate(o3.win = ifelse(x3.win == 1, 0, ifelse(is.na(win), 0, 1))) %>%
  select(-win) %>%
  mutate(x4.moves1 = as.numeric(paste0(x1, x2, x4)),
         x4.moves2 = as.numeric(paste0(x1, x3, x4)),
         x4.moves3 = as.numeric(paste0(x2, x3, x4))) %>%
  left_join(wins, by = c("x4.moves1" = "moves")) %>%
  mutate(x4.win = ifelse(x3.win + o3.win == 1, 0, ifelse(is.na(win), 0, 1))) %>%
  select(-win) %>%
  left_join(wins, by = c("x4.moves2" = "moves")) %>%
  mutate(x4.win = ifelse(x3.win + o3.win == 1, 0, ifelse(is.na(win), x4.win, 1))) %>%
  select(-win) %>%
  left_join(wins, by = c("x4.moves3" = "moves")) %>%
  mutate(x4.win = ifelse(x3.win + o3.win == 1, 0, ifelse(is.na(win), x4.win, 1))) %>%
  select(-win) %>%
  mutate(o4.moves1 = as.numeric(paste0(o1, o2, o4)),
         o4.moves2 = as.numeric(paste0(o1, o3, o4)),
         o4.moves3 = as.numeric(paste0(o2, o3, o4))) %>%
  left_join(wins, by = c("o4.moves1" = "moves")) %>%
  mutate(o4.win = ifelse(x3.win + o3.win + x4.win == 1, 0, ifelse(is.na(win), 0, 1))) %>%
  select(-win) %>%
  left_join(wins, by = c("o4.moves2" = "moves")) %>%
  mutate(o4.win = ifelse(x3.win + o3.win + x4.win == 1, 0, ifelse(is.na(win), o4.win, 1))) %>%
  select(-win) %>%
  left_join(wins, by = c("o4.moves3" = "moves")) %>%
  mutate(o4.win = ifelse(x3.win + o3.win + x4.win == 1, 0, ifelse(is.na(win), o4.win, 1))) %>%
  select(-win) %>%
  mutate(x5.moves1 = as.numeric(paste0(x1, x2, x5)),
         x5.moves2 = as.numeric(paste0(x1, x3, x5)),
         x5.moves3 = as.numeric(paste0(x1, x4, x5)),
         x5.moves4 = as.numeric(paste0(x2, x3, x5)),
         x5.moves5 = as.numeric(paste0(x2, x4, x5)),
         x5.moves6 = as.numeric(paste0(x3, x4, x5))) %>%
  left_join(wins, by = c("x5.moves1" = "moves")) %>%
  mutate(x5.win = ifelse(x3.win + o3.win + x4.win + o4.win == 1, 0, ifelse(is.na(win), 0, 1))) %>%
  select(-win) %>%
  left_join(wins, by = c("x5.moves2" = "moves")) %>%
  mutate(x5.win = ifelse(x3.win + o3.win + x4.win + o4.win == 1, 0, ifelse(is.na(win), x5.win, 1))) %>%
  select(-win) %>%
  left_join(wins, by = c("x5.moves3" = "moves")) %>%
  mutate(x5.win = ifelse(x3.win + o3.win + x4.win + o4.win == 1, 0, ifelse(is.na(win), x5.win, 1))) %>%
  select(-win) %>%
  left_join(wins, by = c("x5.moves4" = "moves")) %>%
  mutate(x5.win = ifelse(x3.win + o3.win + x4.win + o4.win == 1, 0, ifelse(is.na(win), x5.win, 1))) %>%
  select(-win) %>%
  left_join(wins, by = c("x5.moves5" = "moves")) %>%
  mutate(x5.win = ifelse(x3.win + o3.win + x4.win + o4.win == 1, 0, ifelse(is.na(win), x5.win, 1))) %>%
  select(-win) %>%
  left_join(wins, by = c("x5.moves6" = "moves")) %>%
  mutate(x5.win = ifelse(x3.win + o3.win + x4.win + o4.win == 1, 0, ifelse(is.na(win), x5.win, 1))) %>%
  select(-win) %>%
  mutate(evaluation = (x3.win + x4.win + x5.win) - (o3.win + o4.win))

final <-
  results %>%
  select(x1, o1, x2, o2, x3, o3, x4, o4, x5, evaluation)

final %>%
  group_by(x1) %>%
  summarize(eval = mean(evaluation))

final %>%
  filter(x1 == 5) %>%
  group_by(o1) %>%
  summarize(eval = mean(evaluation))

final %>%
  filter(x1 == 5,
         o1 == 1) %>%
  group_by(x2) %>%
  summarize(eval = mean(evaluation))

final %>%
  filter(x1 == 5,
         o1 == 1,
         x2 == 3) %>%
  group_by(o2) %>%
  summarize(eval = mean(evaluation))

final %>%
  filter(x1 == 5,
         o1 == 1,
         x2 == 3,
         o2 == 7) %>%
  group_by(x3) %>%
  summarize(eval = mean(evaluation))

final %>%
  filter(x1 == 5,
         o1 == 1,
         x2 == 3,
         o2 == 7,
         x3 == 4) %>%
  group_by(o3) %>%
  summarize(eval = mean(evaluation))

####

o4.choice <-
  final %>%
  group_by(x1, o1, x2, o2, x3, o3, x4) %>%
  mutate(o4.min.eval = min(evaluation)) %>%
  filter(o4.min.eval == evaluation) %>%
  mutate(o4.min.square = min(o4)) %>%
  filter(o4.min.square == o4) %>%
  ungroup() %>%
  select(-c(o4.min.eval, o4.min.square))

x4.choice <-
  o4.choice %>%
  group_by(x1, o1, x2, o2, x3, o3) %>%
  mutate(x4.max.eval = max(evaluation)) %>%
  filter(x4.max.eval == evaluation) %>%
  mutate(x4.min.square = min(x4)) %>%
  filter(x4.min.square == x4) %>%
  ungroup() %>%
  select(-c(x4.max.eval, x4.min.square))

o3.choice <-
  x4.choice %>%
  group_by(x1, o1, x2, o2, x3) %>%
  mutate(o3.min.eval = min(evaluation)) %>%
  filter(o3.min.eval == evaluation) %>%
  mutate(o3.min.square = min(o3)) %>%
  filter(o3.min.square == o3) %>%
  ungroup() %>%
  select(-c(o3.min.eval, o3.min.square))

x3.choice <-
  o3.choice %>%
  group_by(x1, o1, x2, o2) %>%
  mutate(x3.max.eval = max(evaluation)) %>%
  filter(x3.max.eval == evaluation) %>%
  mutate(x3.min.square = min(x3)) %>%
  filter(x3.min.square == x3) %>%
  ungroup() %>%
  select(-c(x3.max.eval, x3.min.square))

o2.choice <-
  x3.choice %>%
  group_by(x1, o1, x2) %>%
  mutate(o2.min.eval = min(evaluation)) %>%
  filter(o2.min.eval == evaluation) %>%
  mutate(o2.min.square = min(o2)) %>%
  filter(o2.min.square == o2) %>%
  ungroup() %>%
  select(-c(o2.min.eval, o2.min.square))

x2.choice <-
  o2.choice %>%
  group_by(x1, o1) %>%
  mutate(x2.max.eval = max(evaluation)) %>%
  filter(x2.max.eval == evaluation) %>%
  mutate(x2.min.square = min(x2)) %>%
  filter(x2.min.square == x2) %>%
  ungroup() %>%
  select(-c(x2.max.eval, x2.min.square))

o1.choice <-
  x2.choice %>%
  group_by(x1) %>%
  mutate(o1.min.eval = min(evaluation)) %>%
  filter(o1.min.eval == evaluation) %>%
  mutate(o1.min.square = min(o1)) %>%
  filter(o1.min.square == o1) %>%
  ungroup() %>%
  select(-c(o1.min.eval, o1.min.square))

x1.choice <-
  o1.choice %>%
  mutate(x1.max.eval = max(evaluation)) %>%
  filter(x1.max.eval == evaluation) %>%
  mutate(x1.min.square = min(x1)) %>%
  filter(x1.min.square == x1) %>%
  ungroup() %>%
  select(-c(x1.max.eval, x1.min.square))
