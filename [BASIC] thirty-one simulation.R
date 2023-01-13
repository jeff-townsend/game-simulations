library(tidyverse)

####### set up the game
## create a standard deck of cards
# ignoring suit because it doesn't matter for this game
start.time <- Sys.time()
cards <- data.frame(id = c(1:52),
                    card_value = c(rep(c(2:10), each = 4), rep("J", 4), rep("Q", 4), rep("K", 4), rep("A", 4)))
cards$card_points <- as.numeric(ifelse(cards$card_value == "A", 11,
                                       ifelse(cards$card_value %in% c("J", "Q", "K"), 10, cards$card_value)))

simulations <- 5000
num.players <- 6

decks <- data.frame(id = rep(c(1:simulations), each = 52),
                    card_id = cards$id,
                    card_value = cards$card_value,
                    card_points = cards$card_points)

# shuffle the deck
shuffled.decks <-
  decks %>%
  mutate(rand = runif(52*simulations)) %>%
  group_by(id) %>%
  mutate(card_order = rank(rand)) %>%
  ungroup() %>%
  arrange(id, card_order)

# deal the cards
player.cards <-
  shuffled.decks %>%
  rename(game_id = id) %>%
  filter(card_order <= 3*num.players) %>%
  mutate(player_id = rep(rep(1:num.players, each = 3), simulations),
         id = c(1:(3*num.players*simulations))) %>%
  select(id, game_id, player_id, card_id, card_value, card_points)

# keep score
player.scores <-
  player.cards %>%
  group_by(game_id, player_id) %>%
  summarize(card_points1 = min(card_points),
            card_points2 = median(card_points),
            card_points3 = max(card_points),
            total_points = sum(card_points),
            .groups = "drop")

active.standings <- pivot_wider(player.scores %>% select(game_id, player_id, total_points),
                                names_from = player_id,
                                names_glue = "player_{player_id}",
                                values_from = total_points)

standings.audit <-
  active.standings %>%
  mutate(turns_taken = 0)

# the first player can choose either a faceup card or a new card at random
# faceup.card <- data.frame(card_id = shuffled.deck$id[num.players*3+1],
#                           card_value = shuffled.deck$card_value[num.players*3+1],
#                           card_points = shuffled.deck$card_points[num.players*3+1])
# faceup.card <-
#   faceup.card %>%
#   mutate(expected_card_value = ifelse(card_type == "Power Card", NA, as.numeric(card_value)))

# the remaining cards
draw.piles <-
  shuffled.decks %>%
  filter(card_order > 3*num.players) %>%
  select(-rand) %>%
  group_by(id) %>%
  mutate(card_order = rank(card_order)) %>%
  ungroup()

####### now we play

player.turn <- 1
round.turn <- 1
total.turns <- floor((52-3*num.players) / num.players) * num.players

for(round.turn in 1:total.turns){
  # take the top card from the draw pile
  draw.cards <-
    draw.piles %>%
    filter(card_order == 1) %>%
    mutate(game_id = c(1:simulations),
           player_id = player.turn) %>%
    select(game_id, player_id, card_id, card_value, card_points)
  
  taken.cards <- draw.cards
  # taken.card <- data.frame(card_id = ifelse(faceup.card$card_points >= 8, faceup.card$card_id, draw.card$card_id)),
  #                          card_value = ifelse(faceup.card$card_points >= 8, faceup.card$card_value, draw.card$card_value)),
  #                          card_points = ifelse(faceup.card$card_points >= 8, faceup.card$card_points, draw.card$card_points)))
  
  
  # remove card from the draw pile, if applicable
  draw.piles <-
    draw.piles %>%
    filter(card_order >= 2) %>%
    mutate(card_order = card_order - 1)
  
  # evaluate whether to keep or discard drawn card
  
  discard.evaluations <-
    player.cards %>%
    filter(player_id == player.turn) %>%
    arrange(game_id, card_points) %>%
    mutate(card_replacement_order = rep(c(1:3), simulations)) %>%
    group_by(game_id) %>%
    top_n(n = -1, wt = card_replacement_order) %>%
    ungroup() %>%
    select(id, game_id, player_id, card_id, card_value, card_points) %>%
    union_all(taken.cards) %>%
    group_by(game_id) %>%
    mutate(id = max(id, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(game_id, desc(card_points)) %>%
    mutate(evaluation_order = rep(c(1:2), simulations))
  
  player.cards <-
    player.cards %>%
    left_join(discard.evaluations %>% filter(evaluation_order == 1), by = c("id", "game_id", "player_id")) %>%
    mutate(card_id = ifelse(is.na(card_id.y), card_id.x, card_id.y),
           card_value = ifelse(is.na(card_id.y), card_value.x, card_value.y),
           card_points = ifelse(is.na(card_id.y), card_points.x, card_points.y)) %>%
    select(id, game_id, player_id, card_id, card_value, card_points)
  
  player.scores <-
    player.cards %>%
    group_by(game_id, player_id) %>%
    summarize(card_points1 = min(card_points),
              card_points2 = median(card_points),
              card_points3 = max(card_points),
              total_points = sum(card_points),
              .groups = "drop")
  
  active.standings <- pivot_wider(player.scores %>% select(game_id, player_id, total_points),
                                  names_from = player_id,
                                  names_glue = "player_{player_id}",
                                  values_from = total_points)
  
  standings.audit <- rbind(standings.audit, active.standings %>%
                             mutate(turns_taken = round.turn))
  
  player.turn <- ifelse(player.turn == num.players, 1, player.turn + 1)
  round.turn <- round.turn + 1
}
end.time <- Sys.time()
run.time <- end.time - start.time
run.time

# these values should all be the same
# active.standings %>%
#   pivot_longer(cols = c("player_1", "player_2", "player_3", "player_4", "player_5", "player_6"),
#                names_to = "player_id",
#                values_to = "total_points") %>%
#   group_by(player_id) %>%
#   summarize(avg_score = mean(total_points))

# let's start by testing what would've happened if P1 knocked on T1

p1t1.knock.results <-
  standings.audit %>%
  filter(turns_taken == 0) %>%
  inner_join(y = standings.audit %>% filter(turns_taken == 6), by = "game_id") %>%
  select(game_id, player_1.x, player_2.y, player_3.y, player_4.y, player_5.y, player_6.y)
colnames(p1t1.knock.results) <- c("game_id", "player_1", "player_2", "player_3", "player_4", "player_5", "player_6")
p1t1.knock.results <-
  p1t1.knock.results %>%
  mutate(p1_survive = ifelse(player_1 <= player_2 &
                               player_1 <= player_3 &
                               player_1 <= player_4 &
                               player_1 <= player_5 &
                               player_1 <= player_6,
                             0, 1))

p1t1.knock.survival <-
  p1t1.knock.results %>%
  group_by(player_1) %>%
  summarize(survival_rate = mean(p1_survive))
