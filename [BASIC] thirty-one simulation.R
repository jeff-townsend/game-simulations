library(tidyverse)

####### set up the game
## create a standard deck of cards
# ignoring suit because it doesn't matter for this game
deck <- data.frame(id = c(1:52),
                   card_value = c(rep(c(2:10), each = 4), rep("J", 4), rep("Q", 4), rep("K", 4), rep("A", 4)))
deck$card_points <- as.numeric(ifelse(deck$card_value == "A", 11,
                                      ifelse(deck$card_value %in% c("J", "Q", "K"), 10, deck$card_value)))

num.players <- 6

# shuffle the deck
shuffled.deck <-
  deck %>%
  mutate(rand = runif(52),
         card_order = rank(rand)) %>%
  arrange(card_order)

# deal the cards
player.cards <-
  data.frame(id = c(1:(3*num.players)),
             player_id = c(rep(1:num.players, each = 3)),
             card_id = shuffled.deck$id[1:(3*num.players)],
             card_value = shuffled.deck$card_value[1:(3*num.players)],
             card_points = shuffled.deck$card_points[1:(3*num.players)])

# keep score
players <-
  player.cards %>%
  group_by(player_id) %>%
  summarize(card_points1 = min(card_points),
            card_points2 = median(card_points),
            card_points3 = max(card_points),
            total_points = sum(card_points)) %>%
  rename(id = player_id)

# the first player can choose either a faceup card or a new card at random
# faceup.card <- data.frame(card_id = shuffled.deck$id[num.players*3+1],
#                           card_value = shuffled.deck$card_value[num.players*3+1],
#                           card_points = shuffled.deck$card_points[num.players*3+1])
# faceup.card <-
#   faceup.card %>%
#   mutate(expected_card_value = ifelse(card_type == "Power Card", NA, as.numeric(card_value)))

# the remaining cards
draw.pile <-
  shuffled.deck[-1:(-3*num.players),] %>%
  select(-rand) %>%
  mutate(card_order = rank(card_order)) %>%
  rename(card_id = id)

####### now we play

player.turn <- 1
round.turn <- 1
total.turns <- floor(nrow(draw.pile) / num.players) * num.players

for(round.turn in 1:total.turns){
  # take the top card from the draw pile
  draw.card <-
    draw.pile %>%
    filter(card_order == 1) %>%
    select(card_id, card_value, card_points)
  
  taken.card <- draw.card
  # taken.card <- data.frame(card_id = ifelse(faceup.card$card_points >= 8, faceup.card$card_id, draw.card$card_id)),
  #                          card_value = ifelse(faceup.card$card_points >= 8, faceup.card$card_value, draw.card$card_value)),
  #                          card_points = ifelse(faceup.card$card_points >= 8, faceup.card$card_points, draw.card$card_points)))
  
  
  # remove card from the draw pile, if applicable
  draw.pile <-
    draw.pile %>%
    filter(card_order >= 2) %>%
    mutate(card_order = card_order - 1)
  
  # evaluate whether to keep or discard drawn card
  
  discard.evaluation <-
    player.cards %>%
    filter(player_id == player.turn) %>%
    arrange(card_points) %>%
    mutate(card_replacement_order = c(1:3)) %>%
    top_n(n = -1, wt = card_replacement_order) %>%
    select(id, card_id, card_value, card_points) %>%
    union_all(taken.card) %>%
    mutate(id = max(id, na.rm = TRUE)) %>%
    arrange(desc(card_points)) %>%
    mutate(evaluation_order = c(1:2))
  
  player.cards <-
    player.cards %>%
    left_join(discard.evaluation %>% filter(evaluation_order == 1), by = "id") %>%
    mutate(card_id = ifelse(is.na(card_id.y), card_id.x, card_id.y),
           card_value = ifelse(is.na(card_id.y), card_value.x, card_value.y),
           card_points = ifelse(is.na(card_id.y), card_points.x, card_points.y)) %>%
    select(id, player_id, card_id, card_value, card_points)
  
  players <-
    player.cards %>%
    group_by(player_id) %>%
    summarize(card_points1 = min(card_points),
              card_points2 = median(card_points),
              card_points3 = max(card_points),
              total_points = sum(card_points)) %>%
    rename(id = player_id)
  
  player.turn <- ifelse(player.turn == num.players, 1, player.turn + 1)
  round.turn <- round.turn + 1
}
