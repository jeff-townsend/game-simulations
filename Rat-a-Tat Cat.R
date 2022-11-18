library(tidyverse)

## 0-8 have 4 copies each; 9 has 9 copies
## each power card (Peek, Swap, Draw 2) has 3 copies
## total of 54 cards
# create the deck
deck <- data.frame(id = c(1:54),
                   card_type = c(rep("Numbered Card", 45), rep("Power Card", 9)),
                   card_value = c(rep(0:8, each = 4), rep(9, 9), rep(c("Peek", "Swap", "Draw 2"), each = 3)))
players <- 4

shuffled.deck <-
  deck %>%
  mutate(rand = runif(54),
         card_order = rank(rand)) %>%
  arrange(card_order)

hands <-
  data.frame(id = c(1:(4*players)),
             player_id = c(rep(1:players, each = 4)),
             hand_position = c(rep(c("outer left", "inner left", "inner right", "outer right"), players)),
             card_id = shuffled.deck$id[1:(4*players)],
             card_type = shuffled.deck$card_type[1:(4*players)],
             card_value = shuffled.deck$card_value[1:(4*players)]) %>%
  mutate(expected_card_value = as.numeric(ifelse(hand_position %in% c("inner left", "inner right"), 5,
                                                 ifelse(card_type == "Power Card", 5, card_value))))
original.hands <- hands

draw.pile <-
  shuffled.deck[-1:(-4*players),] %>%
  mutate(card_order = rank(card_order))

#######

player.turn <- 1
turn <- 1
total.turns <- ifelse(players == 5, 35, 36)

for(turn in 1:total.turns){
# take the top card from the draw pile
draw.card <-
  draw.pile %>%
  filter(card_order == 1) %>%
  select(id, card_type, card_value) %>%
  rename(card_id = id) %>%
  mutate(expected_card_value = as.numeric(ifelse(card_type == "Power Card", 5, card_value)))

# remove card from the draw pile
draw.pile <-
  draw.pile %>%
  filter(card_order > 1) %>%
  mutate(card_order = card_order - 1)

# evaluate whether to keep or discard drawn card

discard.evaluation <-
  hands %>%
  filter(player_id == player.turn) %>%
  arrange(desc(expected_card_value), desc(hand_position)) %>%
  mutate(card_replacement_order = c(1:4)) %>%
  top_n(n = -1, wt = card_replacement_order) %>%
  select(id, card_id, card_type, card_value, expected_card_value) %>%
  union_all(draw.card) %>%
  mutate(id = max(id, na.rm = TRUE)) %>%
  arrange(expected_card_value) %>%
  mutate(evaluation_order = c(1:2))

hands <-
  hands %>%
  left_join(discard.evaluation %>% filter(evaluation_order == 1), by = "id") %>%
  mutate(card_id = ifelse(is.na(card_id.y), card_id.x, card_id.y),
         card_type = ifelse(is.na(card_id.y), card_type.x, card_type.y),
         card_value = ifelse(is.na(card_id.y), card_value.x, card_value.y),
         expected_card_value = ifelse(is.na(card_id.y), expected_card_value.x, expected_card_value.y)) %>%
  select(id, player_id, hand_position, card_id, card_type, card_value, expected_card_value)

player.turn <- ifelse(player.turn == players, 1, player.turn + 1)
turn <- turn + 1
}

#######

final.power.cards <-
  hands %>%
  select(id, card_type, card_value) %>%
  filter(card_type == "Power Card")

count.final.power.cards <- as.numeric(nrow(final.power.cards))

replacement.numbered.cards <-
  draw.pile %>%
  filter(card_type == "Numbered Card") %>%
  top_n(n = -count.final.power.cards, wt = card_order) %>%
  rename(card_id = id)

power.replacements <- data.frame(id = final.power.cards$id,
                                 replacement_card_value = replacement.numbered.cards$card_value)

final.hands <-
  hands %>%
  left_join(power.replacements, by = "id") %>%
  mutate(card_value = ifelse(card_type == "Power Card", replacement_card_value, card_value)) %>%
  select(-replacement_card_value)

hand.evaluations <-
  final.hands %>%
  group_by(player_id) %>%
  summarize(score = sum(as.numeric(card_value)))
