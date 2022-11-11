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

hands <- data.frame(id = c(1:(4*players)),
                    player_id = c(rep(1:players, each = 4)),
                    hand_position = c(rep(c("outer left", "inner left", "inner right", "outer right"), players)),
                    card_id = shuffled.deck$id[1:(4*players)],
                    card_type = shuffled.deck$card_type[1:(4*players)],
                    card_value = shuffled.deck$card_value[1:(4*players)])

draw.pile <-
  shuffled.deck[-1:(-4*players),] %>%
  mutate(card_order = rank(card_order))

#######

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
