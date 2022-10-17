rm(list = ls())

library(tidyverse)

### Definitions ----
## Deck
french_deck <- c("2h", "3h", "4h", "5h", "6h", "7h", "8h", "9h", "Th", "Jh", "Qh", "Kh", "Ah",
                 "2d", "3d", "4d", "5d", "6d", "7d", "8d", "9d", "Td", "Jd", "Qd", "Kd", "Ad",
                 "2s", "3s", "4s", "5s", "6s", "7s", "8s", "9s", "Ts", "Js", "Qs", "Ks", "As",
                 "2c", "3c", "4c", "5c", "6c", "7c", "8c", "9c", "Tc", "Jc", "Qc", "Kc", "Ac")

## Hand lookup table
hand_lookup <- expand.grid(french_deck, french_deck)
names(hand_lookup) <- c("card_1", "card_2")
hand_lookup <- hand_lookup[hand_lookup$card_1 != hand_lookup$card_2,]
hand_lookup$suit <- ifelse(substr(hand_lookup$card_1, 2, 2) == substr(hand_lookup$card_2, 2, 2), "s", "o")
hand_lookup$hand <- paste(substr(hand_lookup$card_1, 1, 1), substr(hand_lookup$card_2, 1, 1), hand_lookup$suit, sep = "")

# Hard-coded according to https://www.gamblingsites.org/poker/texas-holdem/starting-hand-rankings/ (10 player)
hand_lookup$win_percentage_10p[hand_lookup$hand == "AAo"] <- 0.31
hand_lookup$win_percentage_10p[hand_lookup$hand == "KKo"] <- 0.26
hand_lookup$win_percentage_10p[hand_lookup$hand == "QQo"] <- 0.22
hand_lookup$win_percentage_10p[hand_lookup$hand == "AKs" | hand_lookup$hand == "KAs"] <- 0.202
hand_lookup$win_percentage_10p[hand_lookup$hand == "JJo"] <- 0.191
hand_lookup$win_percentage_10p[hand_lookup$hand == "AQs" | hand_lookup$hand == "QAs"] <- 0.187
hand_lookup$win_percentage_10p[hand_lookup$hand == "KQs" | hand_lookup$hand == "QKs"] <- 0.181
hand_lookup$win_percentage_10p[hand_lookup$hand == "AJs" | hand_lookup$hand == "JAs"] <- 0.175
hand_lookup$win_percentage_10p[hand_lookup$hand == "KJs" | hand_lookup$hand == "JKs"] <- 0.171
hand_lookup$win_percentage_10p[hand_lookup$hand == "TTo"] <- 0.168
hand_lookup$win_percentage_10p[hand_lookup$hand == "AKo" | hand_lookup$hand == "KAo"] <- 0.167
hand_lookup$win_percentage_10p[hand_lookup$hand == "ATs" | hand_lookup$hand == "TAs"] <- 0.16601
hand_lookup$win_percentage_10p[hand_lookup$hand == "QJs" | hand_lookup$hand == "JQs"] <- 0.166
hand_lookup$win_percentage_10p[hand_lookup$hand == "KTs" | hand_lookup$hand == "TKs"] <- 0.161
hand_lookup$win_percentage_10p[hand_lookup$hand == "QTs" | hand_lookup$hand == "TQs"] <- 0.15801
hand_lookup$win_percentage_10p[hand_lookup$hand == "JTs" | hand_lookup$hand == "TJs"] <- 0.158
hand_lookup$win_percentage_10p[hand_lookup$hand == "99o"] <- 0.153
hand_lookup$win_percentage_10p[hand_lookup$hand == "AQo" | hand_lookup$hand == "QAo"] <- 0.149
hand_lookup$win_percentage_10p[hand_lookup$hand == "A9s" | hand_lookup$hand == "9As"] <- 0.146
hand_lookup$win_percentage_10p[hand_lookup$hand == "KQo" | hand_lookup$hand == "QKo"] <- 0.144
hand_lookup$win_percentage_10p[hand_lookup$hand == "88o"] <- 0.14201
hand_lookup$win_percentage_10p[hand_lookup$hand == "K9s" | hand_lookup$hand == "9Ks"] <- 0.142
hand_lookup$win_percentage_10p[hand_lookup$hand == "T9s" | hand_lookup$hand == "9Ts"] <- 0.141
hand_lookup$win_percentage_10p[hand_lookup$hand == "A8s" | hand_lookup$hand == "8As"] <- 0.139
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q9s" | hand_lookup$hand == "9Qs"] <- 0.13801
hand_lookup$win_percentage_10p[hand_lookup$hand == "J9s" | hand_lookup$hand == "9Js"] <- 0.138
hand_lookup$win_percentage_10p[hand_lookup$hand == "AJo" | hand_lookup$hand == "JAo"] <- 0.135
hand_lookup$win_percentage_10p[hand_lookup$hand == "A5s" | hand_lookup$hand == "5As"] <- 0.13402
hand_lookup$win_percentage_10p[hand_lookup$hand == "77o"] <- 0.13401
hand_lookup$win_percentage_10p[hand_lookup$hand == "A7s" | hand_lookup$hand == "7As"] <- 0.134
hand_lookup$win_percentage_10p[hand_lookup$hand == "KJo" | hand_lookup$hand == "JKo"] <- 0.13201
hand_lookup$win_percentage_10p[hand_lookup$hand == "A4s" | hand_lookup$hand == "4As"] <- 0.132
hand_lookup$win_percentage_10p[hand_lookup$hand == "A3s" | hand_lookup$hand == "3As"] <- 0.131
hand_lookup$win_percentage_10p[hand_lookup$hand == "A6s" | hand_lookup$hand == "6As"] <- 0.13
hand_lookup$win_percentage_10p[hand_lookup$hand == "QJo" | hand_lookup$hand == "JQo"] <- 0.129
hand_lookup$win_percentage_10p[hand_lookup$hand == "66o"] <- 0.12801
hand_lookup$win_percentage_10p[hand_lookup$hand == "K8s" | hand_lookup$hand == "8Ks"] <- 0.128
hand_lookup$win_percentage_10p[hand_lookup$hand == "T8s" | hand_lookup$hand == "8Ts"] <- 0.12701
hand_lookup$win_percentage_10p[hand_lookup$hand == "A2s" | hand_lookup$hand == "2As"] <- 0.127
hand_lookup$win_percentage_10p[hand_lookup$hand == "98s" | hand_lookup$hand == "89s"] <- 0.126
hand_lookup$win_percentage_10p[hand_lookup$hand == "J8s" | hand_lookup$hand == "8Js"] <- 0.125
hand_lookup$win_percentage_10p[hand_lookup$hand == "ATo" | hand_lookup$hand == "TAo"] <- 0.12401
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q8s" | hand_lookup$hand == "8Qs"] <- 0.124
hand_lookup$win_percentage_10p[hand_lookup$hand == "K7s" | hand_lookup$hand == "7Ks"] <- 0.12202
hand_lookup$win_percentage_10p[hand_lookup$hand == "KTo" | hand_lookup$hand == "TKo"] <- 0.12201
hand_lookup$win_percentage_10p[hand_lookup$hand == "55o"] <- 0.122
hand_lookup$win_percentage_10p[hand_lookup$hand == "JTo" | hand_lookup$hand == "TJo"] <- 0.121
hand_lookup$win_percentage_10p[hand_lookup$hand == "87s" | hand_lookup$hand == "78s"] <- 0.1201
hand_lookup$win_percentage_10p[hand_lookup$hand == "QTo" | hand_lookup$hand == "TQo"] <- 0.12
hand_lookup$win_percentage_10p[hand_lookup$hand == "44o"] <- 0.11902
hand_lookup$win_percentage_10p[hand_lookup$hand == "22o"] <- 0.11901
hand_lookup$win_percentage_10p[hand_lookup$hand == "33o"] <- 0.119
hand_lookup$win_percentage_10p[hand_lookup$hand == "K6s" | hand_lookup$hand == "6Ks"] <- 0.118
hand_lookup$win_percentage_10p[hand_lookup$hand == "97s" | hand_lookup$hand == "79s"] <- 0.117
hand_lookup$win_percentage_10p[hand_lookup$hand == "K5s" | hand_lookup$hand == "5Ks"] <- 0.116
hand_lookup$win_percentage_10p[hand_lookup$hand == "76s" | hand_lookup$hand == "67s"] <- 0.11501
hand_lookup$win_percentage_10p[hand_lookup$hand == "T7s" | hand_lookup$hand == "7Ts"] <- 0.115
hand_lookup$win_percentage_10p[hand_lookup$hand == "K4s" | hand_lookup$hand == "4Ks"] <- 0.114
hand_lookup$win_percentage_10p[hand_lookup$hand == "K2s" | hand_lookup$hand == "2Ks"] <- 0.11301
hand_lookup$win_percentage_10p[hand_lookup$hand == "K3s" | hand_lookup$hand == "3Ks"] <- 0.113
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q7s" | hand_lookup$hand == "7Qs"] <- 0.11201
hand_lookup$win_percentage_10p[hand_lookup$hand == "86s" | hand_lookup$hand == "68s"] <- 0.112
hand_lookup$win_percentage_10p[hand_lookup$hand == "65s" | hand_lookup$hand == "56s"] <- 0.11101
hand_lookup$win_percentage_10p[hand_lookup$hand == "J7s" | hand_lookup$hand == "7Js"] <- 0.111
hand_lookup$win_percentage_10p[hand_lookup$hand == "54s" | hand_lookup$hand == "45s"] <- 0.10901
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q6s" | hand_lookup$hand == "6Qs"] <- 0.109
hand_lookup$win_percentage_10p[hand_lookup$hand == "75s" | hand_lookup$hand == "57s"] <- 0.10701
hand_lookup$win_percentage_10p[hand_lookup$hand == "96s" | hand_lookup$hand == "69s"] <- 0.107
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q5s" | hand_lookup$hand == "5Qs"] <- 0.106
hand_lookup$win_percentage_10p[hand_lookup$hand == "64s" | hand_lookup$hand == "46s"] <- 0.10403
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q4s" | hand_lookup$hand == "4Qs"] <- 0.10402
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q3s" | hand_lookup$hand == "3Qs"] <- 0.10401
hand_lookup$win_percentage_10p[hand_lookup$hand == "T9o" | hand_lookup$hand == "9To"] <- 0.104
hand_lookup$win_percentage_10p[hand_lookup$hand == "T6s" | hand_lookup$hand == "6Ts"] <- 0.10301
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q2s" | hand_lookup$hand == "2Qs"] <- 0.103
hand_lookup$win_percentage_10p[hand_lookup$hand == "A9o" | hand_lookup$hand == "9Ao"] <- 0.10201
hand_lookup$win_percentage_10p[hand_lookup$hand == "53s" | hand_lookup$hand == "35s"] <- 0.102
hand_lookup$win_percentage_10p[hand_lookup$hand == "85s" | hand_lookup$hand == "58s"] <- 0.10101
hand_lookup$win_percentage_10p[hand_lookup$hand == "J6s" | hand_lookup$hand == "6Js"] <- 0.101
hand_lookup$win_percentage_10p[hand_lookup$hand == "J9o" | hand_lookup$hand == "9Jo"] <- 0.10
hand_lookup$win_percentage_10p[hand_lookup$hand == "K9o" | hand_lookup$hand == "9Ko"] <- 0.09901
hand_lookup$win_percentage_10p[hand_lookup$hand == "J5s" | hand_lookup$hand == "5Js"] <- 0.099
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q9o" | hand_lookup$hand == "9Qo"] <- 0.09801
hand_lookup$win_percentage_10p[hand_lookup$hand == "43s" | hand_lookup$hand == "34s"] <- 0.098
hand_lookup$win_percentage_10p[hand_lookup$hand == "74s" | hand_lookup$hand == "47s"] <- 0.09701
hand_lookup$win_percentage_10p[hand_lookup$hand == "J4s" | hand_lookup$hand == "4Js"] <- 0.097
hand_lookup$win_percentage_10p[hand_lookup$hand == "J3s" | hand_lookup$hand == "3Js"] <- 0.09601
hand_lookup$win_percentage_10p[hand_lookup$hand == "95s" | hand_lookup$hand == "59s"] <- 0.096
hand_lookup$win_percentage_10p[hand_lookup$hand == "J2s" | hand_lookup$hand == "2Js"] <- 0.09501
hand_lookup$win_percentage_10p[hand_lookup$hand == "63s" | hand_lookup$hand == "36s"] <- 0.095
hand_lookup$win_percentage_10p[hand_lookup$hand == "A8o" | hand_lookup$hand == "8Ao"] <- 0.094
hand_lookup$win_percentage_10p[hand_lookup$hand == "52s" | hand_lookup$hand == "25s"] <- 0.093
hand_lookup$win_percentage_10p[hand_lookup$hand == "T5s" | hand_lookup$hand == "5Ts"] <- 0.092
hand_lookup$win_percentage_10p[hand_lookup$hand == "84s" | hand_lookup$hand == "48s"] <- 0.09102
hand_lookup$win_percentage_10p[hand_lookup$hand == "T4s" | hand_lookup$hand == "4Ts"] <- 0.09101
hand_lookup$win_percentage_10p[hand_lookup$hand == "T3s" | hand_lookup$hand == "3Ts"] <- 0.091
hand_lookup$win_percentage_10p[hand_lookup$hand == "42s" | hand_lookup$hand == "24s"] <- 0.0902
hand_lookup$win_percentage_10p[hand_lookup$hand == "T2s" | hand_lookup$hand == "2Ts"] <- 0.0901
hand_lookup$win_percentage_10p[hand_lookup$hand == "98o" | hand_lookup$hand == "89o"] <- 0.09
hand_lookup$win_percentage_10p[hand_lookup$hand == "T8o" | hand_lookup$hand == "8To"] <- 0.08901
hand_lookup$win_percentage_10p[hand_lookup$hand == "A5o" | hand_lookup$hand == "5Ao"] <- 0.089
hand_lookup$win_percentage_10p[hand_lookup$hand == "A7o" | hand_lookup$hand == "7Ao"] <- 0.08801
hand_lookup$win_percentage_10p[hand_lookup$hand == "73s" | hand_lookup$hand == "37s"] <- 0.088
hand_lookup$win_percentage_10p[hand_lookup$hand == "A4o" | hand_lookup$hand == "4Ao"] <- 0.08702
hand_lookup$win_percentage_10p[hand_lookup$hand == "32s" | hand_lookup$hand == "23s"] <- 0.08701
hand_lookup$win_percentage_10p[hand_lookup$hand == "94s" | hand_lookup$hand == "49s"] <- 0.087
hand_lookup$win_percentage_10p[hand_lookup$hand == "93s" | hand_lookup$hand == "39s"] <- 0.085049
hand_lookup$win_percentage_10p[hand_lookup$hand == "J8o" | hand_lookup$hand == "8Jo"] <- 0.08504
hand_lookup$win_percentage_10p[hand_lookup$hand == "A3o" | hand_lookup$hand == "3Ao"] <- 0.08503
hand_lookup$win_percentage_10p[hand_lookup$hand == "62s" | hand_lookup$hand == "26s"] <- 0.08502
hand_lookup$win_percentage_10p[hand_lookup$hand == "92s" | hand_lookup$hand == "29s"] <- 0.08501
hand_lookup$win_percentage_10p[hand_lookup$hand == "K8o" | hand_lookup$hand == "8Ko"] <- 0.085
hand_lookup$win_percentage_10p[hand_lookup$hand == "A6o" | hand_lookup$hand == "6Ao"] <- 0.08401
hand_lookup$win_percentage_10p[hand_lookup$hand == "87o" | hand_lookup$hand == "78o"] <- 0.084
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q8o" | hand_lookup$hand == "8Qo"] <- 0.083
hand_lookup$win_percentage_10p[hand_lookup$hand == "83s" | hand_lookup$hand == "38s"] <- 0.08201
hand_lookup$win_percentage_10p[hand_lookup$hand == "A2o" | hand_lookup$hand == "2Ao"] <- 0.082
hand_lookup$win_percentage_10p[hand_lookup$hand == "82s" | hand_lookup$hand == "28s"] <- 0.081
hand_lookup$win_percentage_10p[hand_lookup$hand == "97o" | hand_lookup$hand == "79o"] <- 0.08
hand_lookup$win_percentage_10p[hand_lookup$hand == "72s" | hand_lookup$hand == "27s"] <- 0.07902
hand_lookup$win_percentage_10p[hand_lookup$hand == "76o" | hand_lookup$hand == "67o"] <- 0.07901
hand_lookup$win_percentage_10p[hand_lookup$hand == "K7o" | hand_lookup$hand == "7Ko"] <- 0.079
hand_lookup$win_percentage_10p[hand_lookup$hand == "65o" | hand_lookup$hand == "56o"] <- 0.076
hand_lookup$win_percentage_10p[hand_lookup$hand == "T7o" | hand_lookup$hand == "7To"] <- 0.07501
hand_lookup$win_percentage_10p[hand_lookup$hand == "K6o" | hand_lookup$hand == "6Ko"] <- 0.075
hand_lookup$win_percentage_10p[hand_lookup$hand == "86o" | hand_lookup$hand == "68o"] <- 0.07401
hand_lookup$win_percentage_10p[hand_lookup$hand == "54o" | hand_lookup$hand == "45o"] <- 0.074
hand_lookup$win_percentage_10p[hand_lookup$hand == "K5o" | hand_lookup$hand == "5Ko"] <- 0.07101
hand_lookup$win_percentage_10p[hand_lookup$hand == "J7o" | hand_lookup$hand == "7Jo"] <- 0.071
hand_lookup$win_percentage_10p[hand_lookup$hand == "75o" | hand_lookup$hand == "57o"] <- 0.0702
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q7o" | hand_lookup$hand == "7Qo"] <- 0.0701
hand_lookup$win_percentage_10p[hand_lookup$hand == "K4o" | hand_lookup$hand == "4Ko"] <- 0.07
hand_lookup$win_percentage_10p[hand_lookup$hand == "K3o" | hand_lookup$hand == "3Ko"] <- 0.069
hand_lookup$win_percentage_10p[hand_lookup$hand == "K2o" | hand_lookup$hand == "2Ko"] <- 0.06802
hand_lookup$win_percentage_10p[hand_lookup$hand == "96o" | hand_lookup$hand == "69o"] <- 0.06801
hand_lookup$win_percentage_10p[hand_lookup$hand == "64o" | hand_lookup$hand == "46o"] <- 0.068
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q6o" | hand_lookup$hand == "6Qo"] <- 0.06601
hand_lookup$win_percentage_10p[hand_lookup$hand == "53o" | hand_lookup$hand == "35o"] <- 0.066
hand_lookup$win_percentage_10p[hand_lookup$hand == "85o" | hand_lookup$hand == "58o"] <- 0.06302
hand_lookup$win_percentage_10p[hand_lookup$hand == "T6o" | hand_lookup$hand == "6To"] <- 0.06301
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q5o" | hand_lookup$hand == "5Qo"] <- 0.063
hand_lookup$win_percentage_10p[hand_lookup$hand == "43o" | hand_lookup$hand == "34o"] <- 0.062
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q4o" | hand_lookup$hand == "4Qo"] <- 0.06101
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q3o" | hand_lookup$hand == "3Qo"] <- 0.061
hand_lookup$win_percentage_10p[hand_lookup$hand == "Q2o" | hand_lookup$hand == "2Qo"] <- 0.0601
hand_lookup$win_percentage_10p[hand_lookup$hand == "74o" | hand_lookup$hand == "47o"] <- 0.06
hand_lookup$win_percentage_10p[hand_lookup$hand == "J6o" | hand_lookup$hand == "6Jo"] <- 0.059
hand_lookup$win_percentage_10p[hand_lookup$hand == "63o" | hand_lookup$hand == "36o"] <- 0.057
hand_lookup$win_percentage_10p[hand_lookup$hand == "J5o" | hand_lookup$hand == "5Jo"] <- 0.05602
hand_lookup$win_percentage_10p[hand_lookup$hand == "95o" | hand_lookup$hand == "59o"] <- 0.05601
hand_lookup$win_percentage_10p[hand_lookup$hand == "52o" | hand_lookup$hand == "25o"] <- 0.056
hand_lookup$win_percentage_10p[hand_lookup$hand == "J4o" | hand_lookup$hand == "4Jo"] <- 0.055
hand_lookup$win_percentage_10p[hand_lookup$hand == "J3o" | hand_lookup$hand == "3Jo"] <- 0.05401
hand_lookup$win_percentage_10p[hand_lookup$hand == "42o" | hand_lookup$hand == "24o"] <- 0.054
hand_lookup$win_percentage_10p[hand_lookup$hand == "J2o" | hand_lookup$hand == "2Jo"] <- 0.05301
hand_lookup$win_percentage_10p[hand_lookup$hand == "84o" | hand_lookup$hand == "48o"] <- 0.053
hand_lookup$win_percentage_10p[hand_lookup$hand == "T5o" | hand_lookup$hand == "5To"] <- 0.052
hand_lookup$win_percentage_10p[hand_lookup$hand == "T4o" | hand_lookup$hand == "4To"] <- 0.0502
hand_lookup$win_percentage_10p[hand_lookup$hand == "32o" | hand_lookup$hand == "23o"] <- 0.0501
hand_lookup$win_percentage_10p[hand_lookup$hand == "T3o" | hand_lookup$hand == "3To"] <- 0.05
hand_lookup$win_percentage_10p[hand_lookup$hand == "73o" | hand_lookup$hand == "37o"] <- 0.04901
hand_lookup$win_percentage_10p[hand_lookup$hand == "T2o" | hand_lookup$hand == "2To"] <- 0.049
hand_lookup$win_percentage_10p[hand_lookup$hand == "62o" | hand_lookup$hand == "26o"] <- 0.04701
hand_lookup$win_percentage_10p[hand_lookup$hand == "94o" | hand_lookup$hand == "49o"] <- 0.047
hand_lookup$win_percentage_10p[hand_lookup$hand == "93o" | hand_lookup$hand == "39o"] <- 0.04501
hand_lookup$win_percentage_10p[hand_lookup$hand == "92o" | hand_lookup$hand == "29o"] <- 0.045
hand_lookup$win_percentage_10p[hand_lookup$hand == "83o" | hand_lookup$hand == "38o"] <- 0.043
hand_lookup$win_percentage_10p[hand_lookup$hand == "82o" | hand_lookup$hand == "28o"] <- 0.042
hand_lookup$win_percentage_10p[hand_lookup$hand == "72o" | hand_lookup$hand == "27o"] <- 0.04

temp <- data.frame("win_percentage_10p" = unique(hand_lookup$win_percentage_10p)) %>% arrange(desc(win_percentage_10p))
temp$rank <- 1:nrow(temp)
hand_lookup <- merge(hand_lookup, temp, by = "win_percentage_10p", all.x = TRUE); rm(temp)
hand_lookup <- select(hand_lookup, card_1, card_2, hand, win_percentage_10p, rank)

  
#' Hand Rank
#' 
#' @description `hand_rank` returns the ranks of all hands passed as arguments.
#'
#' @param community_cards can be either 3, 4, or 5 community cards already flipped or `NULL` if pre-flop
#' @param player_cards is a named list of vectors of player's cards
#' 
#' @returns named list containing hand ranks
#' 
hand_rank <- function(community_cards = NULL, player_cards, hand_lookup = hand_lookup){
  if (is.null(player_cards)){
    stop("No player cards specified")
  }
  
  if (!is.list(player_cards) | length(player_cards) == 0){
    stop("Invalid format for player_cards. Must pass named list")
  }
  
  if (length(player_cards) < 2){
    stop("Cannot rank less than 2 players")
  }
  
  if (is.null(community_cards)){
    # Pre-flop ranking
    if (length(unique(unlist(player_cards))) < length(unlist(player_cards))){
      stop("At least two players have the same cards. Someone is cheating")
    }
    
    if (any(lapply(player_cards, length) > 2) | any(lapply(player_cards, length) < 2)){
      stop("Someone has more or less than 2 cards")
    }
    
    return(lapply(player_cards, function(x){hand_lookup$rank[hand_lookup$card_1 == x[1] & hand_lookup$card_2 == x[2]]}))
  }
  
  if (length(community_cards) < 3){
    stop("Specify at least 3 community cards or pass NULL")
  }
  
  if (length(community_cards) == 3){
    # TODO: flop ranking
    return(0)
  }
  
  if (length(community_cards) == 4){
    # TODO: turn ranking
    return(0)
  }
  
  if (length(community_cards) == 5){
    # TODO: river ranking
    return(0)
  }
  
  if (length(community_cards) > 5){
    stop("You cannot specify more than 5 community cards")
  }
}




hand_rank(player_cards = list("Player 1" = c("Ah", "As"), "Player 2" = c("Ad", "Kh"), "Player 3" = c("Qh", "Qs")))



### MC simulation for n players for k hands
set.seed(69)
n_players <- 10
k_hands <- 1e4

sample_hands <- data.frame()
for (i in 1:k_hands){
  sample_hands <- rbind(sample_hands, sample(french_deck, 2*n_players + 5, replace = FALSE))  
}
names(sample_hands) <- c(paste0("P", 1:n_players, "_c1"), paste0("P", 1:n_players, "_c2"), paste0("com", 1:5))

nlhe <- function(n_players = 10, k_hands = 1e4){
  NULL
}

n.highcard <- 0
n.onepair <- 0
n.twopair <- 0
n.set <- 0
n.straight <- 0
n.flush <- 0
n.boat <- 0
n.quads <- 0
n.straightflush <- 0
n.royal <- 0















