library(R6)

# gameboard and decks -----------------------------------------------------
# Do not change this code

gameboard <- data.frame(
  space = 1:40, 
  title = c(
    "Go", "Mediterranean Avenue", "Community Chest", "Baltic Avenue",
    "Income Tax", "Reading Railroad", "Oriental Avenue", "Chance",
    "Vermont Avenue", "Connecticut Avenue", "Jail", "St. Charles Place",
    "Electric Company", "States Avenue", "Virginia Avenue",
    "Pennsylvania Railroad", "St. James Place", "Community Chest",
    "Tennessee Avenue", "New York Avenue", "Free Parking",
    "Kentucky Avenue", "Chance", "Indiana Avenue", "Illinois Avenue",
    "B & O Railroad", "Atlantic Avenue", "Ventnor Avenue", "Water Works",
    "Marvin Gardens", "Go to jail", "Pacific Avenue",
    "North Carolina Avenue", "Community Chest", "Pennsylvania Avenue",
    "Short Line Railroad", "Chance", "Park Place", "Luxury Tax",
    "Boardwalk"), stringsAsFactors = FALSE)
chancedeck <- data.frame(
  index = 1:15, 
  card = c(
    "Advance to Go", "Advance to Illinois Ave.",
    "Advance to St. Charles Place", "Advance token to nearest Utility",
    "Advance token to the nearest Railroad",
    "Take a ride on the Reading Railroad",
    "Take a walk on the Boardwalk", "Go to Jail", "Go Back 3 Spaces",
    "Bank pays you dividend of $50", "Get out of Jail Free",
    "Make general repairs on all your property", "Pay poor tax of $15",
    "You have been elected Chairman of the Board", 
    "Your building loan matures"), stringsAsFactors = FALSE)
communitydeck <- data.frame(
  index = 1:16, 
  card = c(
    "Advance to Go", "Go to Jail",
    "Bank error in your favor. Collect $200", "Doctor's fees Pay $50",
    "From sale of stock you get $45", "Get Out of Jail Free",
    "Grand Opera Night Opening", "Xmas Fund matures", "Income tax refund",
    "Life insurance matures. Collect $100", "Pay hospital fees of $100",
    "Pay school tax of $150", "Receive for services $25",
    "You are assessed for street repairs",
    "You have won second prize in a beauty contest",
    "You inherit $100"), stringsAsFactors = FALSE)

# RandomDice class --------------------------------------------------------
# Do not change this code

RandomDice <- R6Class(
  classname = "RandomDice",
  public = list(
    verbose = NA,
    initialize = function(verbose = FALSE){
      stopifnot(is.logical(verbose))
      self$verbose = verbose
    },
    roll = function() {
      outcome <- sample(1:6, size = 2, replace = TRUE)
      if(self$verbose){
        cat("Dice Rolled:", outcome[1], outcome[2], "\n")
      }
      outcome
    }
  )
)

# Preset Dice -------------------------------------------------------------
# Do not change this code

PresetDice <- R6Class(
  classname = "PresetDice",
  public = list(
    verbose = NA,
    preset_rolls = double(0),
    position = 1,
    initialize = function(rolls, verbose = FALSE){
      stopifnot(is.logical(verbose))
      stopifnot(is.numeric(rolls))
      self$preset_rolls = rolls
      self$verbose = verbose
    },
    roll = function(){
      if(self$position > length(self$preset_rolls)){
        stop("You have run out of predetermined dice outcomes.")
      }
      outcome <- c(self$preset_rolls[self$position], 
                   self$preset_rolls[self$position + 1])
      self$position <- self$position + 2
      if(self$verbose){
        cat("Dice Rolled:", outcome[1], outcome[2], "\n")
      }
      outcome
    }
  )
)


# Chance and Community Decks ----------------------------------------------
# Do not change this code

# This R6 class object shuffles the card deck when initialized.
# It has one method $draw(), which will draw a card from the deck.
# If all the cards have been drawn (position = deck length), then it will
# shuffle the cards again.
# The verbose option cats the card that is drawn on to the screen.
CardDeck <- R6Class(
  classname = "CardDeck",
  public = list(
    verbose = NA,
    deck_order = double(0), 
    deck = data.frame(),
    position = 1,
    initialize = function(deck, verbose = FALSE){
      stopifnot(is.data.frame(deck),
                is.numeric(deck[[1]]),
                is.character(deck[[2]]))
      self$deck_order <- sample(length(deck[[1]]))
      self$verbose <- verbose
      self$deck <- deck
    },
    draw = function(){
      if(self$position > length(self$deck_order)){
        # if we run out of cards, shuffle deck
        # and reset the position to 1
        if(self$verbose){
          cat("Shuffling deck.\n")
        }
        self$deck_order <- sample(length(self$deck[[1]]))
        self$position <- 1
      }
      outcome <- c(self$deck_order[self$position]) # outcome is the value at position
      self$position <- self$position + 1 # advance the position by 1
      if(self$verbose){
        cat("Card:", self$deck[outcome, 2], "\n")
      }
      outcome # return the outcome
    }
  )
)


# R6 Class SpaceTracker ---------------------------------------------------
# Do not change this code

SpaceTracker <- R6Class(
  classname = "SpaceTracker",
  public = list(
    counts = rep(0, 40),
    verbose = TRUE,
    tally = function(x){
      self$counts[x] <- self$counts[x] + 1
      if(self$verbose){
        cat("Added tally to ", x, ": ", gameboard$title[x], ".\n", sep = "")
      }
    },
    initialize = function(verbose){
      self$verbose <- verbose
    }
  )
)


# R6 Class Player ---------------------------------------------------------
## You'll need to expand on this

Player <- R6Class(
  classname = "Player",
  public = list(
    pos = 1,
    verbose = TRUE,
    in_jail = FALSE,
    was_in_jail = FALSE,
    rounds_in_jail = 0,
    double_straight = 0,
    move_fwd = function(n){
      if(self$verbose){
        cat("Player starts at ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
        cat("Player moves forward ", n, ".\n", sep = "")
      }
      self$pos <- self$pos + n
      if(self$pos > 40){
        self$pos <- self$pos - 40
      }
      if(self$verbose){
        cat("Player is now at ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
      }
    },
    initialize = function(verbose = FALSE, pos = 1) {
      self$verbose <- verbose
      self$pos <- pos
    },
    jail_turn = function() {
      self$rounds_in_jail <- self$rounds_in_jail + 1
    },
    double_turn = function() {
      self$double_straight <- self$double_straight + 1
    },
    chance_movement = function(card) {
      if (card == "Go to Jail") {
        self$pos <- 11
        self$in_jail <- TRUE
        if(self$verbose){
          cat("Player goes to jail.\n")
        }
      } else if (card == "Advance to Go") {
        self$pos <- 1
        if(self$verbose){
          cat("Player moves to ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
          # Player moves to 1: Go."
        }
      } else if (card == "Advance to Illinois Ave.") {
        self$pos <- 25
        if(self$verbose){
          cat("Player moves to ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
          # Player moves to 25: Illinois Ave."
        }
      } else if (card == "Advance to St. Charles Place") {
        self$pos <- 12
        if(self$verbose){
          cat("Player moves to ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
        }
      } else if (card == "Advance token to nearest Utility") {
        if (self$pos == 8 | self$pos == 37) {
          self$pos <- 13
        } else { # self$pos == 23
          self$pos <- 29
        }
        if(self$verbose){
          cat("Player moves to ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
        }
      } else if (card == "Advance token to the nearest Railroad") {
        if (self$pos == 8) {
          self$pos <- 16
        } else if (self$pos == 23) {
          self$pos <- 26
        } else { # self$pos == 37
          self$pos <- 6
        }
        if(self$verbose){
          cat("Player moves to ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")

        }
      } else if (card == "Take a ride on the Reading Railroad") {
        self$pos <- 6
        if(self$verbose){
          cat("Player moves to ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
        }
      } else if (card == "Take a walk on the Boardwalk") {
        self$pos <- 40
        if(self$verbose){
          cat("Player moves to ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
        }
      } else if (card == "Go Back 3 Spaces") {
        self$pos <- self$pos - 3
        if (self$pos <= 0) {
          self$pos <- self$pos + 40
        } 
        if(self$verbose){
          cat("Player moves to ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
        }
      }
      self$pos
    }
  )
)


# VERY BASIC turn taking example ------------------------------------------
# You will need to expand this
# You can write helper function if you want

take_turn <- function(player, spacetracker){
  
  dice_rolls <- dice$roll() 
  double <- dice_rolls[1] == dice_rolls [2]
  player$double_straight <- 0
  
  # when the player is already in jail:
  player$was_in_jail <- player$in_jail
  if (player$was_in_jail){
    if (double| player$rounds_in_jail == 2){
      player$in_jail <- FALSE
      if (player$verbose & double){
        cat("In jail but rolled doubles.\n")
      } else if (player$verbose & player$rounds_in_jail == 2){
        cat("Player's third turn in jail. Player must exit jail.\n")
      } 
      player$rounds_in_jail <- 0
      if (player$verbose) {
        cat("Player exits jail.\n")
      }
      player$move_fwd(sum(dice_rolls))
      spacetracker$tally(player$pos)
    } else {
      player$jail_turn() 
      if(player$verbose){
        cat("Player stays in jail.\n")
      }
      spacetracker$tally(player$pos)
    }
  } 
  
  while (double & !player$was_in_jail) {
    player$double_turn()
    if (player$verbose) {
      cat("Doubles count is now ", player$double_straight, ".\n", sep = "")
    }
    if (player$double_straight < 3) {
      player$move_fwd(sum(dice_rolls))
      if (player$pos %in% c(3, 18, 34)) {  # Community Chest spaces
        spacetracker$tally(player$pos)
        if (player$verbose) {
          cat("Draw a Community Chest card.\n")
        }
        card_index <- community$draw()
        card <- communitydeck[card_index, "card"]
        if (card == "Advance to Go") {
          player$pos <- 1
          if(player$verbose){
            cat("Player moves to", player$pos, ": ", gameboard$title[player$pos], ".\n", sep = "")
          }
          spacetracker$tally(player$pos)
        } else if (card == " Go to Jail") {
          player$pos <- 11
          player$in_jail <- TRUE
          if(player$verbose){
            cat("Player goes to jail.\n")
          }
          spacetracker$tally(player$pos)
          break
        } 
      } else if (player$pos %in% c(8, 23, 37)) {  # Chance spaces
        spacetracker$tally(player$pos)
        if (player$verbose) {
          cat("Draw a Chance card.\n")
        }
        card_index <- chance$draw()  
        card <- chancedeck[card_index, "card"] 
        player$chance_movement(card)
        if (card %in% chancedeck$card[1:9]) { # action Chance cards
          spacetracker$tally(player$pos)
        }
        if (card %in% chancedeck$card[8]) { # "Go to Jail" card
          break
        }
      } else if (player$pos == 31) { # land in go to jail 
        player$pos <- 11 
        player$in_jail <- TRUE
        if(player$verbose){
          cat("Player goes to jail.\n")
        }
        spacetracker$tally(player$pos)
        break
      } else {
        spacetracker$tally(player$pos)
      }
      if (player$verbose) {
        cat("\nPlayer rolled doubles, so they take another turn.\n")
      }
      dice_rolls <- dice$roll() 
      double <- dice_rolls[1] == dice_rolls [2]
    } else {  # player$double_straight == 3
      if (player$verbose) {
        cat("Player goes to jail.\n")
      }
      player$double_straight <- 0
      player$pos <- 11
      player$in_jail <- TRUE
      spacetracker$tally(player$pos)
      break
    }
  }
  
  if (!double & !player$was_in_jail) {
    player$move_fwd(sum(dice_rolls))
  }
  
  # if player lands on "Go to jail"
  if (player$pos == 31) {
    player$pos <- 11 
    player$in_jail <- TRUE
    if(player$verbose){
      cat("Player goes to jail.\n")
    }
    spacetracker$tally(player$pos)
  } else if (player$pos %in% c(3, 18, 34)) {  # Community Chest spaces
    spacetracker$tally(player$pos)
    if (player$verbose) {
      cat("Draw a Community Chest card.\n")
    }
    card_index <- community$draw()
    card <- communitydeck[card_index, "card"]
    if (card == communitydeck$card[1]) { # "Advance to Go" card
      player$pos <- 1
      if(player$verbose){
        cat("Player moves to", player$pos, ": ", gameboard$title[player$pos], ".\n", sep = "")
      }
      spacetracker$tally(player$pos)
    } else if (card == communitydeck$card[2]) { # "Go to Jail" card
      player$pos <- 11
      player$in_jail <- TRUE
      if(player$verbose){
        cat("Player goes to jail.\n")
      }
      spacetracker$tally(player$pos)
    }
  } else if (player$pos %in% c(8, 23, 37)) {  # Chance spaces
    spacetracker$tally(player$pos)
    if (player$verbose) {
      cat("Draw a Chance card.\n")
    }
    card_index <- chance$draw()  
    card <- chancedeck[card_index, "card"]
    player$chance_movement(card)
    if (card %in% chancedeck$card[1:9]) {
      spacetracker$tally(player$pos)
    }
  } else if (player$was_in_jail == FALSE & player$in_jail == FALSE) {
    spacetracker$tally(player$pos)
  }
}


