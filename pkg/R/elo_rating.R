

#' Determine the expected probability of player a winning a match, given Elo ratings
#' 
#' @param a Elo rating of player a
#' @param b Elo rating of player b
#' @param ml length of match (if a backgammon game, the minimum winning score)
#' @export
#' @examples 
#' # 50% chance of winning if both players have equal ratings:
#' elo_prob(1500, 1500, 1)
#' 
#' # 87% chance of winning a match to 17 if you're a backgammon expert v average player:
#' elo_prob(1900, 1500, 17)
elo_prob <- function(a, b, ml){
  # 
  # against player b of length ml, with a and b representing their FIBS Elo ratings
  tmp <- 1 - (1 / (10 ^ ((a - b) * sqrt(ml) / 2000) + 1))
  return(tmp)
}   



#' Determine Elo rating of two players given one has won a match
#' 
#' @export
#' @param a Elo rating of player a before match
#' @param b Elo rating of player b before match
#' @param winner whether A or B is the winner 
#' @param ml is match length
#' @param axp total match lengths (experience) of player a until this match
#' @param bxp total match lengths (experience) of player b until this match
#' @details See http://www.fibs.com/ratings.html for formulae
#' @examples 
#' # baptism by fire; a's rating should be 1540.95 after beating a master player:
#' round(elo_rating(a = 1500, b = 1925, ml = 7, axp = 0, bxp = 10000, winner = "a")$a, 2)
#' @author Peter Ellis
elo_rating <- function(a, b, winner = c("a", "b"), ml = 1, axp = 500, bxp = 500){
  winner <- match.arg(winner)
  
  
  # calculate experience-correction multipliers:
  multa <- ifelse(axp < 400, 5 - ((axp + ml) / 100), 1)
  multb <- ifelse(bxp < 400, 5 - ((axp + ml) / 100), 1)
  
  # probability of A winning, using fibs_p function defined earlier:
  winproba <- elo_prob(a = a, b = b, ml = ml)
  
  # match value (points to be distributed between the two players):
  matchvalue <- 4 * sqrt(ml)
  
  # who gets them?:
  if(winner == "b"){
    a <- a - matchvalue * winproba * multa
    b <- b + matchvalue * winproba * multb
  } else {
    a <- a + matchvalue * (1 - winproba) * multa 
    b <- b - matchvalue * (1- winproba) * multb
  }
  
  return(list(a = a, b = b, axp = axp + ml, bxp = bxp + ml))
}

