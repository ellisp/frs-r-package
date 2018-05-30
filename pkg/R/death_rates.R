


#' French death rates 2015
#' 
#' Death rates per thousand for France in 2015
#' 
#' @source https://www.ined.fr/en/everything_about_population/data/france/deaths-causes-mortality/mortality-rates-sex-age/ on 27 May 2018
#' @export
#' @examples
#' life_expectancy(data = french_death_rates_2015[ , c("age", "female")])
#' life_expectancy(age = french_death_rates_2015$age, rate = french_death_rates_2015$male)
french_death_rates_2015 <- data.frame(
    age = c(0, seq(from = 2.5, to = 67.5, by = 5), 75, 85, 100, 150),
    male = c(3.4, 0.3, 0.1, 0.1, 0.3, 0.6, 0.7, 0.9, 1.2, 1.8, 2.9, 4.5, 7.5, 11.1, 14.9, 26.7, 78, 217, 1000),
    female = c(2.8, 0.2, 0.1, 0.1, 0.2, 0.2, 0.2, 0.3, 0.5, 0.9, 1.5, 2.4, 3.5, 4.8, 6.7, 13.9, 51.1, 179, 1000))



#' Life expectancy
#' 
#' Calculate period life expectancy at birth, given a set of death rates at various ages
#' 
#' @export
#' @importFrom stats approx
#' @details Period LEB is the mean length of life of a hypothetical cohort assumed to be exposed, from birth through death, 
#' to the mortality rates observed at a given year.  So for the person born in year X, their death rate at age 10 is
#' assumed to be the death rate as it is in year X; there is no attempt to forecast the death rate for ten year olds
#' in year X + 10.  Hence if death rates are in decline, period life expectancy at birth will be an underestimate of
#' actual cohort life expectancy which can be measured when all members of the cohort are dead (and vice versa if death
#' rates are increasing).
#' 
#' @references https://en.wikipedia.org/wiki/Life_expectancy
#' @param age vector of ages in years
#' @param rate death rate in deaths per 1000 people alive at the given age.  Must be the same length as age.
#' @param data optionally, a data frame or matrix where the first column is used as age and the second column as rate
#' @examples
#' life_expectancy(data = french_death_rates_2015[ , c("age", "female")])
#' life_expectancy(age = french_death_rates_2015$age, rate = french_death_rates_2015$male)
life_expectancy <- function(age = data[, 1], rate = data[ , 2], data = NULL){
  if(length(age) != length(rate) | 
     !class(age) %in% c("numeric", "integer") | 
     !class(rate) %in% c("numeric", "integer")){
    stop("age and rate should be integer or numeric vectors of the same length")
  }
  if(max(rate) != 1000) {
    stop("The highest value of rate should be exactly 1000, indicating the age at which everyone is guaranteed to die at or before.")
  }
  dr <- stats::approx(age, rate, xout = 0:max(age))
  prop_alive <- c(1, cumprod((1000 - dr$y) / 1000))
  deaths <- -diff(prop_alive)
  return(sum(deaths * 0:max(age)))
}

