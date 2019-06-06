# Dissimilarity Index

# returns dissimilarity index given data of a city
dissimilarity <- function(city) {
  # population of areas (t as defined in the report)
  t <- city$pop
  
  # percentage not white of areas (p as defined in the report)
  p <- city$pct.not.white
  
  # population not white of areas (x as defined in the report)
  x <- city$pop.not.white 
  
  # total population of areas (T as defined in the report)
  total_pop <- sum(t)
  
  # total percentage not white (P as defined in the report)
  total_pct_not_white <- sum(x) / total_pop
  
  # numerator and denominator as defined in the report
  numerator <- sum(t * abs(p - total_pct_not_white))
  denominator <- 2 * total_pop * total_pct_not_white * (1 - total_pct_not_white)

  index <- numerator / denominator
  return(index)
}

# returns interaction index given data of a city
interact <- function(city) {
  # population of areas (t as defined in the report)
  t <- city$pop
  
  # population white of areas (y as defined in the report)
  y <- city$pop.white
  
  # population not white of areas (x as defined in the report)
  x <- city$pop.not.white
  
  # total population not white (X as defined in the report)
  total_pop_not_white <- sum(x)
  
  index <- sum((x / total_pop_not_white) * (y / t))
  return (index)
}

# returns isolation index given data of a city
isolation <- function(city) {
  # population of areas (t as defined in the report)
  t <- city$pop
  
  # population not white of areas (x as defined in the report)
  x <- city$pop.not.white
  
  # total population not white (X as defined in the report)
  total_pop_not_white <- sum(x)
  
  index <- sum((x / total_pop_not_white) * (x / t))
  return (index)
}

# returns correlation ratio given data of a city
correlation <- function(city) {
  # total percentage not white (P as defined in the report)
  total_pct_not_white <- sum(city$pop.not.white) / sum(city$pop)
  
  # calculating the isolation_index required for the correlation
  isolation_index <- isolation(city)
  ratio <- (isolation_index - total_pct_not_white) / (1 - total_pct_not_white)
  return(ratio)
}

# returns a the proposed index given data of a city
proposed <- function(city) {
  
  # calculates required indexes used
  diss_index <- dissimilarity(city)
  corr_index <- correlation(city)
  
  index <- (diss_index + corr_index)/2
  return(index)
}