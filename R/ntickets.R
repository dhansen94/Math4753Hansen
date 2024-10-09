#' Calculates the number of  tickets to sell to minimize overbooking
#'
#' @param N Integer, number of seats availible
#' @param gamma Numeric, acceptable probability threshold for overbooking
#' @param p Numberic, the probability that a ticket holder will attend
#'
#' @return  A list containing:
#' \describe{
#'    \item{nd}{The number of tickets to sell using the discrete distribution.}
#'    \item{nc}{The number of tickets to sell using the normal distribution.}
#'    \item{N}{The number of seats avalible.}
#'    \item{p}{The proability that a ticket holder will attend their flight}
#'    \item{gamma}{the acceptable probability threshold for overbooking}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' ntickets(N = 100, gamma = 0.5, p = 0.9)
#' }
ntickets <- function(N, gamma, p) {
  # define the function  and parameters
  nd <- N
  # assigning a variable to the number of seats available
  prob_overbook_discrete <- 1 - pbinom(N, nd, p)
  # calculates the cumulative probability of have N or fewer attendees when nd tickets are sold
  while (prob_overbook_discrete > gamma) {
    nd <- nd + 1
    prob_overbook_discrete <- 1 - pbinom(N, nd, p)  # This  loop increases nd incrementally until the probability of overbooking is less than or equal to gamma
  }


  nc <- N
  # creating another variable to use relative to the number of seat
  prob_overbook_normal <- 1 - pnorm(N + 0.5, mean = nc * p, sd = sqrt(nc * p * (1 - p)))
  # calculates the probability of overbooking using normal approximation
  while (prob_overbook_normal > gamma) {
    nc <- nc + 1
    prob_overbook_normal <- 1 - pnorm(N + 0.5, mean = nc * p, sd = sqrt(nc * p * (1 - p)))
    # another while loop to increase nc until the probability of overbooking is less than or equal to gamma
  }

  n_values <- seq(N, N + 50)
  # generates a sequence of tickets from N to N+50 for plotting
  obj_discrete <- numeric(length(n_values))
  # creates a vector to store the values
  for (i in 1:length(n_values)) {
    n <- n_values[i]
    obj_discrete[i] <- 1 - gamma - pbinom(N, n, p)  # creates an objective function value for each value
  }

  plot(n_values, obj_discrete, type = 'l', col = 'blue',
       xlab = 'Number of Tickets Sold (n)', ylab = 'Objective Function',
       main = 'Discrete Distribution')
  # Plots the objective function values against the number of tickets sold for discrete case
  obj_normal <- numeric(length(n_values))
  # creates a vector for normal approximation
  for (i in 1:length(n_values)) {
    # Iterates over each value
    n <- n_values[i]
    mean_normal <- n * p
    sd_normal <- sqrt(n * p * (1 - p))
    obj_normal[i] <- 1 - gamma - pnorm(N + 0.5, mean = mean_normal, sd = sd_normal)
    # computes the objective value for each n
  }

  plot(n_values, obj_normal, type = 'l', col = 'red',
       xlab = 'Number of Tickets Sold (n)', ylab = 'Objective Function',
       main = 'Normal Approximation')
  # plots the objective function against number of tickets sold for normal approximation
  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
  # gives a named list containing results for both discrete (nd) and normal (nc) cases
}
