#' Compute and Plot Normal Curve
#'
#' @param mu Mean of the normal distribution
#' @param sigma Standard deviation of the normal distribution
#' @param a Upper limit for shading and probability calculation
#'
#' @return A list with \code{mu}, \code{sigma}, and \code{probability}
#' @export
#' @examples
#' mycurve <- myncurve(10, 5, 6)
#' print(mycurve)
#'

myncurve <- function(mu, sigma, a) {

  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        main = paste("Normal Curve with Mean=", mu, "and SD=", sigma))

  x_shade <- seq(mu - 3 * sigma, a, length.out = 1000)
  y_shade <- dnorm(x_shade, mean = mu, sd = sigma)
  polygon(c(x_shade, a), c(y_shade, 0), col = "skyblue", border = NA)

  probability <- pnorm(a, mean = mu, sd = sigma)

  cat("Probability P(X <= a):", probability, "\n")

  return(list(mu = mu, sigma = sigma, probability = probability))
}
