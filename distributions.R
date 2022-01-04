
require(tidyverse)
require(reshape2)

# Functions encoding statistical distributions --------------------------------

## Normal Distribution Functions
normal <- list()
normal$pars <- list(mu = div(HTML("&mu;: mean")), sigma = div(HTML("&sigma;: standard deviation")))
normal$scale <- list(mu = c(-20, 20, 0, 1), sigma = c(0, 10, 1, 0.5)) # min, max, default value, step size
normal$xlim <- c(-5, 5, NA, NA) # default min, default max, minimum possible, maximum possible
normal$density <- function(x, mu, sigma) dnorm(x, mu, sigma)
normal$draw_samples <- function(n, mu, sigma) rnorm(n, mu, sigma)
normal$mean <- function(mu, sigma) mu
normal$variance <- function(mu, sigma) sigma^2
normal$plot <- function(mu, sigma, xlim = NA) {
  if(any(is.na(xlim))) {
    x = seq(mu - 4*sigma, mu + 4*sigma, length.out = 200)
  } else {
    x = seq(min(xlim), max(xlim), length.out = 200)
  }
  density = normal$density(x, mu, sigma)

  data.frame(x = x, density = density) %>%
    ggplot(aes(x, density)) +
    geom_area(alpha = 0.5, fill = "dodgerblue3", color = NA) +
    geom_line(size = 1, color = "dodgerblue4")

}

## Pareto distribution functions
pareto <- list()
pareto$pars <- list(xm = "x: scale", a = div(HTML("&alpha;: shape")))
pareto$scale <- list(xm = c(1, 20, 1, 0.1), a = c(0, 10, 1, 0.1))
pareto$xmax <- 5
pareto$density <- function(x, a, xm) (a*(xm^a))/(x^(a + 1))
pareto$draw_samples <- function(n, a, xm) {
  qpareto <- function(u, a, xm) xm/(1 - u)^(1/a)
  qpareto(runif(n), a, xm)
}
pareto$mean <- function(a, xm) {
  if (a <= 1) {
    Inf
  } else {
    (a*xm)/(a - 1)
  }
}
pareto$variance <- function(a, xm) {
  if (a <= 2) {
    Inf
  } else {
    (a*xm^2)/((a - 1)^2*(a - 2))
  }
}
pareto$plot <- function(a, xm, xmax) {

  if (xmax <= 1) {
    stop("density of pareto distribution is undefined for x < 1")
  } else {
    x = seq(1, xmax, length.out = 200)
  }

  density = pareto$density(x, a, xm)

  data.frame(x = x, density = density) %>%
    ggplot(aes(x, density)) +
    geom_area(alpha = 0.5, fill = "dodgerblue3", color = NA) +
    geom_line(size = 1, color = "dodgerblue4")
}

## Beta distribution functions
beta <- list()
beta$pars <- list(a = div(HTML("&alpha;: shape 1")), b = div(HTML("&beta;: shape 2")))
beta$scale <- list(a = c(0, 5, 1, 0.1), b = c(0, 5, 1, 0.1))
beta$density <- function(x, a, b) dbeta(x, a, b)
beta$draw_samples <- function(n, a, b) rbeta(n, a, b)
beta$mean <- function(a, b) a / (a + b)
beta$variance <- function(a, b) (a * b) / ((a + b)^2 * (a + b + 1))
beta$plot <- function(a, b, xlim) {
  x = seq(0, 1, by = 0.001)
  if (a < 0 | b < 0) stop("a, b must be positive")
  density = beta$density(x, a, b)

  data.frame(x = x, density = density) %>%
    ggplot(aes(x, density)) +
    geom_area(alpha = 0.5, fill = "dodgerblue3", color = NA) +
    geom_line(size = 1, color = "dodgerblue4")
}

## Binomial distribution functions
binomial <- list()
binomial$pars <- list(size = "n: number of Bernoulli trials", p = "p: probability of success")
binomial$scale <- list(size = c(1, 20, 1, 1), p = c(0, 1, 0.5, 0.01))
binomial$density <- function(x, size, p) dbinom(x, size, p)
binomial$draw_samples <- function(n, size, p) rbinom(n, size, p)
binomial$mean <- function(size, p) size*p
binomial$variance <- function(size, p) size*p*(1-p)
binomial$plot <- function(size, p, xlim) {
  x = seq(0, size, by = 1)
  if (size <= 0 | p <= 0 | p >= 1) {
    stop("size must be positive and p must be between 0 and 1")
  }
  probability = binomial$density(x, size, p)

  data.frame(x = x, probability = probability) %>%
    ggplot(aes(x, probability)) +
    geom_bar(stat = "identity", fill = "dodgerblue3", color = "dodgerblue4")
}

## Poisson distribution functions
poisson <- list()
poisson$pars <- list(lambda = div(HTML("&lambda;: mean and variance")))
poisson$scale <- list(lambda = c(0.1, 10, 1, 0.1))
poisson$density <- function(x, lambda) dpois(x, lambda)
poisson$draw_samples <- function(n, lambda) rpois(n, lambda)
poisson$mean <- function(lambda) lambda
poisson$variance <- function(lambda) lambda
poisson$plot <- function(lambda) {
  x = seq(0, ifelse(lambda <= 1, 4, ceiling(lambda*3)), by = 1)
  if(lambda <= 0) {
    stop("lambda must be positive")
  }
  probability = poisson$density(x, lambda)

  data.frame(x = x, probability = probability) %>%
    ggplot(aes(x, probability)) +
    geom_bar(stat = "identity", fill = "dodgerblue3", color = "dodgerblue4")
}

## Gamma distribution functions
gamma <- list()
gamma$pars <- list(k = "k: shape", theta = div(HTML("&theta;: scale")))
gamma$scale <- list(k = c(0.1, 10, 1, 0.1), theta = c(0.1, 3, 1, 0.1))
gamma$xmax <- 20
gamma$density <- function(x, k, theta) dgamma(x, shape = k, scale = theta)
gamma$draw_samples <- function(n, k, theta) rgamma(n, shape = k, scale = theta)
gamma$mean <- function(k, theta) k * theta
gamma$variance <- function(k, theta) k * theta^2
gamma$plot <- function(k, theta, xmax = 20) {
  x = seq(0, xmax, length.out = 200)
  density = gamma$density(x, k, theta)

  data.frame(x = x, density = density) %>%
    ggplot(aes(x, density)) +
    geom_area(alpha = 0.5, fill = "dodgerblue3", color = NA) +
    geom_line(size = 1, color = "dodgerblue4")
}


### Functions to draw and summarize samples ---------------------------------------------

# Function to draw samples from a given distribution
## Draws `n_samples` each of size = `sample_size` from distribution `dist`
draw_samples <- function(sample_size, n_samples = 1e3, dist, pars) {
  pars$n <- sample_size*n_samples
  samples <- do.call(dist$draw_samples, pars)
  matrix(samples, ncol = n_samples)
}

# Function to compare tail of distribution to expected proportion under Normal
## Stolen from Edge's package "stfspack"
tail_ratio <- function (x, k, mu, sigma) {
  mean(x < (mu - k * sigma) | x > (mu + k * sigma))/(1 - (pnorm(k) - pnorm(-k)))
}

# Function to generate summary of samples
summarize_samples <- function(samples, dist, pars) {

  sample_means <- colMeans(samples)
  mean_of_mean <- mean(sample_means)
  variance_of_mean <- var(sample_means)

  data.frame(
    sample.size = nrow(samples),
    expected.mean = do.call(dist$mean, pars),
    expected.variance = do.call(dist$variance, pars)/nrow(samples),
    mean = mean_of_mean,
    variance = variance_of_mean,
    SD1 = tail_ratio(sample_means, 1, mean_of_mean, sqrt(variance_of_mean)),
    SD2 = tail_ratio(sample_means, 2, mean_of_mean, sqrt(variance_of_mean)),
    SD3 = tail_ratio(sample_means, 3, mean_of_mean, sqrt(variance_of_mean)),
    SD4 = tail_ratio(sample_means, 4, mean_of_mean, sqrt(variance_of_mean))
  )

}
