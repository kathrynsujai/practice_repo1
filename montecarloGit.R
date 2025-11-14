# install.packages("ggplot2")
library(ggplot2)
random_sample <- function(size, distribution) {
  if (distribution == "Discrete Uniform") {
    sample_data <- sample(x = 1:10, size, replace = TRUE)
  } else if (distribution == "Gaussian/Normal") {
    sample_data <- rnorm(size, mean = 0, sd = 1)
  } else if (distribution == "Poisson") {
    sample_data <- rpois(size, lambda = 0.75)
  } else if (distribution == "Exponential") {
    sample_data <- rexp(size, rate = 0.5)
  }
  difference <- median(sample_data) - mean(sample_data)
  return (difference)
}

sizes <- c(10, 50, 100)

uniform_simulation <- expand.grid(
  size = sizes,
  rep = 1:10000
) %>%
  mutate(
    distribution = "Discrete Uniform",
    diff = mapply(random_sample, size, distribution)
  )

normal_simulation <- expand.grid(
  size = sizes,
  rep = 1:10000
) %>%
  mutate(
    distribution = "Gaussian/Normal",
    diff = mapply(random_sample, size, distribution)
  )

poisson_simulation <- expand.grid(
  size = sizes,
  rep = 1:10000
) %>%
  mutate(
    distribution = "Poisson",
    diff = mapply(random_sample, size, distribution)
  )

exponential_simulation <- expand.grid(
  size = sizes,
  rep = 1:10000
) %>%
  mutate(
    distribution = "Exponential",
    diff = mapply(random_sample, size, distribution)
  )

total_sim <- bind_rows(
  uniform_simulation, normal_simulation, poisson_simulation, exponential_simulation
)

ggplot(
  total_sim,
  aes(
    x = diff,
    fill = as.factor(size)
  )
) +
  geom_density() +
  facet_grid( distribution ~ size, scales = "free") +
  labs(
    title = "Monte Carlo Comparison of Sample Mean and Difference",
    x = "Mean",
    y = "Density",
    fill = "Sample Size"
  )
