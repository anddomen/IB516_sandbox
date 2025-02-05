# Purpose ----
# To determine the appropriate distribution for the transfer of pathogens from
# contaminated onions to a surface. This script does not do the cleaning and 
# collating.

# load libraries ----
source("Code/setup.R")

# load data ----
onion2surface <- read_csv("Data/Cleaned/Onion2Surface.csv") |> 
  filter(!is.na(CFU_cm2),         # filter out the ONE NA VALUE. hmph.
         logCFU_cm2 >= 0)         # remove values < 0, this may be wrong

# graph data ----
onion2surface |> 
  ggplot(aes(x = logCFU_cm2)) +
  geom_histogram(bins = 10) +
  theme_classic() +
  ggtitle("Onion to surface, negative log values removed")

## normal distribution ----
norm <- gamlss(logCFU_cm2 ~ 1, 
                family = NO(),
                data = onion2surface)
plot(norm)
summary(norm)

### simulate normal, neg values removed ----
# simulate data to check against the real data model
sim.norm <- data.frame(data = rnorm(1000),
                      number = rep(1:1000))
hist(sim.norm$data)                           # graph simulated data
sim.norm.mod <- gamlss(data ~ 1,              # fit model for simulated data
                       family = NO(),
                       data = sim.norm)
plot(sim.norm.mod)                            # plot simulated model

# looks pretty good tbh, i'm calling it normal, may need to check about removing
# the negative values on line 12

### graph the distribution on the data ----
# pull the parameters from the model
sigma <- exp(norm$sigma.coefficients)    # model takes the ln, so need to undo
mu    <- norm$mu.coefficients


# simulate the distribution with the parameters
onion.norm.dist <- rnorm(n = 1000, mean = mu, sd = sigma)

# Combine actual and simulated data for plotting
plot_data <- data.frame(
  value = c(onion2surface$logCFU_cm2, onion.norm.dist),
  type = factor(c(rep("Actual", length(onion2surface$logCFU_cm2)),
                  rep("Simulated", length(onion.norm.dist))))
)

# graph
plot_data |> 
  ggplot(aes(x = value, fill = type)) +
  # Add histogram of actual data
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5,
                 position = "identity",
                 binwidth = (max(plot_data$value) - min(plot_data$value))/30) +
  # Add smooth density curves
  geom_density(aes(color = type), alpha = 0.2) +
  # Customize colors
  scale_fill_manual(values = c("Actual" = "lightblue", "Simulated" = "pink")) +
  scale_color_manual(values = c("Actual" = "blue", "Simulated" = "red")) +
  # Add labels and title
  labs(title = "Distribution of Log CFU/cm²",
       x = "Log CFU/cm²",
       y = "Density") +
  theme_classic() +
  theme(legend.position = "top")


# Unaltered data model fit testing ----
onion2surface.raw <- read_csv("Data/Cleaned/Onion2Surface.csv") |> 
  filter(!is.na(CFU_cm2),         # filter out the ONE NA VALUE
         Source != "LL")

# graph data ----
onion2surface.raw |> 
  ggplot(aes(x = logCFU_cm2)) +
  geom_histogram(bins = 30) +
  theme_classic() +
  ggtitle("Onion to surface, data unaltered") +
  facet_wrap(vars(Source))

## normal distribution ----
norm.raw <- gamlss(logCFU_cm2 ~ 1, 
               family = NO(),
               data = onion2surface.raw)
plot(norm.raw)
summary(norm.raw)

### graph the distribution on the data ----
# pull the parameters from the model
sigma <- exp(norm.raw$sigma.coefficients)    # model takes the ln, so need to undo
mu    <- norm.raw$mu.coefficients


# simulate the distribution with the parameters
onion.norm.dist <- rnorm(n = 1000, mean = mu, sd = sigma)

# Combine actual and simulated data for plotting
plot_data <- data.frame(
  value = c(onion2surface.raw$logCFU_cm2, onion.norm.dist),
  type = factor(c(rep("Actual", length(onion2surface.raw$logCFU_cm2)),
                  rep("Simulated", length(onion.norm.dist))))
)

# graph
plot_data |> 
  ggplot(aes(x = value, fill = type)) +
  # Add histogram of actual data
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5,
                 position = "identity",
                 binwidth = (max(plot_data$value) - min(plot_data$value))/30) +
  # Add smooth density curves
  geom_density(aes(color = type), alpha = 0.2) +
  # Customize colors
  scale_fill_manual(values = c("Actual" = "lightblue", "Simulated" = "pink")) +
  scale_color_manual(values = c("Actual" = "blue", "Simulated" = "red")) +
  # Add labels and title
  labs(title = "Distribution of Log CFU/cm²",
       x = "Log CFU/cm²",
       y = "Density") +
  theme_classic() +
  theme(legend.position = "top")

