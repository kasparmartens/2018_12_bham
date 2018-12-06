library(tidyverse)
library(patchwork)

generate_data <- function(N, mu1, mu2, sd1 = 1, sd2 = 1){
  y1 <- rnorm(N, mu1, sd1)
  y2 <- rnorm(N, mu2, sd2)
  
  y <- c(y1, y2)
  group <- factor(rep(c("A", "B"), each=N))
  df <- data.frame(y, group)
  df
}

generate_data2 <- function(N, a, b, mu, sd){
  y1 <- runif(N, a, b)
  y2 <- rnorm(N, mu, sd)
  
  y <- c(y1, y2)
  group <- factor(rep(c("A", "B"), each=N))
  df <- data.frame(y, group)
  df
}

N <- 100

df1 <- generate_data(N, -1, 2)
p1 <- df1 %>%
  ggplot(aes(group, y, fill=group)) +
  geom_boxplot() +
  labs(tag = "A")

df2 <- generate_data(N, -0.25, 0.25, 1, 1)
p2 <- df2 %>%
  ggplot(aes(group, y, fill=group)) +
  geom_boxplot() +
  labs(tag = "B")

df3 <- generate_data(N, -0.25, 0.25, 3, 1)
p3 <- df3 %>%
  ggplot(aes(group, y, fill=group)) +
  geom_boxplot() +
  labs(tag = "C")

p <- p1 + p2 + p3

ggsave("fig/boxplot1.png", p, width=9, height=3)
