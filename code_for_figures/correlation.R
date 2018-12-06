library(tidyverse)
library(patchwork)

N <- 100
generate_data <- function(N, a, sd=0.1){
  x <- runif(N, -3, 3)
  y <- a*x + rnorm(N, 0, sd)
  data.frame(x, y)
}

df1 <- generate_data(N, 0.1, 0.1)
p1 <- df1 %>%
  ggplot(aes(x, y)) +
  geom_point() +
  labs(tag = "B")

df2 <- generate_data(N, 0.3, 0.1)
p2 <- df2 %>%
  ggplot(aes(x, y)) +
  geom_point() +
  labs(tag = "A")

df3 <- generate_data(N, 0.02, 0.1)
p3 <- df3 %>%
  ggplot(aes(x, y)) +
  geom_point() +
  labs(tag = "C")

df4 <- generate_data(N, 0.0, 0.1)
p4 <- df4 %>%
  ggplot(aes(x, y)) +
  geom_point() +
  labs(tag = "D")

p <- p2 + p1 + p3 + p4

ggsave("fig/scatterplots.png", p, width=6, height=5)

df_all <- bind_rows(df1, df2, df3, df4) %>%
  mutate(panel = rep(c("B", "A", "C", "D"), each=N))

df_all %>%
  group_by(panel) %>%
  summarise(cor(x, y))
