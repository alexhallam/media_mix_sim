library(rstan)
library(tidyverse)

clean_data <- read_csv("clean_data.csv")
media_data <- clean_data %>% select(contains("m_"))
long_media_array <- c(clean_data$m_tv,clean_data$m_rd,clean_data$m_online)
# data Prep
N <- nrow(clean_data)
Y <- clean_data$sales
max_lag <- 13
num_media <- 3
lag_vec <- seq(0, max_lag - 1)
X_media <- array(data = media_data, dim = c(3,13))
X_media <- array(data = long_media_array, dim = c(nrow(clean_data),3,13))
num_ctrl <- 1
X_ctrl <- clean_data %>% select(price) %>% as.vector()

stan_data <- list(N=N, Y=Y, max_lag=max_lag, num_media=num_media,
                  lag_vec=lag_vec,X_media=X_media,
                  num_ctrl=num_ctrl,X_ctrl=X_ctrl)

m.stan <- stan(file = "model.stan",data = stan_data, iter = 3000, chains = 1)

summary(m.stan)

