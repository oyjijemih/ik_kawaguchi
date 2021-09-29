library(tidyverse)
library(evd)

set.seed(1)
J <- 10
K <- 3
T <- 100
N <- 500
L <- 500

beta <- rnorm(K)
beta[1] <- 4

sigma <- abs(rnorm(K))
mu <- 0.5
omega <- 1

price_xi <- 1
sd_x <- 2
sd_xi <- 0.5
sd_c <- 0.05
sd_p <- 0.05

x_1 <- seq(from = 1, by = 0, length = J)
x_2 <- rnorm(J, mean = 0, sd = sd_x)
x_3 <- rnorm(J, mean = 0, sd = sd_x)

j <- 1:J
X <- tibble(j, x_1, x_2, x_3)
X <- rbind(0, X)

xi <- rnorm(J*T, mean = 0, sd = sd_xi)
c <- rlnorm(J*T, mean = 0, sd = sd_c)
p_c <- rlnorm(J*T, mean = price_xi*xi, sd = sd_p)

t <- 1:T
M_ <- expand_grid(t, j)
M_ <- mutate(M_, xi = xi, c = c, p = p_c+c)

M <- M_ %>%
  group_by(t) %>%
  dplyr::sample_n(as.integer(rdunif(1, 1, J))) %>%
  ungroup()

zero <- as.data.frame(matrix(0, T, 4))
outside <- cbind(t, zero)
names(outside) <- names(M)
M <- rbind(M, outside)
M <- M[order(M$t, M$j), ]
M <- M[, c(2,1,3,4,5)]

i <- 1:N
V <- expand_grid(i, t)
V <- V[order(V$t, V$i), ]
V <- mutate(V, v_x_1=rnorm(N*T), v_x_2=rnorm(N*T), v_x_3=rnorm(N*T), v_p=rnorm(N*T))

df_ <- left_join(M, X, by="j")
df <- left_join(V, df_, by="t")
df <- df[, c(2,1,7,3,4,5,6,11,12,13,8,9,10)]

e <- rgumbel(nrow(df))

compute_indirect_utility <- function(df, beta, sigma, mu, omega){
  data <- mutate(df, 
                 beta_1 = beta[1] + sigma[1]*v_x_1,
                 beta_2 = beta[2] + sigma[2]*v_x_2,
                 beta_3 = beta[3] + sigma[3]*v_x_3,
                 alpha = -exp(mu+omega*v_p),
                 u = beta_1*x_1 + beta_2*x_2 + beta_3*x_3 + alpha*p + xi)
  return(data$u)
}

u <- compute_indirect_utility(df, beta, sigma, mu, omega)
df_u <- mutate(df, u=u)

compute_choice_smooth <- function(df, beta, sigma, mu, omega){
  data <- mutate(df, u = compute_indirect_utility(df, beta, sigma, mu, omega))
  data_ <- mutate(data, exp_u = exp(u))
  data_ <- data_ %>%
    group_by(i, t) %>%
    mutate(q_sum = sum(exp_u)) %>%
    ungroup()
  data_ <- mutate(data_, q=exp_u/(q_sum))
  data <- mutate(data, q=data_$q)
  return(data)
}

df_choice_smooth <- compute_choice_smooth(df, beta, sigma, mu, omega)

compute_share_smooth <- function(data){
  df <- data %>%
    group_by(t, j) %>%
    mutate(s=sum(q)/N) %>%
    ungroup()
  
  df <- df %>%
    group_by(t, i) %>%
    mutate(y = log(s/s[1])) %>%
    ungroup()
  return(df)
}

df_share_smooth <- compute_share_smooth(df_choice_smooth)
