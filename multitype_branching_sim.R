# Libraries
library(tidyverse)
# Reproducibility
set.seed(1)

# Children constant
v <- 5
v2 <- 1
# Matrix combining children and transition probabilities
A <- matrix(c(0.2*v, 0.1*v2, 0.3*v, 0.2*v2), byrow = TRUE, nrow = 2)
# Eigendecomposition
eig_vectors <- eigen(A)$vectors
eig_values <- eigen(A)$values

# Second eigenvalue exploration
end_offspring <- 100
values_seq <- seq(1, end_offspring, by = 1)
collect_eig <- matrix(0, nrow = length(values_seq), ncol = 3)
for (i in seq_along(values_seq)){
  offspring <- values_seq[i]
  i_matrix <- matrix(c(0.2*offspring, 0.1, 0.3*offspring, 0.2), byrow = TRUE, nrow = 2)
  eigen_values <- eigen(i_matrix)$values
  collect_eig[i,] <- c(i, eigen_values[1], eigen_values[2])
}

# Number of generations
n <- 50
# Starting population
starting_nbs <- c(1,1)
# Saving population frequency
save_frequency <- 1

# Power of matrix function
pwr <- function(n){
  return(eig_vectors %*% diag(eig_values)^n %*% solve(eig_vectors))
}
# Leading eigenvalue
leading <- eig_values[1]

# Time steps
steps <- seq(1, n, by = save_frequency)
good_minus_t <- rep(1, floor(n/save_frequency))
mid_minus_t <- rep(1, floor(n/save_frequency))
# Expected population
for (i in seq_along(steps)){
  if (i != 1){
    mat <- pwr(steps[i-1])
    rowsm <- rowSums(mat)
    N_good <- rowsm[1]
    N_mid <- rowsm[2]
    print("Population good")
    print(N_good)
    good_minus_t[i] <- N_good * leading^(-steps[i-1])
    mid_minus_t[i] <- N_mid * leading^(-steps[i-1])
  }

}

# Theoretical values df
df_theory <- data.frame(x=steps,
                  scaled_good = good_minus_t,
                  scaled_mid = mid_minus_t) %>% mutate(good = scaled_good*leading^(steps-1), 
                     mid = scaled_mid*leading^(steps-1))

df_theory %>% 
  select(x, scaled_good, scaled_mid) %>% 
  pivot_longer(!x) %>% 
  ggplot() +
  geom_line(aes(x = x, y = value, col = name), linewidth = 1) +
  theme_bw() +
  labs(x = "Generation", y = "Scaled population", col = "Type")+
  scale_color_discrete(labels=c('Good', 'Mid'))

# Transition matrix for simulation
trans_mat <- matrix(c(0.2,0.3,0.1,0.2), byrow = TRUE, nrow=2)

# Conduct n simulations function
sim_system <- function(n_steps = n){
  n_good <- starting_nbs[1]
  n_mid <- starting_nbs[2]
  pop_saved <- matrix(0, ncol = 2, nrow = length(steps))
  pop_saved[1,] <- c(starting_nbs[1],starting_nbs[2])
  extinct <- FALSE
  step <- 2
  while(!extinct & step <= n_steps){
    if (n_good == 0 & n_mid == 0){
      extinct <- TRUE
      print("Extinct")
    } else {
     randoms_good <- runif(v*n_good)
     randoms_mid <- runif(n_mid)
     n_good <- sum(randoms_good < trans_mat[1,1]) + sum(randoms_mid < trans_mat[2,1])
     n_mid <- sum(trans_mat[1,1] < randoms_good & randoms_good < trans_mat[1,1] + trans_mat[1,2]) + sum(randoms_mid > trans_mat[2,1] & randoms_mid < trans_mat[2,2]+trans_mat[2,1])
     if (step %in% steps){
       pop_saved[which(step == steps),] <- c(n_good, n_mid)
     }
     step <- step + 1
    }
  }
  return(pop_saved)
}


# Empty dataframe
df <- data.frame(sim_nb = integer(),
                 scaled_good = double(),
                 scaled_mid = double(),
                 time = integer())
# Number of systems
number_sim <- 1000
# Placeholder for how many did not go extinct
non_extinct <- 0
while (non_extinct < number_sim){
    sim1 <- sim_system(n_steps = n)
    # If we only want non extinct uncomment
  # if (sim1[length(steps), 1] != 0){
    df_new <- data.frame(sim_nb = as.integer(rep(non_extinct, n)),
                     scaled_good = sim1[,1]*leading^(-(steps-1)),
                     scaled_mid = sim1[,2]*leading^(-(steps-1)),
                     time = as.integer(steps))
    df <- rbind(df, df_new)
    non_extinct <- non_extinct + 1
  # }
}

# Plot by simulation
  df %>% filter(sim_nb < 10) %>%
    pivot_longer(cols = c("scaled_good", "scaled_mid")) %>% 
  ggplot() + geom_line(aes(x = time, y = value, col = name)) + 
    facet_wrap(~sim_nb, ncol = 5) + 
  theme_bw()+
  geom_line(data = df_theory,
            aes(x = x, y = scaled_good), col = "red", alpha=0.5, linetype="dashed")+
  geom_line(data = df_theory,
            aes(x = x, y = scaled_mid), col = "blue", alpha=0.5, linetype = "dashed") +
    labs(x = "Generations", y = "Scaled Population", col = "Type")  +
    scale_color_discrete(labels=c('Good', 'Mid')) +
    annotate("text", x = 3*n/5, y = 1.3*good_minus_t[length(steps)], label = "Theoretical Good", color="red", size = 3, alpha=0.5) +
    annotate("text", x = 3*n/5, y = 1.2*mid_minus_t[length(steps)], label = "Theoretical Mid", color="blue", size = 3, alpha=0.5) +
    theme(strip.text.x = element_blank()) +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.9, 0.4), 
      legend.background = element_rect(fill = "white", colour = "black")
    )

# Plot average
df %>% pivot_longer(cols = c("scaled_good", "scaled_mid")) %>% 
  group_by(time, name) %>% 
  summarise(scaled_average = mean(value)) %>% 
  ggplot() + geom_line(aes(x = time, y = scaled_average, col = name), size = 0.7) +
  theme_bw() +
  geom_line(data = df_theory,
            aes(x = x, y = scaled_good), col = "red", alpha = 0.7, linetype = "dashed")+
  geom_line(data = df_theory,
            aes(x = x, y = scaled_mid), col = "blue", alpha = 0.7, linetype = "dashed") +
  labs(x = "Generations", y = "Scaled Population", 
       col = "Type", caption = paste(number_sim, "simulations")) +
  scale_color_discrete(labels=c('Good', 'Mediocre')) +
  theme(text=element_text(size=22),
        legend.position = "inside", 
        legend.position.inside = c(0.8, 0.5),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
  # annotate("text", x = 3*n/5, y = 1.03*good_minus_t[length(steps)], label = "Theoretical Good", color="red", size = 6, alpha=0.5) +
  # annotate("text", x = 3*n/5, y = 1.03*mid_minus_t[length(steps)], label = "Theoretical Mid", color="blue", size = 6, alpha=0.5)+ 
  

# Adding population
df <- df %>% mutate(good = scaled_good*leading^(steps-1),
              mid = scaled_mid*leading^(steps-1),
              prop_good = ifelse(good == 0 & mid == 0, 0 , good/(good+mid)),
              prop_mid = ifelse(good == 0 & mid == 0, 0, 1 - prop_good))
non_zero <- df %>% filter(time == n) %>% filter(good > 0) %>% select(sim_nb) %>% inner_join(df, by = "sim_nb") 

# Average for non-extinct
non_zero %>% pivot_longer(cols = c("scaled_good", "scaled_mid")) %>%
  group_by(time, name) %>%
  summarise(scaled_average = mean(value)) %>%
  ggplot() + geom_line(aes(x = time, y = scaled_average, col = name)) +
  geom_line(data = df_theory,
            aes(x = x, y = scaled_good), col = "red")+
  geom_line(data = df_theory,
            aes(x = x, y = scaled_mid), col = "blue")


# Plot log population

ggplot(df) + 
  geom_line(aes(x = time, y = log(good+1), group = sim_nb), alpha = 0.3, size = 0.5) + 
  geom_line(data = df_theory, 
            aes(x = steps, y = log(good+1)), col = "blue", size = 2) +
  theme_bw() +
  labs(x = "Generations", y = "Log Population") 
# +
  annotate("text", x = n/2, y = 1.5*good_minus_t[n]*leading^n, label = "Theoretical Good Population Growth", color="blue", size = 6)+ 
  theme(text=element_text(size=16))

  # Plot proportion
df %>% filter(sim_nb < 20) %>% 
  ggplot() + geom_line(aes(x = time, y = prop_good)) + 
  facet_wrap(~sim_nb, ncol = 5)
  
df %>% ggplot() +
  geom_line(aes(x = time, y = prop_good, group = sim_nb), alpha = 0.5) +
  labs(y = "Proportion of population of type good", 
       x = "Generations", 
       caption = "100 simulations") + theme_bw() +
  theme(text=element_text(size=20))
