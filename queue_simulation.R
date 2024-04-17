# Constants
# Service rate
serv <- 1.1
# Arrivals rate
arriv <- 1
# Time limit
len_sim <- 100
# Level 1
level1 <- 5
# Level 2
level2 <- 10
# Number of offspring
nb_split <- 3
index <<- 1

# Libraries
library(tidyverse)
library(scales)

# Single queue instant
sim_que <- function(n_queue, service_rate, arrival_rate){
  # Random service time
  service_instance <- rexp(rate = service_rate, n = 1)
  # Random arrivals time
  arrival_instance <- rexp(rate = arrival_rate, n = 1)
  # If queue is empty, choose arrivals time
  if (n_queue == 0){
    return(c(1, arrival_instance))
    # Otherwise update queue length based on event
  } else if (service_instance < arrival_instance){
    return(c(n_queue - 1, service_instance))
  } else {
    return(c(n_queue + 1, arrival_instance))
  }
}

# Simulate a few instances in queue
many_sim <- function(n_start = 1, t_start = 0, service = serv, arrival = arriv){
  person_in_queue <- n_start
  event_time <- t_start
  # While time is less than maximum time
  while (max(event_time) < len_sim){
    # Simulate single event
    vec <- sim_que(person_in_queue[length(person_in_queue)], service_rate = service, arrival_rate = arrival)
    # Add number to queue history
    person_in_queue <- c(person_in_queue, vec[1])
    event_time <- c(event_time, event_time[length(event_time)] + vec[2])
  }
  data <- data.frame(sim_ind = rep(index, length(event_time)), values = person_in_queue, time = event_time)
  return(data)
}

df <- many_sim(n_start = 0)

# Simulate independent trajectories at the hitting time of a level
st_time <- min(df[df$values == level1,]$time)
next_level_sim <- function(start_time, lvl){
  data <- data.frame()
  for (j in 1:nb_split){
    index <<- index + 1
    new_df <- many_sim(n_start = lvl, t_start = start_time)
    data <- rbind(data, new_df)
  }
  return(data)
}

# Simulate level 1 offspring
new_df <- next_level_sim(st_time, level1)
df <- rbind(df, new_df)

level2_times <- df %>% filter(values == level2) %>% group_by(sim_ind) %>% summarise(min_time = min(time)) %>% select(min_time)

# Simulate level 2 offspring
for (tm in 1:nrow(level2_times)){
  new_df <- next_level_sim(level2_times[tm,]$min_time, level2)
  df <- rbind(df, new_df)
}

linetypes <- data.frame(sim_ind = 1:max(df$sim_ind), ln = c("solid", rep("twodash", nb_split), rep("dotted", max(df$sim_ind) - nb_split - 1))) %>% 
  inner_join(df, by = "sim_ind") %>% select(ln)
# Just a single path
df %>% 
   filter(sim_ind == 1) %>%
ggplot(aes(x = time, y = values, group = as.factor(sim_ind), col = as.factor(sim_ind))) + geom_step() + theme_bw() +
  labs(x = "Time", y = "Queue Length") + 
  theme(legend.position = "none",text = element_text(size=20)) + 
  scale_y_continuous(breaks= pretty_breaks()) +  
  geom_hline(yintercept=level1, linetype="dashed", color = "gray", size = 1.5) +
  annotate("text", x = len_sim/10-5, y=level1+0.5, label = "Level 1", size = 10)

# First split zoom
level1_ind <- 1:(1+nb_split)
df %>% filter(sim_ind %in% level1_ind) %>%
  ggplot(aes(x = time, y = values, group = as.factor(sim_ind), col = as.factor(sim_ind))) + 
  geom_step(linetype = linetypes[df$sim_ind %in% level1_ind, ]) + theme_bw() +
  labs(x = "Time", y = "Queue Length") + 
  theme(legend.position = "none",text = element_text(size=20)) + 
  scale_y_continuous(breaks = pretty_breaks(), 
                     limits=c(0, max(df[df$sim_ind == 1,]$values))) + 
  geom_hline(yintercept=level1, linetype="dashed", color = "gray", size = 1.5) + 
  annotate("text", x = len_sim/10-5, y=level1+0.5, label = "Level 1", size = 10)

# Full scale level 1
df %>% filter(sim_ind %in% level1_ind) %>%
  ggplot(aes(x = time, y = values, group = as.factor(sim_ind), col = as.factor(sim_ind))) + 
  geom_step(linetype = linetypes[df$sim_ind %in% level1_ind, ]) + theme_bw() +
  labs(x = "Time", y = "Queue Length") + 
  theme(legend.position = "none",text = element_text(size=20)) + 
  scale_y_continuous(breaks= pretty_breaks()) + 
  geom_hline(yintercept=c(level1, level2), linetype="dashed", color = "gray", size = 1.5) + 
  annotate("text", x = rep(len_sim/10-5, 2), y=c(level1+0.5, level2+0.5), label = c("Level 1", "Level 2"), size = 10) 

# Zoomed in level 2
  df %>% ggplot(aes(x = time, y = values, group = as.factor(sim_ind), col = as.factor(sim_ind))) + 
  geom_step(linetype = linetypes$ln) + theme_bw() +
  labs(x = "Time", y = "Queue Length") + 
  theme(legend.position = "none",text = element_text(size=20)) +
  scale_y_continuous(breaks= pretty_breaks(), 
                     limits = c(0, max(df[df$sim_ind %in% level1_ind,]$values))) + 
  geom_hline(yintercept=c(level1, level2), linetype="dashed", color = "gray", size = 1.5) + 
  annotate("text", x = rep(len_sim/10-5, 2), y=c(level1+0.5, level2+0.5), label = c("Level 1", "Level 2"), size = 10) 

  # Level 2
df %>% ggplot(aes(x = time, y = values, group = as.factor(sim_ind), col = as.factor(sim_ind))) + geom_step(linetype = linetypes$ln) + 
  theme_bw() +
  labs(x = "Time", y = "Queue Length") + 
  theme(legend.position = "none",text = element_text(size=20)) + 
  scale_y_continuous(breaks= pretty_breaks()) + 
  geom_hline(yintercept=c(level1, level2), linetype="dashed", color = "gray", size = 1.5) + 
  annotate("text", x = rep(len_sim/10-5, 2), y=c(level1+1.5, level2+1.5), label = c("Level 1", "Level 2"), size = 10) 


