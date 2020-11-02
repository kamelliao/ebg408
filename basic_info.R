# import package
library(tidyverse)
library(ggplot2)

# import data
unchange_all <- read.csv("./data_table/unchange_table.csv", header = TRUE)
flat_all <- read.csv("./data_table/flat_all_table.csv", header = TRUE)
short_position_all <- read.csv("./data_table/short_position_all_table.csv", header = TRUE)
long_position_all <- read.csv("./data_table/long_position_all_table.csv", header = TRUE)

overview <- rbind(unchange_all, flat_all, short_position_all, long_position_all)

# color set
color_set <- scale_fill_manual(breaks = c("unchange", "flat", "short-position", "long-position"), 
                                values = c("#7f8183", "#fee9ab", "#fe9c9b", "#73cad7"))
# sorting type
overview$type <- factor(overview$type, levels = c("unchange", "flat", "short-position", "long-position"))

# calculate variables needed
overview <- overview %>%
  mutate(., len = end - start + 1)

# 1 for 4 types----
# 1.1 number of 4 types
ggplot(data = overview, aes(x = type, fill = type)) +
  ggtitle("plot1-1", "# of four types") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar() +
  geom_text(stat='count', aes(label = ..count..), vjust = -0.5) +
  color_set +
  ylim(0, 600) + 
  theme_minimal() +
  theme(legend.position="none")

# 1.2 length of 4 types
len_means <- aggregate(len ~ type, overview, mean)
len_mins <- aggregate(len ~ type, overview, min)
len_maxs <- aggregate(len ~ type, overview, max)
ggplot(data = overview, aes(x = type, y = len, fill = type)) +
  ggtitle("plot1-2", "length dist.") +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed") +
  geom_text(data = len_means, aes(label = round(len))) +
  geom_text(data = len_mins, aes(label = round(len))) +
  geom_text(data = len_maxs, aes(label = round(len))) +
  color_set +
  theme_minimal() +
  theme(legend.position="none")

# 1.3 start and end point
ggplot(data = overview, aes(x = start, y = end, color = type)) +
  geom_point() +
  scale_color_manual(breaks = c("unchange", "flat", "short-position", "long-position"), 
                    values = c("#7f8183", "#fee9ab", "#fe9c9b", "#73cad7")) +
  xlim(1, 100) +
  ylim(1, 100) +
  theme_minimal() +
  theme(legend.position="none")


# 2 for every player----
# calculate # of sections each player have
player_overview_list <- list()
for (i in c(1:160)) {
  player_overview_list[[i]] <- c(player_no = i, table(overview$player.no, overview$type)[i,])
}
player_overview <- do.call(rbind, player_overview_list)
player_overview <- as.data.frame(player_overview)
player_overview <- setNames(player_overview, c("player_no", "unchange", "flat", "short_position", "long_position")) %>%
  mutate(., N = unchange + flat + short_position + long_position)

# 2.1 dist. of # of section each player have
ggplot(data = player_overview, aes(x = N, y = (..count..)/sum(..count..))) +
  ggtitle("plot2-1", "dist. of # of section each player have") +
  geom_bar(data = player_overview, aes(fill = N == 6)) +
  scale_x_continuous(breaks = c(min(player_overview$N):max(player_overview$N))) +
  scale_fill_manual(values = c('grey50', "#dc2c3d")) +
  geom_text(stat='count', aes(label = ..count..), vjust = -0.5) +
  ylim(0, 0.3) +
  ylab("freqeuncy") +
  theme_minimal() +
  theme(legend.position="none")


# 2.2 combination of type at each num
ExploreCombination <- function(num){
  # input total num
  # find combinations of types at given num
  # return a data.frame
  comb_num_i <- player_overview[player_overview$total == num,] %>%
    group_by(., unchange, flat, short_position, long_position) %>%
    summarise(., n = n()) %>%
    mutate(., freq = n/sum(.$n))
  return(comb_num_i)
}

comb_list <- list()
for (i in c(2:11)) {
  comb_list[[i]] <- ExploreCombination(i)
}

avg_comb_list <- list()
for (n in c(2:11)) {
  avg_comb_data <- player_overview %>%
    filter(., N == n)
  num_data_comb <- length(avg_comb_data[, 1])
  avg_U <- round(sum(avg_comb_data$unchange) / (num_data_comb*n), 2)
  avg_F <- round(sum(avg_comb_data$flat) / (num_data_comb*n), 2)
  avg_SP <- round(sum(avg_comb_data$short_position) / (num_data_comb*n), 2)
  avg_LP <- round(sum(avg_comb_data$long_position) / (num_data_comb*n), 2)
  
  avg_comb_list[[n]] <- data.frame(n, avg_U, avg_F, avg_SP, avg_LP)
}

avg_comb <- do.call(rbind, avg_comb_list)

# transform data to fit ggplot2 and sort
gthr_avg_comb <- gather(avg_comb, type, freq, -n)
gthr_avg_comb$type <- factor(gthr_avg_comb$type, levels = c("avg_U", "avg_F", "avg_SP", "avg_LP"))
ggplot(data = gthr_avg_comb, aes(x = n, y = freq, fill = type, label = freq)) +
  ggtitle("plot2-2", "combination of types") +
  geom_col() +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(breaks = c("avg_U", "avg_F", "avg_SP", "avg_LP"), 
                    values = c("#7f8183", "#fee9ab", "#fe9c9b", "#73cad7")) +
  scale_x_continuous(breaks = c(min(gthr_avg_comb$n):max(gthr_avg_comb$n))) +
  theme_minimal() +
  theme(legend.position="none")

# transform data_trial to fit ggplot2 and sort
comb_t_list <- list()
for (n in c(2:11)){
  player_no_list <- player_overview[player_overview$N == n,]$player_no
  n_list <- list()
  for (i_row in 1:length(overview[,1])) {
    data_row <- overview[i_row,]
    if (data_row$player.no %in% player_no_list) {
      n_list[[i_row]] <- data.frame(n = n,
                                    type = data_row$type,
                                    len = data_row$len)
    }
  }
  n_table <- do.call(rbind, n_list)
  comb_t_list[[n]] <- n_table
}
comb_t <- do.call(rbind, comb_t_list) %>%
  mutate(., pct = len/100)

avg_comb_t <- comb_t %>%
  group_by(., n, type) %>%
  summarise(., avg_pct = round(mean(pct), 2))


ggplot(data = avg_comb_t, aes(x = n, y = avg_pct, fill = type, label = avg_pct)) +
  ggtitle("plot2-2", "combination of types (avg(len_type / 100))") +
  geom_col() +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  color_set +
  scale_x_continuous(breaks = c(min(avg_comb_t$n):max(avg_comb_t$n))) +
  theme_minimal() +
  theme(legend.position="none")


player_data <- overview %>%
  filter(., player.no == 1)
