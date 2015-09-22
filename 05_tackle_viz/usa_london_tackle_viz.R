# USA Eagles rugby tackling visualization
# from London 7s 2015
library(dplyr)
library(tidyr)

# import data                       colClasses=c("numeric",rep("factor",9)))
team_tackles <- read.csv("Documents/rugby7s/05_tackle_viz/USA_tackle.csv", header=TRUE)
str(team_tackles)

# calculate tackle attempts per player by direction
player_tackle_attempts <- team_tackles %>% 
  select(ATTEMPT, NUMBER, PLAYER, DIR, RESULT) %>% arrange(NUMBER, PLAYER, DIR)

# calculate basic player stats
player_stats <- player_tackle_attempts %>% 
  group_by(NUMBER, PLAYER) %>% 
  summarise(att=n(), made=sum(RESULT), miss=att-made, eff=round(made/att,2))
max_player <- player_stats[which.max(player_stats$eff),c(2,6)]

# calculate per-player directional statistics
player_dir <- player_tackle_attempts %>%
  group_by(NUMBER, PLAYER, DIR) %>%
  summarise(att=n(), made=sum(RESULT), eff=round(made/att,2))

# calculate directional efficiency
dir_eff <- player_dir %>% 
  group_by(DIR) %>% 
  summarise(dir_eff=round(sum(made)/sum(att),2))
max_dir <- dir_eff[which.max(dir_eff$dir_eff),]
min_dir <- dir_eff[which.min(dir_eff$dir_eff),]

# visualize data

# create a player efficiency matrix for plotting
player_eff <- player_dir %>% 
  select(-(att:made)) %>% 
  spread(DIR, eff)

locations <- matrix(c(2,4,6,8,2,4,6,8,2,4,6,8, 
                      6,6,6,6,4,4,4,4,2,2,2,2), ncol=2, nrow=12)

# visualize attempts
circles <- matrix(1.0, ncol=12, nrow=12)
circles[is.na(player_eff[,3:14])] <- 0
stars(circles[,c(3:1,12:4)], radius=TRUE, draw.segments=FALSE, locations=locations,
      labels=paste(player_stats$NUMBER, player_stats$PLAYER, player_stats$eff), 
      flip.labels=FALSE, scale=FALSE, len=.75,
      main = bquote("Directional Tackle" ~ epsilon ~ "by Player"))

# visualize tackles
stars(player_eff[,c(5:3,14:6)], draw.segments=FALSE, scale=FALSE, 
      radius=FALSE, labels=NULL, locations=locations,
      len=.75, lwd=2, add=TRUE)

# label with statistics
text(3, 6.8, labels=bquote("Max dir" == .(as.numeric(max_dir[,2]))), cex=.8)
text(5, 6.8, labels=bquote("Min dir" == .(as.numeric(min_dir[,2]))), cex=.8)
text(7, 6.8, labels=bquote("Player" == .(as.numeric(max_player[,2]))), cex=.8)