# Per Player tackling visualization

# build a data frame of the team
names <- c("Sherwood", "Liberman", "Woodman", "Frieden", "Kivell", "Meyer", "Soto", "Steanson", "Hoetger", "Laibowitz", "Christie")
numbers=c("33", "44", "16", "40", "20", "35", "56", "87", "70", "41", "87")
position=c("LB", "DB", "DB", "DL", "LB", "DB", "LB", "LB", "DL", "DL", "DL")
team <- data.frame("name"=names, "number"=numbers, "position"=position)

# simulate tackles
player_tackle_attempts <- matrix(sample(0:9, 132, replace=TRUE), ncol=12, nrow=11)
player_tackles_made <- 
  ceiling(player_tackle_attempts-player_tackle_attempts*runif(length(player_tackle_attempts),0,.6))
player_tackles_made[is.na(player_tackles_made)] <- 0
colnames(player_tackle_attempts) <- paste(1:12, "ta", sep="")
colnames(player_tackles_made) <- paste(1:12, "tm", sep="")

# create a matrix of attempted/made tackles by direction
player_tackles <- round(player_tackles_made/player_tackle_attempts,2)
colnames(player_tackles)<-c(1:12)
rownames(player_tackles)<-names
player_tackles[is.na(player_tackles)] <- 0
team_tackles <- cbind(team,player_tackles)

# calculate statistics overall team efficiency, 
# min/max directional tackling, and player efficiency
dir_eff <- round(apply(player_tackles_made, 2, sum)/apply(player_tackle_attempts, 2, sum),2)
max_dir <- paste(which.max(dir_eff), "(", max(dir_eff), ")")
min_dir <- paste(which.min(dir_eff), "(", min(dir_eff), ")")
player_eff <- round(apply(player_tackles_made, 1, sum)/apply(player_tackle_attempts, 1, sum),2)
max_player <- paste(names[which.max(player_eff)], ":", max(player_eff))

# create a background grid of efficiency circles
locations <- matrix(c(2,4,6,8,2,4,6,8,2,4,6, 
                      6,6,6,6,4,4,4,4,2,2,2), ncol=2, nrow=11)
circles <- matrix(1.0, ncol=12, nrow=11)
stars(circles, radius=TRUE, draw.segments=FALSE, locations=locations,
      labels=paste(numbers, names, player_eff), flip.labels=FALSE, scale=FALSE, len=.75,
      main = bquote("Directional Tackle" ~ epsilon ~ "by Player"))

# superimpose player efficiency spider charts
stars(team_tackles[,c(6:4,15:7)], draw.segments=FALSE, scale=FALSE, 
      radius=TRUE, key.loc=c(8,2), labels=NULL, locations=locations,
      len=.75, lwd=2, add=TRUE)

# label with statistics
text(3, 6.8, labels=bquote("Max dir" == .(max_dir)), cex=.8)
text(5, 6.8, labels=bquote("Min dir" == .(min_dir)), cex=.8)
text(7, 6.8, labels=bquote("Player" == .(max_player)), cex=.8)
