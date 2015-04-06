library(devtools)
source_url("https://raw.githubusercontent.com/jliberma/rugby7s/master/func/match_func.R")
source_url("https://raw.githubusercontent.com/jliberma/rugby7s/master/func/poss_func.R")

# Calculate win % by round for first try scorers
first_score_summary <- function() {
  fs <- c()
  for (i in c("all", "Pool", "Shield", "Bowl", "Plate", "Cup")) {
    poss <- poss_filter(round=i)
    score <- match_score(poss)
    win <- match_winner(score)
    trys <- first_try(poss)
    goal <- first_goal(poss)
    full <- cbind(score,win,trys,goal)
    fs <-rbind(fs, c("round" = i, 
                     "matches" = nrow(score),
                     "try" = round(nrow(subset(full,full$win==full$trys))/nrow(full),2), 
                     "goal" = round(nrow(subset(full,full$win==full$goal))/nrow(subset(full,!is.na(full$goal))),2)))
  }
  View(fs)
}

# Calculate win % by round for first try scorers
first_score_summary_core <- function() {
  fsc <- c()
  for (i in c("all", "Pool", "Shield", "Bowl", "Plate", "Cup")) {
    poss <- poss_filter(round=i, team="core")
    score <- match_score(poss)
    win <- match_winner(score)
    trys <- first_try(poss)
    goal <- first_goal(poss)
    full <- cbind(score,win,trys,goal)
    fsc <-rbind(fsc, c("round" = i, 
                     "matches" = nrow(score),
                     "try" = round(nrow(subset(full,full$win==full$trys))/nrow(full),2), 
                     "goal" = round(nrow(subset(full,full$win==full$goal))/nrow(subset(full,!is.na(full$goal))),2)))
  }
  View(fsc)
}

# Plot victory margins in pool play for 1st scorers and comebacks
plot_victory_margin <- function(round) {
  poss <- poss_filter(round=round)
  tm <- match_score(poss)
  tm <- cbind(tm, win = match_winner(tm), first = first_try(poss))
  tm1 <- tm %>% mutate(ws = ifelse(t2s>t1s, t2s, t1s)) %>% mutate(ls = ifelse(t2s>t1s, t1s, t2s))
  x <- tm1 %>% filter(first==win) %>% transmute(diff = ws - ls) 
  y <- tm1 %>% filter(first!=win) %>% transmute(diff = ws - ls)
  avmfr <- round(mean(x[,1],2)) # avm for front runners in pool play
  avmcb <- round(mean(y[,1],2)) # avm for come back winners in pool play
  x <- hist(x$diff, breaks=24)
  y <- hist(y$diff, breaks=24)
  plot(x,col=rgb(0,0,1,1/4), xlim=c(0,60), main=paste(round, "Victory Margin"), xlab="Winning Point Differential" )
  plot(y,col=rgb(1,0,0,1/4), xlim=c(0,60), add=T)
  abline(v=avmfr, col="Blue", lwd=4)
  abline(v=avmcb, col="Red", lwd=4)
  legend("topright", c("Front runner","Comeback"), pch=15,col=c(rgb(0,0,1,1/3),rgb(1,0,0,1/3)))
}

first_score_summary()
first_score_summary_core()
plot_victory_margin("Pool")
#plot_victory_margin("Cup")
