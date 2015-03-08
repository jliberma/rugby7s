library("dplyr")

match_score <- function(matches=1:180, team="all", event="all", round="all") { 
  
  # load data file
  ws7s_poss <- read.csv("ws72014_poss_4.csv", header=TRUE, stringsAsFactors=FALSE)

  # validate input and subset data
  if (event != "all") { 
    if (!event %in% unique(ws7s_poss$EVENT)) stop('invalid event')
    ws7s_poss <- subset(ws7s_poss,ws7s_poss$EVENT==event)
    matches <- unique(ws7s_poss$SERIES.MATCH)
  }
  if (round != "all") { 
    if (!round %in% unique(ws7s_poss$ROUND)) stop('invalid round')
    ws7s_poss <- subset(ws7s_poss,ws7s_poss$ROUND==round)
    matches <- unique(ws7s_poss$SERIES.MATCH)
  }
  if (team != "all") { 
    if (!team %in% unique(ws7s_poss$TEAM)) stop('invalid team')
    ws7s_poss <- subset(ws7s_poss,(ws7s_poss$TEAM==team|ws7s_poss$OPP==team))
    matches <- unique(ws7s_poss$SERIES.MATCH)
  }
  
  # define data structures
  team1 <- numeric(0)
  team2 <- numeric(0)
  first <- character(0)
  win <- character(0)
  goal <- logical(0)
  team1.score <- numeric(0)
  team2.score <- numeric(0)

  for (i in 1:length(matches)) {
    # subset the data by match
    poss <- subset(ws7s_poss,ws7s_poss$SERIES.MATCH==matches[i])
    
    # teams
    team1[i] <- paste(poss[1,]$TEAM)
    team2[i] <- paste(poss[1,]$OPP)

    # first score
    first[i] <- paste(poss[min(grep("TRY", poss$LOST)),]$TEAM)
    if (suppressWarnings(min(grep("TRY", poss$LOST)) == min(grep("converted", poss$LOST.NOTE))))
      { goal[i] <- TRUE } else
        { goal[i] <- FALSE }
    
    # subset possessions per team  
    team1.poss <- subset(poss,poss$TEAM==team1[i])
    team2.poss <- subset(poss,poss$TEAM==team2[i])
    
    # calculate score
    team1.score[i] <- 5 * sum(team1.poss$LOST=="TRY"|team1.poss$LOST=="PT") + 2 * length(grep("TRY: converted", team1.poss$LOST.NOTE))
    team2.score[i] <- 5 * sum(team2.poss$LOST=="TRY"|team2.poss$LOST=="PT") + 2 * length(grep("TRY: converted", team2.poss$LOST.NOTE))  
    
    # track winner, NA for tie
    win[i] <- ifelse(team1.score[i] > team2.score[i], paste(team1[i]),
            ifelse(team2.score[i] > team1.score[i], paste(team2[i]), NA))
    }
  
  # create a match score data.frame
  match.score<-cbind.data.frame(matches,team1,team1.score,team2,team2.score,first,win,goal,stringsAsFactors = FALSE)
  colnames(match.score) <- c("match","t1","t1s","t2","t2s","first","win","goal")
  return(match.score)
}  

first_try <- function(matches) {
  x <- round(sum(matches$first == matches$win, na.rm=TRUE)/nrow(matches),2)
}

first_goal <- function(matches) {
  first_try(filter(matches, goal=="TRUE"))
}

avg_victory_margin <- function(matches) {
  x <- matches %>% 
    mutate(diff = abs(t1s-t2s)) %>% 
    summarise(avm = round(mean(diff),2))
}

# EXAMPLE CODE:
#> source('~/rugby7s/match_scores.R')
#> USA_pool <- match_score(team="USA", round="Pool")
#> USA_pool
#match  t1 t1s  t2 t2s first win  goal
#1      2 USA  17 CAN  15   USA USA  TRUE
#2      9 USA   7 ENG  12   ENG ENG FALSE
#3     17 ARG  26 USA  14   ARG ARG FALSE
#4     52 USA  10 ENG  19   USA ENG FALSE
#5     61 USA  10 AUS  26   USA AUS FALSE
#6     68 USA  31 KEN  12   USA USA  TRUE
#7     97 USA   0 RSA  26   RSA RSA FALSE
#8    106 WAL  12 USA  38   WAL USA FALSE
#9    113 USA  33 KEN   5   USA USA FALSE
#10   137 USA  33 JPN  12   JPN USA FALSE
#11   145 FRA   7 USA  38   USA USA FALSE
#12   153 RSA  26 USA  14   USA RSA  TRUE
#13   182 USA  52 JPN  12   USA USA  TRUE
#> print(first_try(USA_pool))
#[1] 0.62
#> print(first_goal(USA_pool))
#[1] 0.75

# show all stats by round
first_score_summary <- function() { 
  ws7s_poss <- read.csv("ws72014_poss_4.csv", header=TRUE, stringsAsFactors=FALSE)
  stat_table <- c()
  for (i in c("all",unique(ws7s_poss$ROUND))) { 
    x <- match_score(round=i)
    stat_table <-rbind(stat_table, c(i, nrow(x),first_try(x),first_goal(x),avg_victory_margin(x)))
  }
  colnames(stat_table) <- c("round","matches","try","goal","avm")
  print(stat_table)
  #View(stat_table)
}
first_score_summary()

# Plot victory margins in pool play for 1st scorers and comebacks
tm <- match_score(round="Pool")
tm1 <- tm %>% mutate(ws = ifelse(t2s>t1s, t2s, t1s)) %>% mutate(ls = ifelse(t2s>t1s, t1s, t2s))
x <- tm1 %>% filter(first==win) %>% transmute(diff = ws - ls) 
y <- tm1 %>% filter(first!=win) %>% transmute(diff = ws - ls)
x<-hist(x$diff, breaks=24)
y<-hist(y$diff, breaks=24)
plot(x,col=rgb(0,0,1,1/4), xlim=c(0,60), main="Pool Victory Margins", xlab="Points" )
plot(y,col=rgb(1,0,0,1/4), xlim=c(0,60), add=T)
legend("topright", c("First Scorer","Comeback"), pch=15,col=c(rgb(0,0,1,1/3),rgb(1,0,0,1/3)))

# what is the avm for score 1st vs comeback in pool play?
# what happens if we remove the non-core teams from this?
