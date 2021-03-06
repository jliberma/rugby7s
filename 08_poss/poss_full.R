library("dplyr")
library("ggplot2")
library("ggthemes")
library("grid")
library("gridExtra")
library("scales")
source('~/rugby7s/func/poss_func.R')

setwd("/Users/jacobliberman/Documents/rugby7s/08_poss")

if (!file.exists("ws72014_poss.csv")) {
  download.file("https://raw.githubusercontent.com/jliberma/rugby7s/master/data/ws72014_possession_stats.csv", 
                destfile="ws72014_poss.csv", method="curl")
}

ws7s_poss <- read.csv("ws72014_poss.csv", header=TRUE, stringsAsFactors=FALSE)

# possessions per match
poss.per.match <- sapply(split(ws7s_poss,ws7s_poss$SERIES.MATCH), 
                         function(x) nrow(x))
summary(poss.per.match)
boxplot(poss.per.match)

# possessions per match by team
# possessions per match by round
# boxplot ppm by team/round

# create a histogram of possessions per match
ppm <- qplot(poss.per.match, binwidth=1) + 
  geom_vline(x=median(poss.per.match)+.5, colour="red") + 
  xlab("") + ylab("")

# create a density function of possessions per match
cdf <- ggplot(as.data.frame(poss.per.match), 
              aes(x=poss.per.match)) +
  stat_ecdf(geom="smooth") + 
  xlab("") + ylab("")

# plot them side by side
grid.arrange(ppm, cdf, ncol=2)

# possessions per team per match

# calculate scores of matches and return in wide format
all_scores <- match_score_poss(ws7s_poss)

# find winning possession margin
wpm <- all_scores %>% 
  filter(! points.1 == points.2) %>%
  #filter(points.1 == points.2) %>%
  mutate(wpm = ifelse(points.1 > points.2, 
                      count.1-count.2, 
                      count.2-count.1), 
         pos = wpm >= 0) %>%
  select(match, wpm, pos) %>%
  arrange(wpm)
# QUESTION: add ties to the analysis with pos false but abs(wpm)

summary(wpm[,2:3])
# on average, the winning team has 9 possessions
# versus 8 for the losing team
# QUESTION: what is this percentage?

# ties
# 9 ties: 2 1 1 1 1 1 0 0

# graph unsorted wpms by match
wpm1 <- ggplot(wpm, aes(x=match, y=wpm, fill=pos)) +
  geom_bar(stat="identity", position="identity") +
  geom_hline(yintercept=0, colour="grey") +
  theme(axis.title=element_blank()) +
  guides(fill=F) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
wpm1

# probability of winning by possession margin

# calculate tie possession margins
tie_scores <- all_scores %>% 
  filter(points.1==points.2) %>%
  transmute(abs(count.1-count.2))

# print win probability by possession margin
for (i in 1:3)
{
  j <- round(sum(wpm$wpm>=i)/(sum(abs(wpm$wpm)>=i) +
                          length(tie_scores[tie_scores>=i])),2) 
  print(paste("wpm >=",i,":",j))
}

# Match count at each wpm
l_total = data.frame()
for (i in sort(unique(abs(wpm$wpm))))
{
  # how to save this to a vector to be displayed later?
  j <- sum(abs(wpm$wpm)==i) + length(tie_scores[tie_scores==i])
  k <- round(sum(wpm$wpm==i)/(sum(abs(wpm$wpm)==i) +
                                + length(tie_scores[tie_scores==i])),2)
  print(paste(i,j,k))
  l <- data.frame(i,j,k)
  l_total <- rbind(l,l_total)
}

# create data frame of team possessions, opponent possessions, and 
# number of matches
# replace all of this crap with dataframe operations
poss.per.team <- sapply(split(ws7s_poss,ws7s_poss$TEAM), 
                        function(x) nrow(x))
poss.per.opp <- sapply(split(ws7s_poss,ws7s_poss$OPP), 
                        function(x) nrow(x))
match.per.team <- sapply(split(ws7s_poss,ws7s_poss$TEAM), 
                         function(x) length(unique(x$SERIES.MATCH)))
team.poss <- as.data.frame(cbind(poss.per.team, 
                                 poss.per.opp, match.per.team))
team.poss <- cbind(team = factor(names(poss.per.team)), 
                   team.poss)


# calculate total possession differential by team
team.poss.diff <- team.poss %>% 
  mutate(diff = round(poss.per.team-poss.per.opp,1)) %>%
  select(team, diff) %>%
  arrange(-diff)

# order factor levels for plotting
team.poss.diff$team <- factor(team.poss.diff$team, 
                                  levels=team.poss.diff$team[order(team.poss.diff$diff)])

tpd <- ggplot(team.poss.diff) + 
  geom_point(aes(x=team, y=diff), fill="gray", stat="identity") + 
  coord_flip() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype=3, color="darkgray"),
        axis.text.y=element_text(size=rel(0.8))) +
  ylab("") + xlab("")
tpd

# calculate average possession differential by team
avg.team.poss.diff <- team.poss %>% 
  mutate(diff = round((poss.per.team-poss.per.opp)/match.per.team,1)) %>%
  select(team, diff) %>%
  arrange(-diff)

# order factor levels for plotting
avg.team.poss.diff$team <- factor(avg.team.poss.diff$team, 
                              levels=avg.team.poss.diff$team[order(avg.team.poss.diff$diff)])

# this is the meaningful stat
# get average posession diff in cup round only
atpd <- ggplot(avg.team.poss.diff) + 
  geom_point(aes(x=team, y=diff), fill="gray", stat="identity") + 
  coord_flip() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype=3, color="darkgray"),
        axis.text.y=element_text(size=rel(0.8))) +
  ylab("") + xlab("")
atpd

# LOOK AT ATPD IN CUP ROUNDS ONLY, LABEL ON SAME GRAPH
ws7s_poss_cup <- ws7s_poss %>%
  filter(ROUND=="Pool" | ROUND == "Cup")

# create data frame of team possessions, opponent possessions, and 
# number of matches
poss.per.team.cup <- sapply(split(ws7s_poss_cup,ws7s_poss_cup$TEAM), 
                        function(x) nrow(x))
poss.per.opp.cup <- sapply(split(ws7s_poss_cup,ws7s_poss_cup$OPP), 
                       function(x) nrow(x))
match.per.team.cup <- sapply(split(ws7s_poss_cup,ws7s_poss_cup$TEAM), 
                         function(x) length(unique(x$SERIES.MATCH)))
team.poss.cup <- as.data.frame(cbind(poss.per.team.cup, 
                                 poss.per.opp.cup, match.per.team.cup))
team.poss.cup <- cbind(team = factor(names(poss.per.team.cup)), 
                   team.poss.cup)
# calculate average possession differential by team
avg.team.poss.diff.cup <- team.poss.cup %>% 
  mutate(diff = round((poss.per.team.cup-poss.per.opp.cup)/match.per.team.cup,1)) %>%
  select(team, diff) %>%
  arrange(-diff)

# order factor levels for plotting
avg.team.poss.diff.cup$team <- factor(avg.team.poss.diff.cup$team, 
                                  levels=avg.team.poss.diff.cup$team[order(avg.team.poss.diff.cup$diff)])

# average posession diff in cup round only
atpd_cup <- ggplot(avg.team.poss.diff.cup) + 
  geom_point(aes(x=team, y=diff), fill="gray", stat="identity") + 
  coord_flip() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype=3, color="darkgray"),
        axis.text.y=element_text(size=rel(0.8))) +
  ylab("") + xlab("")
atpd_cup

# sources of possession advantage
# reclaims, restart penalties, restart infractions
# holding ball when time expires

# should we remove non-core teams from the analysis?
# or just say more analysis is needed
# the main point is:
# everyone agreess possession is important
# but we have not see a useful start that quantifies possession advantage
# time of possession excludes time ball is out of play
# best teams do not show ab advantage of ball in play time
# passes and other secondary metrics are related to possession
# but most likely indicate inability to score
# doesnt capture the dominant nature of 7s possession
# count the number of scoring opportunities
# small number, small margin between success and failure
# also captures the importance of restart
# includes dead ball as well as time in play
# possession advantages lead to wins
# investigate per-team possession differentials
# remove non-core teams from the analysis
# also look at possession efficiency, which is the number of 
# possessions that result in scores
# key points:

# 17 possessions in a typical IRB 7s match
# the winner averages 9 possessions while the loser averages 8
# each team gets approximately 4 scoring opportunities per half in
# a team with a 1 possession advantage wins 80% of the time
# a team with a 2 possession advantage wins 90% of the time
# a team with a 3 possession advantage wins 98% of the time

# in x$ of matches the teams had equal possessions
# sources of additional possessions include reclaims and holding ball
# when time expires -- either to score or run out the clock
# instead of thinking about possession in terms of time ball is in play,
# think of it as a number that encompasses both ball in play and out
# underscores the critical importance of both winning your own ball
# and taking from opponents, or holding at the end of half
# did not discuss possession efficiency, which is ratio of scoring
# to non-scoring possessions

# efficiency

