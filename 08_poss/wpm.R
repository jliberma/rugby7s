library("dplyr")
library("ggplot2")
library("grid")
library("gridExtra")
source('~/rugby7s/func/poss_func.R')

# TODO
# Add ties to the analysis
# Add match count to winning possession margin

if (!file.exists("ws72014_poss.csv")) {
  download.file("https://raw.githubusercontent.com/jliberma/rugby7s/master/data/ws72014_possession_stats.csv", 
                destfile="ws72014_poss.csv", method="curl")
}

ws7s_poss <- read.csv("ws72014_poss.csv", header=TRUE, stringsAsFactors=FALSE)

poss.per.match <- sapply(split(ws7s_poss,ws7s_poss$SERIES.MATCH), 
                         function(x) nrow(x))

summary(poss.per.match)

# ppm without finals
ws7s_poss_nofinals <- ws7s_poss %>%
  filter(!MATCH==45)
poss.per.match.nofinals <- sapply(split(ws7s_poss_nofinals,ws7s_poss_nofinals$SERIES.MATCH), 
                                  function(x) nrow(x))
summary(poss.per.match.nofinals)
# MAX 31 -> 28
# Mean 17.02 -> 16.89
# Median stays same

# ppm finals only
ws7s_poss_finals <- ws7s_poss %>%
  filter(MATCH==45)
poss.per.match.finals <- sapply(split(ws7s_poss_finals,ws7s_poss_finals$SERIES.MATCH), 
                                function(x) nrow(x))
summary(poss.per.match.finals)
# finals range: 15-31
# Median 15/17/19 -> 18/22/27
# Mean: ~17 -> ~23

# histogram of possessions per match
ppm <- qplot(poss.per.match, binwidth=1) + 
  geom_vline(xintercept=median(poss.per.match)+.5, colour="red") +
  theme(text = element_text(family = "Trebuchet MS", color="#666666")) +
  theme(plot.title = element_text(face="bold", size=24, hjust=0)) +
  xlab("Possessions") + ylab("Count")

# cumulative distribution function of possessions per match
cdf <- data.frame(x=c(poss.per.match,poss.per.match.nofinals),
                  ggg=factor(rep(1:2, c(length(poss.per.match),
                                        length(poss.per.match.nofinals)))))
cdf_plot <- ggplot(cdf, aes(x, colour=ggg)) +
  theme(legend.position = c(0.8, 0.3)) +
  xlab("Possessions") + ylab("Probability") + 
  stat_ecdf(geom="step") +
  theme(text = element_text(family = "Trebuchet MS", color="#666666")) +
  theme(plot.title = element_text(face="bold", size=24, hjust=0)) +
  scale_y_continuous(position = "right") +
  scale_colour_hue(name="Legend", labels=c('FINALS','NOFINALS'))

# plot histogram and cdf side by side
title1=textGrob("Possessions per match", gp=gpar(fontface="bold", 
                                                 fontfamily="Trebuchet MS",
                                                 fontsize=24, col="#666666"))
caption1=textGrob("Source: http://starting7s.com", gp=gpar(fontsize=10,
                                                           fontfamily="Trebuchet MS",
                                                           col="#666666"))

grid.arrange(ppm, cdf_plot, ncol=2, 
             top = title1,
             bottom = caption1
             )

all_scores <- match_score_poss(ws7s_poss)

tie_scores <- all_scores %>% 
  filter(points.1==points.2) %>%
  transmute(abs(count.1-count.2))

wpm <- all_scores %>% 
  filter(! points.1 == points.2) %>%
  mutate(wpm = ifelse(points.1 > points.2, 
                      count.1-count.2, 
                      count.2-count.1), 
         pos = wpm >=0) %>%
  select(match, wpm, pos) %>%
  arrange(wpm)

summary(wpm[,2])

# plot winning possession margin
# TODO: add a legend
wpm1 <- ggplot(wpm, aes(x=match, y=wpm, fill=pos)) +
  geom_bar(stat="identity", position="identity") +
  geom_hline(yintercept=0, colour="grey") +
  labs(title="Winning Possession Margin", subtitle="World Rugby Sevens Series 2014-2015", 
       y="Possession Margin", x="", caption="Source: http://starting7s.com") +
  theme(text = element_text(family = "Trebuchet MS", color="#666666")) +
  theme(plot.title = element_text(face="bold", size=24, hjust=0)) +
  guides(fill=F) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
print(wpm1)

# calculate win probability by possession margin
output <- matrix(ncol=4, nrow=7)
for (i in 1:7)
{
  j <- round(sum(wpm$wpm==i)/(sum(abs(wpm$wpm)==i) +
                                length(tie_scores[tie_scores==i])),2) 
  output[i,1] <- i
  output[i,2] <- j
  output[i,3] <- round(sum(wpm$wpm==i),2)
  output[i,4] <- round(sum(abs(wpm$wpm)==i) + length(tie_scores[tie_scores==i]),2)
}
output <- data.frame(output)
colnames(output)<-c("margin","prob","win","total")

# create a bar plot of win probability by possession margin
# TODO: change to a stacked bar plot with won/(total-won) & labeled with probability
nnp<-ggplot(data=output, aes(x=margin, y=prob)) +
  geom_bar(stat="identity", width=0.75, fill="steelblue") +
  geom_text(aes(label=total), position=position_dodge(width=0.8), hjust=1.5) +
  scale_x_continuous(breaks=seq(1:7)) +
  annotate("rect", xmin = 3.5, xmax = 7.5, ymin = -.009, ymax = 1.01,
             alpha = .3) +
  annotate("text", x = 5.5, y = .5, label = "Possession advantage >= 4 won 29/29") +
  coord_flip() +
  labs(title="Win Probability by Possession Advantage", subtitle="2014-2015 World Rugby Sevens Series", 
       y="Win Probability", x="Possession Advantage", caption="Source: http://starting7s.com") +
  theme(text = element_text(family = "Trebuchet MS", color="#666666")) +
  theme(plot.title = element_text(face="bold", size=24, hjust=0))
print(nnp)