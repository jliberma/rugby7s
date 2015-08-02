library("dplyr")

# These functions take possession data as input.
# first_goal: determines which team scored the first
# converted try in a match.
# first_try: determines which team scored the first
# try in a match.
# match_score: scores all possessions in a match to
# find final score.
# poss_filter: returns a data frame of possession data
# that these functions take as input.

first_goal <- function(poss) {
  sapply(split(poss, poss$SERIES.MATCH),
         function(x) if (suppressWarnings(min(grep("TRY|PT", x$LOST)) == 
                                            min(grep("converted", x$LOST.NOTE))))
         { return(paste(x[min(grep("TRY|PT", x$LOST)),]$TEAM)) } else
         { return(NA) }
  )
}

first_try <- function(poss) {
  
  sapply(split(poss,poss$SERIES.MATCH), 
         function(x) paste(x[min(grep("TRY|PT", x$LOST)),]$TEAM))  
}

# returns a data frame of match scores
match_score <- function(poss) { 
  
  # calculate scores for all matches
  ws7s_match_scores <- poss %>% 
    select(match=SERIES.MATCH, team=TEAM, opp=OPP, lost=LOST, note=LOST.NOTE) %>%
    mutate(score = ifelse(grepl("converted", note),7,
                          ifelse(grepl("TRY|PT", lost),5,
                                 ifelse(grepl("PG|DG", lost),3,0))
    )) %>% 
    group_by(match, team) %>%
    summarise(score = as.integer(sum(score)))
  
  # combine per-row results into single columns
  odd <- ws7s_match_scores[seq(1, nrow(ws7s_match_scores), 2),]
  colnames(odd) <- c("match", "team1", "t1s")
  even <- ws7s_match_scores[seq(2, nrow(ws7s_match_scores), 2),]
  colnames(even) <- c("match", "team2", "t2s")
  merge(odd, even)
}

# returns a data frame of possession data
poss_filter <- function(matches=1:315, team="all", event="all", round="all") { 
  
  # download the data file and save it to an object
  if (!file.exists("ws72014_poss_8.csv")) {
    download.file("https://raw.githubusercontent.com/jliberma/rugby7s/master/data/ws72014_possession_stats.csv", 
                  destfile="ws72014_poss_8.csv", method="curl")
  }
  ws7s_poss <- read.csv("ws72014_poss_8.csv", header=TRUE, stringsAsFactors=FALSE)
  
  # validate and subset by event
  if (event != "all") { 
    if (!event %in% unique(ws7s_poss$EVENT)) stop('invalid event')
    ws7s_poss <- ws7s_poss %>% filter(EVENT==event)
  }
  
  # validate and subset by round
  if (round != "all") { 
    if (!round %in% unique(ws7s_poss$ROUND)) stop('invalid round')
    ws7s_poss <- ws7s_poss %>% filter(ROUND==round)
  }
  
  # validate and subset by team
  if (team != "all" & team != "core" ) {
    if (!team %in% unique(ws7s_poss$TEAM)) stop('invalid team')
    ws7s_poss <- ws7s_poss %>% filter(TEAM==team|OPP==team)
  } else if (team == "core") {
    # filter out all non-core teams
    ws7s_poss <- ws7s_poss %>%
      filter(TEAM != "BRA" & TEAM != "ASM" & TEAM != "ZIM" & TEAM != "PNG") %>%
      filter(OPP != "BRA" & OPP != "ASM" & OPP != "ZIM" & OPP != "PNG")
  } else return(ws7s_poss)

}