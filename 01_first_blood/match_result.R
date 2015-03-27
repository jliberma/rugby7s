library("dplyr")

# download the data file and save it to an object
download.file("https://raw.githubusercontent.com/jliberma/rugby7s/master/01_first_blood/ws72014_poss_5.csv", destfile="ws72014_poss_5.csv", method="curl")
ws7s_poss <- read.csv("ws72014_poss_5.csv", header=TRUE, stringsAsFactors=FALSE)

match_score <- function(matches=1:225, team="all", event="all", round="all") { 

  # validate and subset by event
  if (event != "all") { 
    if (!event %in% unique(ws7s_poss$EVENT)) stop('invalid event')
    ws7s_poss <- ws7s_poss %>% 
      filter(EVENT==event)
    matches <- unique(ws7s_poss$SERIES.MATCH)
  }

  # validate and subset by round
  if (round != "all") { 
    if (!round %in% unique(ws7s_poss$ROUND)) stop('invalid round')
    ws7s_poss <- ws7s_poss %>% 
      filter(ROUND==round)
    matches <- unique(ws7s_poss$SERIES.MATCH)
  }

  # validate and subset by team
  if (team != "all") { 
    if (!team %in% unique(ws7s_poss$TEAM)) stop('invalid team')
    ws7s_poss <- ws7s_poss %>% 
      filter(TEAM==team|OPP==team) 
    matches <- unique(ws7s_poss$SERIES.MATCH)
  }
  
  # calculate scores for all matches
  # note: need to add DG and PT
  ws7s_match_scores <- ws7s_poss %>% 
    select(match=SERIES.MATCH, team=TEAM, opp=OPP, lost=LOST, note=LOST.NOTE) %>%
    mutate(score = ifelse(grepl("converted", note),7,
	    ifelse(grepl("TRY", lost),5,
	    ifelse(grepl("PG", lost),3,0))
      )) %>% 
    group_by(match, team) %>%
    summarise(score = as.integer(sum(score)))
  
  # combine per-row results into single columns
  # question: is there a better way to do this?
  odd <- ws7s_match_scores[seq(1, nrow(ws7s_match_scores), 2),]
  colnames(odd) <- c("match", "team1", "t1s")
  even <- ws7s_match_scores[seq(2, nrow(ws7s_match_scores), 2),]
  colnames(even) <- c("match", "team2", "t2s")
  merge(odd, even)
}  

# Example code
#us7s <- match_score(event="USA7s")
#View(us7s)

#all <- match_score()
#View(all)

#NZL_pool <- match_score(team="NZL", round="Pool")
#View(NZL_pool)

FIJ_pool <- match_score(team="FIJ", round="Pool")
View(FIJ_pool)
