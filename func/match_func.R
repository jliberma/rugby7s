library("dplyr")

# These functions take a match_scores data frame as input
# produced by the match_score function.
# avg_victory_margin: calculates the mean margin of victory
# for all winning teams in the match_scores data frame.
# match_winner: determines who won each match in a match_scores
# data frame.

avg_victory_margin <- function(match_scores) {
  match_scores %>% mutate(diff = abs(t1s-t2s)) %>%
    summarise(avm = round(mean(diff),2))
}

match_winner <- function(match_scores) {   
  # who won each match?
  match_scores %>% 
    transmute(win = ifelse(t1s > t2s, paste(team1), 
                           ifelse(t2s > t1s, paste(team2), NA)))
}