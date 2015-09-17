# Team directional tackling visualization
library(ggplot2)
library(dplyr)
library(tidyr)

# simulate tackles
#set.seed(9940)
T <- data.frame(dir = 1:12, 
                att = sample(5:12, 12, replace=TRUE))

# order tackles by type & get efficiency
T <- T %>% 
  mutate(made = att - ceiling(att*runif(12, 0, .3))) %>% 
  mutate(miss = att - made) %>%
  mutate(eff = round(made/att,2)) 

T$eff[is.nan(T$eff)] <- NA
             
# calculate stats
tmade <- sum(T$made)
tatt <- sum(T$att)
eff <- round(tmade/tatt,2)

# put data in long format to stack
Tl <- T %>% 
  gather(res, count, made:miss) %>% 
  select(dir, res, count)

# visualize team tackle data in a bar chart
ggplot(Tl, aes(factor(dir), count, fill=factor(res))) + 
  geom_bar(width=.75, stat="identity") +
  scale_fill_grey(name="Tackle\nResult", start=.5, end=.1) + 
  xlab("Direction") +
  ylab("Tackle Count") +
  ggtitle("Tackle Attempts by Direction")

# visualize team tackle data in a consultant chart
ggplot(Tl, aes(factor(dir), count, fill=factor(res))) + 
  geom_bar(width=1, stat="identity", color="white") + coord_polar() + 
  scale_fill_grey(name="Tackle\nResult", start=.5, end=.1) + 
  theme(axis.ticks=element_blank(), 
        axis.text.y=element_blank()) + 
  xlab("Direction") +
  ylab("Tackle Count") +
  ggtitle("Tackle Attempts by Direction")