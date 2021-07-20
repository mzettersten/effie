library(tidyverse)

#read in file
d <- read_csv("effie_sequential_sim.csv")

d <- d %>%
  group_by(mean,SD,d,BF_threshold,n_min,n_max,simulation_round) %>%
  mutate(max_round_n=max(n)) %>%
  mutate(final_row=ifelse(n==max_round_n,1,0)) %>%
  mutate(correct_decision=case_when(
    d==0 & bf<=1/BF_threshold ~ "correct",
    d==0 & bf>=BF_threshold ~ "incorrect",
    d>0 & bf>=BF_threshold ~ "correct",
    d>0 & bf<=1/BF_threshold ~ "incorrect",
    TRUE ~ "unclear"))

count_summary <- d %>%
  filter(final_row==1) %>%
  group_by(mean,SD,d,BF_threshold,n_min,n_max,correct_decision) %>%
  summarize(
    n=n()
  ) %>%
  group_by(mean,SD,d,BF_threshold,n_min,n_max) %>%
  mutate(
    proportion=n/sum(n)
  )

ggplot(count_summary,aes(x=n_max,y=proportion,fill=correct_decision))+
  geom_bar(stat="identity",position="stack")+
  facet_wrap(~BF_threshold+d, nrow=3)

ggsave("decision_plot.png")


