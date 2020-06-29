library(tidyverse)
library(here)

# read in data
d <- read_csv(here("..","data","processed_data","effie_test_data_processed.csv")) %>%
  rename(include_yn=`Include?`)

#read in data check file
data_check <- read_csv(here("..","data_check","MASTER_Effie_Coding_trialByTrialCheck_updated.csv")) %>%
  select(-Block,-BlockTrialNumber) %>%
  rename(Trial=TrialNumber, participant_id=subjCode)

#join data an data_check
d <- d %>%
  left_join(data_check)

#### Trial-Level Exclusions ####

### Criteria
## Less than 2s looking

## Other Trial Exclusions: experimenter error, parental interference, fussiness
# based on combining online experimenters' notes and hand-coded from video by researchers blinded to participant data
# currently not completely differentiated/ classified but can be determined based on notes

#percent trials to be removed
d %>%
  summarize(
    N=n(),
    trials_less_than_2s = sum(looking_time<2000),
    percent_less_then_2s_exclusions = trials_less_than_2s/N,
    exclude_other = sum(excludeTrial=="Y",na.rm=T),
    percent_exclude_other = exclude_other/N,
    exclude_other_more_than_2s = sum(excludeTrial=="Y"&looking_time>=2000,na.rm=T),
    percent_exclude_other_more_than_2s = exclude_other_more_than_2s/N
  )

#filter
d_clean <- d %>%
  filter(looking_time>=2000) %>%
  filter(!(excludeTrial %in% c("Y")))

# add new trial counter after trial exclusions
d_clean <- d_clean %>%
  group_by(participant_id) %>%
  arrange(participant_id,Trial) %>%
  mutate(trial_counter = cumsum(!is.na(Trial))) %>%
  mutate(block_counter=case_when(
    trial_counter<5 ~ 1,
    trial_counter<9 ~ 2,
    trial_counter<13 ~ 3,
    TRUE ~ 4
  ))

# write file
write.csv(d_clean,here("..","data","processed_data","effie_test_data_processed_post_trial_exclusion.csv"),row.names=F)

#### Participant-Level Exclusions ####

## general subject exclusions (from participant log)

## participants must contribute at least 8 useable trials
participant_exclusions <- d_clean %>%
  group_by(participant_id) %>%
  summarize(
    num_useable_trials=sum(!is.na(looking_time)),
    include_yn=include_yn[1]
  ) %>%
  ungroup() %>%
  mutate(participant_exclude = case_when(
    num_useable_trials<8 ~ "y",
    include_yn=='N' ~ "y",
    TRUE ~ "n"
  ))

hist(participant_exclusions$num_useable_trials)
table(participant_exclusions$include_yn)

#number of participants removed
participant_exclusions %>%
  summarize(
    N=n(),
    exclude_trialCount = sum(num_useable_trials<8),
    exclude_subjLog = sum(include_yn=='N'),
    exclude_subjLog_sufficientTrials = sum(include_yn=='N'&num_useable_trials>=8),
  )

# join and clean data
d_clean <- d_clean %>%
  left_join(participant_exclusions)
#remove excluded participants
d_clean <- d_clean %>%
  filter(participant_exclude=="n")

# write file
write.csv(d_clean,here("..","data","processed_data","effie_test_data_processed_post_exclusion.csv"),row.names=F)

