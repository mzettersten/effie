library(tidyverse)
library(here)



#### read in data ####

d <- read_delim(here("..","data","processed_data","2020-06-17_Combined_Results_Headturn3.txt"),delim="\t")
subj <- read_csv(here("..","data","Effie_subj_deID.csv"))

#rename columns
colnames(d)[colnames(d)=="Subject ID"] <- "subject"
colnames(d)[colnames(d)=="Looking Time (ms)"] <- "looking_time"


#remove empty rows
subj <- subj %>%
  filter(!is.na(Sub))

#only test trials
d <- d %>%
  filter(!is.na(SoundFile) & SoundFile!= "-")

#fix one repeat subject id
d$subject <- ifelse(d$subject=="040","effie_041",d$subject)

#### clean up subject names ####
d <- d %>%
  mutate(participant_id = case_when(
      subject == "002" ~ "effie_002",
      subject == "003" ~ "effie_003",
      subject == "004" ~ "effie_004",
      subject == "005" ~ "effie_005",
      subject == "006" ~ "effie_006",
      subject == "007" ~ "effie_007",
      subject == "008" ~ "effie_008",
      subject == "009" ~ "effie_009",
      subject == "010" ~ "effie_010",
      subject == "011" ~ "effie_011",
      subject == "012" ~ "effie_012",
      subject == "013" ~ "effie_013",
      subject == "014" ~ "effie_014",
      subject == "015" ~ "effie_015",
      subject == "016" ~ "effie_016",
      subject == "Effie_017" ~ "effie_017",
      subject == "018" ~ "effie_018",
      subject == "019" ~ "effie_019",
      subject == "020" ~ "effie_020",
      subject == "021" ~ "effie_021",
      subject == "022" ~ "effie_022",
      subject == "023" ~ "effie_023",
      subject == "Effie_024" ~ "effie_024",
      subject == "025" ~ "effie_025",
      subject == "026" ~ "effie_026",
      subject == "028" ~ "effie_028",
      subject == "029" ~ "effie_029",
      subject == "030" ~ "effie_030",
      subject  == "Effie_031"  ~ "effie_031",
      subject == "033" ~ "effie_033",
      subject == "034" ~ "effie_034",
      subject == "035" ~ "effie_035",
      subject == "036" ~ "effie_036",
      subject == "037" ~ "effie_037",
      subject == "039" ~ "effie_039",
      subject == "042" ~ "effie_042",
      subject == "043" ~ "effie_043",
      subject == "044" ~ "effie_044",
      subject == "045" ~ "effie_045",
      subject == "046" ~ "effie_046",
      subject == "047" ~ "effie_047",
      subject == "048" ~ "effie_048",
      subject == "051" ~ "effie_051",
      subject == "053" ~ "effie_053",
      subject == "054" ~ "effie_054",
      subject == "Effie_055 " ~ "effie_055",
      subject == "056" ~ "effie_056",
      subject == "057" ~ "effie_057",
      subject == "058" ~ "effie_058",
      subject == "059" ~ "effie_059",
      subject == "061" ~ "effie_061",
      TRUE ~ subject
    )
  )

d <- d %>%
  mutate(participant_id=trimws(participant_id))

subj <- subj %>%
  mutate(participant_id=tolower(Sub))

intersect(unique(d$participant_id),unique(subj$participant_id))

#### join
d <- d %>%
  left_join(subj,by=c("participant_id"))

#define test items
d <- d %>%
  mutate(test_item_file = str_remove(SoundFile,"C:\\\\Experiment\\\\Stimuli\\\\Effie\\\\Audio\\\\")) %>%
  mutate(test_item = str_remove(test_item_file,"_test.wav"))

#redefine condition
d <- d %>%
  rename(training_condition = Condition) %>%
  mutate(training_condition = case_when(
    participant_id == "test" ~ "L1_2",
    TRUE ~ training_condition
  )) %>%
  separate(col="training_condition",into=c("language","frequency"),sep="_", remove=F)

#define trial conditions
l1_consistent <- c("manu","kita")
l2_consistent <- c("sarel","boskot")

d <- d %>%
  mutate(trial_condition = case_when(
    test_item %in% l1_consistent ~ "L1_consistent",
    test_item %in% l2_consistent ~ "L2_consistent",
    TRUE ~ ""
  )) %>%
  mutate(novel_familiar = case_when(
    training_condition %in% c("L1_2","L1_8") & trial_condition == "L1_consistent" ~ "familiar",
    training_condition %in% c("L2_2","L2_8") & trial_condition == "L1_consistent" ~ "novel",
    training_condition %in% c("L1_2","L1_8") & trial_condition == "L2_consistent" ~ "novel",
    training_condition %in% c("L2_2","L2_8") & trial_condition == "L2_consistent" ~ "familiar",
  )) 

#rename frequency condition
d <- d %>%
  mutate(frequency_condition= case_when(
    frequency=="2" ~ "4 Occurrences",
    frequency=="8" ~ "16 Occurrences"
  ))

#rename age
d <- d %>%
  rename(age = `Age (adjusted)`)

# write file
write.csv(d,here("..","data","processed_data","effie_test_data_processed.csv"),row.names=F)

