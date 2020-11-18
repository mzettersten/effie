library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(cowplot)
theme_set(theme_cowplot())
library(gghalves)
library(wesanderson)
library(psych)
library(TOSTER)
library(BayesFactor)

#### DESCRIPTIVES ####

#### read in processed data

d <- read_csv(here("..","data","processed_data","effie_test_data_processed_post_exclusion.csv"))

#descriptives
sample_properties <- d %>%
  group_by(participant_id,Sex,age) %>%
  summarize() %>%
  ungroup() %>%
  summarize(
    N=n(),
    mean_age=mean(age),
    sd_age=sd(age),
    female_n=sum(Sex=="F")
  )
sample_properties


## describe general test trial looking length ##
ggplot(d,aes(x = Trial,y=looking_time))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept=2000)

ggplot(d,aes(x = Trial,y=looking_time))+
  geom_violin(aes(x = as.factor(Trial)))+
  geom_point(alpha=0.5)+
  geom_smooth()+
  geom_hline(yintercept=2000)
ggsave(here("..","figures","distribution_test_looking.pdf"),width=7,height=6)
ggsave(here("..","figures","distribution_test_looking.png"),width=7,height=6)
  

##### summarize within subject ####
subj_novel_familiar <- d %>%
  group_by(participant_id,Experimenter,training_condition,language,frequency,frequency_condition,age,Sex,novel_familiar) %>%
  summarize(n=n(),mean_looking_time=mean(looking_time), sd_looking_time = sd(looking_time))
  
#compute looking time difference metrics
subj_diff <- subj_novel_familiar %>%
  group_by(participant_id,Experimenter,training_condition,language,frequency,frequency_condition,age,Sex) %>%
  summarize(
    N=n(),
    diff_looking_time=mean_looking_time[novel_familiar=="novel"]-mean_looking_time[novel_familiar=="familiar"],
    total_average_looking_time = mean_looking_time[novel_familiar=="novel"]+mean_looking_time[novel_familiar=="familiar"],
    rel_preference=diff_looking_time/total_average_looking_time)

#overall summaries
overall_novel_familiar <- subj_novel_familiar %>%
  group_by(frequency_condition,novel_familiar) %>%
  summarize(
    N=n(),
    overall_looking_time=mean(mean_looking_time),
    sd = sd(mean_looking_time),
    se = sd/sqrt(N),
    ci=qt(0.975, N-1)*sd/sqrt(N),
    lower_ci=overall_looking_time-ci,
    upper_ci=overall_looking_time+ci,
    ) %>%
  ungroup() %>%
  mutate(frequency_condition_ordered=factor(frequency_condition,levels=c("4 Occurrences","16 Occurrences")))

overall_diff <- subj_diff %>%
  group_by(frequency,frequency_condition) %>%
  summarize(
    N=n(),
    diff_looking=mean(diff_looking_time),
    sd=sd(diff_looking_time),
    ci=qt(0.975, N-1)*sd(diff_looking_time,na.rm=T)/sqrt(N),
    lower_ci=diff_looking-ci,
    upper_ci=diff_looking+ci,
    rel_pref=mean(rel_preference),
    rel_ci=qt(0.975, N-1)*sd(rel_preference,na.rm=T)/sqrt(N),
    lower_rel_ci=rel_pref-rel_ci,
    upper_rel_ci=rel_pref+rel_ci
    )

#### TRADITIONAL HEADTURN ANALYSES ####

#### 1) Effect within each condition

## 16 Occurrences Condition
t.test(mean_looking_time~novel_familiar,data=filter(subj_novel_familiar,frequency_condition=="16 Occurrences"),paired=T,var.equal=T)

## 4 Occurrences Condition
t.test(mean_looking_time~novel_familiar,data=filter(subj_novel_familiar,frequency_condition=="4 Occurrences"),paired=T,var.equal=T)

#### 2) Condition difference
m <- lm(diff_looking_time~frequency_condition,data=subj_diff)
summary(m)

#### Plot

pal <- wes_palette("Rushmore1", n=5)
subj_novel_familiar <- subj_novel_familiar %>%
  ungroup() %>%
  mutate(frequency_condition_ordered=factor(frequency_condition,levels=c("4 Occurrences","16 Occurrences"))) %>%
  arrange(frequency_condition_ordered,participant_id,novel_familiar)
ggplot(subj_novel_familiar,aes(x=novel_familiar,y=mean_looking_time, fill=novel_familiar))+
  geom_half_violin(data=filter(subj_novel_familiar, novel_familiar=="novel"),position = position_nudge(x = .1, y = 0), width=1,trim = FALSE, alpha = .8,color=NA,side="r")+
  geom_half_violin(data=filter(subj_novel_familiar, novel_familiar=="familiar"),position = position_nudge(x = -.1, y = 0), width=1,trim = FALSE, alpha = .8,color=NA,side="l")+
  geom_line(aes(group=participant_id),color="black",fill=NA,alpha=0.2,size=0.75,position=position_jitter(width=0.05,seed=1))+
  geom_point(aes(color=novel_familiar,group=participant_id), size = 2.5, alpha=0.5,position=position_jitter(width=0.05,seed=1))+
  geom_point(data=overall_novel_familiar,aes(y=overall_looking_time),color="black",size=5)+
  geom_line(data=overall_novel_familiar,aes(y=overall_looking_time,group=1),color="black",size=1.3)+
  geom_errorbar(data=overall_novel_familiar,aes(y=overall_looking_time,ymin=lower_ci,ymax=upper_ci),width=0,color="black")+
  #geom_boxplot(outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  #scale_colour_brewer(palette = "Dark2")+
  #scale_fill_brewer(palette = "Dark2")+
  scale_colour_manual(values=pal[c(3,4)])+
  scale_fill_manual(values=pal[c(3,4)])+
  facet_wrap(.~frequency_condition_ordered)+
  theme(legend.position="none")+
  xlab("Stimulus Type")+
  ylab("Looking Time (in ms)")+
  theme(axis.title.x = element_text(face="bold", size=20),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16),
        strip.text.x = element_text(size = 16,face="bold"))

ggsave(here("..","figures","main_results.pdf"),width=7,height=6)
ggsave(here("..","figures","main_results.png"),width=7,height=6)


#### LINEAR MIXED-EFFECTS ANALYSIS ####
#compute log looking
hist(d$looking_time)
d <- d %>%
  mutate(log_looking_time=log(looking_time))
hist(d$log_looking_time)

#center trial type and condition
d <- d %>%
  mutate(
    novel_familiar_c = if_else(novel_familiar=="familiar",-0.5,0.5),
    frequency_condition_c = if_else(frequency_condition=="4 Occurrences",-0.5,0.5),
    frequency_condition_4 = if_else(frequency_condition=="4 Occurrences",0,1),
    frequency_condition_16 = if_else(frequency_condition=="4 Occurrences",-1,0),
  )

## fit main model
m <- lmer(log_looking_time ~ novel_familiar_c * frequency_condition_c + 
       (1 + novel_familiar_c | participant_id) + 
       (1 + novel_familiar_c*frequency_condition_c | test_item), 
     data=d,control=lmerControl(optimizer="bobyqa"))
#yields singular fit, apparently due to covariance between by-item random effects --> simplify by removing by-item random slopes
#maximal converging model (comparable results to the maximal model with singular fit)
m <- lmer(log_looking_time ~ novel_familiar_c * frequency_condition_c + 
            (1 + novel_familiar_c | participant_id) + 
            (1 | test_item), 
          data=d,
          control=lmerControl(optimizer="bobyqa"))
summary(m)

#### 1) Effect within each condition

## 16 Occurrences Condition
m <- lmer(log_looking_time ~ novel_familiar_c * frequency_condition_16 + 
            (1 + novel_familiar_c | participant_id) + 
            (1 | test_item), 
          data=d,
          control=lmerControl(optimizer="bobyqa"))
summary(m)

## 4 Occurrences Conditon
m <- lmer(log_looking_time ~ novel_familiar_c * frequency_condition_4 + 
            (1 + novel_familiar_c | participant_id) + 
            (1 | test_item), 
          data=d,
          control=lmerControl(optimizer="bobyqa"))
summary(m)

#### 2) Condition Difference
#Non-significant interaction in the main model indicates no change in effect depending on condition, i.e. no condition difference

#### DIRECTION OF PREF COUNTS ####
subj_diff <- subj_diff %>%
  mutate(preference_cat = if_else(diff_looking_time>0,"novelty_preference","familiarity_preference"))

preference_count <- subj_diff %>%
  group_by(frequency_condition) %>%
  count(preference_cat)

## 16 Occurrences
chisq.test(filter(preference_count,frequency_condition=="16 Occurrences")$n)
#binom.test(filter(preference_count,frequency_condition=="16 Occurrences")$n)

## 4 Occurrences
chisq.test(filter(preference_count,frequency_condition=="4 Occurrences")$n)
#binom.test(filter(preference_count,frequency_condition=="4 Occurrences")$n)

## Condition Difference
chisq.test(cbind(filter(preference_count,frequency_condition=="16 Occurrences")$n,filter(preference_count,frequency_condition=="4 Occurrences")$n),correct=F)


#### EXPLORATORY ANALYSES ####
# a) Within the traditional headturn analyses, we will also fit similar models
# using a looking time difference DV that takes into account participants' relative preference for novel 
# stimuli relative to familiar stimuli, rather than just the raw difference in overall looking time:
# (novel looking time - familiar looking time)/ (novel looking time + familiar looking time).
# We expect models using the difference between familiar and novel looking time as the DV 
# and models using the difference between familiar and novel looking time relative to total looking time to yield comparable results.

#### 1) Effect within each condition

## 16 Occurrences Condition
m <- lm(rel_preference~ 1, data=filter(subj_diff,frequency_condition=="16 Occurrences"))
summary(m)
## 4 Occurrences Condition
m <- lm(rel_preference~ 1, data=filter(subj_diff,frequency_condition=="4 Occurrences"))
summary(m)
#### 2) Condition difference
m <- lm(rel_preference~frequency_condition,data=subj_diff)
summary(m)

# b) Given our wide age range, we will explore the relationship between age and preferential looking, 
# by testing whether age predicts looking time differences and whether it interacts with condition in the models described above. 

#### Traditional headturn

## 16 Occurrences Condition
m <- lm(diff_looking_time~ age, data=filter(subj_diff,frequency_condition=="16 Occurrences"))
summary(m)
## 4 Occurrences Condition
m <- lm(diff_looking_time~ age, data=filter(subj_diff,frequency_condition=="4 Occurrences"))
summary(m)
#### 2) Condition difference
m <- lm(rel_preference~frequency_condition*age,data=subj_diff)
summary(m)

#### Linear mixed-effects analysis

## fit main model
m <- lmer(log_looking_time ~ novel_familiar_c * frequency_condition_c * age + 
            (1 + novel_familiar_c | participant_id) + 
            (1 + novel_familiar_c*frequency_condition_c | test_item), 
          data=d,
          control=lmerControl(optimizer="bobyqa"))
#maximal converging model (comparable results to the maximal model with singular fit)
m <- lmer(log_looking_time ~ novel_familiar_c * frequency_condition_c * age + 
            (1 + novel_familiar_c | participant_id) + 
            (1 | test_item), 
          data=d,
          control=lmerControl(optimizer="bobyqa"))
summary(m)

# c) We will also consider additional analyses comparing the effect sizes between the two conditions. One approach we will explore is computing the effect size within each condition (Cohen’s d_z), along with a confidence interval, and then comparing the two effect sizes. 
## 16 Occurrences Condition
d_z_16 <- mean(filter(subj_diff,frequency_condition=="16 Occurrences")$diff_looking_time)/sd(filter(subj_diff,frequency_condition=="16 Occurrences")$diff_looking_time)
psych::d.ci(d_z_16,n=length(filter(subj_diff,frequency_condition=="16 Occurrences")$diff_looking_time))

## 4 Occurrences Condition
d_z_4 <- mean(filter(subj_diff,frequency_condition=="4 Occurrences")$diff_looking_time)/sd(filter(subj_diff,frequency_condition=="4 Occurrences")$diff_looking_time)
psych::d.ci(d_z_4,n=length(filter(subj_diff,frequency_condition=="4 Occurrences")$diff_looking_time))

# Another approach we will consider is computing an effect size for each infant across the 12 test trials (mean difference in looking time / variance of the difference) and computing a t-test comparing the infant-wise effect sizes between the two conditions. In general, we expect these exploratory analyses to yield similar results to the main analytic approach described in the main analysis section. We will explore these alternative analytic approaches in order to gather information and tune our intuitions about the best approach to use in future experiments.
#infant-wise effect size
subj_eff_size <- subj_novel_familiar %>%
  ungroup() %>%
  group_by(participant_id,frequency_condition) %>%
  summarize(
    s_pooled=sqrt(
      ((n[novel_familiar=="novel"]-1)*sd_looking_time[novel_familiar=="novel"]^2+(n[novel_familiar=="familiar"]-1)*sd_looking_time[novel_familiar=="familiar"]^2)/
        (n[novel_familiar=="novel"]+n[novel_familiar=="familiar"]-2)),
    mean_diff=mean_looking_time[novel_familiar=="novel"]-mean_looking_time[novel_familiar=="familiar"],
    d_pooled=mean_diff/s_pooled #"classic" cohen's D for each subject
  )

## 16 Occurrences Condition
t.test(filter(subj_eff_size,frequency_condition=="16 Occurrences")$d_pooled,mu=0)

## 4 Occurrences Condition
t.test(filter(subj_eff_size,frequency_condition=="4 Occurrences")$d_pooled,mu=0)

## Condition difference
t.test(filter(subj_eff_size,frequency_condition=="16 Occurrences")$d_pooled,filter(subj_eff_size,frequency_condition=="4 Occurrences")$d_pooled)

# d) We will investigate whether the size of the effect interacts with block number (Blocks 1, 2, and 3) in either the Four Occurrences condition or in the Sixteen Occurrences condition.
### summarize by block
##### summarize within subject ####
subj_novel_familiar_block <- d %>%
  group_by(participant_id,Experimenter,training_condition,language,frequency,frequency_condition,age,novel_familiar,block_counter) %>%
  summarize(n=n(),mean_looking_time=mean(looking_time))

#add NAs for missing blocks
dummy_block <- expand.grid(
  participant_id=unique(subj_novel_familiar_block$participant_id),
  novel_familiar=c("familiar","novel"),
  block_counter=c(1,2,3)) %>%
  left_join(select(subj_novel_familiar,-n,-mean_looking_time,-sd_looking_time))

subj_novel_familiar_block <- dummy_block %>%
  left_join(subj_novel_familiar_block)

#compute looking time difference metrics
subj_diff_block <- subj_novel_familiar_block %>%
  group_by(participant_id,Experimenter,training_condition,language,frequency,frequency_condition,age,block_counter) %>%
  summarize(
    N=n(),
    diff_looking_time=
      if_else(
        is.na(mean(mean_looking_time,na.rm=FALSE)), NaN,
        mean_looking_time[novel_familiar=="novel"]-mean_looking_time[novel_familiar=="familiar"]),
    total_average_looking_time =
      if_else(
        is.na(mean(mean_looking_time,na.rm=FALSE)),NaN,
        mean_looking_time[novel_familiar=="novel"]+mean_looking_time[novel_familiar=="familiar"]),
    rel_preference=diff_looking_time/total_average_looking_time
    )

overall_diff_block <- subj_diff_block %>%
  group_by(frequency,frequency_condition,block_counter) %>%
  summarize(
    N=n(),
    diff_looking=mean(diff_looking_time,na.rm=T),
    ci=qt(0.975, N-1)*sd(diff_looking_time,na.rm=T)/sqrt(N),
    lower_ci=diff_looking-ci,
    upper_ci=diff_looking+ci,
    rel_pref=mean(rel_preference),
    rel_ci=qt(0.975, N-1)*sd(rel_preference,na.rm=T)/sqrt(N),
    lower_rel_ci=rel_pref-rel_ci,
    upper_rel_ci=rel_pref+rel_ci
    
  )

ggplot(subj_diff_block, aes(frequency_condition,diff_looking_time,color=frequency_condition))+
  geom_violin()+
  #geom_boxplot(outlier.shape = NA)+
  geom_point(data=overall_diff_block, aes(y=diff_looking), size=8)+
  geom_errorbar(data=overall_diff_block, aes(y=NULL,ymin=lower_ci,ymax=upper_ci), width=0)+
  geom_jitter(width=0.05,size=2)+
  theme(legend.position="none")+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Looking Time Difference (Novel - Familiar) in ms")+
  xlab("Training Condition")+
  facet_wrap(~block_counter)
ggsave(here("..","figures","difference_looking_time_by_condition_byBlock.pdf"),width=7,height=6)

## visual inspection suggests no interaction

# e) We will also explore the effect of choosing more liberal or more stringent data exclusion criteria, specifically testing the same questions while retaining infants who contribute fewer trials (e.g., who contribute only 4 or 6 test trials) or only retaining infants who contribute more trials (e.g., who contribute at least 10 test trials).

d_trial_excl <- read_csv(here("..","data","processed_data","effie_test_data_processed_post_trial_exclusion.csv"))

## less stringent (4 or more useable trials)

## participants must contribute at least 4 useable trials
participant_exclusions_4trials <- d_trial_excl %>%
  group_by(participant_id) %>%
  summarize(
    num_useable_trials=sum(!is.na(looking_time)),
    include_yn=include_yn[1]
  ) %>%
  ungroup() %>%
  mutate(participant_exclude = case_when(
    num_useable_trials<4 ~ "y",
    include_yn=='N' ~ "y",
    TRUE ~ "n"
  ))

# join and clean data
d_clean_4trials <- d_trial_excl %>%
  left_join(participant_exclusions_4trials) %>%
  filter(participant_exclude=="n")

## same dataset as for minmimum 8 trials criterion (bc participants with 3<n<8 were excluded for other reasons)

subj_novel_familiar_4trials <- d_clean_4trials %>%
  group_by(participant_id,Experimenter,training_condition,language,frequency,frequency_condition,age,Sex,novel_familiar) %>%
  summarize(n=n(),mean_looking_time=mean(looking_time), sd_looking_time = sd(looking_time))

#compute looking time difference metrics
subj_diff_4trials <- subj_novel_familiar_4trials %>%
  group_by(participant_id,Experimenter,training_condition,language,frequency,frequency_condition,age,Sex) %>%
  summarize(
    N=n(),
    diff_looking_time=mean_looking_time[novel_familiar=="novel"]-mean_looking_time[novel_familiar=="familiar"],
    total_average_looking_time = mean_looking_time[novel_familiar=="novel"]+mean_looking_time[novel_familiar=="familiar"],
    rel_preference=diff_looking_time/total_average_looking_time)

#### TRADITIONAL HEADTURN ANALYSES

#### 1) Effect within each condition

## 16 Occurrences Condition
t.test(mean_looking_time~novel_familiar,data=filter(subj_novel_familiar_4trials,frequency_condition=="16 Occurrences"),paired=T,var.equal=T)

## 4 Occurrences Condition
t.test(mean_looking_time~novel_familiar,data=filter(subj_novel_familiar_4trials,frequency_condition=="4 Occurrences"),paired=T,var.equal=T)

#### 2) Condition difference
m <- lm(diff_looking_time~frequency_condition,data=subj_diff_4trials)
summary(m)

#### more stringent (12 useable trials only)

## participants must contribute at least 4 useable trials
participant_exclusions_12trials <- d_trial_excl %>%
  group_by(participant_id) %>%
  summarize(
    num_useable_trials=sum(!is.na(looking_time)),
    include_yn=include_yn[1]
  ) %>%
  ungroup() %>%
  mutate(participant_exclude = case_when(
    num_useable_trials<12 ~ "y",
    include_yn=='N' ~ "y",
    TRUE ~ "n"
  ))

# join and clean data
d_clean_12trials <- d_trial_excl %>%
  left_join(participant_exclusions_12trials) %>%
  filter(participant_exclude=="n")

subj_novel_familiar_12trials <- d_clean_12trials %>%
  group_by(participant_id,Experimenter,training_condition,language,frequency,frequency_condition,age,Sex,novel_familiar) %>%
  summarize(n=n(),mean_looking_time=mean(looking_time), sd_looking_time = sd(looking_time))

#compute looking time difference metrics
subj_diff_12trials <- subj_novel_familiar_12trials %>%
  group_by(participant_id,Experimenter,training_condition,language,frequency,frequency_condition,age,Sex) %>%
  summarize(
    N=n(),
    diff_looking_time=mean_looking_time[novel_familiar=="novel"]-mean_looking_time[novel_familiar=="familiar"],
    total_average_looking_time = mean_looking_time[novel_familiar=="novel"]+mean_looking_time[novel_familiar=="familiar"],
    rel_preference=diff_looking_time/total_average_looking_time)

#### TRADITIONAL HEADTURN ANALYSES

#### 1) Effect within each condition

## 16 Occurrences Condition
t.test(mean_looking_time~novel_familiar,data=filter(subj_novel_familiar_12trials,frequency_condition=="16 Occurrences"),paired=T,var.equal=T)

## 4 Occurrences Condition
t.test(mean_looking_time~novel_familiar,data=filter(subj_novel_familiar_12trials,frequency_condition=="4 Occurrences"),paired=T,var.equal=T)

#### 2) Condition difference
m <- lm(diff_looking_time~frequency_condition,data=subj_diff_12trials)
summary(m)

# f) We will also explore the use of equivalence tests to understand the degree to which a null effect in one of our two conditions or a null effect between our two conditions constrains the range of possible underlying effect sizes 
# (i.e. how likely the effect size lies within a particular upper and lower bound).
# using the TOSTER library and following the tutorial by Lakens et al. (2018) AMPPS

#using an equivalence bound  of d = 0.5

## 16 Occurrence condition
pdf(file=here("..","figures","toster_16_occurrences_condition.pdf"))

TOSTone(
  m=filter(overall_diff,frequency_condition=="16 Occurrences")$diff_looking,
  mu=0,
  sd=filter(overall_diff,frequency_condition=="16 Occurrences")$sd,
  n=filter(overall_diff,frequency_condition=="16 Occurrences")$N,
  low_eqbound_d=-0.5,
  high_eqbound_d=0.5)
#'equivalent to zero'
dev.off()

## 4 Occurrence condition
pdf(file=here("..","figures","toster_4_occurrences_condition.pdf"))

TOSTone(
  m=filter(overall_diff,frequency_condition=="4 Occurrences")$diff_looking,
  mu=0,
  sd=filter(overall_diff,frequency_condition=="4 Occurrences")$sd,
  n=filter(overall_diff,frequency_condition=="4 Occurrences")$N,
  low_eqbound_d=-0.5,
  high_eqbound_d=0.5)
#inconclusive
dev.off()

## Condition Difference
pdf(file=here("..","figures","toster_condition_difference.pdf"))

TOSTtwo(
  m1=filter(overall_diff,frequency_condition=="4 Occurrences")$diff_looking, 
  m2=filter(overall_diff,frequency_condition=="16 Occurrences")$diff_looking,
  sd1=filter(overall_diff,frequency_condition=="4 Occurrences")$sd,
  sd2=filter(overall_diff,frequency_condition=="16 Occurrences")$sd, 
  n1 = filter(overall_diff,frequency_condition=="4 Occurrences")$N,
  n2 = filter(overall_diff,frequency_condition=="16 Occurrences")$N,
  low_eqbound_d=-0.5,
  high_eqbound_d=0.5)
dev.off()
#inconclusive

#### Not pre-registered: using Bayesian tests to quantify evidence for the null ####

#### 1) Effect within each condition

## 16 Occurrences Condition
#evidence for the null hypothesis
1/ttestBF(filter(subj_diff,frequency_condition=="16 Occurrences")$diff_looking_time,mu=0)

## 4 Occurrences Condition
#evidence for the null hypothesis
1/ttestBF(filter(subj_diff,frequency_condition=="4 Occurrences")$diff_looking_time,mu=0)

#### 2) Condition difference
#evidence for the null hypothesis
1/ttestBF(filter(subj_diff,frequency_condition=="4 Occurrences")$diff_looking_time,filter(subj_diff,frequency_condition=="16 Occurrences")$diff_looking_time)


#### EXPLORATORY PLOTS ####

#### difference in looking by condition ####
ggplot(subj_diff, aes(frequency_condition,diff_looking_time,color=frequency_condition))+
  geom_violin()+
  geom_boxplot(outlier.shape = NA)+
  geom_point(data=overall_diff, aes(y=diff_looking), size=8)+
  geom_errorbar(data=overall_diff, aes(y=NULL,ymin=lower_ci,ymax=upper_ci), size=1.5,width=0.1)+
  geom_jitter(width=0.05,size=3)+
  theme(legend.position="none")+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_y_continuous(breaks=seq(-9000,5000,1000))+
  ylab("Looking Time Difference (Novel - Familiar) in ms")+
  xlab("Training Condition")
ggsave(here("..","figures","difference_looking_time_by_condition.pdf"),width=7,height=6)

#### looking across trials ####
ggplot(d, aes(trial_counter,looking_time,color=novel_familiar))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~frequency_condition)
ggsave(here("..","figures","looking_time_by_condition_trial.pdf"),width=7,height=6)

#### effect by age ####
ggplot(subj_diff, aes(age,diff_looking_time,color=frequency_condition))+
  geom_point()+
  geom_smooth(se=F)+
  geom_hline(yintercept=0,linetype="dashed")
ggsave(here("..","figures","looking_time_by_condition_age.pdf"),width=7,height=6)

#### effect by gender ####
ggplot(subj_diff, aes(Sex,diff_looking_time,color=frequency_condition))+
  geom_violin()+
  geom_boxplot(outlier.shape = NA)+
  geom_point()+
  geom_hline(yintercept=0,linetype="dashed")+
  facet_wrap(~frequency_condition)
ggsave(here("..","figures","looking_time_by_condition_gender.pdf"),width=7,height=6)

##### by item effects####
subj_test_item <- d %>%
  group_by(participant_id,Experimenter,training_condition,language,frequency,frequency_condition,novel_familiar,age,test_item) %>%
  summarize(n=n(),mean_looking_time=mean(looking_time))

#compute looking time difference metrics
overall_diff_item <- subj_test_item %>%
  group_by(training_condition,frequency_condition,novel_familiar,test_item) %>%
  summarize(
    N=n(),
    looking_time=mean(mean_looking_time),
    ci=qt(0.975, N-1)*sd(mean_looking_time,na.rm=T)/sqrt(N),
    lower_ci=looking_time-ci,
    upper_ci=looking_time+ci,)

ggplot(subj_test_item, aes(test_item,mean_looking_time,color=novel_familiar))+
  geom_violin()+
  #geom_boxplot(outlier.shape = NA)+
  geom_point(data=overall_diff_item, aes(y=looking_time), size=8)+
  geom_errorbar(data=overall_diff_item, aes(y=NULL,ymin=lower_ci,ymax=upper_ci), width=0)+
  geom_jitter(width=0.05,size=2)+
  #theme(legend.position="none")+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Looking Time in ms")+
  xlab("Test  Item")+
  facet_wrap(~training_condition)
ggsave(here("..","figures","looking_time_by_test_item.pdf"),width=7,height=6)

#### Experimenter effects? ####
subj_diff <- subj_diff %>%
  mutate(
    Experimenter_cat= case_when(
      Experimenter %in% c("MZ","DB","HW") ~ "grad",
      Experimenter %in% c("JS") ~ "staff",
      Experimenter %in% c("AM","EK","PB", "RB", "LF") ~ "undergrad",
      TRUE ~ "combo"
    )
  )

ggplot(subj_diff, aes(Experimenter_cat,diff_looking_time))+
  geom_violin()+
  geom_jitter(width=0.05)+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Looking Time Difference (Novel - Familiar) in ms")
ggsave(here("..","figures","experimenter_effects.pdf"),width=7,height=6)
