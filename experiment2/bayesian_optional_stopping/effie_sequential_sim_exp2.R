library(tidyverse)
library(BayesFactor)
library(tictoc)

#### Set seed
set.seed(12051897)

#### PARAMETERS
SD = 3400
d = c(0,0.3, 0.5, 0.8,1)
M = d*3400
BF = c(3,6,10)
n_min=20
n_max=c(30,40,50)
n_round_sims=1000 # careful! If this number is large, the simulation will take a really long time to run!

simulation_d <- data.frame()
tic()
####simulations
for (cur_M in M) {
  for (cur_BF in BF) {
    for (cur_n_max in n_max) {
      for (cur_n_round in 1:n_round_sims) {
        print(paste("Round: ", cur_n_round, " Cur Vals: ", cur_M,", ", cur_BF,", ", cur_n_max,sep=""))
        cur_n=n_min
        #sample n_min participants
        cur_sample <- rnorm(cur_n,mean=cur_M,sd=SD)
        #test BF
        cur_test <- ttestBF(cur_sample,mu=0)
        cur_test_bf=extractBF(cur_test, logbf = FALSE,onlybf=TRUE)
        if (cur_test_bf>=cur_BF | cur_test_bf<=(1/cur_BF)) {
          temp <- data.frame(
            mean=cur_M,
            SD=SD,
            d=cur_M/SD,
            BF_threshold=cur_BF,
            n_min=n_min,
            n_max=cur_n_max,
            simulation_round=cur_n_round,
            n=cur_n,
            bf=cur_test_bf,
            estimated_mean=mean(cur_sample),
            estimated_sd=sd(cur_sample),
            estimated_d=mean(cur_sample)/sd(cur_sample),
            t_stat = t.test(cur_sample,mu=0)$statistic,
            p_val=t.test(cur_sample,mu=0)$p.value
          )
          simulation_d <- bind_rows(simulation_d,temp) 
        } else {
          while (cur_n<cur_n_max & cur_test_bf < cur_BF & cur_test_bf >1/cur_BF) {
            cur_n <- cur_n+1
            cur_sample <- c(cur_sample,rnorm(1,mean=cur_M,sd=SD))
            #test BF
            cur_test <- ttestBF(cur_sample,mu=0)
            cur_test_bf=extractBF(cur_test, logbf = FALSE,onlybf=TRUE)
            temp <- data.frame(
              mean=cur_M,
              SD=SD,
              d=cur_M/SD,
              BF_threshold=cur_BF,
              n_min=n_min,
              n_max=cur_n_max,
              simulation_round=cur_n_round,
              n=cur_n,
              bf=cur_test_bf,
              estimated_mean=mean(cur_sample),
              estimated_sd=sd(cur_sample),
              estimated_d=mean(cur_sample)/sd(cur_sample),
              t_stat = t.test(cur_sample,mu=0)$statistic,
              p_val=t.test(cur_sample,mu=0)$p.value
            )
            simulation_d <- bind_rows(simulation_d,temp)
          }
        }
      }
    }
  }
}
toc()

write_csv(simulation_d,"effie_sequential_sim.csv")
