rm(list=ls())
library(Hmisc); library(MASS); library(tidyverse); library(lme4)

#power function
f_power <- function(a_pr, b_OR, sd_a=1, sd_b=1, covar_uaub=0, t_n, samp_n, 
                    reps_n, seed)  {
      progbar <- winProgressBar(title = "progress bar", min = 0,
                           max = reps_n, width = 300)
      set.seed=seed
      b_val <- se_val <- z_val <- p_val <- rep(NULL, reps_n)
      
      for (i in 1:reps_n) {
            #progress bar to monitor simulation
            Sys.sleep(0.1)
            setWinProgressBar(progbar, i, title=paste(round(i/reps_n*100, 0),
                                                  "% done"))
            #random effects for persons
            rand_effs <- mvrnorm(samp_n, mu=c(0,0), 
                                 Sigma=matrix(c(sd_a,covar_uaub, 
                                                covar_uaub,sd_b),2,2))
            colnames(rand_effs) <- c("ea", "eb")
            
            #generate time and outcome variables; assemble as data frame
            df.rep <- tibble(person_id = rep(1:(samp_n), each=t_n),
                             t = rep(0:(t_n-1), times=samp_n),
                             ua_i = rep(rand_effs[,1], each=t_n),
                             ub_i = rep(rand_effs[,2], each=t_n))
            
            df.rep <- df.rep %>% mutate(pr_tobacco = (1 + exp(-(log(a_pr/(1-a_pr)) +
                                                                      (ub_i+log(b_OR))*t + 
                                                                      ua_i)))^-1,
                                        tobacco = rbinom(samp_n*t_n, 1, pr_tobacco)) 
            #fit model
            out <- glmer(tobacco ~ 1 + t + (1 | person_id) + (-1 + t | person_id), 
                         data=df.rep, family=binomial(link="logit"),
                         control=glmerControl(optCtrl=list(maxfun=100000)))
            
            #collect parameter estimates for each i
            b_val[i] <- summary(out)$coefficient[2,1]
            se_val[i] <- summary(out)$coefficient[2,2]
            z_val[i] <- summary(out)$coefficient[2,3]
            p_val[i] <- summary(out)$coefficient[2,4]
      }
      close(progbar)
      return(tibble(b_val, se_val, z_val, p_val))
}

a<-f_power(a_pr=0.1, b_OR=1.3, sd_a=(pi^2/3)^0.5, sd_b=1, covar_uaub=0, 
                    t_n=6, samp_n=2600/2*.8^2, reps_n=100, seed=1978) 