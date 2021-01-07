
# For easy data manipulation
library(foreach)
library(dplyr)
library(tidyr)
library(broom)
# For pretty plots
library(ggplot2)
# For nice tables
library(knitr)
# For intra-class correlations
library(irr)
# For Bayesian modeling
library(rstan)
# For some useful utility functions
library(hBayesDM)

# Data path and individual subject file names
data_path <- "data/"
files_t1 <- list.files(data_path, pattern = "*1.csv")
files_t2 <- list.files(data_path, pattern = "*2.csv")

# Create long-format stroop task data including all subjects
long_stroop <- foreach(i=seq_along(files_t1), .combine = "rbind") %do% {
  # For time 1
  tmp_t1 <- read.csv(file.path(data_path, files_t1[i]), header = F) %>%
    mutate(subj_num = i,
           time = 1)
  # For time 2 (about 3 weeks apart)
  tmp_t2 <- read.csv(file.path(data_path, files_t2[i]), header = F) %>%
    mutate(subj_num = i,
           time = 2)
  # Condition (0 = congruent, 1=neutral, 2=incongruent), 
  # Correct (1) or incorrect (0), 
  # Reaction time is in seconds
  names(tmp_t1)[1:6] <- names(tmp_t2)[1:6] <- c("Block", "Trial", "Unused", 
                                                "Condition", "Correct", "RT")
  rbind(tmp_t1, tmp_t2)
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## Create columns for the long stroop
long_stroop_0 <- long_stroop %>%
  as_tibble() %>%
  filter(Condition != 1) %>%
  filter(Condition == 0) %>%
  mutate(RT0 = RT) %>%
  select(RT0, subj_num, time)

long_stroop_2 <- long_stroop %>%
  filter(Condition != 1) %>%
  filter(Condition == 2) %>%
  mutate(RT2 = RT) %>%
  select(RT2)

stroop_anton <- long_stroop_0 %>%
  bind_cols(long_stroop_2)

################################
# Calculate the difference between RT0 vs RT2
stroop_anton <- stroop_anton %>% 
  mutate(RTdiff = RT2 - RT0) %>%
  mutate(rowid = row_number())

stroop_anton_split1 <- stroop_anton %>%
  group_by(subj_num) %>%
  mutate(sample_number = n()/2) %>% 
  # Sample half of each groups 
  sample_frac(0.5) %>%
  arrange(rowid)

stroop_anton_split2 <- stroop_anton %>% 
  filter(!(rowid %in% stroop_anton_split1$rowid))

cor(stroop_anton_split1$RTdiff, stroop_anton_split2$RTdiff)


library("splithalf")

difference <- splithalf(data = long_stroop,
                        outcome = "RT",
                        score = "difference",
                        # conditionlist = c(0, 2),
                        halftype = "random",
                        permutations = 5000,
                        var.RT = "RT",
                        # var.condition = "Block",
                        var.participant = "subj_num",
                        var.compare = "Condition",
                        compare1 = 0,
                        compare2 = 2,
                        average = "mean",
                        plot = TRUE)

difference_time <- splithalf(data = long_stroop,
                        outcome = "RT",
                        score = "average",
                        conditionlist = c(1, 2),
                        halftype = "random",
                        permutations = 5000,
                        var.RT = "RT",
                        var.condition = "time",
                        var.participant = "subj_num",
                        average = "mean",
                        plot = TRUE)


#### Only for Time 1
difference <- long_stroop %>% 
  filter(time == 1) %>%
  splithalf(data = .,
            outcome = "RT",
            score = "difference",
            # conditionlist = c(0, 2),
            halftype = "random",
            permutations = 5000,
            var.RT = "RT",
            # var.condition = "Block",
            var.participant = "subj_num",
            var.compare = "Condition",
            compare1 = 0,
            compare2 = 2,
            average = "mean",
            plot = TRUE)

difference <- long_stroop %>% 
  filter(time == 2) %>%
  splithalf(data = .,
            outcome = "RT",
            score = "difference",
            # conditionlist = c(0, 2),
            halftype = "random",
            permutations = 5000,
            var.RT = "RT",
            # var.condition = "Block",
            var.participant = "subj_num",
            var.compare = "Condition",
            compare1 = 0,
            compare2 = 2,
            average = "mean",
            plot = TRUE)
library(lme4)

### HLM
unconditional_model <- long_stroop %>%
  filter(Condition != 1) %>%
  mutate(Condition_dummy = Condition - 1) %>%
  lmer(data = .,
       formula = RT ~ 1 + (1 | subj_num))

hlm_model <- long_stroop %>%
  filter(Condition != 1) %>%
  mutate(Condition_dummy = Condition - 1) %>%
  lmer(data = .,
     formula = RT ~ Condition_dummy + time + (Condition_dummy|subj_num))

hlm_model <- long_stroop %>%
  filter(Condition == 0) %>%
  lmer(data = .,
       formula = RT ~ time + (Trial|subj_num))

gls(data =long_stroop, RT ~ time + (Trial|subj_num),
          cor=corSymm(form=~1|Trial))

# https://stats.stackexchange.com/questions/86958/variance-covariance-structure-for-random-effects-in-lme4
# https://rpubs.com/samuelkn/CovarianceStructuresInR
hlm_model %>% summary()

library(sjstats)
performance::icc(unconditional_model)
performance::icc(hlm_model)

#############################
# Compute Stroop effect for each person at each time (equation 1)
sum_stroop <- long_stroop %>%
  group_by(subj_num, time) %>%
  summarize(stroop_eff = mean(RT[Condition==2]) - mean(RT[Condition==0]))

# Peak at the data
kable(head(sum_stroop), digits = 3)


#summarize
sum_stroop %>%
  ungroup() %>%
  group_by(time) %>%
  summarize(N = n(),
            Mean = round(mean(stroop_eff), 3),
            SD = round(sd(stroop_eff), 3)) %>%
  kable(digits = 3)

# Test for group-level Stroop effect at time 1
sum_stroop %>%
  filter(time==1) %>%
  {t.test(.$stroop_eff)} %>%
  tidy() %>%
  kable(digits = 3)

# Test for group-level Stroop effect at time 2
sum_stroop %>%
  filter(time==2) %>%
  {t.test(.$stroop_eff)} %>%
  tidy() %>%
  kable(digits = 3)

# Format for test-retest analysis
stroop_unpooled <- sum_stroop %>%
  ungroup() %>%
  mutate(time = ifelse(time==1, "Stroop_T1", "Stroop_T2"),
         pooled = "No", 
         Replication = "Sample Mean") %>% # these "pooled" and Replication variables will come in later
  spread(key = time, value = stroop_eff)

# Intraclass correlation of stroop effect at time 1 and 2
stroop_unpooled %>%
  select(Stroop_T1, Stroop_T2) %>%
  {icc(., model = "twoway", type = "agreement", unit = "average")[c(1, 7:15)]} %>%
  as.data.frame() %>%
  kable(digits = 3)

# Pearson's correlation
stroop_unpooled %>%
  select(Stroop_T1, Stroop_T2) %>%
  {cor.test(.$Stroop_T1, .$Stroop_T2)} %>%
  tidy() %>%
  kable(digits = 3)
