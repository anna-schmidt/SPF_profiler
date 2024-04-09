#------------------------------
# running fish stats on cleaned data
#------------------------------

# load required libraries
library(lme4)
#install.packages("lmerTest")
library(lmerTest)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(nlme)
library(pracma) # trapz function

# Water temperature

## Run linear mixed-effects model to determine if there is significant variation in water temperature (continuous) over time and space (fixed effects), with or without fish (fixed effects), accounting for enclosure (random effects due to the repeated measures per enclosure)

# re-name data set for this analysis
fish_stats <- cleaned_final

# add in a week column - month seems too broad, and day seems too fine? can discuss further...
fish_stats$week <- as.numeric(format(as.Date(fish_stats$profile.datetime), "%U")) # Note: %U tool begins each week at Sunday

# Fit the linear mixed-effects model ## week? or profile.datetime????
LMM <- lmer(water.temperature ~ fish_treatment * week * specified.depth + (1 | enclosure), data = fish_stats)

# Print the summary of the model
summary(LMM)

# Anova of the model
anova(LMM)
# Type III Analysis of Variance Table with Satterthwaite's method
#                                     Sum Sq Mean Sq NumDF  DenDF    F value    Pr(>F)    
# fish_treatment                          51      25     2     39 3.8918e+01 5.427e-10 ***
# week                                661407  661407     1 480343 1.0174e+06 < 2.2e-16 ***
# specified.depth                     215727  215727     1 480343 3.3184e+05 < 2.2e-16 ***
# fish_treatment:week                    149      74     2 480343 1.1436e+02 < 2.2e-16 ***
# fish_treatment:specified.depth         427     214     2 480343 3.2842e+02 < 2.2e-16 ***
# week:specified.depth                351747  351747     1 480343 5.4107e+05 < 2.2e-16 ***
# fish_treatment:week:specified.depth    664     332     2 480343 5.1058e+02 < 2.2e-16 ***


# fish treatment has a significant effect on water temp
# week and specified depth also have a significant effect on water temp
# fish_treatment:profile.datetime = significant; fish_treatment:specified.depth = significant; this means that the effect of fish treatment on water temp varies by time and depth (?)
# 

# So next, compare 5 meters vs. 15 meters, for dawn, noon, dusk, and midnight? Maybe 6, 12, 18, 0?


# subset data into observations at 5 and 15 meters, for just the hours we want to compare

subset5m <- list()
for (hour in c(6, 12, 18, 0)) {
  subset5m[[as.character(hour)]] <- fish_stats[fish_stats$specified.depth == 5 & fish_stats$hour_profile.datetime == hour, ]
}
subset15m <- list()
for (hour in c(6, 12, 18, 0)) {
  subset15m[[as.character(hour)]] <- fish_stats[fish_stats$specified.depth == 15 & fish_stats$hour_profile.datetime == hour, ]
}

# set up empty lists to accept the results of the t-tests

ttests_5m <- list()
ttests_15m <- list()

# perform multiple t-tests (and therefore add bonferroni adjustment); store in above lists

for (hour in c(6, 12, 18, 0)) {
  ttests_5m[[as.character(hour)]] <- pairwise.t.test(subset5m[[as.character(hour)]]$water.temperature,
                                              subset5m[[as.character(hour)]]$fish_treatment, p.adjust.method = "bonferroni")
  ttests_15m[[as.character(hour)]] <- pairwise.t.test(subset15m[[as.character(hour)]]$water.temperature,
                                               subset15m[[as.character(hour)]]$fish_treatment,p.adjust.method = "bonferroni")
}

ttests_5m
# no sig diffs

ttests_15m
# no sig diffs

# Therefore, mixed-effects model likely better to use in this case - t-tests don't capture nuance by not accounting for random effect of enclosure

#------------------------------

# Chlorophyll

# Use Area Under the Curve & ANOVA analysis

# find AUC where X = specified depth, y = chlorophyll.a
fish_stats2 <- fish_stats %>%
  group_by(enclosure) %>%
  summarise(AUC = trapz(specified.depth, chlorophyll.a))

# perform a t-test to see if there is a significant difference between the mean AUC for enclosures with vs. without fish

with_fish <- c("E02", "E07", "E08", "E14", "E17", "E19", "E23")
no_fish <- c("E01", "E09", "E10", "E12", "E16", "E18", "E24")

AUC_fish <- fish_stats2$AUC[fish_stats2$enclosure %in% with_fish]
AUC_no_fish <- fish_stats2$AUC[fish_stats2$enclosure %in% no_fish]

t.test(AUC_fish, AUC_no_fish) # p-value = 0.0878

# what about a linear mixed effects model for chlorophyll AUC averages...

merged_data <- merge(fish_stats2, fish_stats[, c("enclosure", "fish_treatment")], by = "enclosure")

LMM_chloro <- lmer(AUC ~ fish_treatment + (1 | enclosure), data = merged_data)
# Perhaps follow-up, not sure what means but still got a p-value...
# Warning messages:
#   1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                     Model failed to converge with max|grad| = 49.186 (tol = 0.002, component 1)
#                   2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                     Model is nearly unidentifiable: very large eigenvalue
#                                   - Rescale variables?
anova(LMM_chloro)
# fish_treatment p-value = 3.861e-12 ***

#------------------------------

# Phycocyanin

# approach with similar averaged Area Under Curve, linear mixed model, ANOVA:

fish_stats3 <- fish_stats %>%
  group_by(enclosure) %>%
  summarise(AUC_P = trapz(specified.depth, phycocyanin))

merged_data_P <- merge(fish_stats3, fish_stats[, c("enclosure", "fish_treatment")], by = "enclosure")

LMM_phyco <- lmer(AUC_P ~ fish_treatment + (1 | enclosure), data = merged_data_P)
anova(LMM_phyco)
# fish_treatment p-value < 2.2e-16 ***

