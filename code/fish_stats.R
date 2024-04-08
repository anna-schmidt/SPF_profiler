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
fish_stats <- cleaned_chlorophyll  # NOTE: replace cleaned_chlorophyll with final clean dataframe

# add in a week column - month seems too broad, and day seems too fine for this particular analysis
fish_stats$week <- as.numeric(format(as.Date(fish_stats$profile.datetime), "%U")) # Note: %U tool begins each week at Sunday

# Fit the linear mixed-effects model
LMM <- lmer(water.temperature ~ fish_treatment * week * specified.depth + (1 | enclosure), data = fish_stats)

# Print the summary of the model
summary(LMM)

# Anova of the model
anova(LMM)
# Type III Analysis of Variance Table with Satterthwaite's method
#                                     Sum Sq Mean Sq NumDF  DenDF    F value    Pr(>F)    
# fish_treatment                          51      26     2     39 3.9391e+01 4.713e-10 ***
# week                                661869  661869     1 480838 1.0178e+06 < 2.2e-16 ***
# specified.depth                     215873  215873     1 480838 3.3196e+05 < 2.2e-16 ***
# fish_treatment:week                    151      75     2 480838 1.1586e+02 < 2.2e-16 ***
# fish_treatment:specified.depth         429     214     2 480838 3.2982e+02 < 2.2e-16 ***
# week:specified.depth                351988  351988     1 480838 5.4127e+05 < 2.2e-16 ***
# fish_treatment:week:specified.depth    667     333     2 480838 5.1253e+02 < 2.2e-16 ***

#------------------------------

# Chlorophyll

# Use Area Under the Curve Repeated Measures (since multiple measures from same enclosure) ANOVA analysis

# find AUC where X = specified depth, y = chlorophyll.a
fish_stats2 <- fish_stats %>%
  group_by(enclosure) %>%
  summarise(AUC = trapz(specified.depth, chlorophyll.a))

# perform a t-test to see if there is a significant difference between the mean AUC for enclosures with vs. without fish

with_fish <- c("E02", "E07", "E08", "E14", "E17", "E19", "E23")
no_fish <- c("E01", "E09", "E10", "E12", "E16", "E18", "E24")

AUC_fish <- fish_stats2$AUC[fish_stats2$enclosure %in% with_fish]
AUC_no_fish <- fish_stats2$AUC[fish_stats2$enclosure %in% no_fish]

t.test(AUC_fish, AUC_no_fish) # p-value = 0.08668

