#------------------------------
# removing dates before May 04 for enclosures with fish; finding outliers using this update, and lastly, re-calculating fish stats
#------------------------------

# removing pre-fish dates from ONLY fish enclosures
no_fish <- master_times %>%
  filter((profile.datetime >= ymd("2023-05-04") & fish_treatment == "yes") | (fish_treatment == "no"))


#------------------------------

# Making boxplots (commented out), removing outliers found from boxplots, trying out removing IQR threshold outliers (commented out)

# Boxplots of all of our parameter values on one plot
# plot.new()
# par(mfrow = c(3, 3))  # setting up the plotting layout
# lapply(names(subset), function(col) {
#   boxplot(subset[[col]], main = col, ylab = "Value")
# })

# Most obvious outliers:
# chlorophyll.a > 100
# phycocyanin > 4
# conductivity < 200

variables <- c("water.temperature","conductivity","pH.value","chlorophyll.a","phycocyanin","oxygen.concentration","photosynthetically.active.radiation.up")

no_outliers_chlorophyll.a <- no_fish$chlorophyll.a > 100
no_outliers_phycocyanin <- no_fish$phycocyanin > 4
no_outliers_conductivity <- no_fish$conductivity < 200

no_boxplot_outliers <- no_fish[!no_outliers_chlorophyll.a & !no_outliers_phycocyanin & !no_outliers_conductivity,]

# IQR threshold method - discarded but discussed in paper
# 
# remove_outliers <- function(column, threshold = 3) {
#   Q1 <- quantile(column, 0.25) # establish quantiles
#   Q3 <- quantile(column, 0.75)
#   IQR <- Q3 - Q1
#   lower_bound <- Q1 - threshold * IQR # establish thresholds
#   upper_bound <- Q3 + threshold * IQR
#   no_IQR_outliers <- ifelse(column < lower_bound | column > upper_bound, NA, column) # remove outliers, replace the missing values with NAs
#   return(no_IQR_outliers)
# }
# 
# # Apply function to defined variables columns
# no_IQR_outliers <- lapply(no_fish[variables], remove_outliers)
# # Turn that into a dataframe
# no_IQR_outliers_df <- data.frame(lapply(no_IQR_outliers, as.vector))
# # Get rid of rows (again, whole rows) with NAs
# no_IQR_outliers_df <- na.omit(no_IQR_outliers_df)

#------------------------------

# Removing more outliers based on more boxplot assessment

master_cleaning <- no_boxplot_outliers

filtered_conduc <- master_cleaning %>%
  filter(!(enclosure == "E01" & conductivity > 300),
         !(enclosure == "E02" & conductivity > 300),
         !(enclosure == "E16" & conductivity > 275),
         !(enclosure == "E23" & conductivity > 280))
####-------------removed 35 points

filtered_chloro <- filtered_conduc %>%
  filter(!(enclosure == "E02" & chlorophyll.a > 50),
         !(enclosure == "E07" & chlorophyll.a > 75),
         !(enclosure == "E08" & chlorophyll.a > 50),
         !(enclosure == "E09" & chlorophyll.a > 50),
         !(enclosure == "E12" & chlorophyll.a > 25),
         !(enclosure == "E17" & chlorophyll.a > 75))
####------------- # removed 12 more points

filtered_phyco <- filtered_chloro %>%
  filter(!(enclosure == "E01" & phycocyanin > 2),
         !(enclosure == "E08" & phycocyanin > 3),
         !(enclosure == "E09" & phycocyanin > 2),
         !(enclosure == "E09" & phycocyanin > 2))
####------------- # removed 1 more point

#------------------------------

# Removing even more outliers based on profile depth plots across all times and depths

cleaned_chlorophyll <- filtered_phyco%>%
  filter(!(enclosure == "E01" & specified.depth > 15 & chlorophyll.a > 15),
         !(enclosure == "E02" & chlorophyll.a > 40),
         !(enclosure == "E02" & profile.datetime == "2023-05-11 00:00:00" & chlorophyll.a > 20),
         !(enclosure == "E07" & chlorophyll.a > 25),
         !(enclosure == "E08" & chlorophyll.a > 30),
         !(enclosure == "E09" & chlorophyll.a > 25),
         !(enclosure == "E10" & chlorophyll.a > 25),
         !(enclosure == "E12" & chlorophyll.a > 15),
         !(enclosure == "E14" & chlorophyll.a > 15),
         !(enclosure == "E14" & profile.datetime %in% c("2023-04-29 00:00:00", "2023-05-13 00:00:00", "2023-05-17 00:00:00", "2023-06-01 00:00:00") & chlorophyll.a > 10),
         !(enclosure == "E16" & profile.datetime %in% c("2023-05-19 00:00:00","2023-05-21 00:00:00") & specified.depth < 5 & chlorophyll.a > 10),
         !(enclosure == "E17" & chlorophyll.a > 25),
         !(enclosure == "E18" & chlorophyll.a > 15),
         !(enclosure == "E19" & chlorophyll.a > 25),
         !(enclosure == "E23" & profile.datetime %in% c("2023-05-01 00:00:00", "2023-05-12 00:00:00", "2023-05-19 00:00:00", "2023-05-20 00:00:00", "2023-05-23 00:00:00","2023-05-24 00:00:00") & chlorophyll.a > 20),
         !(enclosure == "E23" & profile.datetime %in% c("2023-05-25 00:00:00","2023-05-26 00:00:00","2023-05-27 00:00:00","2023-05-29 00:00:00","2023-06-01 00:00:00","2023-06-02 00:00:00") & chlorophyll.a > 25),
         !(enclosure == "E24" & chlorophyll.a > 30),
         !(enclosure == "L01" & chlorophyll.a > 15))
####------------- # removed 287 more rows

# phycocyanin

cleaned_phyco <- cleaned_chlorophyll %>%
  filter(!(enclosure != "E24" & phycocyanin > 2),
         !(enclosure == "E01" & specified.depth < 5 & phycocyanin > 0.5),
         !(enclosure == "E02" & phycocyanin > 1),
         !(enclosure == "E07" & specified.depth < 5 & phycocyanin > 1),
         !(enclosure == "E07" & specified.depth > 15 & phycocyanin > 1),
         !(enclosure == "E08" & specified.depth < 5 & phycocyanin > 1),
         !(enclosure == "E08" & specified.depth > 10 & phycocyanin > 1.5),
         !(enclosure == "E09" & phycocyanin > 1),
         !(enclosure == "E10" & phycocyanin > 1),
         !(enclosure == "E12" & profile.datetime == "2023-05-21 00:00:00" & specified.depth < 6 & phycocyanin > 0.2),
         !(enclosure == "E12" & phycocyanin > 0.6),
         !(enclosure == "E14" & phycocyanin > 0.6),
         !(enclosure == "E14" & specified.depth < 10 & phycocyanin > 0.4),
         !(enclosure == "E16" & specified.depth < 6 & phycocyanin > 0.6),
         !(enclosure == "E17" & phycocyanin > 0.6),
         !(enclosure == "E18" & phycocyanin > 0.6),
         !(enclosure == "E19" & profile.datetime %in% c("2023-04-30 00:00:00","2023-05-18 00:00:00","2023-05-19 00:00:00","2023-05-20 00:00:00","2023-05-21 00:00:00","2023-05-23 00:00:00","2023-05-24 00:00:00","2023-05-25 00:00:00") & specified.depth < 10 & phycocyanin > 0.5),
         !(enclosure == "E19" & phycocyanin > 1.5),
         !(enclosure == "E23" & specified.depth < 10 & phycocyanin > 0.75))
#!(enclosure == "L01")) ## funky graph!
####------------- # removed 494 more rows


# oxygen concentration

cleaned_oxygen <- cleaned_phyco %>%
  filter(!(enclosure == "E01" & specified.depth < 10 & oxygen.concentration < 10),
         !(enclosure == "E19" & profile.datetime %in% c("2023-04-25 00:00:00","2023-05-26 00:00:00") & oxygen.concentration < 6))
####------------- # removed 1 more row

# conductivity
cleaned_final <- cleaned_oxygen %>%
  filter(!(enclosure == "E01" & conductivity > 300),
         !(enclosure == "E19" & profile.datetime %in% c("2023-04-25 00:00:00","2023-05-05 00:00:00") & conductivity > 275),
         !(enclosure == "E23" & profile.datetime == "2023-05-23 00:00:00" & specified.depth < 10 & conductivity > 265))
# E09, funky clustering by hour but didn't remove any rows for that enclosure...
####------------- # removed 0 more rows



########################################

########################################


# Updated/Latest Version Fish Stats

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

####------------------------
# Water temperature

## Run linear mixed-effects model to determine if there is significant variation in water temperature (continuous) over time and space (fixed effects), with or without fish (fixed effects), accounting for enclosure (random effects due to the repeated measures per enclosure)

# re-name data set for this analysis
fish_stats <- cleaned_final

# Fit the linear mixed-effects model

LMM <- lmer(water.temperature ~ fish_treatment * profile.datetime * specified.depth + (1 | enclosure), data = fish_stats)

# Print the summary of the model
summary(LMM) # RESULTS:
# Linear mixed model fit by REML. t-tests use Satterthwaite's method
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.0660 -0.6326 -0.0432  0.5870  4.8420 
# Random effects:
#  Groups    Name        Variance Std.Dev.
#  enclosure (Intercept) 0.01723  0.1313  
#  Residual              0.56669  0.7528  
# Fixed effects:
#                                                      Estimate Std. Error         df
# (Intercept)                                        -5.938e+03  5.676e+00  3.914e+05
# fish_treatmentyes                                   2.939e+01  1.008e+01  3.942e+05
# profile.datetime                                    3.534e-06  3.370e-09  3.905e+05
# specified.depth                                     3.844e+02  5.441e-01  3.907e+05
# fish_treatmentyes:profile.datetime                 -1.745e-08  5.983e-09  3.941e+05
# fish_treatmentyes:specified.depth                  -2.531e+00  9.715e-01  3.942e+05
# profile.datetime:specified.depth                   -2.286e-07  3.231e-10  3.907e+05
# fish_treatmentyes:profile.datetime:specified.depth  1.502e-09  5.768e-10  3.942e+05
#                                                      t value Pr(>|t|)    
# (Intercept)                                        -1046.124  < 2e-16 ***
# fish_treatmentyes                                      2.916  0.00354 ** 
# profile.datetime                                    1048.720  < 2e-16 ***
# specified.depth                                      706.478  < 2e-16 ***
# fish_treatmentyes:profile.datetime                    -2.916  0.00355 ** 
# fish_treatmentyes:specified.depth                     -2.605  0.00918 ** 
# profile.datetime:specified.depth                    -707.513  < 2e-16 ***
# fish_treatmentyes:profile.datetime:specified.depth     2.605  0.00920 ** 
# 
# Correlation of Fixed Effects:
#                 (Intr) fsh_tr prfl.d spcfd. fsh_trtmntys:p. fsh_trtmntys:s. prf.:.
# fsh_trtmnty     -0.563                                                            
# profil.dttm     -1.000  0.563                                                     
# specfd.dpth     -0.870  0.490  0.870                                              
# fsh_trtmntys:p.  0.563 -1.000 -0.563 -0.490                                       
# fsh_trtmntys:s.  0.487 -0.869 -0.487 -0.560  0.869                                
# prfl.dttm:.      0.870 -0.490 -0.870 -1.000  0.490           0.560                
# fsh_trt:.:.     -0.487  0.869  0.487  0.560 -0.869          -1.000          -0.560


# fit warnings:
# Some predictor variables are on very different scales: consider rescaling


## Re-scale? There was a warning message about re-scaling (commented above) - I don't think I got this message when I went by week instead of all times

# fish_stats$week <- as.numeric(format(as.Date(fish_stats$profile.datetime), "%U")) # Note: %U tool begins each week at Sunday

#fish_stats$specified.depth <- scale(fish_stats$specified.depth)
#fish_stats$fish_treatment <- as.factor(fish_stats$fish_treatment)
#fish_stats$enclosure <- as.factor(fish_stats$enclosure)
#LMM <- lmer(water.temperature ~ fish_treatment * week * specified.depth + (1 | enclosure), data = fish_stats)

#summary(LMM)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: water.temperature ~ fish_treatment * week * specified.depth +  
#     (1 | enclosure)
#    Data: fish_stats
# 
# REML criterion at convergence: 958164.9
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.6509 -0.6750 -0.0853  0.6093  4.1880 
# 
# Random effects:
#  Groups    Name        Variance Std.Dev.
#  enclosure (Intercept) 0.0169   0.1300  
#  Residual              0.6651   0.8156  
# Number of obs: 394223, groups:  enclosure, 14
# 
# Fixed effects:
#                                          Estimate Std. Error         df  t value Pr(>|t|)
# (Intercept)                            -7.544e+00  5.351e-02  1.682e+01 -140.986  < 2e-16
# fish_treatmentyes                       5.754e-01  7.920e-02  2.018e+01    7.265 4.73e-07
# week                                    8.637e-01  1.075e-03  3.942e+05  803.568  < 2e-16
# specified.depth                         1.067e+01  2.115e-02  3.942e+05  504.616  < 2e-16
# fish_treatmentyes:week                 -2.751e-02  1.892e-03  3.942e+05  -14.545  < 2e-16
# fish_treatmentyes:specified.depth      -5.136e-01  3.803e-02  3.942e+05  -13.508  < 2e-16
# week:specified.depth                   -6.910e-01  1.074e-03  3.942e+05 -643.510  < 2e-16
# fish_treatmentyes:week:specified.depth  2.450e-02  1.893e-03  3.942e+05   12.942  < 2e-16
#                                           
# (Intercept)                            ***
# fish_treatmentyes                      ***
# week                                   ***
# specified.depth                        ***
# fish_treatmentyes:week                 ***
# fish_treatmentyes:specified.depth      ***
# week:specified.depth                   ***
# fish_treatmentyes:week:specified.depth ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) fsh_tr week   spcfd. fsh_t: fsh_:. wk:sp.
# fsh_trtmnty -0.676                                          
# week        -0.394  0.266                                   
# specfd.dpth -0.003  0.002  0.007                            
# fsh_trtmnt:  0.224 -0.478 -0.568 -0.004                     
# fsh_trtmn:.  0.001  0.002 -0.004 -0.556 -0.005              
# wk:spcfd.dp  0.003 -0.002 -0.007 -0.997  0.004  0.554       
# fsh_trtm::. -0.002 -0.002  0.004  0.565  0.005 -0.997 -0.567


# Anova of the model unscaled
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

# Anova of the model SCALED

# Type III Analysis of Variance Table with Satterthwaite's method
#                                     Sum Sq Mean Sq NumDF  DenDF    F value    Pr(>F)    
# fish_treatment                          35      35     1     20     52.782 4.728e-07 ***
# week                                537174  537174     1 394203 807617.279 < 2.2e-16 ***
# specified.depth                     199614  199614     1 394203 300111.167 < 2.2e-16 ***
# fish_treatment:week                    141     141     1 394203    211.564 < 2.2e-16 ***
# fish_treatment:specified.depth         121     121     1 394203    182.462 < 2.2e-16 ***
# week:specified.depth                341864  341864     1 394203 513977.601 < 2.2e-16 ***
# fish_treatment:week:specified.depth    111     111     1 394203    167.497 < 2.2e-16 ***

# Either way:
# fish treatment has a significant effect on water temp
# week and specified depth also have a significant effect on water temp
# fish_treatment:profile.datetime = significant; fish_treatment:specified.depth = significant; this means that the effect of fish treatment on water temp varies by time and depth- best to proceed with visualization to fully grasp the complexity of the data set!

# NOON Water temp

unique(fish_stats$hour_profile.datetime)

fish_stats_noon <- fish_stats[fish_stats$hour_profile.datetime == 12 & fish_stats$specified.depth == 18,]
LMM_noon <- lmer(water.temperature ~ fish_treatment + (1 | enclosure), data = fish_stats_noon) #boundary (singular) fit: see ?isSingular... try t-test?
anova(LMM_noon)

t.test(water.temperature ~ fish_treatment, data = fish_stats_noon) # p values at noon vary by depth - very significant for more shallow depths; becomes less significant around 15 m-17.5 m (p=0.04-0.02); no significance at 18 m (p=0.6935)

#------------------------------

# Chlorophyll

# Use Area Under the Curve & t.test analysis

# find AUC where X = specified depth, y = chlorophyll.a, trapz = pracma package function

fish_stats2 <- fish_stats %>%
  group_by(enclosure) %>%
  summarise(AUC = trapz(specified.depth, chlorophyll.a), .groups = 'drop') %>%
  left_join(select(fish_stats, enclosure, fish_treatment) %>% distinct())

with_fish <- c("E02", "E07", "E08", "E14", "E17", "E19", "E23")
no_fish <- c("E01", "E09", "E10", "E12", "E16", "E18", "E24")

AUC_fish <- fish_stats2$AUC[fish_stats2$enclosure %in% with_fish]
AUC_no_fish <- fish_stats2$AUC[fish_stats2$enclosure %in% no_fish]

# perform a t-test to see if there is a significant difference between the mean AUC for enclosures with vs. without fish

t.test(AUC_fish, AUC_no_fish) # p-value = 0.01119

# what about a linear mixed effects model for chlorophyll AUC averages...

#LMM_chloro <- lmer(AUC ~ fish_treatment + (1 | enclosure), data = merged_data)
# Warning messages:
#   1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                     Model failed to converge with max|grad| = 49.186 (tol = 0.002, component 1)
#                   2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                     Model is nearly unidentifiable: very large eigenvalue
#                                   - Rescale variables?
#anova(LMM_chloro)
# fish_treatment p-value < 2.2e-16 ***??

# turns out LMM/anova not best since only 1 AUC per enclosure - though larger p-vals, I would trust the t-test more. Or maybe there is an even better test I have not considered yet.

ggplot(fish_stats2, aes(x = fish_treatment, y = AUC, fill = fish_treatment)) +
  geom_boxplot() +
  labs(title = "Chlorophyll AUC by Fish Treatment", x = "Fish Treatment", y = "Chlorophyll AUC")

# NOON chlorophyll

fish_stats_noon_chloro <- fish_stats %>%
  filter(hour_profile.datetime == 12) %>%
  group_by(enclosure) %>%
  summarise(AUC = trapz(specified.depth, chlorophyll.a), .groups = 'drop') %>%
  left_join(select(fish_stats, enclosure, fish_treatment) %>% distinct())

AUC_fish_noon <- fish_stats_noon_chloro$AUC[fish_stats_noon_chloro$enclosure %in% with_fish]
AUC_no_fish_noon <- fish_stats_noon_chloro$AUC[fish_stats_noon_chloro$enclosure %in% no_fish]


#LMM_chloro_noon <- lmer(AUC ~ fish_treatment + (1 | enclosure), data = fish_stats_noon_chloro)
#anova(LMM_chloro_noon)

t.test(AUC_fish_noon, AUC_no_fish_noon, alternative="less") # p = 0.01273

# ggplot(fish_stats_noon_chloro, aes(x = fish_treatment, y = AUC, fill = fish_treatment)) +
#   geom_boxplot() +
#   labs(title = "Chlorophyll AUC by Fish Treatment at Noon", x = "Fish Treatment", y = "Chlorophyll AUC")

#------------------------------

# Phycocyanin

# approach with similar averaged Area Under Curve, linear mixed model, ANOVA:

fish_stats3 <- fish_stats %>%
  group_by(enclosure) %>%
  summarise(AUC_P = trapz(specified.depth, phycocyanin), .groups = 'drop') %>%
  left_join(select(fish_stats, enclosure, fish_treatment) %>% distinct())

AUC_fish_P <- fish_stats3$AUC_P[fish_stats3$enclosure %in% with_fish]
AUC_no_fish_P <- fish_stats3$AUC_P[fish_stats3$enclosure %in% no_fish]

# again, LMM likely not most appropriate here
#LMM_phyco <- lmer(AUC_P ~ fish_treatment + (1 | enclosure), data = fish_stats3)
#anova(LMM_phyco)
# fish_treatment p-value < 2.2e-16 ***

t.test(AUC_fish_P,AUC_no_fish_P, alternative = "less") # p = 0.02521
ggplot(fish_stats3, aes(x = fish_treatment, y = AUC_P, fill = fish_treatment)) +
  geom_boxplot() +
  labs(title = "Phycocyanin AUC by Fish Treatment", x = "Fish Treatment", y = "Phycocyanin AUC")


# NOON Phycocyanin

fish_stats_noon_phyco <- fish_stats %>%
  filter(hour_profile.datetime == 12) %>%
  group_by(enclosure) %>%
  summarise(AUC_P = trapz(specified.depth, phycocyanin), .groups = 'drop') %>%
  left_join(select(fish_stats, enclosure, fish_treatment) %>% distinct())

AUC_fish_P_noon <- fish_stats_noon_phyco$AUC_P[fish_stats_noon_phyco$enclosure %in% with_fish]
AUC_no_fish_P_noon <- fish_stats_noon_phyco$AUC_P[fish_stats_noon_phyco$enclosure %in% no_fish]

t.test(AUC_fish_P_noon, AUC_no_fish_P_noon, alternative="less") # p = 0.05

# ggplot(fish_stats_noon_phyco, aes(x = fish_treatment, y = AUC_P, fill = fish_treatment)) +
#   geom_boxplot() +
#   labs(title = "Phycocyanin AUC by Fish Treatment at Noon", x = "Fish Treatment", y = "Phycocyanin AUC")


#------------------------------
fish_stats_noon_phyco

# Just chlorophyll & phycocyanin graphed together/making things look nicer
library(gridExtra)
filtered_data_C <- fish_stats_noon_chloro %>% filter(enclosure != "E16")
chloro_plot <- ggplot(fish_stats_noon_chloro, aes(x = fish_treatment, y = AUC, fill = fish_treatment)) +
  geom_violin() +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
  scale_fill_viridis_d(begin=0.7) +
  labs(x = "Fish Treatment", y = "Chlorophyll AUC") + 
  theme(legend.position="none")

filtered_data_P <- fish_stats_noon_phyco %>% filter(enclosure != "E16")
phyco_plot<- ggplot(fish_stats_noon_phyco, aes(x = fish_treatment, y = AUC_P, fill = fish_treatment)) +
  geom_violin() +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
  scale_fill_viridis_d(begin=0.7) +
  labs(x = "Fish Treatment", y = "Phycocyanin AUC")+
  theme(legend.position="none")

combined_plot <- grid.arrange(
  chloro_plot + labs(title = NULL), 
  phyco_plot + labs(title = NULL), 
  ncol = 2,
  top = "Area Under the Curve by Fish Treatment at Noon"
)

# doing the same with all time points for Supplemental 3
library(gridExtra)
filtered_C <- fish_stats2 %>% filter(enclosure != "E16")
chloro_plot_all <- ggplot(fish_stats2, aes(x = fish_treatment, y = AUC, fill = fish_treatment)) +
  geom_violin() +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
  scale_fill_viridis_d(begin=0.7) +
  labs(x = "Fish Treatment", y = "Chlorophyll AUC") + 
  theme(legend.position="none")

filtered_data_P <- fish_stats3 %>% filter(enclosure != "E16")
phyco_plot_all <- ggplot(fish_stats3, aes(x = fish_treatment, y = AUC_P, fill = fish_treatment)) +
  geom_violin() +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
  scale_fill_viridis_d(begin=0.7) +
  labs(x = "Fish Treatment", y = "Phycocyanin AUC")+
  theme(legend.position="none")

combined_plot <- grid.arrange(
  chloro_plot_all + labs(title = NULL), 
  phyco_plot_all + labs(title = NULL), 
  ncol = 2,
  top = "Area Under the Curve by Fish Treatment, All Times"
)

#------------------------------

# PAR

fish_stats4 <- fish_stats

negative_par_count <- sum(fish_stats4$photosynthetically.active.radiation.up < 0)
print(negative_par_count) # 40,078 rows with negative PAR! won't exclude in outlier step but exclude for PAR analysis

positive_PAR <- fish_stats4[fish_stats4$photosynthetically.active.radiation.up >= 0, ] # 354145 rows

positive_PAR2 <- fish_stats4[fish_stats4$photosynthetically.active.radiation.up >= 0 & fish_stats4$specified.depth <= 6, ] # epilimnion only; 126030 rows

positive_PAR3 <- fish_stats4[fish_stats4$photosynthetically.active.radiation.up >= 0 & fish_stats4$hour_profile.datetime==12, ] # only noon; 15972 rows

positive_PAR4 <- fish_stats4[fish_stats4$photosynthetically.active.radiation.up >= 0 & fish_stats4$hour_profile.datetime==12 & fish_stats4$specified.depth <= 6, ] # only noon and only epilimnion; 5579 rows

# not linear... so log transform first?

positive_PAR$log_PAR <- log(positive_PAR$photosynthetically.active.radiation.up + 1)
LMM_PAR <- lmer(log_PAR ~ fish_treatment + specified.depth + (1 | enclosure), data = positive_PAR)
summary(LMM_PAR)
anova(LMM_PAR) # fish barely do not sig affect PAR p= 0.08662

positive_PAR2$log_PAR2 <- log(positive_PAR2$photosynthetically.active.radiation.up + 1)
LMM_PAR2 <- lmer(log_PAR2 ~ fish_treatment + specified.depth + (1 | enclosure), data = positive_PAR2)
summary(LMM_PAR2)
anova(LMM_PAR2) # fish don't have as much effect in epilimnion only, p=0.1104

positive_PAR3$log_PAR3 <- log(positive_PAR3$photosynthetically.active.radiation.up + 1)
LMM_PAR3 <- lmer(log_PAR3 ~ fish_treatment + specified.depth + (1 | enclosure), data = positive_PAR3)
summary(LMM_PAR3)
anova(LMM_PAR3) # no sig effect just looking at noon, p=0.1547

positive_PAR4$log_PAR4 <- log(positive_PAR4$photosynthetically.active.radiation.up + 1)
LMM_PAR4 <- lmer(log_PAR4 ~ fish_treatment + specified.depth + (1 | enclosure), data = positive_PAR4)
summary(LMM_PAR4)
anova(LMM_PAR4) # only noon and only epilimnion, make sense with above results that p=0.105 not significant

#------------------------------

# pH

fish_stats5 <- fish_stats
library(ggplot2)
my_palette <- viridis::viridis(2, begin = 0.7)[1:2]

plot_shallow <- ggplot(subset_shallow, aes(x = specified.depth, y = pH.value, color = fish_treatment)) +
  geom_point() +
  geom_smooth(method = "lm", color="black") +
  facet_wrap(~fish_treatment) +
  theme_minimal() +
  scale_color_manual(values= my_palette) +
  labs(title = "Epilimnion", x="Depth",y="pH")+
  theme(legend.position = "none")
plot_deep <- ggplot(subset_deep, aes(x = specified.depth, y = pH.value, color = fish_treatment)) +
  geom_point() +
  geom_smooth(method = "lm", color="black") +
  facet_wrap(~fish_treatment) +
  theme_minimal() +
  scale_color_manual(values= my_palette) +
  labs(title = "Hypolimnion", x="Depth",y="pH")+
  theme(legend.position = "none")

shallow_stats
combined <- grid.arrange(plot_shallow, plot_deep, ncol=2, top = "pH Value by Depth")
combined

subset_shallow <- fish_stats5[fish_stats5$specified.depth <= 6, ]

subset_deep <- fish_stats5[fish_stats5$specified.depth > 6, ]

LMM_pH_shallow <- lmer(pH.value ~ fish_treatment + specified.depth + (1 | enclosure), data = subset_shallow)
summary(LMM_pH_shallow)
anova(LMM_pH_shallow)

LMM_pH_deep <- lmer(pH.value ~ fish_treatment + specified.depth + (1 | enclosure), data = subset_deep)
summary(LMM_pH_deep)
anova(LMM_pH_deep) # p = 0.3836 for fish_treat; for specified depth, p <2e-16 *** - perhaps depth masking fish effect?

extract_stats <- function(model) {
  coef_summary <- summary(model)$coefficients
  slope <- coef_summary["specified.depth", "Estimate"]
  p_value <- coef_summary["specified.depth", "Pr(>|t|)"]
  return(data.frame(slope = slope, p_value = p_value))
}

# Extract slope and p-value for shallow and deep subsets
shallow_stats <- extract_stats(LMM_pH_shallow)
deep_stats <- extract_stats(LMM_pH_deep)

specific <- fish_stats5[fish_stats5$specified.depth == 5, ]
t.test(pH.value ~ fish_treatment, data= specific) # p-value < 2.2e-16


yes_fish <- subset_deep[subset_deep$fish_treatment == "yes", ]
no_fish <- subset_deep[subset_deep$fish_treatment == "no", ]

# Calculate standard deviation and variance for "yes" treated fish
yes_fish_sd <- sd(yes_fish$pH.value)
yes_fish_var <- var(yes_fish$pH.value)
yes_fish_sd # 0.377
yes_fish_var # 0.142
min(yes_fish$pH.value) # 6.95
max(yes_fish$pH.value) # 9.13

# Calculate standard deviation and variance for "no" treated fish
no_fish_sd <- sd(no_fish$pH.value)
no_fish_var <- var(no_fish$pH.value)
no_fish_sd # 0.381
no_fish_var # 0.145
min(no_fish$pH.value) # 6.9545
max(no_fish$pH.value) # 9.62


##############

# Conductivity & Oxygen

fish_stats6 <- fish_stats
subset_shallow <- fish_stats6[fish_stats6$specified.depth <= 6, ]

subset_deep <- fish_stats6[fish_stats6$specified.depth > 6, ]

LMM_oxygen_shallow <- lmer(oxygen.concentration ~ fish_treatment + specified.depth + (1 | enclosure), data = subset_shallow)
anova(LMM_oxygen_shallow) # p= 0.03205

LMM_oxygen_deep <- lmer(oxygen.concentration ~ fish_treatment + specified.depth + (1 | enclosure), data = subset_deep)
anova(LMM_oxygen_deep) # p= 0.5409

LMM_conduc_shallow <- lmer(conductivity ~ fish_treatment + specified.depth + (1 | enclosure), data = subset_shallow)
anova(LMM_conduc_shallow) # p= 0.5987

LMM_conduc_deep <- lmer(conductivity ~ fish_treatment + specified.depth + (1 | enclosure), data = subset_deep)
anova(LMM_conduc_deep) # p= 0.226

# Plots

grouped <- fish_stats6 %>%
  group_by(specified.depth, fish_treatment) %>%
  summarise(avg_oxygen = mean(oxygen.concentration),
            avg_conductivity = mean(conductivity)) %>%
  ungroup()

grouped_shallow <- subset_shallow %>%
  group_by(specified.depth, fish_treatment) %>%
  summarise(avg_oxygen = mean(oxygen.concentration),
            avg_conductivity = mean(conductivity)) %>%
  ungroup()

grouped_deep <- subset_deep %>%
  group_by(specified.depth, fish_treatment) %>%
  summarise(avg_oxygen = mean(oxygen.concentration),
            avg_conductivity = mean(conductivity)) %>%
  ungroup()

# Plotting average oxygen concentration
oxygen_plot <- ggplot(grouped, aes(x = avg_oxygen, y = specified.depth, color = fish_treatment)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = fish_treatment), se = FALSE) +
  labs(x = "Oxygen Concentration", y = "Depth", title = "Oxygen Concentration by Depth") +
  scale_color_manual(values= my_palette) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_y_reverse(limits = c(20, 0))

# Plotting average conductivity
condc_plot <- ggplot(grouped, aes(x = avg_conductivity, y = specified.depth, color = fish_treatment)) +
  geom_point() +
  geom_smooth(method = "lm",aes(group = fish_treatment), se = FALSE) +
  labs(x = "Conductivity", y = "Depth", title = "Conductivity by Depth") +
  scale_color_manual(values= my_palette) +
  theme_minimal() +
  scale_y_reverse(limits = c(20, 0))

combined2 <- grid.arrange(oxygen_plot, condc_plot, ncol=2)
combined2

# SEPARATE SHALLOW & DEEP

# shallow
oxygen_shallow <- ggplot(grouped_shallow, aes(x = avg_oxygen, y = specified.depth, color = fish_treatment)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = fish_treatment), se = FALSE) +
  labs(x = "Oxygen Concentration", y = "Depth", title = "Epilimnion Oxygen Concentration") +
  scale_color_manual(values= my_palette) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_y_reverse(limits = c(6, 0))

condc_shallow <- ggplot(grouped_shallow, aes(x = avg_conductivity, y = specified.depth, color = fish_treatment)) +
  geom_point() +
  geom_smooth(method = "lm",aes(group = fish_treatment), se = FALSE) +
  labs(x = "Conductivity", y = "Depth", title = "Epilimnion Conductivity") +
  scale_color_manual(values= my_palette) +
  theme_minimal() +
  scale_y_reverse(limits = c(6, 0))
combined_shallow <- grid.arrange(oxygen_shallow, condc_shallow, ncol=2)
combined_shallow

# deep

oxygen_deep <- ggplot(grouped_deep, aes(x = avg_oxygen, y = specified.depth, color = fish_treatment)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = fish_treatment), se = FALSE) +
  labs(x = "Oxygen Concentration", y = "Depth", title = "Hypolimnion Oxygen Concentration") +
  scale_color_manual(values= my_palette) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_y_reverse(limits = c(20, 6))

condc_deep <- ggplot(grouped_deep, aes(x = avg_conductivity, y = specified.depth, color = fish_treatment)) +
  geom_point() +
  geom_smooth(method = "lm",aes(group = fish_treatment), se = FALSE) +
  labs(x = "Conductivity", y = "Depth", title = "Epilimnion Conductivity") +
  scale_color_manual(values= my_palette) +
  theme_minimal() +
  scale_y_reverse(limits = c(20, 6))
combined_deep <- grid.arrange(oxygen_deep, condc_deep, ncol=2)
combined_deep

both <- grid.arrange(combined_shallow, combined_deep, ncol=1)
both