library (tidyverse)
library (ggplot2)
library(NonCompart)
library(dplyr)
library(pkr)
library(ncar)

data<- xgxr::case1_pkpd

# filtering cycle 1 & CMT ==2
CYCLE1 <-data |>
  filter(CYCLE == 1 & CMT == 2)

# change ID into factor
CYCLE1$ID <- as.factor(CYCLE1$ID)

# CYCLE1 DOSE 별 time-concentration plot
ggplot(CYCLE1, aes (NOMTIME, LIDV, color= ID)) +
  geom_point(size =2, alpha= 0.5) +
  geom_line (size = 1, alpha = 0.5) + 
  facet_wrap(~DOSE)
theme_bw() +
  labs (title = 'cycle 1',
        x = 'Time (hour)', y= 'Concentration (ng/ml')

# creating data set by dose
DOSE_3 <- CYCLE1 |>
  filter (DOSE == 3)

DOSE_10 <- CYCLE1 |>
  filter (DOSE == 10)

DOSE_30 <- CYCLE1 |>
  filter (DOSE == 30)

DOSE_100 <- CYCLE1 |>
  filter (DOSE == 100)

DOSE_300 <- CYCLE1 |>
  filter (DOSE == 300)


# plot for each dose data set (3,10,30,100,300)  
ggplot(DOSE_3, aes (NOMTIME, LIDV, color= ID)) +
  geom_point(size =2, alpha= 0.5) +
  geom_line (size = 1, alpha = 0.5) + 
  theme_bw() +
  labs (title = 'DOSE = 3mg ',
        x = 'Time (hour)', y= 'Concentration (ng/ml')
scale_x_continuous(limits = c (0,5)) #NOMTIME min-max
scale_y_continuous(limits=c (0,1))  #LIDV min-max

ggplot(DOSE_10, aes (NOMTIME, LIDV, color= ID)) +
  geom_point(size =2, alpha= 0.5) +
  geom_line (size = 1, alpha = 0.5) + 
  theme_bw() +
  labs (title = 'DOSE = 10mg ',
        x = 'Time (hour)', y= 'Concentration (ng/ml')
scale_x_continuous(limits = c (0,5)) #NOMTIME min-max
scale_y_continuous(limits=c (0,1))  #LIDV min-max

ggplot(DOSE_30, aes (NOMTIME, LIDV, color= ID)) +
  geom_point(size =2, alpha= 0.5) +
  geom_line (size = 1, alpha = 0.5) + 
  theme_bw() +
  labs (title = 'DOSE = 30mg ',
        x = 'Time (hour)', y= 'Concentration (ng/ml')
scale_x_continuous(limits = c (0,5)) #NOMTIME min-max
scale_y_continuous(limits=c (0,1))  #LIDV min-max


ggplot(DOSE_100, aes (NOMTIME, LIDV, color= ID)) +
  geom_point(size =2, alpha= 0.5) +
  geom_line (size = 1, alpha = 0.5) + 
  theme_bw() +
  labs (title = 'DOSE = 100mg ',
        x = 'Time (hour)', y= 'Concentration (ng/ml')
scale_x_continuous(limits = c (0,5)) #NOMTIME min-max
scale_y_continuous(limits=c (0,1))  #LIDV min-max

ggplot(DOSE_300, aes (NOMTIME, LIDV, color= ID)) +
  geom_point(size =2, alpha= 0.5) +
  geom_line (size = 1, alpha = 0.5) + 
  theme_bw() +
  labs (title = 'DOSE = 300mg ',
        x = 'Time (hour)', y= 'Concentration (ng/ml')
scale_x_continuous(limits = c (0,5)) #NOMTIME min-max
scale_y_continuous(limits=c (0,1))  #LIDV min-max


# NCA

tblNCA(DOSE_10, key = c('ID', 'DOSE'),
       colTime = "TIME", colConc = "LIDV", dose = 10,
       adm = "Extravascular", doseUnit = "mg", timeUnit = "h",
       concUnit = "ng/mL", down = "Linear", R2ADJ= 0.8, MW =0)


# Cmax for each dose 

CYCLE1 <- CYCLE1 |>
  group_by (ID) |>
  mutate (CMAX = max (LIDV))


CYCLE1|>
  group_by(DOSE) |>
  summarise_at(vars(CMAX), 
               list(Min = min, Mean = mean, Max = max, Sd = sd))  

# A tibble: 5 × 5
# DOSE    Min   Mean    Max      Sd
# <int>  <dbl>  <dbl>  <dbl>   <dbl>
# 1     3 0.05   0.0549 0.0872 0.00960
# 2    10 0.0576 0.126  0.256  0.0455 
# 3    30 0.155  0.314  0.708  0.133  
# 4   100 0.507  1.17   2.55   0.418  
# 5   300 2.01   3.43   5.97   1.05   
