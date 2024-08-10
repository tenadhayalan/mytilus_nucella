## process pH for Orion
# Created by Nyssa Silbiger
# Edited on 07/27/2021
library(tidyverse)
library(seacarb)
library(broom)
library(here)
library(lubridate)
library(ggridges)
library(jtools)
library(interactions)
library(sandwich)
library(patchwork)

## bring in pH calibration files and raw data files

TrisCalibrationLog<- read.csv("Data/pH_temp/tris_cal.csv")

pHcalib<-TrisCalibrationLog %>%
  mutate(TrisCalDate = ymd(date))


pHData<-read.csv("Data/pH_temp/pH_temp.csv")
pHData<-pHData%>%
  mutate(TrisCalDate = ymd(TrisCalDate),
         Sampling_Date = ymd(date))

# Needed for phosphate data


## take the mV calibration files by each date and use them to calculate pH
pHSlope<-pHcalib %>%
  # extract only the orion data
  nest_by(TrisCalDate)%>%
  mutate(fitpH = list(lm(mVTris~TTris, data = data))) %>% # linear regression of mV and temp of the tris
  summarise(broom::tidy(fitpH)) %>% # make the output tidy
  dplyr::select(TrisCalDate, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%# put slope and intercept in their own column
  right_join(.,pHData) %>% # join with the pH sample data
  mutate(mVTris = TempInSitu*TTris + `(Intercept)`) %>% # calculate the mV of the tris at temperature in which the pH of samples were measured
  #  drop_na(TempInSitu)%>%
  drop_na(mV) %>%
  mutate(pH = pH(Ex=mV,Etris=mVTris,S=Salinity_lab,T=TempInSitu))  # calculate pH of the samples using the pH seacarb function

view(pHSlope)

write.csv(pHSlope, "Data/ph_temp_final.csv")
