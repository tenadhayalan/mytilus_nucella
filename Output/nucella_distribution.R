library(tidyverse)

nucella <- read.csv("Data/nucella_initial.csv")

nucella$pH= as.character(nucella$pH)

ggplot(data=na.omit(nucella), aes(x=pH,y=blotted_wet_weight_g, group=interaction(pH,temp),fill = temp, color = temp))+
  geom_boxplot(alpha=0.5, outlier.alpha=0) + 
  geom_jitter(width=0.25, pch=21, size=2) +
  labs(x="pH", y= "Wet weight (g)")+
  theme_bw()
