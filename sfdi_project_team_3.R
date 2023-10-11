#Data Collection
data = read.csv("train.csv")
View(data)
str(data)
summary(data)
library(tidyverse)
ggplot(data,aes(MSZoning))+
  geom_bar()
#Alley can be removed from the dataset as most of it is NA
ggplot(data,aes(Alley))+
  geom_bar()
cor(data)
