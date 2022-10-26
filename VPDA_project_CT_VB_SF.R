setwd('C:/Users/Asus/Documents/FEUP/MDSE/VPD/VPDA project')

library(tidyverse)

emigration <- read_csv2("emigration.csv") 
imigration <- read_csv2("imigration.csv") 


e <- ggplot(data = emigration) + geom_point(mapping = aes(x = Anos, y = Total))

i <- ggplot(data = imigration) + geom_point(mapping = aes(x = Anos, y = Total))

ggplot(mapping = aes(x = Anos, y = Total)) + 
  geom_point(data = emigration, color='darkgreen') +
  geom_point(data = imigration, color = 'orange') +
  geom_line(data=emigration)+
  labs(title = "Emigration and imigration throughout the years in Portugal") +
  labs(subtitle = paste( "printed in:" , today() ) )
  

