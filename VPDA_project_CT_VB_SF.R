setwd('C:/Users/Asus/Documents/FEUP/MDSE/VPD/VPDA project')

library(tidyverse)
library(readr)
library(reshape2)
library(RColorBrewer)
library(ggthemes)
library(ggHoriPlot)
library( ggplot2 )
library( arules )
library(zoo)

emigration <- read_csv2("emigration.csv") 
imigration <- read_csv2("imigration.csv") 

emigration <- mutate(emigration, type=replicate(14, "Emigration"))
imigration <- mutate(imigration, type=replicate(14, "Imigration"))

#----Treating Emigration file
colnames(emigration)[colnames(emigration) == "Menos de 15"] = "LessThan15"
colnames(emigration)[colnames(emigration) == "15-19"] = "from15to19"
colnames(emigration)[colnames(emigration) == "20-24"] = "from20to24"
colnames(emigration)[colnames(emigration) == "25-29"] = "from25to29"
colnames(emigration)[colnames(emigration) == "30-34"] = "from30to34"
colnames(emigration)[colnames(emigration) == "35-39"] = "from35to39"
colnames(emigration)[colnames(emigration) == "40-44"] = "from40to44"
colnames(emigration)[colnames(emigration) == "45-49"] = "from45to49"
colnames(emigration)[colnames(emigration) == "50-54"] = "from50to54"
colnames(emigration)[colnames(emigration) == "55-59"] = "from55to59"
colnames(emigration)[colnames(emigration) == "60-64"] = "from60to64"
colnames(emigration)[colnames(emigration) == "65 ou mais"] = "from65orMore"

emigration <- mutate(emigration, Total=gsub(" ", "", emigration$Total, fixed = TRUE))
emigration <- mutate(emigration, Total=as.numeric(gsub(" ", "", emigration$Total, fixed = TRUE)))
emigration <- mutate(emigration, LessThan15=gsub(" ", "", emigration$LessThan15, fixed = TRUE))
emigration <- mutate(emigration, LessThan15=as.numeric(gsub(" ", "", emigration$LessThan15, fixed = TRUE)))
emigration <- mutate(emigration, from15to19=gsub(" ", "", emigration$from15to19, fixed = TRUE))
emigration <- mutate(emigration, from15to19=as.numeric(gsub(" ", "", emigration$from15to19, fixed = TRUE)))
emigration <- mutate(emigration, from20to24=gsub(" ", "", emigration$from20to24, fixed = TRUE))
emigration <- mutate(emigration, from20to24=as.numeric(gsub(" ", "", emigration$from20to24, fixed = TRUE)))
emigration <- mutate(emigration, from25to29=gsub(" ", "", emigration$from25to29, fixed = TRUE))
emigration <- mutate(emigration, from25to29=as.numeric(gsub(" ", "", emigration$from25to29, fixed = TRUE)))
emigration <- mutate(emigration, from30to34=gsub(" ", "", emigration$from30to34, fixed = TRUE))
emigration <- mutate(emigration, from30to34=as.numeric(gsub(" ", "", emigration$from30to34, fixed = TRUE)))
emigration <- mutate(emigration, from35to39=gsub(" ", "", emigration$from35to39, fixed = TRUE))
emigration <- mutate(emigration, from35to39=as.numeric(gsub(" ", "", emigration$from35to39, fixed = TRUE)))
emigration <- mutate(emigration, from40to44=gsub(" ", "", emigration$from40to44, fixed = TRUE))
emigration <- mutate(emigration, from40to44=as.numeric(gsub(" ", "", emigration$from40to44, fixed = TRUE)))
emigration <- mutate(emigration, from45to49=gsub(" ", "", emigration$from45to49, fixed = TRUE))
emigration <- mutate(emigration, from45to49=as.numeric(gsub(" ", "", emigration$from45to49, fixed = TRUE)))
emigration <- mutate(emigration, from50to54=gsub(" ", "", emigration$from50to54, fixed = TRUE))
emigration <- mutate(emigration, from50to54=as.numeric(gsub(" ", "", emigration$from50to54, fixed = TRUE)))
emigration <- mutate(emigration, from55to59=gsub(" ", "", emigration$from55to59, fixed = TRUE))
emigration <- mutate(emigration, from55to59=as.numeric(gsub(" ", "", emigration$from55to59, fixed = TRUE)))
emigration <- mutate(emigration, from65orMore=gsub(" ", "", emigration$from65orMore, fixed = TRUE))
emigration <- mutate(emigration, from65orMore=as.numeric(gsub(" ", "", emigration$from65orMore, fixed = TRUE)))



colnames(emigration)[colnames(emigration) == "LessThan15"] = "< 15"
colnames(emigration)[colnames(emigration) == "from15to19"] = "[15, 19]"
colnames(emigration)[colnames(emigration) == "from20to24"] = "[20, 24]"
colnames(emigration)[colnames(emigration) == "from25to29"] = "[25, 29]"
colnames(emigration)[colnames(emigration) == "from30to34"] = "[30, 34]"
colnames(emigration)[colnames(emigration) == "from35to39"] = "[35, 39]"
colnames(emigration)[colnames(emigration) == "from40to44"] = "[40, 44]"
colnames(emigration)[colnames(emigration) == "from45to49"] = "[45, 49]"
colnames(emigration)[colnames(emigration) == "from50to54"] = "[50, 54]"
colnames(emigration)[colnames(emigration) == "from55to59"] = "[55, 59]"
colnames(emigration)[colnames(emigration) == "from60to64"] = "[60, 64]"
colnames(emigration)[colnames(emigration) == "from65orMore"] = ">= 65"




#----Treating Imigration file
colnames(imigration)[colnames(imigration) == "Menos de 15"] = "LessThan15"
colnames(imigration)[colnames(imigration) == "15-19"] = "from15to19"
colnames(imigration)[colnames(imigration) == "20-24"] = "from20to24"
colnames(imigration)[colnames(imigration) == "25-29"] = "from25to29"
colnames(imigration)[colnames(imigration) == "30-34"] = "from30to34"
colnames(imigration)[colnames(imigration) == "35-39"] = "from35to39"
colnames(imigration)[colnames(imigration) == "40-44"] = "from40to44"
colnames(imigration)[colnames(imigration) == "45-49"] = "from45to49"
colnames(imigration)[colnames(imigration) == "50-54"] = "from50to54"
colnames(imigration)[colnames(imigration) == "55-59"] = "from55to59"
colnames(imigration)[colnames(imigration) == "60-64"] = "from60to64"
colnames(imigration)[colnames(imigration) == "65 ou mais"] = "from65orMore"

imigration <- mutate(imigration, Total=gsub(" ", "", imigration$Total, fixed = TRUE))
imigration <- mutate(imigration, Total=as.numeric(gsub(" ", "", imigration$Total, fixed = TRUE)))
imigration <- mutate(imigration, LessThan15=gsub(" ", "", imigration$LessThan15, fixed = TRUE))
imigration <- mutate(imigration, LessThan15=as.numeric(gsub(" ", "", imigration$LessThan15, fixed = TRUE)))
imigration <- mutate(imigration, from15to19=gsub(" ", "", imigration$from15to19, fixed = TRUE))
imigration <- mutate(imigration, from15to19=as.numeric(gsub(" ", "", imigration$from15to19, fixed = TRUE)))
imigration <- mutate(imigration, from20to24=gsub(" ", "", imigration$from20to24, fixed = TRUE))
imigration <- mutate(imigration, from20to24=as.numeric(gsub(" ", "", imigration$from20to24, fixed = TRUE)))
imigration <- mutate(imigration, from25to29=gsub(" ", "", imigration$from25to29, fixed = TRUE))
imigration <- mutate(imigration, from25to29=as.numeric(gsub(" ", "", imigration$from25to29, fixed = TRUE)))
imigration <- mutate(imigration, from30to34=gsub(" ", "", imigration$from30to34, fixed = TRUE))
imigration <- mutate(imigration, from30to34=as.numeric(gsub(" ", "", imigration$from30to34, fixed = TRUE)))
imigration <- mutate(imigration, from35to39=gsub(" ", "", imigration$from35to39, fixed = TRUE))
imigration <- mutate(imigration, from35to39=as.numeric(gsub(" ", "", imigration$from35to39, fixed = TRUE)))
imigration <- mutate(imigration, from40to44=gsub(" ", "", imigration$from40to44, fixed = TRUE))
imigration <- mutate(imigration, from40to44=as.numeric(gsub(" ", "", imigration$from40to44, fixed = TRUE)))
imigration <- mutate(imigration, from45to49=gsub(" ", "", imigration$from45to49, fixed = TRUE))
imigration <- mutate(imigration, from45to49=as.numeric(gsub(" ", "", imigration$from45to49, fixed = TRUE)))
imigration <- mutate(imigration, from50to54=gsub(" ", "", imigration$from50to54, fixed = TRUE))
imigration <- mutate(imigration, from50to54=as.numeric(gsub(" ", "", imigration$from50to54, fixed = TRUE)))
imigration <- mutate(imigration, from55to59=gsub(" ", "", imigration$from55to59, fixed = TRUE))
imigration <- mutate(imigration, from55to59=as.numeric(gsub(" ", "", imigration$from55to59, fixed = TRUE)))
imigration <- mutate(imigration, from60to64=gsub(" ", "", imigration$from60to64, fixed = TRUE))
imigration <- mutate(imigration, from60to64=as.numeric(gsub(" ", "", imigration$from60to64, fixed = TRUE)))
imigration <- mutate(imigration, from65orMore=gsub(" ", "", imigration$from65orMore, fixed = TRUE))
imigration <- mutate(imigration, from65orMore=as.numeric(gsub(" ", "", imigration$from65orMore, fixed = TRUE)))

colnames(imigration)[colnames(imigration) == "LessThan15"] = "< 15"
colnames(imigration)[colnames(imigration) == "from15to19"] = "[15, 19]"
colnames(imigration)[colnames(imigration) == "from20to24"] = "[20, 24]"
colnames(imigration)[colnames(imigration) == "from25to29"] = "[25, 29]"
colnames(imigration)[colnames(imigration) == "from30to34"] = "[30, 34]"
colnames(imigration)[colnames(imigration) == "from35to39"] = "[35, 39]"
colnames(imigration)[colnames(imigration) == "from40to44"] = "[40, 44]"
colnames(imigration)[colnames(imigration) == "from45to49"] = "[45, 49]"
colnames(imigration)[colnames(imigration) == "from50to54"] = "[50, 54]"
colnames(imigration)[colnames(imigration) == "from55to59"] = "[55, 59]"
colnames(imigration)[colnames(imigration) == "from60to64"] = "[60, 64]"
colnames(imigration)[colnames(imigration) == "from65orMore"] = ">= 65"


#---- Merge files into 1 table
total <- rbind(emigration, imigration)

e <- ggplot(data = emigration) + geom_point(mapping = aes(x = Anos, y = Total))
i <- ggplot(data = imigration) + geom_point(mapping = aes(x = Anos, y = Total))

########
#---- PLOT 1: Emigration and immigration throughout the years in Portugal
########
emigration2 <- emigration[,c(-15)]
imigration2 <- imigration[,c(-15)]
diffs <- imigration2 - emigration2
diffs <- mutate(diffs, Anos=emigration$Anos)
diffs <- diffs[,-c(3:15)]
diffs <- mutate(diffs, dif=ifelse( Total >= 0, "Positive migration balance", "Negative migration balance" ))


p <- ggplot() + 
  geom_bar(data=diffs, aes(x=Anos, y=Total,fill = dif), stat='identity') +
  geom_point(data=total, aes(x = Anos,y=Total, color = factor( type)), size = 3.5) +
  geom_line(data=total, aes(x = Anos, y=Total, color = factor( type)), size = 1.5) +
  geom_vline(xintercept=2011,colour="gray", linetype=3) +
  geom_vline(xintercept=2014,colour="gray", linetype=3) +
  geom_vline(xintercept=2020,colour="ivory", linetype=3) +
  scale_x_continuous(
    name = 'Years',
    breaks=seq(from = 2008, to = 2021, by = 1),
    labels = function(x) {x})+
  geom_rect(data=total[1,],aes(xmin = 2011, xmax = 2014, ymin = -Inf, ymax = Inf, fill="Troika Intervention")) +
  geom_rect(data=total[1,],aes(xmin = 2020, xmax = Inf, ymin = -Inf, ymax = Inf, fill="Covid Pandemic")) +
  scale_fill_manual("", breaks=c("Troika Intervention","Covid Pandemic", "Positive migration balance", "Negative migration balance"), values = alpha(c("#FCECA5", "#F1CCD7", "#2E765E","#FC3C80"), 0.4)) +
  labs(title = "Emigration and immigration throughout the years in Portugal") +
  guides(color = guide_legend(title = "Direction of flow")) +
  labs(subtitle = "Effects economical and social event on population flow from 2008 to 2021" ) +
  ylab("Number of people") +
  xlab("Year") +
  labs(caption = "Source: PORTDATA, @2022 \nAuthors: Cátia Teixeira, Sónia Ferreira and Vasco Bartolomeu @FEUP-MECD") +
  theme(plot.title = element_text(size = 18, face = "bold", family = "Helvetica")) + # changes the size of the title
  theme(plot.subtitle = element_text(size = 10)) +
  theme(plot.caption = element_text(size = 7, family = "Helvetica"))+
  theme_bw()

p


##---- HEATMAP's

# #--Heatmap 1
# emigration2 <- emigration[,c(-15)]
# imigration2 <- imigration[,c(-15)]
# diffs <- emigration2 - imigration2
# diffs <- mutate(diffs, Anos=emigration$Anos)
# diffs <- diffs[,-c(3:15)]
# 
# 
# emigration3 <- emigration[,c(-2,-15)]
# imigration3 <- imigration[,c(-2,-15)]
# diffs2 <- imigration3 - emigration3
# diffs2 <- mutate(diffs, Anos=emigration$Anos)
# 
# normalize <- function(x, na.rm = TRUE) {
#   +     return((x- min(x)) /(max(x)-min(x)))
# }
# 
# diffs2 <- normalize(diffs2)
# diffs2 <- mutate(diffs2, Anos=emigration$Anos)
# 
# data1 <- melt(diffs2, id=c("Anos"))
# plot1 <- ggplot(data1, aes(variable, Anos, fill=value)) + geom_tile() + 
#   xlab("Age") +
#   ylab("Years") +
#   ggtitle("Migration Flow", subtitle = "Per year, what is the migration flux of Portugal?" ) +
#   labs(fill = "Values" ) +
#   labs(caption = "Source: PORTDATA, @2022 \nAuthors: Cátia Teixeira, Sónia Ferreira and Vasco Bartolomeu @FEUP-MECD") +
#   scale_fill_distiller(palette = 'PiYG')
# plot1


hm_emi <- emigration[,c(-2,-15)] #exclude columns
hm_imi <- imigration[,c(-2,-15)]

#--Heatmap Emigration
hme_mtl <- melt(hm_emi, id=c("Anos"))
hm_emigrations <- ggplot(hme_mtl, aes(variable, Anos, fill=value)) + geom_tile() +
  scale_y_continuous(
    name = 'Years',
    breaks=seq(from = 2008, to = 2021, by = 1),
    labels = function(x) {x})+
  xlab("Age") +
  ylab("Years") +
  labs( title = "Emigration in Portugal") +
  labs(subtitle = "from 2008 to 2021" ) +
  labs(fill = "Values" ) +
  labs(caption = "Source: PORTDATA, @2022 \nAuthors: Cátia Teixeira, Sónia Ferreira and Vasco Bartolomeu @FEUP-MECD") +
  scale_fill_distiller(palette = 'Blues',direction=+1)+
  theme_bw()
hm_emigrations

#--Heatmap Imigration



# legend <- c( "Menos de 15", "15-19", "20-24","25-29","30-34","35-39","40-44","45-49","50-54",
#              "55-59","60-64", "65 ou mais")
# result_df <-data.frame(legend) 


hmi_mtl <- melt(hm_imi, id=c("Anos"))
hm_imigrations <- ggplot(hmi_mtl, aes(variable, Anos, fill=value)) + 
  geom_tile() +
  scale_y_continuous(
    name = 'Years',
    breaks=seq(from = 2008, to = 2021, by = 1),
    labels = function(x) {x})+
  xlab("Age") +
  ylab("Years") +
  ggtitle("Immigration in Portugal", subtitle = "From 2008 to 2021" ) +
  labs(fill = "Values" ) +
  labs(caption = "Source: PORTDATA, @2022 \nAuthors: Cátia Teixeira, Sónia Ferreira and Vasco Bartolomeu @FEUP-MECD") +
  scale_fill_distiller(palette = 'Blues', direction=+1)+
  theme_bw()
  # axis(1, at=result_df$legend, labels=result_df$legend,cex.axis=0.6)
hm_imigrations


#--Heatmap calc differences btw emigration and immigration

min_max <- function( x , new_min , new_max )
{
  if( missing (new_min ) ) new_min <- 0 ## set default value for the parameter
  if( missing (new_max ) ) new_max <- 1
  return( ( x - min(x) ) / (max( x )-min( x ) ) * ( new_max - new_min) + new_min )
}


hm_diffs <- hm_imi - hm_emi 
hm_diffs <- min_max(hm_diffs, -1, 1)
hm_diffs <- mutate(hm_diffs, Anos=emigration$Anos)


hmd_melt <- melt(hm_diffs, id=c("Anos"))
hm_diffs <- ggplot(hmd_melt, aes(variable, Anos, fill=value)) + geom_tile() + 
  scale_y_continuous(
    name = 'Years',
    breaks=seq(from = 2008, to = 2021, by = 1),
    labels = function(x) {x})+
  xlab("Age") +
  ylab("Years") +
  labs(title = "Migration Balance of Portugal throughout the years" ) +
  labs(subtitle = "Per year, what is the migration balance of Portugal between 2008 to 2021?" ) +
  labs(fill = "Values" ) +
  labs(caption = "Source: PORTDATA, @2022 \nAuthors: Cátia Teixeira, Sónia Ferreira and Vasco Bartolomeu @FEUP-MECD") +
  scale_fill_distiller(palette = 'BrBG')+
  theme_bw()
hm_diffs


#--Heatmap Emigration and Immigration

hm_tot <- total[,c(-2)]
hmtot_melt <- melt(hm_tot, id=c("Anos","type"))
hm_emi_imi <- ggplot(hmtot_melt, aes(variable, Anos, fill=value)) + geom_tile() +
  scale_y_continuous(
    name = 'Years',
    breaks=seq(from = 2008, to = 2021, by = 1),
    labels = function(x) {x})+
  xlab("Age") +
  ylab("Years") +
  ggtitle("Migration Flow", subtitle = "Per year, what is the migration flux of Portugal?" ) +
  labs(fill = "Values") +
  labs(caption = "Source: PORTDATA, @2022 \nAuthors: Cátia Teixeira, Sónia Ferreira and Vasco Bartolomeu @FEUP-MECD") +
  scale_fill_distiller(palette = 'Blues', direction=+1) +
  facet_wrap(~type, ncol=2) +
  theme_bw()
hm_emi_imi




#--Plot: Nationalities of legal immigrants to Portugal
nat <- read_csv2('nationalities.csv')

nat_long <- tidyr::pivot_longer(nat, 2:17, names_to = 'Nationality', values_to = 'value')

africa <- c("Angola", "Cape Verde", "Guiné-Bissau", "Mozambique", "S. Tomé e Príncipe")
europe <- c("France", "Italy", "Moldova", "Romania", "Spain", "Ukraine", "United Kingdom")
america <- c("Brazil")
asia <- c("India", "Nepal", "China")

continent_label <- function(x) { 
  if(x %in% africa) y <- "Africa"
  if(x %in% europe) y <- "Europe"
  if(x %in% america) y <- "America"
  if(x %in% asia) y <- "Asia"
  return(y)
}



nat_long$continent <- sapply(nat_long$Nationality, continent_label)


# color_label <- function(x) { 
#   if(identical(tolower(x), tolower("Africa"))) y <- "Peach"
#   if(identical(tolower(x), tolower("Europe"))) y <- "Blues"
#   if(identical(tolower(x), tolower("America"))) y <- "Greens"
#   if(identical(tolower(x), tolower("Asia"))) y <- "Purples"
#   return(y)
# }
# 

nat_long %>%
  arrange(continent)


nat_long %>% ggplot() +
  geom_horizon(mapping = aes(Anos, value), origin = 'min', horizonscale = 4) +
  facet_wrap(~Nationality, ncol = 1, strip.position = 'right') +
  scale_fill_hcl( palette = "Peach", reverse = T)+
  theme_few() +
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(angle = 0),
    legend.position = 'none',
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) +
  scale_x_continuous(
    name = 'Years',
    breaks=seq(from = 2008, to = 2021, by = 1),
    labels = function(x) {x}) +
  ggtitle('Nationalities of legal immigrants in Portugal', subtitle= "Foreign population with legal resident status, by main origins") +
  labs(caption = "Source: PORTDATA, @2022 \nAuthors: Cátia Teixeira, Sónia Ferreira and Vasco Bartolomeu @FEUP-MECD")

