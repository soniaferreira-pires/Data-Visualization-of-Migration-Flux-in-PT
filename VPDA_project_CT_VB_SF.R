setwd('C:/Users/Asus/Documents/FEUP/MDSE/VPD/VPDA project')

library(tidyverse)
library(readr)
library(reshape2)

#Treating the File Emigration
colnames(emigration)[colnames(emigration) == "Menos de 15"] = "MenosDe15"
colnames(emigration)[colnames(emigration) == "15-19"] = "De15ate19"
colnames(emigration)[colnames(emigration) == "20-24"] = "De20ate24"
colnames(emigration)[colnames(emigration) == "25-29"] = "De25ate29"
colnames(emigration)[colnames(emigration) == "30-34"] = "De30ate34"
colnames(emigration)[colnames(emigration) == "35-39"] = "De35ate39"
colnames(emigration)[colnames(emigration) == "40-44"] = "De40ate44"
colnames(emigration)[colnames(emigration) == "45-49"] = "De45ate49"
colnames(emigration)[colnames(emigration) == "50-54"] = "De50ate54"
colnames(emigration)[colnames(emigration) == "55-59"] = "De55ate59"
colnames(emigration)[colnames(emigration) == "60-64"] = "De60ate64"
colnames(emigration)[colnames(emigration) == "65 ou mais"] = "De65ouMais"



emigration <- mutate(emigration, Total=gsub(" ", "", emigration$Total, fixed = TRUE))
emigration <- mutate(emigration, Total=as.numeric(gsub(" ", "", emigration$Total, fixed = TRUE)))
emigration <- mutate(emigration, MenosDe15=gsub(" ", "", emigration$MenosDe15, fixed = TRUE))
emigration <- mutate(emigration, MenosDe15=as.numeric(gsub(" ", "", emigration$MenosDe15, fixed = TRUE)))
emigration <- mutate(emigration, De15ate19=gsub(" ", "", emigration$De15ate19, fixed = TRUE))
emigration <- mutate(emigration, De15ate19=as.numeric(gsub(" ", "", emigration$De15ate19, fixed = TRUE)))
emigration <- mutate(emigration, De20ate24=gsub(" ", "", emigration$De20ate24, fixed = TRUE))
emigration <- mutate(emigration, De20ate24=as.numeric(gsub(" ", "", emigration$De20ate24, fixed = TRUE)))
emigration <- mutate(emigration, De25ate29=gsub(" ", "", emigration$De25ate29, fixed = TRUE))
emigration <- mutate(emigration, De25ate29=as.numeric(gsub(" ", "", emigration$De25ate29, fixed = TRUE)))
emigration <- mutate(emigration, De30ate34=gsub(" ", "", emigration$De30ate34, fixed = TRUE))
emigration <- mutate(emigration, De30ate34=as.numeric(gsub(" ", "", emigration$De30ate34, fixed = TRUE)))
emigration <- mutate(emigration, De35ate39=gsub(" ", "", emigration$De35ate39, fixed = TRUE))
emigration <- mutate(emigration, De35ate39=as.numeric(gsub(" ", "", emigration$De35ate39, fixed = TRUE)))
emigration <- mutate(emigration, De40ate44=gsub(" ", "", emigration$De40ate44, fixed = TRUE))
emigration <- mutate(emigration, De40ate44=as.numeric(gsub(" ", "", emigration$De40ate44, fixed = TRUE)))
emigration <- mutate(emigration, De45ate49=gsub(" ", "", emigration$De45ate49, fixed = TRUE))
emigration <- mutate(emigration, De45ate49=as.numeric(gsub(" ", "", emigration$De45ate49, fixed = TRUE)))
emigration <- mutate(emigration, De50ate54=gsub(" ", "", emigration$De50ate54, fixed = TRUE))
emigration <- mutate(emigration, De50ate54=as.numeric(gsub(" ", "", emigration$De50ate54, fixed = TRUE)))
emigration <- mutate(emigration, De55ate59=gsub(" ", "", emigration$De55ate59, fixed = TRUE))
emigration <- mutate(emigration, De55ate59=as.numeric(gsub(" ", "", emigration$De55ate59, fixed = TRUE)))
emigration <- mutate(emigration, De65ouMais=gsub(" ", "", emigration$De65ouMais, fixed = TRUE))
emigration <- mutate(emigration, De65ouMais=as.numeric(gsub(" ", "", emigration$De65ouMais, fixed = TRUE)))



#Treating the File Imigration
colnames(imigration)[colnames(imigration) == "Menos de 15"] = "MenosDe15"
colnames(imigration)[colnames(imigration) == "15-19"] = "De15ate19"
colnames(imigration)[colnames(imigration) == "20-24"] = "De20ate24"
colnames(imigration)[colnames(imigration) == "25-29"] = "De25ate29"
colnames(imigration)[colnames(imigration) == "30-34"] = "De30ate34"
colnames(imigration)[colnames(imigration) == "35-39"] = "De35ate39"
colnames(imigration)[colnames(imigration) == "40-44"] = "De40ate44"
colnames(imigration)[colnames(imigration) == "45-49"] = "De45ate49"
colnames(imigration)[colnames(imigration) == "50-54"] = "De50ate54"
colnames(imigration)[colnames(imigration) == "55-59"] = "De55ate59"
colnames(imigration)[colnames(imigration) == "60-64"] = "De60ate64"
colnames(imigration)[colnames(imigration) == "65 ou mais"] = "De65ouMais"



imigration <- mutate(imigration, Total=gsub(" ", "", imigration$Total, fixed = TRUE))
imigration <- mutate(imigration, Total=as.numeric(gsub(" ", "", imigration$Total, fixed = TRUE)))
imigration <- mutate(imigration, MenosDe15=gsub(" ", "", imigration$MenosDe15, fixed = TRUE))
imigration <- mutate(imigration, MenosDe15=as.numeric(gsub(" ", "", imigration$MenosDe15, fixed = TRUE)))
imigration <- mutate(imigration, De15ate19=gsub(" ", "", imigration$De15ate19, fixed = TRUE))
imigration <- mutate(imigration, De15ate19=as.numeric(gsub(" ", "", imigration$De15ate19, fixed = TRUE)))
imigration <- mutate(imigration, De20ate24=gsub(" ", "", imigration$De20ate24, fixed = TRUE))
imigration <- mutate(imigration, De20ate24=as.numeric(gsub(" ", "", imigration$De20ate24, fixed = TRUE)))
imigration <- mutate(imigration, De25ate29=gsub(" ", "", imigration$De25ate29, fixed = TRUE))
imigration <- mutate(imigration, De25ate29=as.numeric(gsub(" ", "", imigration$De25ate29, fixed = TRUE)))
imigration <- mutate(imigration, De30ate34=gsub(" ", "", imigration$De30ate34, fixed = TRUE))
imigration <- mutate(imigration, De30ate34=as.numeric(gsub(" ", "", imigration$De30ate34, fixed = TRUE)))
imigration <- mutate(imigration, De35ate39=gsub(" ", "", imigration$De35ate39, fixed = TRUE))
imigration <- mutate(imigration, De35ate39=as.numeric(gsub(" ", "", imigration$De35ate39, fixed = TRUE)))
imigration <- mutate(imigration, De40ate44=gsub(" ", "", imigration$De40ate44, fixed = TRUE))
imigration <- mutate(imigration, De40ate44=as.numeric(gsub(" ", "", imigration$De40ate44, fixed = TRUE)))
imigration <- mutate(imigration, De45ate49=gsub(" ", "", imigration$De45ate49, fixed = TRUE))
imigration <- mutate(imigration, De45ate49=as.numeric(gsub(" ", "", imigration$De45ate49, fixed = TRUE)))
imigration <- mutate(imigration, De50ate54=gsub(" ", "", imigration$De50ate54, fixed = TRUE))
imigration <- mutate(imigration, De50ate54=as.numeric(gsub(" ", "", imigration$De50ate54, fixed = TRUE)))
imigration <- mutate(imigration, De55ate59=gsub(" ", "", imigration$De55ate59, fixed = TRUE))
imigration <- mutate(imigration, De55ate59=as.numeric(gsub(" ", "", imigration$De55ate59, fixed = TRUE)))
imigration <- mutate(imigration, De60ate64=gsub(" ", "", imigration$De60ate64, fixed = TRUE))
imigration <- mutate(imigration, De60ate64=as.numeric(gsub(" ", "", imigration$De60ate64, fixed = TRUE)))
imigration <- mutate(imigration, De65ouMais=gsub(" ", "", imigration$De65ouMais, fixed = TRUE))
imigration <- mutate(imigration, De65ouMais=as.numeric(gsub(" ", "", imigration$De65ouMais, fixed = TRUE)))

total <- rbind(emigration, imigration)

e <- ggplot(data = emigration) + geom_point(mapping = aes(x = Anos, y = Total))

i <- ggplot(data = imigration) + geom_point(mapping = aes(x = Anos, y = Total))


## ESTE É O GRAFICO COM LEGENDAS

emigration2 <- emigration[,c(-15)]
imigration2 <- imigration[,c(-15)]
diffs <- imigration2 - emigration2
diffs <- mutate(diffs, Anos=emigration$Anos)
diffs <- diffs[,-c(3:15)]
diffs <- mutate(diffs, dif=ifelse( Total >= 0, "Saldo Positivo", "Saldo Negativo" ))

p <- ggplot() + 
  geom_bar(data=diffs, aes(x=Anos, y=Total,fill = dif), stat='identity') +
  geom_point(data=total, aes(x = Anos,y=Total, color = factor( type))) +
  geom_line(data=total, aes(x = Anos, y=Total, color = factor( type))) +
  geom_vline(xintercept=2011,colour="blue", linetype=3) + 
  geom_vline(xintercept=2014,colour="blue", linetype=3) + 
  geom_vline(xintercept=2020,colour="red", linetype=3) + 
  geom_rect(data=total[1,],aes(xmin = 2011, xmax = 2014, ymin = -Inf, ymax = Inf, fill="Troika Intervention")) +
  geom_rect(data=total[1,],aes(xmin = 2020, xmax = Inf, ymin = -Inf, ymax = Inf, fill="Covid Pandemic")) +
  scale_fill_manual("", breaks=c("Troika Intervention","Covid Pandemic", "Saldo Positivo", "Saldo Negativo"), values = alpha(c("blue", "red","yellow","green"), 0.3)) +
  labs(title = "Emigration and imigration throughout the years in Portugal") +
  guides(color = guide_legend(title = "Direction of flow")) +
  labs(subtitle = "Effects economical and social event on population flow from 2008 to 2021" ) +
  ylab("Number of people") + 
  xlab("Year") + 
  labs(caption = "Source: PORTDATA, @2022 \nAuthors: Cátia Teixeira, Sónia Ferreira and Vasco Bartolomeu @FEUP-MECD") +
  theme(plot.title = element_text(size = 18, face = "bold", family = "Helvetica")) + # changes the size of the title
  theme(plot.subtitle = element_text(size = 10)) +
  theme(plot.caption = element_text(size = 7, family = "Helvetica"))

p


emigration2 <- emigration[,c(-15)]
imigration2 <- imigration[,c(-15)]
diffs <- emigration2 - imigration2
diffs <- mutate(diffs, Anos=emigration$Anos)
diffs <- diffs[,-c(3:15)]


emigration3 <- emigration[,c(-2,-15)]
imigration3 <- imigration[,c(-2,-15)]
diffs2 <- imigration3 - emigration3
diffs2 <- mutate(diffs, Anos=emigration$Anos)

normalize <- function(x, na.rm = TRUE) {
  +     return((x- min(x)) /(max(x)-min(x)))
}

diffs2 <- normalize(diffs2)
diffs2 <- mutate(diffs2, Anos=emigration$Anos)

data1 <- melt(diffs2, id=c("Anos"))
plot1 <- ggplot(data1, aes(variable, Anos, fill=value)) + geom_tile() + 
  xlab("Age") +
  ylab("Years") +
  ggtitle("Migration Flow", subtitle = "Per year, what is the migration flux of Portugal?" ) +
  labs(fill = "Values" ) +
  labs(caption = "Source: PORTDATA, @2022 \nAuthors: Cátia Teixeira, Sónia Ferreira and Vasco Bartolomeu @FEUP-MECD") +
  scale_fill_distiller(palette = 'PiYG')
plot1


# HEATMAP DIFERENCES
imigration4 <- mutate(imigration, Total=gsub(" ", "", imigration$Total, fixed = TRUE))
emigration4 <- mutate(emigration, Total=gsub(" ", "", emigration$Total, fixed = TRUE))

emigration5 <- emigration[,c(-2,-15)]
imigration5 <- imigration[,c(-2,-15)]
diffs2 <- emigration5 - imigration5
diffs2 <- mutate(diffs2, Anos=emigration$Anos)

data2 <- melt(diffs2, id=c("Anos"))
plot_difs <- ggplot(data2, aes(variable, Anos, fill=value)) + geom_tile() + 
  xlab("Age") +
  ylab("Years") +
  ggtitle("Migration Flow", subtitle = "Per year, what is the migration flux of Portugal?" ) +
  labs(fill = "Values" ) +
  labs(caption = "Source: PORTDATA, @2022 \nAuthors: Cátia Teixeira, Sónia Ferreira and Vasco Bartolomeu @FEUP-MECD") +
  scale_fill_distiller(palette = 'PiYG')
plot_difs

# HEATMAP EMIGRATION
em <- emigration
em <- em[,c(-2,-15)]
em_mtl <- melt(em, id=c("Anos"))

plot2 <- ggplot(em_mtl, aes(variable, Anos, fill=value)) + geom_tile() + 
  xlab("Age") +
  ylab("Years") +
  ggtitle("Migration Flow", subtitle = "Per year, what is the migration flux of Portugal?" ) +
  labs(fill = "Values" ) +
  labs(caption = "Source: PORTDATA, @2022 \nAuthors: Cátia Teixeira, Sónia Ferreira and Vasco Bartolomeu @FEUP-MECD") +
  scale_fill_distiller(palette = 'PiYG') 
plot2


# HEATMAP IMIGRATION
im <- imigration
im <- im[,c(-2,-15)]
im_mtl <- melt(im, id=c("Anos"))
im_mtl

plot3 <- ggplot(im_mtl, aes(variable, Anos, fill=value)) + geom_tile() + 
  xlab("Age") +
  ylab("Years") +
  ggtitle("Migration Flow", subtitle = "Per year, what is the migration flux of Portugal?" ) +
  labs(fill = "Values" ) +
  labs(caption = "Source: PORTDATA, @2022 \nAuthors: Cátia Teixeira, Sónia Ferreira and Vasco Bartolomeu @FEUP-MECD") +
  scale_fill_distiller(palette = 'PiYG')

plot3


t <- total[,c(-2)]
tmelt <- melt(t, id=c("Anos","type"))
plot4 <- ggplot(tmelt, aes(variable, Anos, fill=value)) + geom_tile() +
  xlab("Age") +
  ylab("Years") +
  ggtitle("Migration Flow", subtitle = "Per year, what is the migration flux of Portugal?" ) +
  labs(fill = "Values" ) +
  labs(caption = "Source: PORTDATA, @2022 \nAuthors: Cátia Teixeira, Sónia Ferreira and Vasco Bartolomeu @FEUP-MECD") +
  scale_fill_distiller(palette = 'PiYG') +
  facet_wrap(~type, ncol=2)
plot4



