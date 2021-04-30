#Milciades Arauz
#Differences of passes between a league matches and tournament matches.
#the upcoming data is about one season of an german soccer(football) team which is Bayern Munich

library(dslabs)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RCurl)

#The raw data is imported from github by an url and read the csv file that is 
#in the repository. 
urlFile <- 'https://raw.githubusercontent.com/MilEAr19/IS4300-Final-Project/main/Passes_of_League_Tournament.csv'
passes <- read.csv(urlFile)

#Before using the any data we have to modify the names of the variables that 
#we're going to use  
names(passes) <- c("Player","Pos","Age","CompT.Pass","AttemT.Pass","Comp.T.","CompL.Pass","AttTT","Cmp.TT")

#Also we have to change the values of the "Player" column because the rstudio 
#doesn't recognizes the diaeresis(ü), accent marks(á,é,í,c), caron(s), or circumflex(ô) in some of the names. 
passes$Player[c(4,8,9,14,15,17,18,19)] <- c("Thiago Alcántara","Jérôme Boateng","Thomas Müller",
                                            "Ivan Perisic","Javi Martínez", "Niklas Süle", "Lucas Hernández",
                                            "Álvaro Odriozola")
                                   
# Now we just transform the data into a data frame and change its name
Passes <- as.data.frame(passes)

# The first histogram is about the amount of passes in the league the Bayern Munich
#did in the season 2019-2020

Pass1 <- Passes$CompL.Pass
hist(Pass1, col= "red", main = "League Passes" , labels = TRUE)

Pass2<- Passes$CompT.Pass
hist(Pass2, col = "blue", main = "Tournament Passes")

# Boxplot Comparison between the two data
boxplot(Passes$CompL.Pass, Passes$CompT.Pass , main ="League Passes vs Tournament Passes" ,horizontal = TRUE ,
        at = c(1,2), names = c("League", "Tournament"), col = c("red", "blue"))

# Another question might be asked and is which player contributed
# with the most passes between the league and tournament matches? 
#We name the scatter plot "posplot" due to the position of the players 
posplot <- Passes %>% ggplot()
#in the axis we use "CompL.Pass in x and CompT.Pass in y, also adding the title and the name of the
#players in that position. 
posplot<- posplot + geom_point(aes(CompL.Pass, CompT.Pass, col= Pos), size = 2.7) + 
   geom_text(aes(CompL.Pass, CompT.Pass), label = Passes$Player, size = 2.4) + ggtitle("Passes per Postion/Player")

posplot
#Finally, a TWO SAMPLE T TEST to represent and differentiate the amount of passes of this two samples

pass_final <- t.test(Passes$CompL.Pass, Passes$CompT.Pass) 

pass_final


