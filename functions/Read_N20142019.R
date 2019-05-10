rm(list = ls())
library(readr)

eredivisie <- NULL
for (i in 2014:2018) {
  nam <- paste("N1",i,i+1,sep="")
  N2 <- assign(nam, read_csv(paste("data/N1",i,i+1,".csv", sep=""), 
                       col_types = cols_only(Date = col_datetime(format = "%d/%m/%y"), 
                                             FTHG = col_integer(), 
                                             FTAG = col_integer(), 
                                             HomeTeam = col_character(),
                                             AwayTeam = col_character())))
  N2 <- N2[complete.cases(N2[,c("Date","HomeTeam","AwayTeam")])==TRUE,]
  Season <- paste(i,"/",i+1, sep="")
  Week <- as.integer(floor(difftime(N2$Date,N2$Date[1],units="weeks"))+1)
  N2 <- cbind(Season,Week,N2)
  eredivisie <- rbind(eredivisie,N2)
}
names(eredivisie) <- c('Season','Week','Date','HomeTeam','AwayTeam','HomeGoals','AwayGoals')
save(eredivisie, file="results/eredivisie.RData")