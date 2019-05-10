knitr::opts_chunk$set(cache=TRUE, message=FALSE, warning=FALSE, fig.width=9,
                      fig.height=5, dpi=90,fig.path='results/',
                      purl=FALSE)

library(rjags)
library(coda)
library(mcmcplots)
library(stringr)
library(plyr)
library(xtable)
library(ggplot2)

source("functions/plotPost.R")
source("functions/Read_N20142019.R")
load(file="results/eredivisie.RData")

set.seed(12345)

# -1 Away win, 0 Draw, 1 Home win
eredivisie$MatchResult <- sign(eredivisie$HomeGoals - eredivisie$AwayGoals) 

# Creating a data frame d with only the complete match results
d <- na.omit(eredivisie)

teams <- unique(c(d$HomeTeam, d$AwayTeam))
seasons <- unique(d$Season)

# A list for JAGS with the data from d where the strings are coded as integers
data_list <- list(HomeGoals = d$HomeGoals, AwayGoals = d$AwayGoals, 
                  HomeTeam = as.numeric(factor(d$HomeTeam, levels=teams)),
                  AwayTeam = as.numeric(factor(d$AwayTeam, levels=teams)),
                  Season = as.numeric(factor(d$Season, levels=seasons)),
                  n_teams = length(teams), n_games = nrow(d), 
                  n_seasons = length(seasons))

# Convenience function to generate the type of column names Jags outputs.
col_name <- function(name, ...) {
  paste0(name, "[", paste(..., sep=",") , "]")
}

qplot(Season, HomeTeam, data=d, ylab="Team", xlab = "Season")

m3_string <- "model {
for(i in 1:n_games) {
HomeGoals[i] ~ dpois(lambda_home[Season[i], HomeTeam[i],AwayTeam[i]])
AwayGoals[i] ~ dpois(lambda_away[Season[i], HomeTeam[i],AwayTeam[i]])
}

for(season_i in 1:n_seasons) {
for(home_i in 1:n_teams) {
for(away_i in 1:n_teams) {
lambda_home[season_i, home_i, away_i] <- exp( home_baseline[season_i] + skill[season_i, home_i] - skill[season_i, away_i])
lambda_away[season_i, home_i, away_i] <- exp( away_baseline[season_i] + skill[season_i, away_i] - skill[season_i, home_i])
}
}
}

skill[1, 1] <- 0 
for(j in 2:n_teams) {
skill[1, j] ~ dnorm(group_skill, group_tau)
}

group_skill ~ dnorm(0, 0.0625)
group_tau <- 1/pow(group_sigma, 2)
group_sigma ~ dunif(0, 3)

home_baseline[1] ~ dnorm(0, 0.0625)
away_baseline[1] ~ dnorm(0, 0.0625)

for(season_i in 2:n_seasons) {
skill[season_i, 1] <- 0 
for(j in 2:n_teams) {
skill[season_i, j] ~ dnorm(skill[season_i - 1, j], season_tau)
}
home_baseline[season_i] ~ dnorm(home_baseline[season_i - 1], season_tau)
away_baseline[season_i] ~ dnorm(away_baseline[season_i - 1], season_tau)
}

season_tau <- 1/pow(season_sigma, 2) 
season_sigma ~ dunif(0, 3) 
}"

# Compiling the model
m3 <- jags.model(textConnection(m3_string), data=data_list, n.chains=3, n.adapt=10000)
# Burning some samples on the altar of the MCMC god
update(m3, 10000)
# Generating MCMC samples
s3 <- coda.samples(m3, variable.names=c("home_baseline", "away_baseline","skill", "season_sigma", "group_sigma", "group_skill"), n.iter=40000, thin=8)
# Merging the three MCMC chains into one matrix
ms3 <- as.matrix(s3)

plot(s3[, "group_skill"])
plot(s3[, "group_sigma"])
plot(s3[, "season_sigma"])

plotPost(exp(ms3[,col_name("home_baseline",5)]) - exp(ms3[,col_name("away_baseline",5)]), compVal = 0, xlab = "Home advantage in number of goals")

team_skill <- ms3[, str_detect(string=colnames(ms3), "skill\\[5,")]
team_skill <- (team_skill - rowMeans(team_skill)) + ms3[, "home_baseline[5]"]
team_skill <- exp(team_skill)
colnames(team_skill) <- teams
team_skill <- team_skill[,order(colMeans(team_skill), decreasing=T)]
old_par <- par(mar=c(2,0.7,0.7,0.7), xaxs='i')
caterplot(team_skill, labels.loc="above", val.lim=c(0.7, 3.8))
par(old_par)

plotPost(team_skill[, "Ajax"] - team_skill[, "PSV Eindhoven"], compVal = 0, xlab = "← PSV     vs     Ajax →")

n <- nrow(ms3)
m3_pred <- sapply(1:nrow(eredivisie), function(i) {
  home_team <- which(teams == eredivisie$HomeTeam[i])
  away_team <- which(teams == eredivisie$AwayTeam[i])
  season <- which(seasons == eredivisie$Season[i])
  home_skill <- ms3[, col_name("skill", season, home_team)] 
  away_skill <- ms3[, col_name("skill", season, away_team)]
  home_baseline <- ms3[, col_name("home_baseline", season)]
  away_baseline <- ms3[, col_name("away_baseline", season)]
  
  home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
  away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))
  home_goals_table <- table(home_goals)
  away_goals_table <- table(away_goals)
  match_results <- sign(home_goals - away_goals)
  match_results_table <- table(match_results)
  
  mode_home_goal <- as.numeric(names(home_goals_table)[ which.max(home_goals_table)])
  mode_away_goal <- as.numeric(names(away_goals_table)[ which.max(away_goals_table)])
  match_result <-  as.numeric(names(match_results_table)[which.max(match_results_table)])
  rand_i <- sample(seq_along(home_goals), 1)
  
  c(mode_home_goal = mode_home_goal, mode_away_goal = mode_away_goal, match_result = match_result,
    mean_home_goal = mean(home_goals), mean_away_goal = mean(away_goals),
    rand_home_goal = home_goals[rand_i], rand_away_goal = away_goals[rand_i],
    rand_match_result = match_results[rand_i])
})
m3_pred <- t(m3_pred)

hist(eredivisie$HomeGoals, breaks= (-1:10) + 0.5, xlim=c(-0.5, 10), main = "Distribution of the number of goals\nscored by a home team in a match",
    xlab = "")

hist(m3_pred[ , "mode_home_goal"], breaks= (-1:10) + 0.5, xlim=c(-0.5, 10),
    main = "Distribution of predicted most \nprobable score by a home team in\na match",
    xlab = "")

hist(m3_pred[ , "mean_home_goal"], breaks= (-1:10) + 0.5, xlim=c(-0.5, 10),
    main = "Distribution of predicted mean \n score by a home team in a match",
    xlab = "")

hist(m3_pred[ , "rand_home_goal"], breaks= (-1:10) + 0.5, xlim=c(-0.5, 10),
    main = "Distribution of randomly drawn \n score by a home team in a match",
    xlab = "")

mean(eredivisie$HomeGoals == m3_pred[ , "mode_home_goal"], na.rm=T)
mean((eredivisie$HomeGoals - m3_pred[ , "mean_home_goal"])^2, na.rm=T)

hist(eredivisie$MatchResult, breaks= (-2:1) + 0.5, xlim=c(-1.5, 1.5), ylim=c(0, 1000), main = "Actual match results",
    xlab = "")

hist(m3_pred[ , "match_result"], breaks= (-2:1) + 0.5, xlim=c(-1.5, 1.5), ylim=c(0, 1000), main = "Predicted match results",
    xlab = "")

hist(m3_pred[ , "rand_match_result"], breaks= (-2:1) + 0.5, xlim=c(-1.5, 1.5), ylim=c(0, 1000), main = "Randomized match results",
    xlab = "")

mean(eredivisie$MatchResult == m3_pred[ , "match_result"], na.rm=T)

eredivisie_forecast <- eredivisie[is.na(eredivisie$HomeGoals), c("Season", "Week", "HomeTeam", "AwayTeam")]
m3_forecast <- m3_pred[is.na(eredivisie$HomeGoals),] 
eredivisie_forecast$mean_home_goals <- round(m3_forecast[,"mean_home_goal"], 1) 
eredivisie_forecast$mean_away_goals <- round(m3_forecast[,"mean_away_goal"], 1)
eredivisie_forecast$mode_home_goals <- m3_forecast[,"mode_home_goal"] 
eredivisie_forecast$mode_away_goals <- m3_forecast[,"mode_away_goal"]
eredivisie_forecast$predicted_winner <- ifelse(m3_forecast[ , "match_result"] == 1, eredivisie_forecast$HomeTeam, 
                                           ifelse(m3_forecast[ , "match_result"] == -1, eredivisie_forecast$AwayTeam, "Draw"))

rownames(eredivisie_forecast) <- NULL
print(xtable(eredivisie_forecast, align="cccccccccc"), type="html")

eredivisie_sim <- eredivisie[is.na(eredivisie$HomeGoals), c("Season", "Week", "HomeTeam", "AwayTeam")]
eredivisie_sim$home_goals <- m3_forecast[,"rand_home_goal"] 
eredivisie_sim$away_goals <- m3_forecast[,"rand_away_goal"]
eredivisie_sim$winner <- ifelse(m3_forecast[ , "rand_match_result"] == 1, eredivisie_forecast$HomeTeam, 
                            ifelse(m3_forecast[ , "rand_match_result"] == -1, eredivisie_forecast$AwayTeam, "Draw"))

rownames(eredivisie_sim) <- NULL
print(xtable(eredivisie_sim, align="cccccccc"), type="html")

plot_goals <- function(home_goals, away_goals) { 
  old_par <- par(mar = c(0, 0, 0, 0))
  par(mfrow = c(2, 2), mar=rep(2.2, 4))
  
	n_matches <- length(home_goals) 
	goal_diff <- home_goals - away_goals 
	match_result <- ifelse(goal_diff < 0, "away_win", ifelse(goal_diff > 0, "home_win", "equal")) 
	hist(home_goals, xlim = c(-0.5, 10), breaks = (0:100) - 0.5)
	hist(away_goals, xlim = c(-0.5, 10), breaks = (0:100) - 0.5) 
	hist(goal_diff, xlim = c(-6, 6), breaks = (-100:100) - 0.5)
	barplot(table(match_result)/n_matches, ylim = c(0, 1))
	par(old_par)
}

n <- nrow(ms3)
home_team <- which(teams == "AZ Alkmaar")
away_team <- which(teams == "PSV Eindhoven")
season <- which(seasons == "2018/2019")
home_skill <- ms3[, col_name("skill", season, home_team)] 
away_skill <- ms3[, col_name("skill", season, away_team)]
home_baseline <- ms3[, col_name("home_baseline", season)]
away_baseline <- ms3[, col_name("away_baseline", season)]

home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))

old_par <- par(mfrow = c(2, 2), mar=rep(2.2, 4))
plot_goals(home_goals, away_goals)
par(old_par)

##    AZ     Draw     PSV 
##   3.90    4.00     1.78

1 / c(AZ =  mean(home_goals > away_goals), Draw = mean(home_goals == away_goals), PSV = mean(home_goals < away_goals))

goals_payout <- laply(0:6, function(home_goal) {
  laply(0:6, function(away_goal) {
    1 / mean(home_goals == home_goal & away_goals  == away_goal)
  })
})

colnames(goals_payout) <- paste("PSV Eindhoven", 0:6, sep=" - ")
rownames(goals_payout) <- paste("AZ Alkmaar", 0:6, sep=" - ")
goals_payout <- round(goals_payout, 1)
print(xtable(goals_payout, align="cccccccc"), type="html")
