setwd("~/Data science/Tennis_betmaker/Data.csv")
seed<- 1245
set.seed(seed)

library(plyr)
library(xgboost)

results<-read.csv('Data.csv',stringsAsFactor=F)
str(results)

#Players ranks are character. lets convert to numeric, & set NAs to 1000
results$WRank=as.numeric(results$WRank)
results$LRank=as.numeric(results$LRank)
results$WRank[is.na(results$WRank)]<-1000
results$LRank[is.na(results$LRank)]<-1000

#format the date, extract the Year
results$Date=as.Date(results$Date,format='%d/%m/%Y')
results$Year=as.numeric(format(results$Date,"%Y"))

#some cleanup on tournament names. 
regex=regexpr("Dubai ",results$Tournament)
results$Tournament[regex!=-1]="Dubai Open"
regex=regexpr("Valencia Open",results$Tournament)
results$Tournament[regex!=-1]="Valencia Open"
regex=regexpr("Buenos Aires",results$Tournament)
results$Tournament[regex!=-1]="ATP Buenos Aires"
#still lots of inconsistencies in tournament names. Will try to fix this in the future.

#cleanup the Series names, convert to integer values equivalent to the ATP points of the series
results$Series[results$Series %in% c("ATP250","International","International Series")] <-250
results$Series[results$Series %in% c("ATP500","International Gold")] <-500
results$Series[results$Series %in% c("Masters","Masters 1000")] <-1000
results$Series[results$Series %in% c("Masters Cup")] <-1500
results$Series[results$Series %in% c("Grand Slam")] <-2000
results$Series=as.numeric(results$Series)

#convert rounds to integers
results$Round[results$Round %in% c("1st Round","0th Round","Round Robin")] <-1
results$Round[results$Round== "2nd Round"] <-2
results$Round[results$Round== "3rd Round"] <-3
results$Round[results$Round== "4th Round"] <-4
results$Round[results$Round== "Quarterfinals"] <-5
results$Round[results$Round== "Semifinals"] <-6
results$Round[results$Round== "The Final"] <-7
results$Round=as.numeric(results$Round)

#retain only completed matches, get rid of the injury withdrawals & walkovers.
#drop the comments column
results=results[results$Comment %in% c("Completed","Compleed","Full Time","Sched","NSY"),]
results$Comment=NULL

#for matches with Best.of value<3, reset it to 3
results$Best.of[results$Best.of<3] <- 3

#add Tourney_index column, for chronological sorting. 
#1st tournament of 2000 has index 0, last tournament of 2016 has index n
results$Tourney_index=1
k=1
for (i in 2:nrow(results))
{
  if (results$Tournament[i]!=results$Tournament[i-1])
  {
    k=k+1
  }
  results$Tourney_index[i]=k
}

#functions for handling missing values (will be used later)
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

is.infinite.data.frame <- function(x)
  do.call(cbind, lapply(x, is.infinite))

results[is.na(results)]<-0

#sets percentage for the players in the match
results$Wsets_r=results$Wsets/(results$Wsets+results$Lsets)
results$Lsets_r=1-results$Wsets_r

#average game differential per set for the players in the match
results$Wgame_diff=(results$W1+results$W2+results$W3+results$W4+results$W5
                    -results$L1-results$L2-results$L3-results$L4-results$L5)
results$Wgame_diff=results$Wgame_diff/(results$Wsets+results$Lsets)
results$Lgame_diff=results$Wgame_diff*(-1)

results[is.na(results)]<-0
results[is.nan(results)]<-0
results[is.infinite(results)]<-0

#count of wins & losses for all players in a tournament
wins_by_t=count(results,vars=c('Winner','Tourney_index'))
losses_by_t=count(results,vars=c('Loser','Tourney_index'))

#average number of sets won by a player in a tournament
Wplayer_sets=aggregate(results$Wsets_r~results$Winner+results$Tourney_index,FUN=mean)
Lplayer_sets=aggregate(results$Lsets_r~results$Loser+results$Tourney_index,FUN=mean)

#average game differential in a set for the players in a tournament
Wplayer_games=aggregate(results$Wgame_diff~results$Winner+results$Tourney_index,FUN=mean)
Lplayer_games=aggregate(results$Lgame_diff~results$Loser+results$Tourney_index,FUN=mean)

names(Wplayer_sets)<-c('Player','Tourney_index','Wsets_r')
names(Lplayer_sets)<-c('Player','Tourney_index','Lsets_r')
#same player could've won a few matches & also lost a match in the tournament. 
#so merge both into 1 dataframe
Player_sets=merge(Wplayer_sets,Lplayer_sets,all=T)

names(Wplayer_games)<-c('Player','Tourney_index','Wgames')
names(Lplayer_games)<-c('Player','Tourney_index','Lgames')
Player_games=merge(Wplayer_games,Lplayer_games,all=T)

names(wins_by_t)=c('Player','Tourney_index','Wins')
names(losses_by_t)=c('Player','Tourney_index','Losses')
winloss_by_t=merge(wins_by_t,losses_by_t,all=T)

#merge the winloss, sets percentage & game differential dfs into 1 dataframe
Player_stats=merge(Player_sets,winloss_by_t,all=T)
Player_stats=merge(Player_stats,Player_games,all=T)
Player_stats[is.na(Player_stats)]<-0

#average sets percentage for a player across the tournament= weighted average of set percentages for matches where he was a winner
#& matches where he was the loser.
#similarly for game differential
Player_stats$sets_r=((Player_stats$Wsets_r*Player_stats$Wins)+
                       (Player_stats$Lsets_r*Player_stats$Losses))/(Player_stats$Wins+Player_stats$Losses)
Player_stats$games_d=((Player_stats$Wgames*Player_stats$Wins)+
                        (Player_stats$Lgames*Player_stats$Losses))/(Player_stats$Wins+Player_stats$Losses)

Player_stats=Player_stats[order(Player_stats$Tourney_index),]

#keep only relevant columns
Player_stats=Player_stats[,c('Player','Tourney_index','Wins','Losses','sets_r','games_d')]
tail(Player_stats)
#cleanup environment
rm(list=setdiff(ls(),c('results','Player_stats','is.nan.data.frame','is.infinite.data.frame')))

earliest=min(results$Tourney_index[results$Year==2001])
latest=max(results$Tourney_index)
Player_YOY_stats=data.frame(Player=character(),Wins=numeric(),Losses=numeric(),Win_Perc=numeric(),sets_r=numeric(),games_d=numeric(),Tourney_index=numeric())

prev_start=0 #will be used if a new tournament is added in the calendar, or tournament name changed

for (idx in earliest:latest)
{
  curr_data=results[results$Tourney_index==idx,] #current tournament results
  curr_tournament=curr_data$Tournament[1]        #current tournament name, year
  curr_year=curr_data$Year[1]
  if(idx %% 100==0)
    print (idx)
  
  #start & end indexes for Year-on-year(rolling window) of current tournament 
  YOY_start_idx=results$Tourney_index[(results$Tournament==curr_tournament & results$Year==(curr_year-1))][1]
  if (is.na(YOY_start_idx)) #tournament name not found.
    YOY_start_idx=prev_start+1 #pick start index from previous iteration of the loop, increment 1
  YOY_end_idx=idx-1         #tournament just before the current one
  
  players=union(curr_data$Winner,curr_data$Loser)
  curr_df=data.frame(Player=character(),Wins=numeric(),Losses=numeric(),Win_Perc=numeric(),sets_r=numeric(),games_d=numeric())
  for (Player in players)
  {
    #stats of Player in all tournaments in rolling window  
    stats_p=Player_stats[Player_stats$Player==Player & Player_stats$Tourney_index %in% c(YOY_start_idx:YOY_end_idx),]
    
    Wins=sum(stats_p$Wins)
    Losses=sum(stats_p$Losses)
    if (Wins==0 & Losses==0) #wildcard player. no stats data exists. set everything to 0
    {
      sets_r=0
      games_d=0
      Win_Perc=0
    }
    else
    {
      #compute mean sets percentage, games differential & win percentage 
      sets_r=mean(stats_p$sets_r)
      games_d=mean(stats_p$games_d)
      Win_Perc=Wins/(Wins+Losses)
    }
    
    curr_row=data.frame(cbind(Player,Wins,Losses,Win_Perc,sets_r,games_d))
    curr_df=rbind(curr_df,curr_row)
    
  }
  curr_df[is.na(curr_df)]<-0
  curr_df$Tourney_index=idx  #add tourney index column after data has been calculated for each Player
  
  prev_start=YOY_start_idx
  Player_YOY_stats=rbind(Player_YOY_stats,curr_df) #bind & move to next tournament
  
  
}

Player_YOY_stats[is.na(Player_YOY_stats)]<-0

tail(Player_YOY_stats)
#save the files to avoid computing this again
write.csv(Player_YOY_stats,"Player_YOY_stats.csv",row.names=F)
write.csv(Player_stats,"Player_stats_by_tourney.csv",row.names=F)

rm(list=setdiff(ls(),c('results','Player_stats','is.nan.data.frame','is.infinite.data.frame','Player_YOY_stats')))

wimb_2015=results$Tourney_index[results$Tournament=="Wimbledon" & results$Year==2015][1]
latest=max(results$Tourney_index)

#filter only results & YOY_stats of relevant tournaments
results_for_modelling=results[results$Tourney_index %in% wimb_2015:latest,]
modelling_df=Player_YOY_stats[Player_YOY_stats$Tourney_index %in% wimb_2015:latest,]
#make a copy of modelling_df. 1 will be used for merging on match Winner, the other on Loser
modelling_df2=modelling_df

names(modelling_df)<-c("Winner","Wins_W","Loss_W","Win_Perc_W","sets_r_W","games_d_W","Tourney_index")
names(modelling_df2)<-c("Loser","Wins_L","Loss_L","Win_Perc_L","sets_r_L","games_d_L","Tourney_index")

results_for_modelling=merge(results_for_modelling,modelling_df,all.x=T)
results_for_modelling=merge(results_for_modelling,modelling_df2,all.x=T)

results_for_modelling=results_for_modelling[,c(1:14,25,26,35,36,55:69)]

set.seed(seed)
A=sample(nrow(results_for_modelling),nrow(results_for_modelling)/2)
#subsetA has Pler1 as Winner, Player2 as Loser
subsetA=results_for_modelling[A,]
#subset2 has Player2 as Winner, Player1 as Loser
subsetB=results_for_modelling[-A,]

#rename columns accordingly for subsetA & subsetB
names(subsetA)[2:3]<- c("Player2","Player1")
names(subsetA)[13:16]<-c("P1_Rank","P2_Rank","P1sets","P2sets")
names(subsetA)[17:18]<-c("B365P1","B365P2")
names(subsetA)[20:33]<-c("P1_sets_r","P2_sets_r","P1_game_diff","P2_game_diff","P1_Wins","P1_Loss","P1_Win_Perc",
                         "P1_sets_avg","P1_games_avg","P2_Wins","P2_Loss","P2_Win_Perc","P2_sets_avg","P2_games_avg")

names(subsetB)[2:3]<- c("Player1","Player2")
names(subsetB)[13:16]<-c("P2_Rank","P1_Rank","P2sets","P1sets")
names(subsetB)[17:18]<-c("B365P2","B365P1")
names(subsetB)[20:33]<-c("P2_sets_r","P1_sets_r","P2_game_diff","P1_game_diff","P2_Wins","P2_Loss","P2_Win_Perc",
                         "P2_sets_avg","P2_games_avg","P1_Wins","P1_Loss","P1_Win_Perc","P1_sets_avg","P1_games_avg")

subsetB<-subsetB[names(subsetA)]
#merge back
results_for_modelling=rbind(subsetA,subsetB)
#again, if there's a cleaner & more elegant way to do this, plz suggest. :-)

#reorder based on Tourney index, round & date, so that it is (close enough) to the original ordering.
results_for_modelling=results_for_modelling[order(results_for_modelling$Tourney_index,
                                                  results_for_modelling$Round,
                                                  results_for_modelling$Date),]
tail(results_for_modelling)

rm(list=setdiff(ls(),c('results_for_modelling','wimb_2015','latest')))
#store indexes of major tournaments. I'll use these for splitting into train, val & test sets subsequently.
uso_2015=results_for_modelling$Tourney_index[results_for_modelling$Tournament=="US Open" & results_for_modelling$Year==2015][1]
uso_2016=results_for_modelling$Tourney_index[results_for_modelling$Tournament=="US Open" & results_for_modelling$Year==2016][1]
wimb_2016=results_for_modelling$Tourney_index[results_for_modelling$Tournament=="Wimbledon" & results_for_modelling$Year==2016][1]

#retain only useful columns
modelling_useful_cols=results_for_modelling[,c(1:6,8:14,20,24:33)]

#label-encode Player names
players=union(modelling_useful_cols$Player1,modelling_useful_cols$Player2)
levels <- unique(players)
modelling_useful_cols$Player1 <- as.numeric(factor(modelling_useful_cols$Player1, levels=levels))
modelling_useful_cols$Player2 <- as.numeric(factor(modelling_useful_cols$Player2, levels=levels))

#handle character & factor columns. 
#for some reason, our percentage features have got saved as factor, so need to take care.
for (f in names(modelling_useful_cols)) {
  if (class(modelling_useful_cols[[f]])=="character") {
    levels <- unique(c(modelling_useful_cols[[f]]))
    modelling_useful_cols[[f]] <- as.numeric(factor(modelling_useful_cols[[f]], levels=levels))
  }
  if (class(modelling_useful_cols[[f]])=="factor") {
    modelling_useful_cols[[f]] <- as.numeric(as.character(modelling_useful_cols[[f]]))
  }
}
str(modelling_useful_cols)

test=modelling_useful_cols[modelling_useful_cols$Tourney_index %in% uso_2016:latest,]
train=modelling_useful_cols[modelling_useful_cols$Tourney_index %in% uso_2015:(uso_2016-1),]
tr=modelling_useful_cols[modelling_useful_cols$Tourney_index %in% wimb_2015:(wimb_2016-1),]
val=modelling_useful_cols[modelling_useful_cols$Tourney_index %in% wimb_2016:(uso_2016-1),]

Y_tr=tr$P1_sets_r
Y_val=val$P1_sets_r
Y_train=train$P1_sets_r
Y_test=test$P1_sets_r

train$Tourney_index=NULL
train$P1_sets_r=NULL
test$Tourney_index=NULL
test$P1_sets_r=NULL
tr$Tourney_index=NULL
tr$P1_sets_r=NULL
val$Tourney_index=NULL
val$P1_sets_r=NULL

dtrain  <- xgb.DMatrix(as.matrix(train), label = Y_train)
dtr  <- xgb.DMatrix(as.matrix(tr), label = Y_tr)
dval  <- xgb.DMatrix(as.matrix(val), label = Y_val)
dtest  <- xgb.DMatrix(as.matrix(test))

param <- list(objective = "reg:linear", 
              eval_metric = "rmse",
              booster = "gbtree", 
              eta = 0.02,
              subsample = 0.8,
              colsample_bytree = 0.8,
              max_depth = 1
)
#strangely enough, I got best results for max_depth=1
set.seed(619)
m1 <- xgb.train(data = dtr
                , param
                , nrounds = 500
                , watchlist = list(model = dtr, valid = dval)
                , early_stopping_rounds = 10
                ,print_every_n = 25
)

#own RMSE function
rmse<-function(actual,pred)
{
  sqrt( sum( (actual - pred)^2 , na.rm = TRUE ) / length(actual) )
}
set.seed(619)
m2<-xgb.train(data=dtrain,
              param,
              nround=366,
              watchlist=list(model=dtrain),
              print_every_n=25)

p2=predict(m2,dtest)
p2[p2<0]<-0 #postprocessing out of range predictions
p2[p2>1]<-1
rmse(Y_test,p2)

#sanity check
print("rmse of 0.5 benchmark:")
rmse(Y_test,0.5)

df=data.frame(round(Y_test),round(p2))
table(df$round.Y_test.,df$round.p2.)

feat_imp=xgb.importance(feature_names = names(train),model = m1)
feat_imp

bet_data=results_for_modelling[,c("Tourney_index","Player1","Player2","P1_Rank","P2_Rank","B365P1","B365P2")]
players=union(bet_data$Player1,bet_data$Player2)
levels <- unique(players)
bet_data$Player1 <- as.numeric(factor(bet_data$Player1, levels=levels))
bet_data$Player2 <- as.numeric(factor(bet_data$Player2, levels=levels))


test2=bet_data[bet_data$Tourney_index %in% uso_2016:latest,2:7]
train2=bet_data[bet_data$Tourney_index %in% uso_2015:(uso_2016-1),2:7]
tr2=bet_data[bet_data$Tourney_index %in% wimb_2015:(wimb_2016-1),2:7]
val2=bet_data[bet_data$Tourney_index %in% wimb_2016:(uso_2016-1),2:7]

dtrain2  <- xgb.DMatrix(as.matrix(train2), label = Y_train)
dtr2  <- xgb.DMatrix(as.matrix(tr2), label = Y_tr)
dval2  <- xgb.DMatrix(as.matrix(val2), label = Y_val)
dtest2  <- xgb.DMatrix(as.matrix(test2))

param <- list(objective = "reg:linear", 
              eval_metric = "rmse",
              booster = "gbtree", 
              eta = 0.02,
              subsample = 0.8,
              colsample_bytree = 0.8,
              max_depth = 4
)
set.seed(619)
b1 <- xgb.train(data = dtr2
                , param
                , nrounds = 500
                , watchlist = list( model = dtr2,valid = dval2)
                , early_stopping_rounds = 10
                ,print_every_n=25
)

set.seed(619)
b2<-xgb.train(data=dtrain2,
              param,
              nround=72,
              watchlist=list(model=dtrain2),
              print_every_n=25)

p_b365=predict(b2,dtest2)
p_b365[p_b365<0]<-0
p_b365[p_b365>1]<-1
rmse(Y_test,p_b365)
df=data.frame(round(Y_test),round(p_b365))
table(df$round.Y_test.,df$round.p_b365.)

