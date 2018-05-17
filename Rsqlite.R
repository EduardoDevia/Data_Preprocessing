#install.packages("RSQLite") #perhaps needed

library(DBI)
library(RSQLite)
library(proto)
library(gsubfn)
library(RSQLite)
library(sqldf)
library(tcltk)
library(sqldf)
setwd("~/Downloads/OneDrive-2018-02-07")
#=================== connect to the sqlite file
sqlite    <- dbDriver("SQLite")
EuropeSoccer <- dbConnect(sqlite,"database.sqlite")

#=================== Load Files
dbListTables(EuropeSoccer)
Country<-dbReadTable(EuropeSoccer,"Country")
Match<-dbReadTable(EuropeSoccer,"Match")
Player<-dbReadTable(EuropeSoccer,"Player")
Player_Attributes<-dbReadTable(EuropeSoccer,"Player_Attributes")
Team<-dbReadTable(EuropeSoccer,"Team")
Team_Attributes<-dbReadTable(EuropeSoccer,"Team_Attributes")
colnames(Country)[which(names(Country) == "id")] <- "country_id"



#==================== Delete Column no needed
Match<-subset(Match,select= c(-1,-(98:100),-(12:55),-(78:85)))
Match<-na.omit(Match)
Team<-subset(Team, select = c(-1,-5))
Player<-subset(Player, select = c(-1))
Team_Attributes<-subset(Team_Attributes,select=c(-7))
Team_Attributes<-na.omit(Team_Attributes)

#==================== Save files to check them in Excel
#write.table(Country, file = "S:/chedevia/Country.csv", col.names = NA,  sep = ",",  qmethod = "double") 
#write.table(Match, file = "S:/chedevia/Match.csv",  col.names = NA, sep = ",",  qmethod = "double") 
#write.table(Player, file = "S:/chedevia/Player.csv",  col.names = NA, sep = ",",  qmethod = "double") 
#write.table(Player_Attributes, file = "S:/chedevia/Player_Attributes.csv", col.names = NA,  sep = ",",  qmethod = "double") 
#write.table(Team, file = "S:/chedevia/Team.csv",  col.names = NA, sep = ",",  qmethod = "double") 
#write.table(Team_Attributes, file = "S:/chedevia/Team_Attributes.csv",  col.names = NA, sep = ",",  qmethod = "double") 

#==================== Join basic tables

Team_Joined<- merge(Team_Attributes,Team,by=c("team_fifa_api_id","team_api_id"))
Player_Joined<-merge(Player_Attributes,Player,by=c("player_fifa_api_id","player_api_id"))
Match_Joined<-merge(Match,Country,"country_id"="id")
rm(Country)
rm(Player)
rm(Player_Attributes)
rm(Team)
rm(Team_Attributes)
rm(Match)


#======== Convert to date from char
str(Team_Joined)
Team_Joined$date<-as.Date(Team_Joined$date)
Player_Joined$date<-as.Date(Player_Joined$date)
Match_Joined$date<-as.Date(Match_Joined$date)
Player_Joined$birthday<-as.Date(Player_Joined$birthday)

#==================== Sort data by id and date
Player_Joined<-Player_Joined[order(Player_Joined$player_api_id,Player_Joined$date),]
Team_Joined<-Team_Joined[order(Team_Joined$team_api_id,Team_Joined$date),]
rownames(Player_Joined) <- NULL
rownames(Team_Joined) <- NULL
#==================== Add finish date
#==================== For Team
Team_Joined$LastDate<-'2017-01-01'
Team_Joined$LastDate<-as.Date(Team_Joined$LastDate)
for (i in 1:(nrow(Team_Joined)-1)) { ifelse(Team_Joined[i,1]!=Team_Joined[i+1,1],Team_Joined[i,26]<-'2017-01-01',Team_Joined[i,26]<-Team_Joined[i+1,4]) }
#==================== For Player
Player_Joined$LastDate<-'2017-01-01'
Player_Joined$LastDate<-as.Date(Player_Joined$LastDate)
for (i in 1:(nrow(Player_Joined)-1)) { ifelse(Player_Joined[i,1]!=Player_Joined[i+1,1],Player_Joined[i,47]<-'2017-01-01',Player_Joined[i,47]<-Player_Joined[i+1,4])  }

#==================== Match
#======== Remove Extra columns
Match_Final<-subset(Match_Joined, select = c(-(36:59),-(1:2),-6))

firstcol<-NCOL(Match_Final)+1

#======== Join Team Home with Match for Team attributes
Match_Final<-sqldf("Select * from Match_Final
                   left join Team_Joined on Team_Joined.team_api_id = Match_Final.home_team_api_id 
                   and Match_Final.date > Team_Joined.date
                   and Match_Final.date <= Team_Joined.lastdate")
#Renaming Home Columns
lastcol<-NCOL(Match_Final)
colnames(Match_Final)[firstcol:lastcol]<-paste("Home", colnames(Match_Final)[firstcol:lastcol],sep= " ")
Match_Final<-subset(Match_Final,select = c(-(firstcol:(firstcol+3)),-(lastcol)))


#======== Join Team Away with Match for Team attributes
firstcol<-NCOL(Match_Final)+1
Match_Final<-sqldf("Select * from Match_Final
                   left join Team_Joined on Team_Joined.team_api_id = Match_Final.away_team_api_id 
                   and Match_Final.date > Team_Joined.date
                   and Match_Final.date <= Team_Joined.lastdate")
#======== Renaming Away Columns
lastcol<-NCOL(Match_Final)
colnames(Match_Final)[firstcol:lastcol]<-paste("Away", colnames(Match_Final)[firstcol:lastcol],sep= " ")
Match_Final<-subset(Match_Final,select = c(-(firstcol:(firstcol+3)),-(lastcol)))


#======== Categorized overall player

for ( i in 5:6){
  for (j in 1:183766) {Player_Joined[j,i]<-ifelse(Player_Joined[j,i]<40,"Bad",
                              ifelse(Player_Joined[j,i]<60,"More_less",
                                     ifelse(Player_Joined[j,i]<75,"Regular",
                                            ifelse(Player_Joined[j,i]<85,"Good",
                                                   ifelse(Player_Joined[j,i]<90,"Very_Good","Exceptional")
                                            )
                                     )
                              )
  )
    }
}
#====== Categorized rest of the attributes

for ( i in 10:42){
  for (j in 1:183766) {Player_Joined[j,i]<-ifelse(Player_Joined[j,i]<40,"Bad",
                                                  ifelse(Player_Joined[j,i]<60,"More_less",
                                                         ifelse(Player_Joined[j,i]<75,"Regular",
                                                                ifelse(Player_Joined[j,i]<85,"Good",
                                                                       ifelse(Player_Joined[j,i]<90,"Very_Good","Exceptional")
                                                                )
                                                         )
                                                  )
  )
  }
}

#======= BMI Calculation

Player_Joined$BMI<-Player_Joined$height/sqrt(Player_Joined$weight*(0.453592))
Player_Joined$BMI<-ifelse(Player_Joined$BMI<25,"Normal",ifelse(Player_Joined$BMI<29,"Over Weight","Obese"))
#======= High Calc
lastrow<-NROW(Player_Joined)
for (i in 1:lastrow) {Player_Joined[i,45]<-ifelse(Player_Joined[i,45]<155,"Short",
                                                  ifelse(Player_Joined[i,45]<165,"Av_Short",
                                                         ifelse(Player_Joined[i,45]<175,"Avg","Tall")))}
#===== Age Calculation
Player_Joined$Age<-as.numeric(format(Player_Joined$LastDate, format="%Y")) -as.numeric(format(Player_Joined$birthday, format="%Y"))

Player_Joined<-subset(Player_Joined,select = c(-46,-44,-43))

Match_Final<-subset(Match_Final,select = c(-4,-5))
#================================ Setting draws and no draws

Match_Final$result<-ifelse(Match_Final$home_team_goal==Match_Final$away_team_goal,"Draw","No Draw")


#============Changing values to categorical ======================

#summary checking the type of line / factor/ int/ etc
library(dplyr)
glimpse(Match_Final2)
#Change some fields to categorical
Match_Final$result<-as.factor(Match_Final$result)
Match_Final$name<-as.factor(Match_Final$name)
Match_Final$`Home buildUpPlaySpeedClass`<-as.factor(Match_Final$`Home buildUpPlaySpeedClass`)
Match_Final$`Home buildUpPlayDribblingClass`<-as.factor(Match_Final$`Home buildUpPlayDribblingClass`)
Match_Final$`Home buildUpPlayPassingClass`<-as.factor(Match_Final$`Home buildUpPlayPassingClass`)
Match_Final$`Home buildUpPlayPositioningClass`<-as.factor(Match_Final$`Home buildUpPlayPositioningClass`)
Match_Final$`Home chanceCreationPassingClass`<-as.factor(Match_Final$`Home chanceCreationPassingClass`)
Match_Final$`Home chanceCreationCrossingClass`<-as.factor(Match_Final$`Home chanceCreationCrossingClass`)
Match_Final$`Home chanceCreationShootingClass`<-as.factor(Match_Final$`Home chanceCreationShootingClass`)
Match_Final$`Home chanceCreationPositioningClass`<-as.factor(Match_Final$`Home chanceCreationPositioningClass`)
Match_Final$`Home defencePressureClass`<-as.factor(Match_Final$`Home defencePressureClass`)
Match_Final$`Home defenceAggressionClass`<-as.factor(Match_Final$`Home defenceAggressionClass`)
Match_Final$`Home defenceTeamWidthClass`<-as.factor(Match_Final$`Home defenceTeamWidthClass`)
Match_Final$`Home defenceDefenderLineClass`<-as.factor(Match_Final$`Home defenceDefenderLineClass`)
Match_Final$`Home team_long_name`<-as.factor(Match_Final$`Home team_long_name`)
Match_Final$`Away buildUpPlaySpeedClass`<-as.factor(Match_Final$`Away buildUpPlaySpeedClass`)
Match_Final$`Away buildUpPlayDribblingClass`<-as.factor(Match_Final$`Away buildUpPlayDribblingClass`)
Match_Final$`Away buildUpPlayPassingClass`<-as.factor(Match_Final$`Away buildUpPlayPassingClass`)
Match_Final$`Away buildUpPlayPositioningClass`<-as.factor(Match_Final$`Away buildUpPlayPositioningClass`)
Match_Final$`Away chanceCreationPassingClass`<-as.factor(Match_Final$`Away chanceCreationPassingClass`)
Match_Final$`Away chanceCreationCrossingClass`<-as.factor(Match_Final$`Away chanceCreationCrossingClass`)
Match_Final$`Away chanceCreationShootingClass`<-as.factor(Match_Final$`Away chanceCreationShootingClass`)
Match_Final$`Away chanceCreationPositioningClass`<-as.factor(Match_Final$`Away chanceCreationPositioningClass`)
Match_Final$`Away defencePressureClass`<-as.factor(Match_Final$`Away defencePressureClass`)
Match_Final$`Away defenceAggressionClass`<-as.factor(Match_Final$`Away defenceAggressionClass`)
Match_Final$`Away defenceTeamWidthClass`<-as.factor(Match_Final$`Away defenceTeamWidthClass`)
Match_Final$`Away defenceDefenderLineClass`<-as.factor(Match_Final$`Away defenceDefenderLineClass`)
Match_Final$`Away team_long_name`<-as.factor(Match_Final$`Away team_long_name`)


Match_Final2$result<-as.factor(Match_Final2$result)
Match_Final2$name<-as.factor(Match_Final2$name)

Match_Final2$`Home buildUpPlaySpeedClass`<-as.factor(Match_Final2$`Home buildUpPlaySpeedClass`)
Match_Final2$`Home buildUpPlayDribblingClass`<-as.factor(Match_Final2$`Home buildUpPlayDribblingClass`)
Match_Final2$`Home buildUpPlayPassingClass`<-as.factor(Match_Final2$`Home buildUpPlayPassingClass`)
Match_Final2$`Home buildUpPlayPositioningClass`<-as.factor(Match_Final2$`Home buildUpPlayPositioningClass`)
Match_Final2$`Home chanceCreationPassingClass`<-as.factor(Match_Final2$`Home chanceCreationPassingClass`)
Match_Final2$`Home chanceCreationCrossingClass`<-as.factor(Match_Final2$`Home chanceCreationCrossingClass`)
Match_Final2$`Home chanceCreationShootingClass`<-as.factor(Match_Final2$`Home chanceCreationShootingClass`)
Match_Final2$`Home chanceCreationPositioningClass`<-as.factor(Match_Final2$`Home chanceCreationPositioningClass`)
Match_Final2$`Home defencePressureClass`<-as.factor(Match_Final2$`Home defencePressureClass`)
Match_Final2$`Home defenceAggressionClass`<-as.factor(Match_Final2$`Home defenceAggressionClass`)
Match_Final2$`Home defenceTeamWidthClass`<-as.factor(Match_Final2$`Home defenceTeamWidthClass`)
Match_Final2$`Home defenceDefenderLineClass`<-as.factor(Match_Final2$`Home defenceDefenderLineClass`)
Match_Final2$`Home team_long_name`<-as.factor(Match_Final2$`Home team_long_name`)
Match_Final2$`Away buildUpPlaySpeedClass`<-as.factor(Match_Final2$`Away buildUpPlaySpeedClass`)
Match_Final2$`Away buildUpPlayDribblingClass`<-as.factor(Match_Final2$`Away buildUpPlayDribblingClass`)
Match_Final2$`Away buildUpPlayPassingClass`<-as.factor(Match_Final2$`Away buildUpPlayPassingClass`)
Match_Final2$`Away buildUpPlayPositioningClass`<-as.factor(Match_Final2$`Away buildUpPlayPositioningClass`)
Match_Final2$`Away chanceCreationPassingClass`<-as.factor(Match_Final2$`Away chanceCreationPassingClass`)
Match_Final2$`Away chanceCreationCrossingClass`<-as.factor(Match_Final2$`Away chanceCreationCrossingClass`)
Match_Final2$`Away chanceCreationShootingClass`<-as.factor(Match_Final2$`Away chanceCreationShootingClass`)
Match_Final2$`Away chanceCreationPositioningClass`<-as.factor(Match_Final2$`Away chanceCreationPositioningClass`)
Match_Final2$`Away defencePressureClass`<-as.factor(Match_Final2$`Away defencePressureClass`)
Match_Final2$`Away defenceAggressionClass`<-as.factor(Match_Final2$`Away defenceAggressionClass`)
Match_Final2$`Away defenceTeamWidthClass`<-as.factor(Match_Final2$`Away defenceTeamWidthClass`)
Match_Final2$`Away defenceDefenderLineClass`<-as.factor(Match_Final2$`Away defenceDefenderLineClass`)
Match_Final2$`Away team_long_name`<-as.factor(Match_Final2$`Away team_long_name`)


Match_Final2$ `home_player_1 overall_rating`<-as.factor(Match_Final2$ `home_player_1 overall_rating`)
Match_Final2$ `home_player_2 overall_rating`<-as.factor(Match_Final2$ `home_player_2 overall_rating`)
Match_Final2$ `home_player_3 overall_rating`<-as.factor(Match_Final2$ `home_player_3 overall_rating`)
Match_Final2$ `home_player_4 overall_rating`<-as.factor(Match_Final2$ `home_player_4 overall_rating`)
Match_Final2$ `home_player_5 overall_rating`<-as.factor(Match_Final2$ `home_player_5 overall_rating`)
Match_Final2$ `home_player_6 overall_rating`<-as.factor(Match_Final2$ `home_player_6 overall_rating`)
Match_Final2$ `home_player_7 overall_rating`<-as.factor(Match_Final2$ `home_player_7 overall_rating`)
Match_Final2$ `home_player_8 overall_rating`<-as.factor(Match_Final2$ `home_player_8 overall_rating`)
Match_Final2$ `home_player_9 overall_rating`<-as.factor(Match_Final2$ `home_player_9 overall_rating`)
Match_Final2$ `home_player_10 overall_rating`<-as.factor(Match_Final2$ `home_player_10 overall_rating`)
Match_Final2$ `home_player_11 overall_rating`<-as.factor(Match_Final2$ `home_player_11 overall_rating`)



Match_Final2$ `away_player_1 overall_rating`<-as.factor(Match_Final2$ `away_player_1 overall_rating`)
Match_Final2$ `away_player_2 overall_rating`<-as.factor(Match_Final2$ `away_player_2 overall_rating`)
Match_Final2$ `away_player_3 overall_rating`<-as.factor(Match_Final2$ `away_player_3 overall_rating`)
Match_Final2$ `away_player_4 overall_rating`<-as.factor(Match_Final2$ `away_player_4 overall_rating`)
Match_Final2$ `away_player_5 overall_rating`<-as.factor(Match_Final2$ `away_player_5 overall_rating`)
Match_Final2$ `away_player_6 overall_rating`<-as.factor(Match_Final2$ `away_player_6 overall_rating`)
Match_Final2$ `away_player_7 overall_rating`<-as.factor(Match_Final2$ `away_player_7 overall_rating`)
Match_Final2$ `away_player_8 overall_rating`<-as.factor(Match_Final2$ `away_player_8 overall_rating`)
Match_Final2$ `away_player_9 overall_rating`<-as.factor(Match_Final2$ `away_player_9 overall_rating`)
Match_Final2$ `away_player_10 overall_rating`<-as.factor(Match_Final2$ `away_player_10 overall_rating`)
Match_Final2$ `away_player_11 overall_rating`<-as.factor(Match_Final2$ `away_player_11 overall_rating`)


#================================Join all Players=====================
#===== Join Player with Match

firstcol<-NCOL(Match_Final)+1
Match_Final2<-sqldf("Select * from Match_Final
                   left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final.home_player_1
                   and Match_Final.date > Player_Joined.date
                   and Match_Final.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("home_player_1", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))

firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.home_player_2
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("home_player_2", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))

firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.home_player_3
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("home_player_3", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))

firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.home_player_4
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("home_player_4", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))

firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.home_player_5
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("home_player_5", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))


firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.home_player_6
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("home_player_6", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))


firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.home_player_7
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("home_player_7", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))


firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.home_player_8
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("home_player_8", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))


firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.home_player_9
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("home_player_9", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))


firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.home_player_10
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("home_player_10", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))


firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.home_player_11
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("home_player_11", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))

str(Match_Final2)



firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.away_player_1
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("away_player_1", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))


firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.away_player_2
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("away_player_2", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))

firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.away_player_3
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("away_player_3", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))

firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.away_player_4
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("away_player_4", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))

firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.away_player_5
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("away_player_5", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))


firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.away_player_6
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("away_player_6", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))


firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.away_player_7
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("away_player_7", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))


firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.away_player_8
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("away_player_8", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))


firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.away_player_9
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("away_player_9", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))


firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.away_player_10
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("away_player_10", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))


firstcol<-NCOL(Match_Final2)+1
Match_Final2<-sqldf("Select * from Match_Final2
                    left join Player_Joined on 
                    Player_Joined.player_api_id = Match_Final2.away_player_11
                    and Match_Final2.date > Player_Joined.date
                    and Match_Final2.date <= Player_Joined.lastdate")
#  Renaming Away Columns
lastcol<-NCOL(Match_Final2)
colnames(Match_Final2)[firstcol:lastcol]<-paste("away_player_11", colnames(Match_Final2)[firstcol:lastcol],sep= " ")
Match_Final2<-subset(Match_Final2,select = c(-(firstcol:(firstcol+3)),-(lastcol-2)))

#==================== Remove extra columns=================
Match<-subset(Match_Final,select = c(-(3:27),-1))
Match<-na.omit(Match)


Match_Final2<-subset(Match_Final2,select = c(-(3:27),-1))
Match_Final2<-na.omit(Match_Final2)

rm(EuropeSoccer)
rm(firstcol)
rm(i)
rm(j)
rm(lastcol)
rm(lastrow)
rm(sqlite)
rm(Match_Joined)
rm(Team_Joined)
rm(Player_Joined)
