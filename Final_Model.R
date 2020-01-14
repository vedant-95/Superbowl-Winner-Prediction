#Loading the dataset
setwd('C:/Users/vedan/Desktop/studies/UCI/Fall Semester/Data and Programming Analytics/project files/')
dataset = read.csv('Post_1990 - Copy.csv')
dataset = na.omit(dataset)

#Loading the libraries
library(InformationValue)
library(scorecard)
library(car)

#Finding out information gains
info_value = iv(dataset, y = "Superbowl_Winner")
str(info_value)

#Choosing the top 40 information gain columns
dataset = dataset[,c('Superbowl_Winner','Wins','Losses','Offense_field_goals_XPM','Offense_scoring_Rk','Offense_touchdowns_Rk','Offense_field_goals_XP_Att',
'Offense_kicking_KO','Defense_scoring_Rk','Offense_touchdowns_Total','Offense_touchdowns_Rsh','Offense_game_stats_TO',
'Defense_rushing_Rk','Offense_game_stats_Rk','Defense_touchdowns_Rk','Defense_game_stats_1st.G','Offense_touchdowns_Rec','Offense_passing_Avg',
'Defense_scoring_XPM','Defense_touchdowns_Total','Offense_game_stats_3rd_Pct','Defense_game_stats_Pts.G','Defense_game_stats_4th_Att',
'Defense_game_stats_Yds.P','Offense_game_stats_Pts.G','Offense_rushing_Att.G','Defense_passing_1st.','Defense_rushing_Att.G',
'Defense_tackles_Sck','Defense_passing_Sck','Offense_rushing_1st','Defense_rushing_1st','Defense_interceptions_PDef','Offense_game_stats_3rd_Att',
'Defense_game_stats_Rk','Offense_game_stats_Yds.P','Defense_passing_Att.G',
'Offense_punting_Punts','Offense_punting_Rk','Offense_passing_1st','Offense_kicking_returns')]

#Converting dependent variable into a factor                     
dataset$Superbowl_Winner = factor(dataset$Superbowl_Winner, levels = c(0, 1))

#Creating a temporary matrix to iteratively calculate correlations
temp = dataset[,c('Wins','Losses','Offense_field_goals_XPM','Offense_scoring_Rk','Offense_touchdowns_Rk','Offense_field_goals_XP_Att',
                     'Offense_kicking_KO','Defense_scoring_Rk','Offense_touchdowns_Total','Offense_touchdowns_Rsh','Offense_game_stats_TO',
                     'Defense_rushing_Rk','Offense_game_stats_Rk','Defense_touchdowns_Rk','Defense_game_stats_1st.G','Offense_touchdowns_Rec','Offense_passing_Avg',
                     'Defense_scoring_XPM','Defense_touchdowns_Total','Offense_game_stats_3rd_Pct','Defense_game_stats_Pts.G','Defense_game_stats_4th_Att',
                     'Defense_game_stats_Yds.P','Offense_game_stats_Pts.G','Offense_rushing_Att.G','Defense_passing_1st.','Defense_rushing_Att.G',
                     'Defense_tackles_Sck','Defense_passing_Sck','Offense_rushing_1st','Defense_rushing_1st','Defense_interceptions_PDef','Offense_game_stats_3rd_Att',
                     'Defense_game_stats_Rk','Offense_game_stats_Yds.P','Defense_passing_Att.G',
                     'Offense_punting_Punts','Offense_punting_Rk','Offense_passing_1st','Offense_kicking_returns')]

temp$Losses <- NULL
temp$Offense_scoring_Rk <- NULL
temp$Offense_touchdowns_Rk <- NULL
temp$Offense_field_goals_XP_Att <- NULL
temp$Offense_kicking_KO <- NULL
temp$Offense_touchdowns_Total <- NULL
temp$Defense_touchdowns_Rk <- NULL
temp$Defense_scoring_XPM <- NULL
temp$Offense_game_stats_Rk <- NULL
temp$Defense_passing_Sck <- NULL
temp$Offense_game_stats_Pts.G <- NULL
temp$Defense_game_stats_Pts.G <- NULL
temp$Defense_game_stats_1st.G <- NULL
temp$Defense_scoring_Rk <- NULL
temp$Offense_punting_Rk <- NULL
temp$Defense_game_stats_Rk <- NULL
temp$Defense_rushing_Rk <- NULL
temp$Offense_game_stats_Yds <- NULL
temp$Offense_game_stats_Yds.P <- NULL
temp$Defense_game_stats_Yds.P <- NULL
temp$Offense_passing_1st <- NULL
temp$Offense_rushing_Att.G <- NULL
temp$Defense_rushing_Att.G <- NULL
temp$Offense_field_goals_XPM <- NULL
temp$Offense_game_stats_TO <- NULL
temp$Defense_game_stats_4th_Att <- NULL
temp$Defense_tackles_Sck <- NULL
temp$Defense_passing_Att.G <- NULL
temp$Offense_kicking_returns <- NULL
temp$Offense_touchdowns_Rec <- NULL
temp$Defense_touchdowns_Total <- NULL
temp$Offense_touchdowns_Rsh <- NULL

dataset$Losses <- NULL
dataset$Offense_scoring_Rk <- NULL
dataset$Offense_touchdowns_Rk <- NULL
dataset$Offense_field_goals_XP_Att <- NULL
dataset$Offense_kicking_KO <- NULL
dataset$Offense_touchdowns_Total <- NULL
dataset$Defense_touchdowns_Rk <- NULL
dataset$Defense_scoring_XPM <- NULL
dataset$Offense_game_stats_Rk <- NULL
dataset$Defense_passing_Sck <- NULL
dataset$Offense_game_stats_Pts.G <- NULL
dataset$Defense_game_stats_Pts.G <- NULL
dataset$Defense_game_stats_1st.G <- NULL
dataset$Defense_scoring_Rk <- NULL
dataset$Offense_punting_Rk <- NULL
dataset$Defense_game_stats_Rk <- NULL
dataset$Defense_rushing_Rk <- NULL
dataset$Offense_game_stats_Yds <- NULL
dataset$Offense_game_stats_Yds.P <- NULL
dataset$Defense_game_stats_Yds.P <- NULL
dataset$Offense_passing_1st <- NULL
dataset$Offense_rushing_Att.G <- NULL
dataset$Defense_rushing_Att.G <- NULL
dataset$Offense_field_goals_XPM <- NULL
dataset$Offense_game_stats_TO <- NULL
dataset$Defense_game_stats_4th_Att <- NULL
dataset$Defense_tackles_Sck <- NULL
dataset$Defense_passing_Att.G <- NULL
dataset$Offense_kicking_returns <- NULL
dataset$Offense_touchdowns_Rec <- NULL
dataset$Defense_touchdowns_Total <- NULL
dataset$Offense_touchdowns_Rsh <- NULL

#VIF function
vif(lm(Offense_field_goals_XPM ~ ., data=temp))

correlations = cor(temp)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')

#Uncomment this to test on 2019 data
test_set = tail(dataset,32)
dataset = head(dataset,-32)

#Uncomment this to test on 2018
#test_set = tail(dataset,32)
#dataset = head(dataset,-32)

# Fitting classifier to the Training set
model <- glm(Superbowl_Winner ~. ,family=binomial(link='logit'),data=dataset)
summary(model)

#Uncomment to check for 2018 data
# Predicting the Test set results
#prob_pred = predict(model, type = 'response', newdata = test_set[-1])
#y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
#cm = table(test_set[, 1], y_pred > 0.5)

# Predicting the Test set results

prob_pred = predict(model, type = 'response', newdata = test_set[-1])
avg = sum(prob_pred)/length(prob_pred)
prob_pred = prob_pred/avg
prob_pred[prob_pred>1]
