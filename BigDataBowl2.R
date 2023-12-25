library(sportyR)
library(ggplot2)
library(gganimate)
library(dplyr)
library(ROCR)
library(stringr)
library(gifski)
library(gginnards)
library(tidyr)
setwd("/Users/sithijamanage/Fall 2023/BigDataBowl2024/nfl-big-data-bowl-2024")
#Read in Data
games <- read.csv("games.csv") #gameId, season, week, homeTeamAbbr, visitorTeamAbbr, homeFinalScore, visitorFinal Score
players <- read.csv("players.csv") #nflID, position, displayName
plays <- read.csv("plays.csv") #gameId, playId, ballCarrierId, defendersInTheBox
tackles <- read.csv("tackles.csv") #gameId, playId, nflId, tackle, assist, forcedFumble
tracking_week_1 <- read.csv("tracking_week_1.csv")
tracking_week_2 <- read.csv("tracking_week_2.csv")
tracking_week_3 <- read.csv("tracking_week_3.csv")
tracking_week_4 <- read.csv("tracking_week_4.csv")
tracking_week_5 <- read.csv("tracking_week_5.csv")
tracking_week_6 <- read.csv("tracking_week_6.csv")
tracking_week_7 <- read.csv("tracking_week_7.csv")
tracking_week_8 <- read.csv("tracking_week_8.csv")
tracking_week_9 <- read.csv("tracking_week_9.csv")
tracking_week_1 <- rbind(tracking_week_1,tracking_week_2,tracking_week_3,
                         tracking_week_4,tracking_week_5,tracking_week_6,
                         tracking_week_7,tracking_week_8,tracking_week_9)

# Create the field 
nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)

#Animate the play
animate_play  <- function(tracking, gameNumber, playNumber){
 #playNum = unique(tracking$playId)[playNumber]
 #gameNum = unique(tracking$gameId)[gameNumber]
  playNum = playNumber
  gameNum = gameNumber
 specific_play <- tracking[tracking$gameId==gameNum & tracking$playId==playNum,]
 
 team1 = names(table(specific_play$club)[!names(table(specific_play$club)) %in% "football"])[1]
 team2 = names(table(specific_play$club)[!names(table(specific_play$club)) %in% "football"])[2]
 specific_play[specific_play["club"] == team1, "color"] <- "red"
 specific_play[specific_play["club"] == team2, "color"] <- "blue"
 cat(team1, team2)
 specific_play[specific_play["club"] == "football", "color"] <- "white"
 
 if(length(which(tackles$gameId==gameNum & tackles$playId==playNum))>0){
   tackler = tackles[which(tackles$gameId==gameNum & tackles$playId==playNum),]$nflId
   specific_play[specific_play$nflId %in% tackler, "color"] <- "yellow"
 }else{
   tackler = "No Tackler"
 }
 
 cat(" Tackler: ", tackler)
 tacklerString = paste(as.character(tackler), collapse = " ")
 
 # Move the "football" player based on playDirection
# if (specific_play$playDirection[1] == "left") {
#   specific_play[specific_play$displayName == "football", "x"] <- specific_play[specific_play$displayName == "football", "x"] - 1
# } else if (specific_play$playDirection[1] == "right") {
#   specific_play[specific_play$displayName == "football", "x"] <- specific_play[specific_play$displayName == "football", "x"] + 1
 #}
 
 # Draw a triangle between specified players
 triangle_players_positions <- specific_play[specific_play$nflId %in% c(52630, 39984, 44825 ),]
 triangle_players_positions <- triangle_players_positions %>% arrange(frameId) %>%
   group_by(frameId) %>%
   mutate(x.tri = list(c(x[1], x[2], x[3])),
   y.tri = list(c(y[1], y[2], y[3])))
    View(specific_play)
   
 play_anim <- nfl_field +
   geom_point(
     data = specific_play,
     aes(x, y),
     color = specific_play$color
   ) +
   geom_text(
     data = specific_play,
     aes(x = 2, y = 0, label = frameId),
     vjust = 2
   ) +
   geom_text(
     data = specific_play,
     aes(x = 60, y = -5, label = event),
     vjust = -1,
     hjust = 0.5
   ) +
   #geom_polygon(
  #   data = triangle_players_positions,
  #   aes(x = x, y = y),
  #   fill = NA,
  #   colour = "black",
  #   alpha = 0.9
  # ) +
   transition_time(specific_play$frameId)
   
 play_anim
 #delete_layers(play_anim, "GeomPolygon")
}#animate_play

# Missed Tackles
tracking_week_1 <- tracking_week_1 %>%
  left_join(tackles %>% select(gameId, playId, nflId, pff_missedTackle), 
            by = c("gameId", "playId", "nflId")) 
tracking_week_1$pff_missedTackle[is.na(tracking_week_1$pff_missedTackle)] <- 0

## missedTackle_plays has all the games and plays with missed tackles
missedTackle_plays <- tracking_week_1 %>%
  filter(pff_missedTackle == 1) %>%
  select(gameId, playId, nflId) %>%
  distinct()
## adding first_contact (missed tackle) frame. Assumption: Missed Tackler made contact.
missedTackle_plays <- missedTackle_plays %>%
  left_join( tracking_week_1 %>% 
      filter(event == "first_contact") %>% 
      select(gameId, playId, nflId, frameId),
    by = c("gameId", "playId", "nflId")
  )
#if nflId says tackle for that play, remove their row from missed tackle play 
# Create a logical vector to identify rows to remove from missedTackle_plays
missedTackle_plays <- missedTackle_plays %>%
  anti_join(tackles %>% filter(tackle == 1), 
            by = c("gameId", "playId", "nflId"))



# Made Tackles
tracking_week_1 <- tracking_week_1 %>%
  left_join(tackles %>% select(gameId, playId, nflId, tackle), 
            by = c("gameId", "playId", "nflId")) %>%
  mutate(tackleMade = ifelse(tackle == 1, 1, 0)) %>%
  select(-tackle)
tracking_week_1 <- tracking_week_1 %>%
  mutate(tackleMade = ifelse(is.na(tackleMade), 0, tackleMade))
## missedTackle_plays has all the games and plays with missed tackles
madeTackle_plays <- tracking_week_1 %>%
  filter(tackleMade == 1) %>%
  select(gameId, playId, nflId) %>%
  distinct()
## adding first_contact (missed tackle) frame 
madeTackle_plays <- madeTackle_plays %>%
  left_join(
    tracking_week_1 %>% 
      filter(event == "tackle") %>% 
      select(gameId, playId, nflId, frameId),
    by = c("gameId", "playId", "nflId")
  )
madeTackle_plays <- madeTackle_plays %>%
  distinct()
# Next Step: Add Ball Carrier's info to missedTackle and madeTackle 
# to compute difference in orientation.
  
#animate_play(tracking_week_1,
#             missedTackle_plays[36,1], #2 is
#             missedTackle_plays[36,2]) #Great example of missed tackle
#35 is like a wheel route miss. 
#animate_play(tracking_week_1, 2022090800, 364)

#Tracking the football
football_tracking_week_1 <- tracking_week_1[tracking_week_1$displayName=="football",]

missedTackles <- tracking_week_1 %>% #NA-frame missed Tackles get dropped, good.
  inner_join(missedTackle_plays, by = c("gameId", "playId", "nflId", "frameId"))
madeTackles <- tracking_week_1 %>% 
  inner_join(madeTackle_plays, by = c("gameId", "playId", "nflId", "frameId"))
missedTackles$tackleSuccess = rep(0,nrow(missedTackles))
madeTackles$tackleSuccess = rep(1, nrow(madeTackles))
missedTackles$pff_missedTackle=NULL; missedTackles$tackleMade=NULL;
madeTackles$pff_missedTackle=NULL; madeTackles$tackleMade=NULL;

tackleAttempts <- rbind(missedTackles, madeTackles)

# Create the ballCarrier data frame
ballCarrier <- tracking_week_1 %>%
  inner_join(tackleAttempts %>% select(gameId, playId, frameId), 
             by = c("gameId", "playId", "frameId")) %>%
  inner_join(plays %>% select(gameId, playId, ballCarrierId, yardsToGo), 
             by = c("gameId", "playId")) %>%
  select(gameId, playId, frameId, ballCarrierId, yardsToGo) %>%
  distinct()
names(ballCarrier)[4]="nflId"
ballCarrier <- ballCarrier %>%
  inner_join(tracking_week_1, by = c("gameId", "playId", "nflId", "frameId"))
ballCarrier$pff_missedTackle=NULL; ballCarrier$tackleMade=NULL

# Create an updated tackleAttempts data frame with the additional columns
tackleAttempts <- tackleAttempts %>%
  inner_join(ballCarrier %>% select(gameId, playId, frameId, x, y, s, a, dis, o, dir, yardsToGo), 
             by = c("gameId", "playId", "frameId"))

tackleAttempts$impactAngle <- abs(tackleAttempts$dir.x - tackleAttempts$dir.y)
tackleAttempts$speedDiff   <- abs(tackleAttempts$s.x - tackleAttempts$s.y)
tackleAttempts$accDiff   <- abs(tackleAttempts$a.x - tackleAttempts$a.y)

# Logistic Regression to predict Tackle vs Missed Tackle
logreg_model <- glm(tackleSuccess~yardsToGo+impactAngle+speedDiff+accDiff,data = tackleAttempts)
summary(logreg_model)

# PLOTS = = = = 
ggplot(data = tackleAttempts,aes(x=impactAngle)) +
  geom_histogram(bins=30) +
  facet_grid(tackleSuccess~.)

#animate_play(tracking_week_1, tackleAttempts[244,1], 
#             tackleAttempts[244,2])

#TESTING LOGISTIC REGRESSION MODEL
# Step 1: Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(tackleAttempts), 0.8 * nrow(tackleAttempts))  # 70% for training, adjust as needed
train_data <- tackleAttempts[train_indices, ]
test_data <- tackleAttempts[-train_indices, ]

# Step 2: Make predictions
train_predictions <- predict(logreg_model, newdata = train_data, type = "response")
test_predictions <- predict(logreg_model, newdata = test_data, type = "response")

# Step 3: Evaluate accuracy
# Define a threshold for classification (e.g., 0.5)
threshold <- 0.64

# Calculate training accuracy

train_accuracy <- mean((train_predictions >= threshold) == train_data$tackleSuccess)

# Calculate testing accuracy
test_accuracy <- mean((test_predictions >= threshold) == test_data$tackleSuccess)

cat("Training Accuracy:", train_accuracy, "\n")
cat("Testing Accuracy:", test_accuracy, "\n")

# Calculate true positives, false negatives, true negatives, and false positives
true_positives <- sum((test_predictions >= threshold) & (test_data$tackleSuccess == 1))
false_negatives <- sum((test_predictions < threshold) & (test_data$tackleSuccess == 1))
true_negatives <- sum((test_predictions < threshold) & (test_data$tackleSuccess == 0))
false_positives <- sum((test_predictions >= threshold) & (test_data$tackleSuccess == 0))

# Calculate sensitivity and specificity
sensitivity <- true_positives / (true_positives + false_negatives)
specificity <- true_negatives / (true_negatives + false_positives)

cat("Sensitivity (True Positive Rate):", sensitivity, "\n")
cat("Specificity (True Negative Rate):", specificity, "\n")
cat("Training Accuracy:", train_accuracy, "\n")
cat("Testing Accuracy:", test_accuracy, "\n")

pred <- ROCR::prediction(test_predictions, test_data$tackleSuccess)
perf <- ROCR::performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE, main="ROC Curve Logistic Regression Model")

auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]] #0.78

# Create a scatter plot of the data points
ggplot(data = tackleAttempts, aes(x = impactAngle, y = tackleSuccess)) +
  geom_point() +
  labs(title = "Logistic Regression - Yard to Go",
       x = "Yards to Go (yd)",
       y = "Tackle Probability") +
  
  # Add the logistic regression line
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "#FF007F") +
  # Customize the theme if needed
  theme_minimal()
### Tackle metric

duos <- tackles %>% #duos includes good double tackles and miss one make one tackles.
  group_by(gameId, playId) %>% #we'll need another df to hold two miss tackles.
  filter(n() == 2) %>%
  arrange(gameId, playId)

duosSuccessful <- merge(duos, tracking_week_1, by = c("gameId", "playId", "nflId"), all.x = TRUE)

names(plays)[3] = "nflId" #change ballCarrierId to nflId
plays_subset <- plays %>% #has gameId,playId,nflId of ballCarrier vs duosSuccessful
  filter(paste(gameId, playId) %in% paste(duosSuccessful$gameId, duosSuccessful$playId)) 


duosSuccessful_ballCarrierInfo <- merge(tracking_week_1, plays_subset, by = c("gameId", "playId", "nflId")) #has 3 game,play,nfl combos that got dropped from the original 4037
duosSuccessful_ballCarrierInfo <- duosSuccessful_ballCarrierInfo %>%
  select(names(tracking_week_1))
duosSuccessful_ballCarrierInfo$nflId = rep(0, length(duosSuccessful_ballCarrierInfo$nflId))

common_columns <- intersect(names(duosSuccessful_ballCarrierInfo), names(duosSuccessful))

# Bind rows using only the common columns
# Carries tracking data on each of the 3 players on each frame.
triosSuccessful <- bind_rows(#there are 3 rows that won't have a ballCarrier.
  duosSuccessful_ballCarrierInfo %>% select(all_of(common_columns)),
  duosSuccessful %>% select(all_of(common_columns))
)


# Function to calculate Euclidean distance
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

triosSuccessful <- triosSuccessful %>% 
  filter(!is.na(frameId))

# Calculate distances for each frame
triosSuccessful_tacklerLabelled <- triosSuccessful %>%
  group_by(gameId, playId, frameId) %>%
  mutate(
    distance_to_tackler1 = ifelse(nflId != 0, euclidean_distance(x[nflId == 0], y[nflId == 0], x[nflId != 0][1], y[nflId != 0][1]), NA_real_),
    distance_to_tackler2 = ifelse(nflId != 0, euclidean_distance(x[nflId == 0], y[nflId == 0], x[nflId != 0][2], y[nflId != 0][2]), NA_real_),
    tacklerLabel = case_when(
      nflId == 0 ~ 0,  # Label 0 for the ball carrier
      nflId!=0 & row_number() == 2 & distance_to_tackler1 <= distance_to_tackler2 ~ 1,
      nflId!=0 & row_number() == 2 & distance_to_tackler1 > distance_to_tackler2 ~ 2,
      nflId!=0 & row_number() == 3 & distance_to_tackler2 < distance_to_tackler1 ~ 1, #<=
      nflId!=0 & row_number() == 3 & distance_to_tackler2 >= distance_to_tackler1 ~ 2, #>
      TRUE ~ 3  # error case.
    )
  ) %>% mutate(x_0 = x[tacklerLabel == 0][1]) %>%
        mutate(x_1 = x[tacklerLabel == 1][1]) %>%
        mutate(x_2 = x[tacklerLabel == 2][1]) %>%
        mutate(y_0 = y[tacklerLabel == 0][1]) %>%
        mutate(y_1 = y[tacklerLabel == 1][1]) %>%
        mutate(y_2 = y[tacklerLabel == 2][1]) %>%
  ungroup() 
#Calculating Triangle Metrics
triosSuccessful_Metrics <- triosSuccessful_tacklerLabelled %>% 
  rowwise() %>%
  mutate(
    #area
    area = 0.5 * abs(x_0 * (y_1 - y_2) + x_1 * (y_2 - y_0) + x_2 * (y_0 - y_1)),
    
    #angles
    v1x = x_1 - x_0,
    v1y = y_1 - y_0,
    v2x = x_2 - x_0,
    v2y = y_2 - y_0,
    
    angle0 = acos((v1x * v2x + v1y * v2y) / (sqrt(v1x^2 + v1y^2) * sqrt(v2x^2 + v2y^2))) * 180 / pi,
    
    v1x = x_0 - x_1,
    v1y = y_0 - y_1,
    v2x = x_2 - x_1,
    v2y = y_2 - y_1,
    
    angle1 = acos((v1x * v2x + v1y * v2y) / (sqrt(v1x^2 + v1y^2) * sqrt(v2x^2 + v2y^2))) * 180 / pi,
    
    v1x = x_0 - x_2,
    v1y = y_0 - y_2,
    v2x = x_1 - x_2,
    v2y = y_1 - y_2,
    
    angle2 = acos((v1x * v2x + v1y * v2y) / (sqrt(v1x^2 + v1y^2) * sqrt(v2x^2 + v2y^2))) * 180 / pi,
    
    #slope
    mid_x = (x_1 + x_2) / 2,
    mid_y = (y_1 + y_2) / 2,
    
    abs_slope = abs((mid_y - y_0) / (mid_x - x_0))
    ) %>% 
  filter(!is.nan(angle0) & !is.nan(angle1) & !is.nan(angle2)) %>%
  filter(tacklerLabel==0)


#%>%
#  select(-c(distance_to_tackler1, distance_to_tackler2))

# Explicitly count occurrences of each label
#table(triosSuccessful_TEST$tacklerLabel)

  triosSuccessful_Metrics$trioSuccess <- rep(1, nrow(triosSuccessful_Metrics))
  
  # PAD the plays = =
  combinations_to_duplicate <- triosSuccessful_Metrics %>%
    group_by(gameId, playId) %>%
    summarise(num_frames = n()) %>%
    filter(num_frames < 128)  
  
  triosSuccessful_Metrics <- triosSuccessful_Metrics %>%
    left_join(combinations_to_duplicate, by = c("gameId", "playId")) %>%
    filter(frameId == 1) %>%
    slice(rep(1, each = 128 - first(num_frames))) %>%
    bind_rows(triosSuccessful_Metrics, .) #now triosSuccessful_Metrics each play has 128 frames.
  
# TRIOS FAILED = = = = = = = = = = = = = = = =
singles <- tackles %>%group_by(gameId, playId) %>%
          filter(n() == 1) %>%
          filter(pff_missedTackle == 1 & tackle==0 & assist==0 & forcedFumble==0) %>%
          ungroup()
  
singles_tracking <- tracking_week_1 %>% 
  filter(paste(gameId, playId,nflId) %in% paste(singles$gameId, singles$playId, singles$nflId)) 

singles_team_tracking <- tracking_week_1 %>% #holds all defending players positions during tackles, inlcuding tackler's.
  filter(paste(gameId, playId, club) %in% paste(singles_tracking$gameId, singles_tracking$playId, singles_tracking$club)) 

singles_team_tracking <- singles_team_tracking %>% #holds second closest defenders position.
  left_join(singles_tracking, by = c("gameId", "playId", "frameId", "club")) %>%
  mutate(
    distance_to_single = sqrt((x.x - x.y)^2 + (y.x - y.y)^2)
  ) %>%
  filter(distance_to_single!=0) %>%
  group_by(gameId, playId, frameId) %>%
  mutate(
    closest = ifelse(distance_to_single == min(distance_to_single), 1, 0)
  )%>%
  filter(closest==1) %>%
  mutate(tacklerLabel=2) #%>%
  #select(-starts_with("x."), -starts_with("y.")) 

singles_tracking$tacklerLabel <- rep(1, nrow(singles_tracking))
  
names(singles_team_tracking) <- str_replace_all(names(singles_team_tracking), "\\.x$", "")
duosFailed <- bind_rows(singles_team_tracking, singles_tracking %>% select(gameId, playId, nflId, frameId, displayName, time, jerseyNumber, club, playDirection, x, y, s, a, dis, o, dir, event, tacklerLabel))

plays_subset_1 <- plays %>% #has gameId,playId,nflId of ballCarrier vs duosFailed
  filter(paste(gameId, playId) %in% paste(duosFailed$gameId, duosFailed$playId)) 

duosFailed_ballCarrierInfo <- merge(tracking_week_1, plays_subset_1, by = c("gameId", "playId", "nflId")) 
duosFailed_ballCarrierInfo <- duosFailed_ballCarrierInfo %>%
  select(names(tracking_week_1))
duosFailed_ballCarrierInfo$nflId = rep(0, length(duosFailed_ballCarrierInfo$nflId))
duosFailed_ballCarrierInfo$tacklerLabel = rep(0, length(duosFailed_ballCarrierInfo$nflId))

common_columns_1 <- intersect(names(duosFailed_ballCarrierInfo), names(duosFailed))

# Carries tracking data on each of the 3 players on each frame.
triosFailed <- bind_rows(
  duosFailed_ballCarrierInfo %>% select(all_of(common_columns_1)),
  duosFailed %>% select(all_of(common_columns_1))
)

#GETTING x_0,...,y_3
triosFailed <- triosFailed %>%
  group_by(gameId, playId, frameId) %>%
  mutate(x_0 = x[tacklerLabel == 0][1]) %>%
  mutate(x_1 = x[tacklerLabel == 1][1]) %>%
  mutate(x_2 = x[tacklerLabel == 2][1]) %>%
  mutate(y_0 = y[tacklerLabel == 0][1]) %>%
  mutate(y_1 = y[tacklerLabel == 1][1]) %>%
  mutate(y_2 = y[tacklerLabel == 2][1]) %>%
  ungroup() 


#TRIANGLE METRICS FOR TRIOSFAILED
triosFailed_Metrics <- triosFailed %>% 
  rowwise() %>%
  mutate(
    #area
    area = 0.5 * abs(x_0 * (y_1 - y_2) + x_1 * (y_2 - y_0) + x_2 * (y_0 - y_1)),
    #angles
    v1x = x_1 - x_0,
    v1y = y_1 - y_0,
    v2x = x_2 - x_0,
    v2y = y_2 - y_0,
    angle0 = acos((v1x * v2x + v1y * v2y) / (sqrt(v1x^2 + v1y^2) * sqrt(v2x^2 + v2y^2))) * 180 / pi,
    v1x = x_0 - x_1,
    v1y = y_0 - y_1,
    v2x = x_2 - x_1,
    v2y = y_2 - y_1,
    angle1 = acos((v1x * v2x + v1y * v2y) / (sqrt(v1x^2 + v1y^2) * sqrt(v2x^2 + v2y^2))) * 180 / pi,
    v1x = x_0 - x_2,
    v1y = y_0 - y_2,
    v2x = x_1 - x_2,
    v2y = y_1 - y_2,
    angle2 = acos((v1x * v2x + v1y * v2y) / (sqrt(v1x^2 + v1y^2) * sqrt(v2x^2 + v2y^2))) * 180 / pi,
    #slope
    mid_x = (x_1 + x_2) / 2,
    mid_y = (y_1 + y_2) / 2,
    abs_slope = abs((mid_y - y_0) / (mid_x - x_0))
  ) %>% 
  filter(!is.nan(angle0) & !is.nan(angle1) & !is.nan(angle2)) %>%
  filter(tacklerLabel==0)

triosFailed_Metrics$trioSuccess <- rep(0, nrow(triosFailed_Metrics))

# PAD the plays = =
combinations_to_duplicate <- triosFailed_Metrics %>%
  group_by(gameId, playId) %>%
  summarise(num_frames = n()) %>%
  filter(num_frames < 128)  

triosFailed_Metrics <- triosFailed_Metrics %>%
  left_join(combinations_to_duplicate, by = c("gameId", "playId")) %>%
  filter(frameId == 1) %>%
  slice(rep(1, each = 128 - first(num_frames))) %>%
  bind_rows(triosFailed_Metrics, .) #now triosFailed_Metrics each play has 128 frames.

triosSuccessful_Metrics <- triosSuccessful_Metrics %>%
  arrange(gameId, playId, frameId)
triosFailed_Metrics <- triosFailed_Metrics %>%
  arrange(gameId, playId, frameId)
#write.csv(triosSuccessful_Metrics, "triosSuccessful_Metrics.csv")
#write.csv(triosFailed_Metrics, "triosFailed_Metrics.csv")

input_Successful <- triosSuccessful_Metrics %>%
  select(c(gameId, playId, frameId, area, angle0, angle1, angle2, abs_slope, trioSuccess))
input_Failed <- triosFailed_Metrics %>%
  select(c(gameId, playId, frameId, area, angle0, angle1, angle2, abs_slope, trioSuccess))
inputDf <- rbind(input_Failed, input_Successful)
inputDf$abs_slope[which(inputDf$abs_slope==Inf)] = 1000

#write.csv(inputDf, "inputDf.csv")
#write.csv(inputDf[5249:5376,], "Play1223.csv") #Play1223
#write.csv(inputDf[295297:295424,], "Play2608.csv") #Play2608
#write.csv(inputDf[769:897,], "Play1613.csv") #Play1613
#write.csv(inputDf[1281:1408,], "Play1099.csv") #Play1099
#write.csv(inputDf[1793:1920,], "Play1544.csv") #Play1544
#write.csv(inputDf[1921:2048,], "Play3182.csv") #Play3182
#write.csv(inputDf[244993:245120,], "Play4083.csv") #Play4083
write.csv(inputDf[34689:34816,], "Play3046.csv") #Play3046
write.csv(inputDf[6273:6400,], "Play3046_2.csv") #Play3046
predictions <- read.csv("/Users/sithijamanage/Fall 2023/BigDataBowl2024/nfl-big-data-bowl-2024/fulllength_predictions.csv", header=FALSE)
inputDf$predictions = predictions$V1


# Creating Tackler Team Rankings = = = = = = = = = = = = = = = = = = = = = = =
tackler_lookup <- triosSuccessful_tacklerLabelled %>%
  filter(tacklerLabel %in% c(1, 2)) %>%
  select(gameId, playId, frameId, nflId, tacklerLabel)
tackler_lookup_failed <- triosFailed %>%
  filter(tacklerLabel %in% c(1,2)) %>%
  select(gameId, playId, frameId, nflId, tacklerLabel)

tackler_wide <- tackler_lookup %>%
  pivot_wider(names_from = tacklerLabel, values_from = nflId,
              names_prefix = "tackler")
tackler_wide_failed <- tackler_lookup_failed %>%
  pivot_wider(names_from = tacklerLabel, values_from = nflId,
              names_prefix = "tackler")
tacklers_wide = rbind(tackler_wide, tackler_wide_failed)

#inputDf_TEST = inputDf[1:1280,]
# Loop through inputDf and update columns based on conditions

# Merge inputDf_TEST with the pivoted tackler_lookup
named_data <- merge(inputDf, tacklers_wide, by = c("gameId", "playId", "frameId"), all.x = TRUE)

duo_rankings <- named_data %>%
  group_by(tackler1, tackler2) %>%
  summarize(metric_score = mean(predictions, na.rm = TRUE))
duo_rankings$duo_name = rep("NO_NAME", nrow(duo_rankings))
for(i in 1:nrow(duo_rankings)){
  print(i)
  duo_rankings$duo_name <- apply(duo_rankings[, c("tackler1", "tackler2")], 1, function(x) paste(sort(x), collapse = "_"))
}#for

duo_rankings_avg <- duo_rankings %>%
  group_by(duo_name) %>%
  summarize(avg_metric_score = mean(metric_score, na.rm = TRUE))%>%
  arrange(desc(avg_metric_score))
duo_rankings_avg$duo_rank = seq(from=1,to=nrow(duo_rankings_avg))

#Getting the actual names of the duos.
duo_rankings_avg$duo_display_names <- NA

# Loop through rows of duo_rankings_avg
for (i in 1:nrow(duo_rankings_avg)) {
  if(i%%100==0)
    print(i)
  # Initialize an empty vector to store display names
  display_names <- character()
  
  # Loop through rows of players
  for (j in 1:nrow(players)) {
    # Check if nflId from players is in duo_name
    if (players$nflId[j] %in% unlist(strsplit(duo_rankings_avg$duo_name[i], "_"))) {
      # Add display name to the vector
      display_names <- c(display_names, players$displayName[j])
    }
  }
  
  # Combine display names and assign to duo_display_names column
  duo_rankings_avg$duo_display_names[i] <- paste(display_names, collapse = "_")
}




