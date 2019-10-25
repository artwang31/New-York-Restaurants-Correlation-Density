library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(Amelia)
library(ggplot2)

# Which boro has the lowest average and highest average score? 
# What is the type of cuisine with the lowest average score?

restaurants <- DOHMH_New_York_City_Restaurant_Inspection_Results

class(restaurants)
colnames(restaurants)

restaurants <- select(restaurants, 'BORO', 'DBA','CUISINE DESCRIPTION', 'INSPECTION DATE', 'VIOLATION DESCRIPTION',
                      'SCORE', 'GRADE','GRADE DATE', 'RECORD DATE', 'ZIPCODE') # 10 variables selected

which(is.na(restaurants$SCORE))
rest_scores <- restaurants %>% drop_na(SCORE) # drop NAs from the Score Column
head(rest_scores)

rest_scores <- rest_scores[order(rest_scores$BORO, na.last=NA) , ]
rest_scores
which(rest_scores$BORO == 0) # drop the restaurants that dont have a Boro

rest_scores <- rest_scores[97:377165,] 
str(rest_scores)
which(is.na(rest_scores$BORO)) # some BOROs don't have 0s but have NAs
rest_scores <- rest_scores[1:376594,]
head(rest_scores)
tail(rest_scores)

rest_scoresboro <- split(rest_scores, rest_scores$BORO)
class(rest_scoresboro) # class type list
# Restaurants score 0 and 13 = A, 14 to 27 = B, 28+ or more = C 

# Best and Worst Bronx Restaurants
min(rest_scoresboro$Bronx$SCORE) # -1 
which(rest_scoresboro$Bronx$SCORE == 3) # Clean restaurants in the Bronx
rest_scoresboro[[1]][84, 2]

max(rest_scoresboro$Bronx$SCORE) # 164
which(rest_scoresboro$Bronx$SCORE == 30) # Dirty restaurants in the Bronx
rest_scoresboro[[1]][25881, 2] # 99 cents hot pizza

# Best an Worst Manhattan Restaurants
min(rest_scoresboro$Manhattan$SCORE) # -1
which(rest_scoresboro$Manhattan$SCORE == 2 & rest_scores$Manhattan$ZIPCODE == 10016)
rest_scoresboro[[3]][112273, 2] # SweetCatch Poke

#
#
#
rest_scores
unique(rest_scores$`CUISINE DESCRIPTION`) # 84 unique cuisines
cuisine_types <- split(rest_scores, rest_scores$`CUISINE DESCRIPTION`)

