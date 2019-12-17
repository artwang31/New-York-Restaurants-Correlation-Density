library(tidyverse)
library(data.table)
library(dplyr)
library(tidyr)

# Uploading dataset from https://data.cityofnewyork.us/browse?q=food+grades 
restaurants_original <- fread(paste0("NYC Restaurant Inspections.csv"), header = T, stringsAsFactors = F, data.table = T)
save(restaurants_original, file = "restaurants_original.Rdata")
restaurants <- restaurants_original

### UNDERSTANDING DATA SET VARIABLES ###
# colnames(restaurants) 
# [1] "CAMIS"                 "DBA"                   "BORO"                  "BUILDING"             
# [5] "STREET"                "ZIPCODE"               "PHONE"                 "CUISINE DESCRIPTION"  
# [9] "INSPECTION DATE"       "ACTION"                "VIOLATION CODE"        "VIOLATION DESCRIPTION"
# [13] "CRITICAL FLAG"         "SCORE"                 "GRADE"                 "GRADE DATE"           
# [17] "RECORD DATE"           "INSPECTION TYPE"       "Latitude"              "Longitude"            
# [21] "Community Board"       "Council District"      "Census Tract"          "BIN"                  
# [25] "BBL"                   "NTA"     

# Changing column names, easier to understand
colnames(restaurants)[2] = "RESTAURANT NAME"
colnames(restaurants)[3] = "NYC BORO"
colnames(restaurants)[8] = "CUISINE"

### DATA TIDYING ###
restaurants <- dplyr::select(restaurants, "RESTAURANT NAME","NYC BORO","STREET","ZIPCODE","CUISINE","SCORE","GRADE")
str(restaurants)
colnames(restaurants)
class(restaurants)
nrow(restaurants)  # 379,819 
ncol(restaurants) # 7

# Only accept restaurants with NYC Gov documented restaurant scores, complete.cases
which(is.na(restaurants$SCORE))  # 17,205 restaurants without a score
restaurant_scores <- restaurants[ , c("SCORE")]  
restaurants_complete <- restaurants[complete.cases(restaurant_scores), ]

# Adding Scores and Grades based on NYC Grading Scale to fill in missing values

# An inspection score of 0-13 is an A, 14-27 points is a B, and 28-40 is a C, 40 or more is Temporary Closure.
restaurants_complete$GRADE <- ifelse(restaurants_complete$SCORE <= 13, 'A',
                                     ifelse(restaurants_complete$SCORE <= 27 & restaurants_complete$SCORE > 13, 'B',
                                            ifelse(restaurants_complete$SCORE <= 40 & restaurants_complete$SCORE > 27, 'C', 'NA')))

# Duplicate rows removed
restaurants_complete <- restaurants_complete[order(restaurants_complete$`NYC BORO`),] 
complete <- distinct(restaurants_complete)
str(complete)

# Indexing Restaurants into Boroughs
unique(complete$`NYC BORO`)  # "0" Bronx" "Brooklyn" "Manhattan" Queens" "Staten Island"
which(complete$`NYC BORO` == 'Bronx')
bronx <- complete[25:10562,]

which(complete$`NYC BORO` == 'Brooklyn')
brooklyn <- complete[10563:39398,]

which(complete$`NYC BORO` == 'Manhattan')
manhattan <- complete[39399:84155,]

which(complete$`NYC BORO` == 'Queens')
queens <- complete[84156:110348,]

which(complete$`NYC BORO` == 'Staten Island')
staten_island <- complete[110349:114248,]

allboros <- c(bronx, brooklyn, manhattan, queens, staten_island)

# Amount of Restaurants, Min, Max, and Mean restaurant scores in each Borough
# BRONX
length(bronx$`NYC BORO`) # 10538 Restaurants
max(bronx$SCORE)  # 164
min(bronx$SCORE)  # -1 
mean(bronx$SCORE)  # 15.68
# BROOKLYN
length(brooklyn$`NYC BORO`) # 28836 Restaurants
max(brooklyn$SCORE) # 151
min(brooklyn$SCORE) # -1
mean(brooklyn$SCORE)  # 16.04
# MANHATTAN
length(manhattan$`NYC BORO`) # 44757 Restaurants
max(manhattan$SCORE) # 153
min(manhattan$SCORE) # -1
mean(manhattan$SCORE)  # 15.86
# QUEENS
length(queens$`NYC BORO`) # 26193 Restaurants
max(queens$SCORE)  # 164
min(queens$SCORE)  # -1
mean(queens$SCORE)  # 15.78
# STATEN ISLAND
length(staten_island$`NYC BORO`) # 3900
max(staten_island$SCORE)  # 117
min(staten_island$SCORE) # -1
mean(staten_island$SCORE) # 15.65

# Which letter grade is given out most often per Borough
length(grep("A", bronx$GRADE)) # 7007
length(grep("B", bronx$GRADE)) # 2649
length(grep("C", bronx$GRADE)) # 882 
length(grep("NA", bronx$GRADE)) # 488

length(grep("A", brooklyn$GRADE)) # 19459
length(grep("B", brooklyn$GRADE))  # 7084
length(grep("C", brooklyn$GRADE))  # 2293
length(grep("NA", brooklyn$GRADE))  # 1608

length(grep("A", manhattan$GRADE))  # 30099
length(grep("B", manhattan$GRADE))  # 10992 
length(grep("C", manhattan$GRADE))  # 3666
length(grep("NA", manhattan$GRADE))  # 2245

length(grep("A", queens$GRADE))  # 17792
length(grep("B", queens$GRADE))  # 6323
length(grep("C", queens$GRADE))  # 2078
length(grep("NA", queens$GRADE))  # 1331

length(grep("A", staten_island$GRADE))  # 2548
length(grep("B", staten_island$GRADE))  # 1028
length(grep("C", staten_island$GRADE))  # 324
length(grep("NA", staten_island$GRADE))  # 159

# Visualization for Cuisine Type and Restaurant Scores
library(ggplot2)

length(unique(complete$CUISINE)) # 84 unique cuisine types.

ggplot(complete, aes(complete$SCORE, complete$CUISINE, color = complete$CUISINE)) +
  geom_point(show.legend = FALSE) +
  xlab('RESTAURANT SCORE') +
  ylab('CUISINE TYPE')

### DATA ANALYSIS ###
library(MASS)

# Chi-squared test for independence 
# P <= .05 reject null, P >= .05 cannot reject null
# Null Hypothesis - No relationship between Cuisine and Score
# Null Hypothesis - No relationship between Zipcode and Score

bronx_cuisine_score <- chisq.test(table(bronx$CUISINE, bronx$SCORE))
# data:  table(bronx$CUISINE, bronx$SCORE)
# X-squared = 4811.2, df = 4784, p-value = 0.3879
bronx_zipcode_score <- chisq.test(table(bronx$ZIPCODE, bronx$SCORE))
# data:  table(bronx$ZIPCODE, bronx$SCORE)
# X-squared = 2107.8, df = 2300, p-value = 0.9982
ggplot(bronx, aes(bronx$SCORE, bronx$CUISINE, color = bronx$CUISINE)) +
  geom_point(show.legend = FALSE) 

brooklyn_cuisine_score <- chisq.test(table(brooklyn$CUISINE, brooklyn$SCORE))
# data:  table(brooklyn$CUISINE, brooklyn$SCORE)
# X-squared = 10123, df = 8970, p-value < 2.2e-16
brooklyn_zipcode_score <- chisq.test(table(brooklyn$ZIPCODE, brooklyn$SCORE))
# data:  table(brooklyn$ZIPCODE, brooklyn$SCORE)
# X-squared = 4464.8, df = 4600, p-value = 0.9217
ggplot(brooklyn, aes(brooklyn$SCORE, brooklyn$CUISINE, color = brooklyn$CUISINE)) +
  geom_point(show.legend = FALSE) 

manhattan_cuisine_score <- chisq.test(table(manhattan$CUISINE, manhattan$SCORE))
# data:  table(manhattan$CUISINE, manhattan$SCORE)
# X-squared = 10997, df = 9322, p-value < 2.2e-16
manhattan_zipcode_score <- chisq.test(table(manhattan$ZIPCODE, manhattan$SCORE))
# data:  table(manhattan$ZIPCODE, manhattan$SCORE)
# X-squared = 8075.9, df = 9794, p-value = 1
ggplot(manhattan, aes(manhattan$SCORE, manhattan$ZIPCODE, color = manhattan$CUISINE)) +
  geom_point(show.legend = FALSE) 

queens_cuisine_score <- chisq.test(table(queens$CUISINE, queens$SCORE))
# data:  table(queens$CUISINE, queens$SCORE)
# X-squared = 10263, df = 8778, p-value < 2.2e-16
queens_zipcode_score <- chisq.test(table(queens$ZIPCODE, queens$SCORE))
# data:  table(queens$ZIPCODE, queens$SCORE)
# X-squared = 7166.6, df = 7296, p-value = 0.8582  
ggplot(queens, aes(queens$SCORE, queens$ZIPCODE, color = queens$ZIPCODE)) +
  geom_point(show.legend = FALSE) 

staten_island_cuisine_score <- chisq.test(table(staten_island$CUISINE, staten_island$SCORE))
# data:  table(staten_island$CUISINE, staten_island$SCORE)
# X-squared = 3710.2, df = 3975, p-value = 0.9988
staten_island_zipcode_score <- chisq.test(table(staten_island$ZIPCODE, staten_island$SCORE))
# data:  table(staten_island$ZIPCODE, staten_island$SCORE)
# X-squared = 867.75, df = 900, p-value = 0.7745
ggplot(staten_island, aes(staten_island$SCORE, staten_island$ZIPCODE, color = staten_island$ZIPCODE)) +
  geom_point(show.legend = FALSE) 

# FEATURE ENGINEERING FOR MANHATTAN TO IMPROVE VISUALIZATION
as.factor(manhattan$ZIPCODE)
str(manhattan)
length(unique(manhattan$CUISINE)) # 80 unique cuisines

# Top 10 restaurants by volume
as.data.frame(table(manhattan$CUISINE))
# American 12,661
# Chinese 2715
# Italian 2639
# Japanese 2139
# Pizza 1667
# Mexican 1490
# French 1053
# Indian 761
# Delicatessen 716
# Mediterranean 632

patterns <- c('American','Chinese','Italian','Japanese','Pizza',
              'Mexican','French','Indian','Delicatessen','Meditteranean')

manhattan_top10 <- manhattan[manhattan$CUISINE %in% patterns, ]

# HYPOTHESIS TESTING FOR MANHATTAN RESTAURANTS
manhattan_top10_cuisine_score <- chisq.test(table(manhattan_top10$CUISINE, manhattan_top10$SCORE))
# data:  table(manhattan_top10$CUISINE, manhattan_top10$SCORE)
# X-squared = 1348.5, df = 872, p-value < 2.2e-16
manhattan_top10_zipcode_score <- chisq.test(table(manhattan_top10$ZIPCODE, manhattan_top10$SCORE))
# data:  table(manhattan_top10$ZIPCODE, manhattan_top10$SCORE)
# X-squared = 7183.5, df = 8284, p-value = 1

### DATA VISUALIZATIONS ###
# Cuisine and Score - Manhattan
ggplot(manhattan_top10, aes(manhattan_top10$CUISINE, manhattan_top10$SCORE, color = GRADE)) +
  geom_jitter(show.legend = TRUE, size = 0.25) +
  xlab('CUISINE') +
  ylab('RESTAURANT SCORE') 
# Zip Code and Score - Manhattan
ggplot(manhattan_top10, aes(manhattan_top10$SCORE, manhattan_top10$ZIPCODE, color = GRADE)) +
  geom_jitter(show.legend = TRUE, size = 0.25) +
  xlab('RESTAURANT SCORE') +
  ylab('MANHATTAN ZIPCODE') 

# OTHER TESTS CONSIDERED BUT NOT USED IN REPORT

library(rpart.plot)
library(rpart)

art <- rpart(SCORE ~ CUISINE + ZIPCODE, data = manhattan_top10)
rpart.plot(art)
summary(art)
printcp(art)

#CP            nsplit rel error    xerror       xstd
#1 0.01254235      0 1.0000000 1.0001092 0.01923212
#2 0.01000000      1 0.9874577 0.9876399 0.01894708

# Multiple Regression Test
tester1 <- lm(SCORE ~ CUISINE, data = manhattan_top10)
summary(artey)

# ANOVA Test
tester2 <- aov(SCORE ~ CUISINE, data = manhattan_top10)
summary(arte)

# t.test
t.test(manhattan_top10$SCORE)




