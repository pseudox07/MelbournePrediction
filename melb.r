#load needed library
library(glmnet)
library(lava)
library(stringr)
library(dplyr)
library(tidyr)
library(dummies)
library(ggcorrplot)
library(corrplot)
library(VIF)
library(MASS)
library(car)
library(rpart.plot)
library(tree)
library(Metrics)
library(randomForest)
library(gvlma)
library(party)

options(scipen=999)  #removing scientific notations.
options(max.print=1000000) #maximum print

#import data from full dataset
melb <- read.csv("melb/Melbourne_housing_FULL.csv")

#getting NA
missing_NA <- function(dataset){
  
  Total_NA <- sum(is.na(dataset))
  Column_sums <- colSums(is.na(dataset))
  cat("Total NA in the dataset in all in the columns- \n\n",Total_NA)
  cat("\n--------------##-----------------")
  Column_names <- colnames(dataset)[apply(dataset,2,anyNA)]
  cat('\n\n Names of NA columns in the dataset-\n\n',Column_names)
  cat('\n\n Total NA by column in the dataset-\n\n',Column_sums)
  cat("\n--------------##-----------------")
}

#getting data overview fucntion
data_overview <- function(dataset){
  data <- dim(dataset)
  cat("\nTotal Number of [rows vs columns] in the dataset- \n",data)
  cat("\n--------------##-----------------")
  Column_datatypes <- sapply(dataset,class)
  cat('\n\n Datatypes of all the columns in the dataset-\n',Column_datatypes)
  cat("\n--------------##-----------------")
  Column_Names <- colnames(dataset)
  cat('\n\n Names of all the columns in the dataset-\n',Column_Names)    
}


data_overview(melb)
missing_NA(melb)

#preprocessing data

#correcting data types
#checking datatypes of all column in dataset
melb$Suburb <- as.factor(melb$Suburb)
melb$Address <- as.character(melb$Address)    #changing dataset from factor to character
melb$Rooms <- as.factor(melb$Rooms)   #changing dataset from integer to factor
melb$Method <- as.factor(melb$Method)
melb$Distance<- as.integer(melb$Distance)   #changing dataset from factor to integer
melb$Bathroom <- as.factor(melb$Bathroom)    #changing dataset from integer to factor
melb$Car <- as.numeric(melb$Car)            #changing dataset from integer to factor
melb$Postcode <- as.factor(melb$Postcode)
melb$SellerG <- as.factor(melb$SellerG)
melb$Bathroom <- as.factor(melb$Bathroom )
melb$CouncilArea <- as.factor(melb$CouncilArea )
melb$Regionname  <- as.factor(melb$Regionname  )
melb$Propertycount  <- as.factor(melb$Propertycount  )
melb$Type  <- as.factor(melb$Type  )
sapply(melb, class)   #Re-checking datatypes of all column in dataset

#checking NA values in each column

colSums(is.na(melb))

#Remove NA values of Price as its dependent variable
melb1 <- subset(melb,(!is.na(melb[,5])))
colSums(is.na(melb1))
dim(melb1)

#Remove BuildingArea Column as it consist more then 60% of NA values
melb2 <- melb1[,c(1:14,16:21)]
colSums(is.na(melb2))
dim(melb2)

melb4 <- melb2

#73% of the data for the rooms and Bedrooms is same i.e example if rooms==2 then bedroom2 ==2
temp <- melb4[,c("Rooms","Bedroom2")]
bedroom2 <- temp[which(temp$Rooms == temp$Bedroom2),]
(length(bedroom2$Rooms) / length(melb4$Rooms)) * 100

#thus assigning the NA's of Bedrooms with the values of rooms.
my.na <- is.na(melb4$Bedroom2)
melb4$Bedroom2[my.na] <- melb4$Rooms[my.na]
colSums(is.na(melb4))

#outliers checking and elimination
boxplot(melb4$Price ~ melb4$Regionname, horizontal = TRUE, ylab = "REGION NAME", xlab = "PRICE OF HOUSE", main = "BOXPLOT: PRICE OF HOUSE BY REGION", las = 1)
boxplot(melb4$Landsize ~ melb4$Regionname, horizontal = TRUE, xlab = "Landsize of houses", ylab = "Region Name", main = "BOXPLOT OF LANDSIZE OF HOUSES BY REGION", las = 1)
boxplot(melb4$Distance ~ melb4$Type, horizontal = TRUE, ylab = "Type of House", xlab = "Distance from CBD", main = "Boxplot of distance from CBD vs type of houses", las = 1)

#Checking outliers based on summary
boxplot(melb4$Price,main = "PRICE OF HOUSES.")
melb4$Rooms <- as.integer(melb4$Rooms)
boxplot(melb4$Rooms,main = "NUMBER OF ROOMS.")
boxplot(melb4$Bedroom2,main = "NUMBER OF BEDROOMS.")
boxplot(melb4$Landsize,main="LANDSIZE OF HOUSES.")

#Removing outlier from Rooms column
outliers <- boxplot(melb4$Rooms, plot=FALSE)$out
melb4[which(melb4$Rooms %in% outliers),]
#outliers removed for rooms
melb4 <- melb4[-which(melb4$Rooms %in% outliers),]
boxplot(melb4$Rooms ,main = "NUMBER OF ROOMS.(OUTLIERS REMOVED)")

#Removing outlier from Bedroom2 column
outliers <- boxplot(melb4$Bedroom2, plot=FALSE)$out
melb4[which(melb4$Bedroom2 %in% outliers),]
melb4 <- melb4[-which(melb4$Bedroom2 %in% outliers),]
boxplot(melb4$Bedroom2, main="NUMBER OF BEDROOMS.(OUTLIERS REMOVED)")

#Removing outlier from Landsize column
outliers <- boxplot(melb4$Landsize, plot=FALSE)$out
melb4[which(melb4$Landsize %in% outliers),]
melb4 <- melb4[-which(melb4$Landsize %in% outliers),]
boxplot(melb4$Landsize,main = "HOUSES LANDSIZEs.(OUTLIERS REMOVED)")

#landsize column fixation
#Making new dataframe of Bedrooms & Landsize
bed.land.df <- melb4[,c("Bedroom2","Landsize")]
unique(bed.land.df$Bedroom2)
colSums(is.na(bed.land.df))
bed.land.df <- na.omit(bed.land.df)
bed.land.df <- bed.land.df[which(bed.land.df$Landsize > 0),]

colSums(is.na(bed.land.df))

bed.land.df_0 <- bed.land.df[which(bed.land.df$Bedroom2 == 0),]
bed.land.df_1 <-  bed.land.df[which(bed.land.df$Bedroom2 == 1),]
bed.land.df_2 <- bed.land.df[which(bed.land.df$Bedroom2 == 2),]
bed.land.df_3 <-  bed.land.df[which(bed.land.df$Bedroom2 == 3),]
bed.land.df_4 <- bed.land.df[which(bed.land.df$Bedroom2 == 4),]
bed.land.df_5 <-  bed.land.df[which(bed.land.df$Bedroom2 == 5),]
bed.land.df_6 <- bed.land.df[which(bed.land.df$Bedroom2 == 6),]
bed.land.df_7 <-  bed.land.df[which(bed.land.df$Bedroom2 == 7),]

#Replacing Na values with 0
melb4$Landsize[which(is.na(melb4$Landsize))] <- 0

#120 logic is used here under the assumption that minimun sq feet required is 120 mtrs

melb4$Landsize[which(melb4$Landsize < 120)] <- 0

#Replacing 0 values with median values
melb4$Landsize[which(melb4$Bedroom2 == 0 & melb4$Landsize== 0)] <- median(bed.land.df_0$Landsize[which(bed.land.df_0$Landsize > 1)]) 
melb4$Landsize[which(melb4$Bedroom2 == 1 & melb4$Landsize== 0)] <- median(bed.land.df_1$Landsize[which(bed.land.df_1$Landsize > 1)]) 
melb4$Landsize[which(melb4$Bedroom2 == 2 & melb4$Landsize== 0)] <- median(bed.land.df_2$Landsize[which(bed.land.df_2$Landsize > 1)]) 
melb4$Landsize[which(melb4$Bedroom2 == 3 & melb4$Landsize== 0) ] <- median(bed.land.df_3$Landsize[which(bed.land.df_3$Landsize > 1)]) 
melb4$Landsize[which(melb4$Bedroom2 == 4 & melb4$Landsize== 0) ] <- median(bed.land.df_4$Landsize[which(bed.land.df_4$Landsize > 1)]) 
melb4$Landsize[which(melb4$Bedroom2 == 5 & melb4$Landsize== 0) ] <- median(bed.land.df_5$Landsize[which(bed.land.df_5$Landsize > 1)]) 
melb4$Landsize[which(melb4$Bedroom2 == 6 & melb4$Landsize== 0) ] <- median(bed.land.df_6$Landsize[which(bed.land.df_6$Landsize > 1)]) 
melb4$Landsize[which(melb4$Bedroom2 == 7 & melb4$Landsize== 0) ] <- median(bed.land.df_7$Landsize[which(bed.land.df_7$Landsize > 1)]) 

#Checking if all the value got atleast 100 those are zero
melb4$Landsize[which(melb4$Landsize < 120)]
summary(melb4)

#Car Column
#Putting median in all the NA values of Car column
melb4$Car <- as.numeric(melb4$Car)
melb4$Car[is.na(melb4$Car)] <- median(melb4$Car[which(!is.na(melb4$Car))])
colSums(is.na(melb4))

#Putting 0 in all the NA values of YearBuilt column
melb4$YearBuilt <- as.numeric(melb4$YearBuilt)
melb4$YearBuilt[which(is.na(melb4$YearBuilt))] <- 0

#Bathrooms column fixation
#a ratio of 3:2 to maximise its value and desirability.
#So 
#1 - 1
#2 - 1
#3 - 2
#4 - 2.5
#5 - 3
#6 - 4
#7 - 4.5

melb4$Bathroom <- as.integer(melb4$Bathroom)
melb4$Bathroom[which(is.na(melb4$Bathroom) & melb4$Bedroom2== 1)] <- 1
melb4$Bathroom[which(is.na(melb4$Bathroom) & melb4$Bedroom2== 2)] <- 1
melb4$Bathroom[which(is.na(melb4$Bathroom) & melb4$Bedroom2== 3)] <- 2
melb4$Bathroom[which(is.na(melb4$Bathroom) & melb4$Bedroom2== 4)] <- 2.5
melb4$Bathroom[which(is.na(melb4$Bathroom) & melb4$Bedroom2== 5)] <- 3
melb4$Bathroom[which(is.na(melb4$Bathroom) & melb4$Bedroom2== 6)] <- 4
melb4$Bathroom[which(is.na(melb4$Bathroom) & melb4$Bedroom2== 7)] <- 4.5

summary(melb4)

melb4 <- melb4[which(melb4$CouncilArea != '#N/A'),]
colSums(is.na(melb4))
dim(melb4)

#reverting back the datatypes which were changed in order to calculate the median.
melb4$Car <- as.numeric(melb4$Car)
melb4$Bathroom <- as.factor(melb4$Bathroom)
melb4$YearBuilt <- as.factor(melb4$YearBuilt)
melb4$Rooms <- as.factor(melb4$Rooms)

#omitting the missing values from lattitude and longitude
melb4 <- na.omit(melb4)
#------------
str(melb4)
#dropping unused levels from the dataframe.
melb4$Postcode <- droplevels(melb4$Postcode)
melb4$CouncilArea <- droplevels(melb4$CouncilArea)
melb4$Regionname <- droplevels(melb4$Regionname)
melb4$Propertycount <- droplevels(melb4$Propertycount)
str(melb4)

table(melb4$CouncilArea)
#-------------
#converting RegionName into numeric
melb4$Regionname <- as.character(melb4$Regionname)
melb4$Regionname[melb4$Regionname == 'Eastern Metropolitan'] <- 1
melb4$Regionname[melb4$Regionname == 'Eastern Victoria'] <- 2
melb4$Regionname[melb4$Regionname == 'Northern Metropolitan'] <- 3
melb4$Regionname[melb4$Regionname == 'Northern Victoria'] <- 4
melb4$Regionname[melb4$Regionname == 'South-Eastern Metropolitan'] <- 5
melb4$Regionname[melb4$Regionname == 'Southern Metropolitan'] <- 6
melb4$Regionname[melb4$Regionname == 'Western Metropolitan'] <- 7
melb4$Regionname[melb4$Regionname == 'Western Victoria'] <- 8

#converting method into numeric
melb4$Method = as.character(melb4$Method)
melb4$Method[melb4$Method == 'PI'] <- 1
melb4$Method[melb4$Method == 'PN'] <- 2
melb4$Method[melb4$Method == 'S'] <- 3
melb4$Method[melb4$Method == 'SA'] <- 4
melb4$Method[melb4$Method == 'SN'] <- 5
melb4$Method[melb4$Method == 'SP'] <- 6
melb4$Method[melb4$Method == 'SS'] <- 7
melb4$Method[melb4$Method == 'VB'] <- 8
melb4$Method[melb4$Method == 'W'] <- 9

#converting type into numeric
melb4$Type <- as.character(melb4$Type)
melb4$Type[melb4$Type == 'h'] <- 1
melb4$Type[melb4$Type == 't'] <- 2
melb4$Type[melb4$Type == 'u'] <- 3



melb4 <- melb4 %>% separate(Date,sep = "/",into = c("Day","Month","Year"))

#converting month into season.
#spring (March, April, May), 
#summer (June, July, August), 
#autumn (September, October, November) 
#winter (December, January, February).

melb4$Season <- melb4$Month
melb4$Season <- as.numeric(melb4$Season)
melb4$Season[which(melb4$Season == 3 | melb4$Season == 4 | melb4$Season == 5)] = "Spring"
melb4$Season[which(melb4$Season == 6 |melb4$Season == 7 | melb4$Season == 8)] = "Summer"
melb4$Season[which(melb4$Season == 9 | melb4$Season == 10 | melb4$Season == 11)] = "Autumn"
melb4$Season[which(melb4$Season == 12 | melb4$Season == 1 | melb4$Season == 2)] = "Winter"

melb4$Season <- as.character(melb4$Season)
melb4$Season[melb4$Season == 'Spring'] <- 1
melb4$Season[melb4$Season == 'Summer'] <- 2
melb4$Season[melb4$Season == 'Autumn'] <- 3
melb4$Season[melb4$Season == 'Winter'] <- 4

#ignore this for colorosation of the graph
mycolors <- c("#771C19", "#AA3929", "#8E9CA3", "#556670", "#000000",
              "#E25033", "#F27314", "#F8A31B", "#E2C59F", "#B6C5CC",
              "#99CCCC","#FFCC99")

mytheme <- theme(axis.text.x = element_text(angle = 90, size = 10, vjust = .4),
                 plot.title = element_text(size = 15, vjust = 2),
                 axis.title.x = element_text(size = 12, vjust = -.35))

mytheme2 <- theme(axis.text.x = element_text(size = 10, vjust = .4),
                  plot.title = element_text(size = 15, vjust = 2),
                  axis.title.x = element_text(size = 12, vjust = -.35))
#price dsitribution
melb$Date <- dmy(as.character(melb$Date))
Price_Date <- melb[c("Suburb","Price","Date")] %>% na.omit()
averPrice_date <- Price_Date %>% group_by(Date) %>%
  summarise(Average = sum(Price)/n())

ggplot(Price_Date, aes(Price))+
  geom_histogram(binwidth = 100000,color = "white", fill = "#771C19")+
  mytheme2+
  scale_x_continuous(breaks = c(1000000,2000000,3000000,4000000),
                     labels = c("$1m","$2m","$3m","$4m"))+
  ggtitle("House Price Distribution in Melbourne")

#top 10 suburbs w houses
top10sub_by_houses <- melb %>% group_by(Suburb) %>%
  summarise(Number = n()) %>% arrange(desc(Number)) %>%
  head(10)

suburb_vs_price <- melb[c("Suburb","Price")] %>% na.omit() #remove 2688 NA observations
top10sub_by_averprice <- suburb_vs_price %>% group_by(Suburb) %>%
  summarise(Average = sum(Price)/n()) %>%
  arrange(desc(Average)) %>% head(10)

ggplot(top10sub_by_averprice, aes(reorder(Suburb, Average), Average, fill = Suburb))+
  geom_bar(stat = "identity")+
  mytheme2+
  theme(legend.position = "none")+
  labs(x = "Suburb", y = "Average Price of House",
       title = "Top 10 Suburbs by the Average Price of House")+
  scale_fill_manual(values = mycolors)+
  coord_flip()

#price trend
pricetrend_by_top10sub <- Price_Date[Price_Date$Suburb %in% top10sub_by_averprice$Suburb,] %>%
  group_by(Suburb, Date) %>% summarise(Average = sum(Price)/n())

ggplot(pricetrend_by_top10sub, aes(Date, Average))+
  geom_line(aes(color = Suburb))+
  facet_wrap(~Suburb, nrow = 5)+
  mytheme+
  theme(legend.position = "none")+
  labs(y = "Average Price", title = "Top10 Suburb Average Price Trend")+
  scale_color_manual(values = mycolors)+
  scale_y_continuous(breaks = c(1000000,2000000,3000000,4000000),
                     labels = c("$1m","$2m","$3m","$4m"))

#correlation checking of data
my_corrdata <- melb4[,-c(1,2,7,18)]
#converting the datacolumn into numeric
my_corrdata$Regionname <- as.numeric(my_corrdata$Regionname)
my_corrdata$Method <- as.numeric(my_corrdata$Method)
my_corrdata$Type <- as.numeric(my_corrdata$Type)
my_corrdata$Rooms <- as.numeric(my_corrdata$Rooms)
my_corrdata$Distance <- as.numeric(my_corrdata$Distance)
my_corrdata$Postcode <- as.numeric(my_corrdata$Postcode)
my_corrdata$Bedroom2 <- as.numeric(my_corrdata$Bedroom2)
my_corrdata$Bathroom <- as.numeric(my_corrdata$Bathroom)
my_corrdata$Car <- as.numeric(my_corrdata$Car)
my_corrdata$YearBuilt <- as.numeric(my_corrdata$YearBuilt)
my_corrdata$Day <- as.numeric(my_corrdata$Day)
my_corrdata$Month <- as.numeric(my_corrdata$Month)
my_corrdata$Year <- as.numeric(my_corrdata$Year)
my_corrdata$Propertycount <- as.numeric(my_corrdata$Propertycount)
my_corrdata$Season <- as.numeric(my_corrdata$Season)
corr <- round(cor(my_corrdata),1)

corrplot(corr)

#normalization of data
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

melb4$Landsize <- normalize(melb4$Landsize)
melb4$Distance <- normalize(melb4$Distance)

melb4$Rooms <- as.numeric(melb4$Rooms)
melb4$Rooms <- normalize(melb4$Rooms)

melb4$Bathroom <- as.numeric(melb4$Bathroom)
melb4$Bathroom <- normalize(melb4$Bathroom)


melb4$Car <- as.numeric(melb4$Car)
melb4$Car <- normalize(melb4$Car)

melb4$Propertycount <- as.numeric(melb4$Propertycount)
melb4$Propertycount <- normalize(melb4$Propertycount)
colnames(melb4)

head(melb4)

#one hot encoding and datatype corrections
melb4$Regionname <- as.factor(melb4$Regionname) #one hot needed
melb4$Method <- as.factor(melb4$Method) #one hot needed
melb4$Type <- as.factor(melb4$Type) #one hot needed 
melb4$Rooms <- as.factor(melb4$Rooms) 
melb4$Car <- as.numeric(melb4$Car)
melb4$Bedroom2 <- as.factor(melb4$Bedroom2) 
melb4$Bathroom <- as.factor(melb4$Bathroom) 
melb4$YearBuilt <- as.numeric(melb4$YearBuilt)
melb4$Day <- as.numeric(melb4$Day)
melb4$Month <- as.factor(melb4$Month) #as we have create a new variable season using Month we will not be using month
melb4$Propertycount <- as.character(melb4$Propertycount) 
melb4$Propertycount <- as.numeric(melb4$Propertycount)
melb4$Season <- as.numeric(melb4$Season) #one hot encoding needed

#one hot encoding of type
type_ <- factor(melb4$Type)
dumm <- as.data.frame(model.matrix(~type_)[,-1])
melb4 <- cbind(dumm,melb4)

#one hot encoding of Method
Method_ <- factor(melb4$Method)
dumm <- as.data.frame(model.matrix(~Method_)[,-1])
melb4 <- cbind(dumm,melb4)

#one hot encoding of season
Season_ <- factor(melb4$Season)
dumm <- as.data.frame(model.matrix(~Season_)[,-1])
melb4 <- cbind(dumm,melb4)

#replace space in council with _
melb4$CouncilArea <- str_replace_all(melb4$CouncilArea,c(" "="_"))
Council_ <- factor(melb4$CouncilArea)
dumm <- as.data.frame(model.matrix(~Council_)[,-1])
melb4 <- cbind(dumm,melb4)


colnames(melb4)

#removing the Rooms from the final dataset as we observed high collinearity between Bedroom2 and Rooms
test_df <- melb4[,-c(42,43,45,47,48,49,50,51,53,54,58,59,62,63,64)]
colnames(test_df)
#splitting the data into train and test
create_train_test <- function(data, size = 0.7, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

set.seed(123)
train_data <- create_train_test(test_df,train = TRUE)
test_data <- create_train_test(test_df,train = FALSE)

#Modelling

#linear regression

colnames(train_data)

#full model 
model_l <- lm(Price ~ .,data = train_data)
summary(model_l)
plot(model_l)

vif(model_l)
options(warn=-1)
predicted_ys <- predict(model_l,newdata=test_data)
observed_ys <- test_data$Price
SSE <- sum((observed_ys - predicted_ys) ^ 2)
SST <- sum((observed_ys - mean(observed_ys)) ^ 2)
r2 <- 1 - SSE/SST
r2_results <- data_frame(method = "Linear Regression", r2)
r2

#stepwise

step_model <- stepAIC(model_l,direction="both",trace=1)
summary(step_model)
step_model$anova
options(warn=-1)

predicted_ys <- predict(step_model,newdata=test_data)
observed_ys <- test_data$Price
sm.SSE <- sum((observed_ys - predicted_ys) ^ 2)
sm.SST <- sum((observed_ys - mean(observed_ys)) ^ 2)
sm.r2 <- 1 - sm.SSE/sm.SST
r2_results <- bind_rows(r2_results,
                        data_frame(method="Stepwise Model",  
                                   sm.r2 ))
sm.r2
vif(step_model)

#lasso
x <- model.matrix(Price~.,data=train_data)
x_train <- x[,-1]
y_train <- train_data$Price
crossval <-  cv.glmnet(x = x_train, y = y_train)
plot(crossval)
penalty <- crossval$lambda.min
penalty
fit1 <-glmnet(x = x_train, y = y_train, alpha = 1, lambda = penalty ) #estimate the model with that
c <- coef(fit1)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables<-variables[variables %ni% '(Intercept)']
variables
summary(fit1)

x_1 <-model.matrix(Price~.,data=test_data)
x_test <- x_1[,-1]
y_test <- test_data$Price

summary(x_1)
predicted_ys <- predict(fit1, s = penalty, newx = x_test)
observed_ys <- test_data$Price
lm.SSE <- sum((observed_ys - predicted_ys) ^ 2)
lm.SST <- sum((observed_ys - mean(observed_ys)) ^ 2)
lm.r2 <- 1 - lm.SSE/lm.SST
r2_results <- bind_rows(r2_results,
                        data_frame(method="Lasso Regression",  
                                   lm.r2 ))
lm.r2

#decision tree
set.seed(123)


tree.model <- rpart(Price ~ .,data = train_data)
summary(tree.model)
rpart.plot(tree.model,type = 5,extra=101)

# prune the tree 
pfit<- prune(tree.model, cp=tree.model$cptable[which.min(tree.model$cptable[,"xerror"]),"CP"])
rpart.plot(pfit,type = 5,extra=101)

tree.pred <- predict(pfit,newdata = test_data)
dt.SSE <- sum((test_data[,"Price"]-tree.pred)^2)
dt.SST <- sum((test_data[,"Price"] - mean(test_data[,"Price"]))^2)
dt.r2 <- 1 - dt.SSE/dt.SST
r2_results <- bind_rows(r2_results,
                        data_frame(method="Decision Tree",  
                                   dt.r2 ))
dt.r2

#random forest
rf <- randomForest(Price ~ .,data = train_data)
rf.pred <- predict(rf,newdata = test_data)
SSE.rf <- sum((test_data[,"Price"]-rf.pred)^2)
SST.rf <- sum((test_data[,"Price"] - mean(test_data[,"Price"]))^2)
r2.rf <- 1 - SSE.rf/SST.rf
r2_results <- bind_rows(r2_results,
                        data_frame(method="Random Forest",  
                                   r2.rf ))
r2.rf
