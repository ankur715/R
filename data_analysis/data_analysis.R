## Exploratory Data Analysis in R

#working directory
path <- "C:/Users/ankur/Documents/Data Science/Projects/R/R"

#set working directory
setwd(path)

#Load Datasets
train <- read.csv("Train.csv")
test <- read.csv("Test.csv")

#check dimesions ( number of row & columns) in data set
dim(train)
dim(test)

#check the variables and their types in train
str(train)

table(is.na(train))

colSums(is.na(train))

summary(train)

library(ggplot2)
ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + 
  geom_point(size = 2.5, color="navy") + xlab("Item Visibility") + 
  ylab("Item Outlet Sales") + ggtitle("Item Visibility vs Item Outlet Sales")

ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + 
  geom_bar(stat = "identity", color = "purple") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + 
  ggtitle("Outlets vs Total Sales") + theme_bw()

ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + 
  geom_bar( stat = "identity") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + 
  xlab("Item Type") + ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")

ggplot(train, aes(Item_Type, Item_MRP)) +geom_boxplot() +ggtitle("Box Plot") + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + xlab("Item Type") + 
  ylab("Item MRP") + ggtitle("Item Type vs Item MRP")

test$Item_Outlet_Sales <-  1

combi <- rbind(train, test)

combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
table(is.na(combi$Item_Weight))

# Trouble with Continuous Variables & Categorical Variables
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0,
                                median(combi$Item_Visibility), combi$Item_Visibility)

levels(combi$Outlet_Size)[1] <- "Other"
library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,
                                    c("LF" = "Low Fat", "reg" = "Regular"))
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
table(combi$Item_Fat_Content)

# Data Manipulation in R
library(dplyr)
a <- combi%>%
  group_by(Outlet_Identifier)%>%
  tally()
head(a)

names(a)[2] <- "Outlet_Count"

combi <- full_join(a, combi, by = "Outlet_Identifier")

# Count of Item Identifiers
b <- combi%>%
  group_by(Item_Identifier)%>%
  tally()

names(b)[2] <- "Item_Count"
head(b)

combi <- merge(b, combi, by = "Item_Identifier")
combi

# outlet years
c <- combi%>%
  select(Outlet_Establishment_Year)%>% 
  mutate(Outlet_Year = 2013 - combi$Outlet_Establishment_Year)
head(c)

# Item Type New
q <- substr(combi$Item_Identifier,1,2)
q <- gsub("FD","Food",q)
q <- gsub("DR","Drinks",q)
q <- gsub("NC","Non-Consumable",q)
table(q)

combi$Item_Type_New <- q

## Label Encoding and One Hot Encoding
combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)
sample <- select(combi, Outlet_Location_Type)
demo_sample <- data.frame(model.matrix(~.-1,sample))
head(demo_sample)

# Predictive Modeling using Machine Learning
combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Item_Fat_Content, Outlet_Establishment_Year,Item_Type))
str(combi)
new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]

# Linear (Multiple) Regression
linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train)
summary(linear_model)


