food_table<-read.csv(file.choose(), header = TRUE)
food_table<-data.frame(food_table)

head(food_table)
tail(food_table)
summary(food_table)
dim(food_table)
class(food_table)

ggplot(food_table) + 
  geom_bar(mapping = aes(x = food_table$food_type), fill = "magenta")

##the normalization function is created
nor <-function(x) {(x-min(x))/(max(x)-min(x))}

##Run nomalization on first 4 coulumns of dataset because they are the predictors
food_table_norm <- as.data.frame(lapply(food_table[,c(15,16,17,18,19,20,21,22,23,24,25,26,27)], nor))


boxplot(food_table_norm$Saturated_Fats,food_table_norm$Alcohol, food_table_norm$Solid_Fats, food_table_norm$Added_Sugars,food_table_norm$Calories, col = "Yellow")


#Heatmap
data <- as.matrix(food_table_norm)
heatmap(data, col = terrain.colors(256))

#sorted my dataset in the increasing order of Saturated_fats
food_table<-food_table[order(food_table$Saturated_Fats), ]
plot(food_table$Saturated_Fats)
plot(food_table$Solid_Fats)
plot(food_table$Calories)
plot(food_table$Added_Sugars)
plot(food_table$Alcohol)

cor(food_table$Milk, food_table$Calories)
cor(food_table$Meats, food_table$Calories)
cor(food_table$Soy, food_table$Calories)
cor(food_table$Drybeans_Peas, food_table$Calories)
cor(food_table$Oils, food_table$Calories)
cor(food_table$Solid_Fats, food_table$Calories)
cor(food_table$Added_Sugars, food_table$Calories)
cor(food_table$Alcohol, food_table$Calories)
cor(food_table$Saturated_Fats, food_table$Calories)

#calories consumed from different categories of food that are consumed by Americans in general
n<-nrow(food_table)
d<-ncol(food_table)

meat_fish = subset(food_table, food_table$food_type == "Meat/fish" )
avg_calories_meat_fish = sum(meat_fish$Calories)/nrow(meat_fish)
avg_calories_meat_fish #average calories consumed from Meat/fish category

beans_tofu = subset(food_table, food_table$food_type == "beans/tofu" )
avg_calories_beans_tofu = sum(beans_tofu$Calories)/nrow(beans_tofu)
avg_calories_beans_tofu #average calories consumed from beans/tofu category

milk_milkproducts = subset(food_table, food_table$food_type == "milk/milk products" )
avg_calories_milk_milkproducts = sum(milk_milkproducts$Calories)/nrow(milk_milkproducts)
avg_calories_milk_milkproducts #average calories consumed from milk/milk products category

greenveg = subset(food_table, food_table$food_type == "Green vegetables" )
avg_calories_greenveg = sum(greenveg$Calories)/nrow(greenveg)
avg_calories_greenveg #average calories consumed from Green vegetables category

nocolor_vegetables = subset(food_table, food_table$food_type == "No color vegetables" )
avg_calories_nocolor_vegetables = sum(nocolor_vegetables$Calories)/nrow(nocolor_vegetables)
avg_calories_nocolor_vegetables #average calories consumed from No color vegetables category

Fruits = subset(food_table, food_table$food_type == "Fruits" )
avg_calories_Fruits = sum(Fruits$Calories)/nrow(Fruits)
avg_calories_Fruits #average calories consumed from Fruits category

Grains = subset(food_table, food_table$food_type == "Grains" )
avg_calories_Grains = sum(Grains$Calories)/nrow(Grains)
avg_calories_Grains #average calories consumed from Grains category

Sweets = subset(food_table, food_table$food_type == "Sweets" )
avg_calories_Sweets = sum(Sweets$Calories)/nrow(Sweets)
avg_calories_Sweets #average calories consumed from Sweets category

Coffee = subset(food_table, food_table$food_type == "Coffee" )
avg_calories_Coffee = sum(Coffee$Calories)/nrow(Coffee)
avg_calories_Coffee #average calories consumed from Coffee category

Tea = subset(food_table, food_table$food_type == "Tea" )
avg_calories_Tea = sum(Tea$Calories)/nrow(Tea)
avg_calories_Tea #average calories consumed from Tea category

Snacks = subset(food_table, food_table$food_type == "Snacks" )
avg_calories_Snacks = sum(Snacks$Calories)/nrow(Snacks)
avg_calories_Snacks #average calories consumed from Snacks category

Soft_drinks = subset(food_table, food_table$food_type == "Soft drinks" )
avg_calories_Soft_drinks = sum(Soft_drinks$Calories)/nrow(Soft_drinks)
avg_calories_Soft_drinks #average calories consumed from Soft_drinks category

Pickles = subset(food_table, food_table$food_type == "Pickles" )
avg_calories_Pickles = sum(Pickles$Calories)/nrow(Pickles)
avg_calories_Pickles #average calories consumed from Pickles category

Fried_food = subset(food_table, food_table$food_type == "Fried food" )
avg_calories_Fried_food = sum(Fried_food$Calories)/nrow(Fried_food)
avg_calories_Fried_food #average calories consumed from Fried food category

#total amount of fats consumned from different food items
totalamt = food_table$Portion_Amount*food_table$Saturated_Fats
sumtotalamt = sum(totalamt)
fat_per_item = sumtotalamt/2000
fat_per_item

#total amount of calories consumned from different food items
totalcalories = food_table$Portion_Amount*food_table$Calories
sum_total_calories = sum(totalcalories)
calorie_per_item = sum_total_calories/2000
calorie_per_item

#total amount of sugars consumned from different food items
totalsugars = food_table$Portion_Amount*food_table$Added_Sugars
sum_total_sugars = sum(totalsugars)
sugar_per_item = sum_total_sugars/2000
sugar_per_item

#total amount vegetables consumed from different food items
totalveg = food_table$Portion_Amount*food_table$Vegetables
sum_total_vegetables = sum(totalveg)
veg_per_item = sum_total_vegetables/2000
veg_per_item

#total amount alcohol consumed from different food items
totalalcohol = food_table$Portion_Amount*food_table$Alcohol
sum_total_alcohol = sum(totalalcohol)
alc_per_item = sum_total_alcohol/2000
alc_per_item


plot(totalamt)

#food intake by Obese people
ObeseData<-read.csv(file.choose(), header = TRUE)
ObeseData<-data.frame(ObeseData)

average_foodtype_intake = ObeseData$times_per_week*ObeseData$Total_number
average_foodtype_intake

average_times_meat_fish_intake = sum(average_foodtype_intake[0:4])/111
average_times_meat_fish_intake
average_times_beans_tofu_intake = sum(average_foodtype_intake[4:8])/111
average_times_beans_tofu_intake
average_times_milk_intake = sum(average_foodtype_intake[8:12])/111
average_times_milk_intake
average_times_greenveg_intake  = sum(average_foodtype_intake[12:16])/111
average_times_greenveg_intake
average_times_nocolorveg_intake = sum(average_foodtype_intake[16:20])/111
average_times_nocolorveg_intake
average_times_fruits_intake = sum(average_foodtype_intake[20:24])/111
average_times_fruits_intake
average_times_grains_intake = sum(average_foodtype_intake[24:28])/111
average_times_grains_intake
average_times_sweets_intake = sum(average_foodtype_intake[28:30])/138
average_times_sweets_intake
average_times_coffee_intake = sum(average_foodtype_intake[30:32])/138
average_times_coffee_intake
average_times_tea_intake = sum(average_foodtype_intake[32:34])/138
average_times_tea_intake
average_times_snacks_intake = sum(average_foodtype_intake[34:36])/138
average_times_snacks_intake
average_times_softdrinks_intake = sum(average_foodtype_intake[36:38])/138
average_times_softdrinks_intake
average_times_pickle_intake = sum(average_foodtype_intake[38:40])/138
average_times_pickle_intake
average_times_fried_intake = sum(average_foodtype_intake[40:42])/138
average_times_fried_intake

#calorie intake by obesse people from different food categories
weekly_calories_meat_fish = avg_calories_meat_fish*average_times_meat_fish_intake #weekly calorie intake from meat/fish
weekly_calories_beans_tofu = avg_calories_beans_tofu*average_times_beans_tofu_intake
weekly_calories_milk = avg_calories_milk_milkproducts*average_times_milk_intake
weekly_calories_greenveg = avg_calories_greenveg*average_times_greenveg_intake
weekly_calories_nocolorveg = avg_calories_nocolor_vegetables*average_times_nocolorveg_intake
weekly_calories_fruits = avg_calories_Fruits*average_times_fruits_intake
weekly_calories_grains = avg_calories_Grains*average_times_grains_intake
weekly_calories_sweets = avg_calories_Sweets*average_times_sweets_intake
weekly_calories_coffee = avg_calories_Coffee*average_times_coffee_intake
weekly_calories_tea = avg_calories_Tea*average_times_tea_intake
weekly_calories_snacks = avg_calories_Snacks*average_times_snacks_intake
weekly_calories_softdrinks = avg_calories_Soft_drinks*average_times_softdrinks_intake
weekly_calories_pickle = avg_calories_Pickles*average_times_pickle_intake
weekly_calories_friedfood = avg_calories_Fried_food*average_times_fried_intake


weekly_calories_meat_fish
weekly_calories_beans_tofu
weekly_calories_milk
weekly_calories_greenveg
weekly_calories_nocolorveg
weekly_calories_fruits
weekly_calories_grains
weekly_calories_sweets
weekly_calories_coffee
weekly_calories_tea
weekly_calories_snacks
weekly_calories_softdrinks
weekly_calories_pickle
weekly_calories_friedfood

#Model1: Linear Regression
mm <- lm(food_table_norm$Calories ~ food_table_norm$Saturated_Fats)  # build linear regression model on full data
print(mm)
summary(mm)  # model summary

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(food_table_norm), 0.8*nrow(food_table_norm))  # row indices for training data
trainingData <- food_table_norm[trainingRowIndex, ]  # model training data
testData  <- food_table_norm[-trainingRowIndex, ]  

# Build the model on training data -
lmMod <- lm(food_table_norm$Calories ~ food_table_norm$Saturated_Fats  + food_table_norm$Solid_Fats , data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

summary (lmMod) 

actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)

#Model2: KKNN

food_table<-read.csv(file.choose(), header = TRUE) 
food_table<-data.frame(food_table)
#Calories.factor
food_table$Calories.factor <- factor(food_table$Calories)
food_table$Calories.cat <- NA
food_table$Calories.cat <- ifelse(food_table$Calories>=200, 'High Calorie food item', food_table$Calories.cat)
food_table$Calories.cat <- ifelse((food_table$Calories<200 & food_table$Calories>100) , 'Medium Calorie Item', food_table$Calories.cat)
food_table$Calories.cat <- ifelse(food_table$Calories<=100, 'Low Calorie Item', food_table$Calories.cat)

food_table$Calories.cat <- factor(food_table$Calories.cat, levels = c("High Calorie food item", "Medium Calorie Item", "Low Calorie Item"))

##Generate a random number that is 90% of the total number of rows in dataset.
ran <- sample(1:nrow(food_table), 0.8 * nrow(food_table)) 

##the normalization function is created
nor <-function(x) {(x-min(x))/(max(x)-min(x))}

##Run nomalization on the selected coulumns of dataset because they are the predictors
food_table_norm <- as.data.frame(lapply(food_table[,c(10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,27)], nor))

summary(food_table_norm)

##extract training set
food_table_train <- food_table_norm[ran,] 
##extract testing set
food_table_test <- food_table_norm[-ran,] 
##extract 12th column of train dataset because it will be used as 'cl' argument in knn function.
food_table_target_category <- food_table[ran,26]
##extract 12th column if test dataset to measure the accuracy
food_table_test_category <- food_table[-ran,26]
##load the package class
library(class)
##run knn function
pr <- knn(food_table_train, food_table_test, cl=food_table_target_category, k=3)

##create confusion matrix
tab <- table(pr,food_table_test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
pr

