#load necessary libraries
library(caTools)
library(tidyverse)
library(naniar)
library(ggplot2)
library(dplyr)
library(corrplot)
library(dummy)
library(ggplot2)
library(lattice)
library(caret)


#Load the dataset
cost_data <- read.csv("/data/cost_of_living_us.csv")
cost_data
#<-----------------------------------Pre Processing ------------------------------------------->
#Display the number of rows and columns
cat("Number of rows:", nrow(cost_data), "\n")
cat("Number of columns:", ncol(cost_data), "\n")

#Print out the name of the columns in the dataset 
colnames(cost_data)

#summary statistics for numeric variables
summary(cost_data)
str(cost_data)

#Check for missing values in the entire dataset
missing_values <- colSums(is.na(cost_data))
missing_values

#Visualize missing values using a heatmap
missing_plot <- gg_miss_var(cost_data)
print(missing_plot)

#Impute missing values in 'median_family_income' with the mean of the column
cost_data <- cost_data %>%
  mutate(median_family_income = ifelse(is.na(median_family_income), mean(median_family_income, na.rm = TRUE), median_family_income))

missing_plot <- gg_miss_var(cost_data)
print(missing_plot)
colnames(cost_data)

View(cost_data)

#Perform dummy encoding
dummy_encoded_data <- model.matrix(~ isMetro - 1, data = cost_data)


#Attach the dummy variable to the original data frame
cost_data <- cbind(cost_data, dummy_encoded_data)

#Remove the original 'isMetro' colum
cost_data <- cost_data[, !names(cost_data) %in% "isMetro"]

#Remove the IsMetroFalse Column to avoid high collenarity 
cost_data <- cost_data[, !names(cost_data) %in% "isMetroFALSE"]

#Checking
colnames(cost_data)

#Round all numeric columns to 2 decimal places
cost_data[, sapply(cost_data, is.numeric)] <- 
  lapply(cost_data[, sapply(cost_data, is.numeric)], round, 2)


#Adding a column and count the number of family members 
cost_data <- cost_data %>%
  mutate(
    parent_no = as.numeric(str_extract(family_member_count, "\\d+(?=p)")),
    children_no = as.numeric(str_extract(family_member_count, "\\d+(?=c)"))
  )

#Drop the family_member_count column 
cost_data <- cost_data[, !names(cost_data) %in% "family_member_count"]

#Using tapply to calculate median total cost by county
median_total_cost_by_county <- tapply(cost_data$total_cost, 
                                      cost_data$county, median)

# Create a new column 'median_total_cost' and populate it with median values
cost_data$median_total_cost <- median_total_cost_by_county[match(cost_data$county, 
                                                                 names(median_total_cost_by_county))]

#-----------------------------------Descriptive Statistics & EDA --------------------------------------------#
#summary statistics for numeric variables
summary(cost_data)
str(cost_data)

# Select the numerical columns
numeric_columns <- cost_data[, c("housing_cost", "food_cost", "transportation_cost", 
                                 "healthcare_cost", "other_necessities_cost", 
                                 "childcare_cost", "taxes", "total_cost", 
                                 "median_family_income" ,"parent_no", "children_no" ,          
                                 "median_total_cost")]

# Calculate the standard deviation for each numeric column
standard_deviations <- round(apply(numeric_columns, 2, sd))

# Print the result
print(standard_deviations)

selected_columns <- c("housing_cost", "food_cost", "transportation_cost", 
                      "healthcare_cost", "other_necessities_cost", 
                      "childcare_cost", "taxes", "total_cost", 
                      "median_family_income","parent_no", "children_no", 
                      "median_total_cost")

# Calculate the mean for each selected column and round to two decimal points
means <- round(colMeans(cost_data[, selected_columns], na.rm = TRUE), 2)

# Print the result
print(means)

# Assuming your dataset is named 'cost_data'
# Replace 'cost_data' with the actual name of your dataset

# Load the necessary library
library(ggplot2)

# Load the necessary library
library(ggplot2)

#Calculate the mean and standard deviation for housing_cost-------------------------------------------
mean_housing_cost <- mean(cost_data$housing_cost, na.rm = TRUE)
sd_housing_cost <- sd(cost_data$housing_cost, na.rm = TRUE)

#Create a histogram for housing_cost
ggplot(cost_data, aes(x = housing_cost)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of housing_cost",
       x = "Housing Cost",
       y = "Frequency") +
  geom_vline(xintercept = mean_housing_cost, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean_housing_cost, y = 0, vjust = -1, label = paste("Mean: ", round(mean_housing_cost, 2)), color = "red") +
  annotate("text", x = mean_housing_cost, y = 0, vjust = -2, label = paste("SD: ", round(sd_housing_cost, 2)), color = "red")

#Calculate the mean and standard deviation for food_cost------------------------------------------------
mean_food_cost <- mean(cost_data$food_cost, na.rm = TRUE)
sd_food_cost <- sd(cost_data$food_cost, na.rm = TRUE)

#Create a histogram for food_cost
ggplot(cost_data, aes(x = food_cost )) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Food Cost ",
       x = "Food Cost",
       y = "Frequency") +
  geom_vline(xintercept = mean_food_cost, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean_food_cost, y = 0, vjust = -1, label = paste("Mean: ", round(mean_food_cost, 2)), color = "red") +
  annotate("text", x = mean_food_cost, y = 0, vjust = -2, label = paste("SD: ", round(sd_food_cost, 2)), color = "red")
#------------------------------------------------------------------------------------------------------------
#Calculate the mean and standard deviation for transportation_cost ------------------------------------------------
mean_transportation_cost <- mean(cost_data$transportation_cost, na.rm = TRUE)
sd_transportation_cost <- sd(cost_data$transportation_cost, na.rm = TRUE)

#Create a histogram for transportation_cost
ggplot(cost_data, aes(x = transportation_cost )) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Transportation Cost ",
       x = "Transportation Cost",
       y = "Frequency") +
  geom_vline(xintercept = mean_transportation_cost, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean_transportation_cost, y = 0, vjust = -1, label = paste("Mean: ", round(mean_transportation_cost, 2)), color = "red") +
  annotate("text", x = mean_transportation_cost, y = 0, vjust = -2, label = paste("SD: ", round(sd_transportation_cost, 2)), color = "red")
#------------------------------------------------------
#Calculate the mean and standard deviation for healthcare_cost ------------------------------------------------
mean_healthcare_cost <- mean(cost_data$healthcare_cost, na.rm = TRUE)
sd_healthcare_cost <- sd(cost_data$healthcare_cost, na.rm = TRUE)

#Create a histogram for healthcare_cost
ggplot(cost_data, aes(x = healthcare_cost )) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Healthcare Cost ",
       x = "Healthcare Cost",
       y = "Frequency") +
  geom_vline(xintercept = mean_healthcare_cost, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean_healthcare_cost, y = 0, vjust = -1, label = paste("Mean: ", round(mean_healthcare_cost, 2)), color = "red") +
  annotate("text", x = mean_healthcare_cost, y = 0, vjust = -2, label = paste("SD: ", round(sd_healthcare_cost, 2)), color = "red")
#Calculate the mean and standard deviation for childcare_cost  ------------------------------------------------
mean_childcare_cost <- mean(cost_data$childcare_cost, na.rm = TRUE)
sd_childcare_cost <- sd(cost_data$childcare_cost, na.rm = TRUE)

#Create a histogram for childcare_cost
ggplot(cost_data, aes(x = childcare_cost )) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Childcare Cost",
       x = "Childcare Cost",
       y = "Frequency") +
  geom_vline(xintercept = mean_childcare_cost, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean_childcare_cost, y = 0, vjust = -1, label = paste("Mean: ", round(mean_childcare_cost, 2)), color = "red") +
  annotate("text", x = mean_childcare_cost, y = 0, vjust = -2, label = paste("SD: ", round(sd_childcare_cost, 2)), color = "red")

#Create a boxplot for childcare_cost using ggplot2
ggplot(cost_data, aes(y = childcare_cost)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Childcare Cost",
       y = "Childcare Cost")
#---------------------------------------------------------------------------------------------------------
#Calculate the mean and standard deviation for taxes ------------------------------------------------
mean_taxes<- mean(cost_data$taxes, na.rm = TRUE)
sd_taxes <- sd(cost_data$taxes, na.rm = TRUE)

#Create a histogram for healthcare_cost
ggplot(cost_data, aes(x = taxes )) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Taxes ",
       x = "Taxes",
       y = "Frequency") +
  geom_vline(xintercept = mean_taxes, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean_taxes, y = 0, vjust = -1, label = paste("Mean: ", round(mean_taxes, 2)), color = "red") +
  annotate("text", x = mean_taxes, y = 0, vjust = -2, label = paste("SD: ", round(sd_taxes, 2)), color = "red")
#-------------------------------------------------------------------------------------------------
# Calculate the frequency of each unique value in 'parent_no'
frequency_table <- table(cost_data$parent_no)
# Print the frequency table
print(frequency_table)

#Print unique values in parent_no column
unique_values <- unique(cost_data$parent_no)
print(unique_values)
#Create a pie chart
pie(table(cost_data$parent_no), 
    col = c("skyblue", "lightgreen"),
    main = "Number of Families with 1 or 2 Parents")


# Create a bar plot
bar_counts <- barplot(table(cost_data$parent_no), 
                      col = c("skyblue", "lightgreen"), 
                      main = "Number of Families with 1 or 2 Parents",
                      xlab = "Number of Parents",
                      ylab = "Count")
#--------------------------------------------------------------------------------------------
# Calculate the frequency of each unique value in 'children_no'
frequency_table <- table(cost_data$children_no)

# Print the frequency table
print(frequency_table)

# Create a bar plot
bar_counts <- barplot(table(cost_data$children_no), 
                      col = c("skyblue", "lightgreen"), 
                      main = "Number of Children in a family",
                      xlab = "Number of Parents",
                      ylab = "Count")
#------------------------------------------------------------------------------------------------------------
#Calculate the mean and standard deviation for other_necessities_cost ------------------------------------------------
mean_other_necessities_cost<- mean(cost_data$other_necessities_cost, na.rm = TRUE)
sd_other_necessities_cost<- sd(cost_data$other_necessities_cost, na.rm = TRUE)

#Create a histogram for healthcare_cost
ggplot(cost_data, aes(x = other_necessities_cost )) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Other Necessities Costs ",
       x = "other necessities cost($)",
       y = "Frequency") +
  geom_vline(xintercept = mean_other_necessities_cost, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean_other_necessities_cost, y = 0, vjust = -1, label = paste("Mean: ", round(mean_other_necessities_cost, 2)), color = "red") +
  annotate("text", x = mean_other_necessities_cost, y = 0, vjust = -2, label = paste("SD: ", round(sd_other_necessities_cost, 2)), color = "red")
#-------------------------------------------------------------------------------------------
#Calculate the mean and standard deviation for #total_cost  ------------------------------------------------
mean_total_cost <- mean(cost_data$total_cost, na.rm = TRUE)
sd_total_cost<- sd(cost_data$total_cost, na.rm = TRUE)

#Create a histogram for total_cost
ggplot(cost_data, aes(x = total_cost )) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Total Cost ",
       x = "Total Cost($)",
       y = "Frequency") +
  geom_vline(xintercept = mean_total_cost, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean_total_cost, y = 0, vjust = -1, label = paste("Mean: ", round(mean_total_cost, 2)), color = "red") +
  annotate("text", x = mean_total_cost, y = 0, vjust = -2, label = paste("SD: ", round(sd_total_cost, 2)), color = "red")
#-----------------------------------------------------------------------------------------------------------------------------------
#Calculate the mean and standard deviation for median_family_income   ------------------------------------------------
mean_median_family_income <- mean(cost_data$median_family_income, na.rm = TRUE)
sd_median_family_income<- sd(cost_data$median_family_income, na.rm = TRUE)

#Create a histogram for median_family_income
ggplot(cost_data, aes(x = median_family_income )) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of median family income",
       x = "median family income($)",
       y = "Frequency") +
  geom_vline(xintercept = mean_median_family_income, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean_median_family_income, y = 0, vjust = -1, label = paste("Mean: ", round(mean_median_family_income, 2)), color = "red") +
  annotate("text", x = mean_median_family_income, y = 0, vjust = -2, label = paste("SD: ", round(sd_median_family_income, 2)), color = "red")
#Calculate the mean and standard deviation for median_total_cost  ------------------------------------------------
mean_median_total_cost <- mean(cost_data$median_total_cost, na.rm = TRUE)
sd_median_total_cost<- sd(cost_data$median_total_cost, na.rm = TRUE)

#Create a histogram for median_total_cost 
ggplot(cost_data, aes(x = median_total_cost )) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of median total cost",
       x = "Median total cost($)",
       y = "Frequency") +
  geom_vline(xintercept = mean_median_total_cost, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean_median_total_cost, y = 0, vjust = -1, label = paste("Mean: ", round(mean_median_total_cost, 2)), color = "red") +
  annotate("text", x = mean_median_total_cost, y = 0, vjust = -2, label = paste("SD: ", round(sd_median_total_cost, 2)), color = "red")
#----------------------------------------------------------------------------
#Creating the box plot for all the columns with numerical values
boxplot(cost_data$housing_cost, main = "Box Plot for Housing Cost", col = "lightblue", border = "black")
boxplot(cost_data$food_cost, main = "Box Plot for Food Cost", col = "lightblue", border = "black")
boxplot(cost_data$transportation_cost, main = "Box Plot for Transportation Cost", col = "lightblue", border = "black")
boxplot(cost_data$healthcare_cost, main = "Box Plot for Healthcare Cost", col = "lightblue", border = "black")
boxplot(cost_data$other_necessities_cost, main = "Box Plot for Other Necessities Cost", col = "lightblue", border = "black")
boxplot(cost_data$median_family_income, main = "Box Plot for Median Family Income", col = "lightblue", border = "black")
boxplot(cost_data$total_cost, main = "Box Plot for Total Cost", col = "lightblue", border = "black")
boxplot(cost_data$taxes, main = "Box Plot for Taxes", col = "lightblue", border = "black")
boxplot(cost_data$median_total_cost, main = "Box Plot for Median Total Cost", col = "lightblue", border = "black")
#---------------------------------------------------------
#Creating the Q-Q plot for all the columns with numerical values 
qqnorm(cost_data$housing_cost, main = "Q-Q Plot for Housing Cost")
qqline(cost_data$housing_cost, col = 2)

qqnorm(cost_data$food_cost, main = "Q-Q Plot for Food Cost")
qqline(cost_data$food_cost, col = 2)  # Add a reference line for a perfect normal distribution

qqnorm(cost_data$transportation_cost, main = "Q-Q Plot for Transportation Cost")
qqline(cost_data$transportation_cost, col = 2)  # Add a reference line for a perfect normal distribution

qqnorm(cost_data$healthcare_cost, main = "Q-Q Plot for Healthcare Cost")
qqline(cost_data$healthcare_cost, col = 2)  # Add a reference line for a perfect normal distribution

qqnorm(cost_data$other_necessities_cost, main = "Q-Q Plot for Other necessities Cost")
qqline(cost_data$other_necessities_cost, col = 2)  # Add a reference line for a perfect normal distribution

qqnorm(cost_data$childcare_cost, main = "Q-Q Plot for Childcare Cost")
qqline(cost_data$childcare_cost, col = 2)  # Add a reference line for a perfect normal distribution

qqnorm(cost_data$median_family_income, main = "Q-Q Plot for Median Family Income")
qqline(cost_data$median_family_income, col = 2)  # Add a reference line for a perfect normal distribution

qqnorm(cost_data$median_total_cost, main = "Q-Q Plot for Median Total Cost")
qqline(cost_data$median_total_cost, col = 2)  # Add a reference line for a perfect normal distribution

qqnorm(cost_data$total_cost, main = "Q-Q Plot for Total Cost")
qqline(cost_data$total_cost, col = 2)  # Add a reference line for a perfect normal distribution

qqnorm(cost_data$taxes, main = "Q-Q Plot for Taxes")
qqline(cost_data$taxes, col = 2)  # Add a reference line for a perfect normal distribution

#The Abderson-Darling test - Test the normality of the column with numerical values 
library(nortest)
ad.test(cost_data$housing_cost)
ad.test(cost_data$food_cost)
ad.test(cost_data$transportation_cost)
ad.test(cost_data$healthcare_cost)
ad.test(cost_data$other_necessities_cost)
ad.test(cost_data$childcare_cost)
ad.test(cost_data$median_family_income)
ad.test(cost_data$median_total_cost)
ad.test(cost_data$children_no)
ad.test(cost_data$total_cost)
ad.test(cost_data$taxes)

#--------------------------------------------------------------
#Create a histogram with custom labels in the legend for isMetroTRUE
ggplot(cost_data, aes(x = median_family_income, fill = factor(isMetroTRUE))) +
  geom_histogram(binwidth = 1000, position = "identity", alpha = 0.7, color = "black") +
  labs(title = "Histogram of Median Family Income in Metro areas vs. Non Metro areas",
       x = "Median Family Income",
       y = "Frequency",
       fill = "Legend") +
  scale_fill_manual(values = c("1" = "blue", "0" = "orange"),
                    name = "Metro Area",
                    labels = c("1" = "Metro Area", "0" = "Non-Metro Area")) +
  theme_minimal()

#----------------------------------------------------------------------------------
ggplot(cost_data, aes(x = factor(isMetroTRUE), group = factor(isMetroTRUE))) +
  geom_bar(aes(y = median_family_income, fill = "Median Family Income"), stat = "identity", position = position_dodge(width = 0.8)) +
  geom_bar(aes(y = housing_cost, fill = "Housing Cost"), stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Comparison of Median Family Income and Housing Cost by Metro Area",
       x = "Metro Area",
       y = "Value",
       fill = "Legend") +
  scale_fill_manual(values = c("blue","orange")) +
  theme_minimal() +
  theme(legend.position = "right")  # Position the legend at the top
#----------------------------------------------------------------------------------
#Total cost and healthcare in metro vs. non metro
ggplot(cost_data, aes(x = factor(isMetroTRUE), group = factor(isMetroTRUE))) +
  geom_bar(aes(y = total_cost, fill = "Total Cost"), stat = "identity", position = position_dodge(width = 0.8)) +
  geom_bar(aes(y = healthcare_cost, fill = "Healthcare Cost"), stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Healthcare Cost & Total Cost In Metro Area",
       x = "Metro Area",
       y = "Value",
       fill = "Legend") +
  scale_fill_manual(values = c("blue","orange")) +
  theme_minimal() +
  theme(legend.position = "right")  # Position the legend at the top
#--------------------------------------------------------------------
# Scatterplot for income vs. cost of living
plot(cost_data$median_family_income, cost_data$median_total_cost, main = "Median Family Income vs. Median cost of Living", xlab = "Median Income", ylab = "Median Cost of Living")


#Calculate median family savings
cost_data$median_family_savings <- cost_data$median_family_income - cost_data$median_total_cost

#Print or view the updated dataset with the new column
print(cost_data)
colnames(cost_data)

# Scatterplot for income vs. family savings
plot(cost_data$median_family_income, cost_data$median_family_savings, main = "Median Family Income vs. Family Savings", xlab = "Median Income", ylab = "Median Family Savings")

# Scatterplot for cost of living vs. family savings
plot(cost_data$median_total_cost, cost_data$median_family_savings, main = "Median Cost of Living vs. Median Family Savings", xlab = "Median Cost of Living", ylab = "Median Family Savings")

#-----------------------------------Hypothesis Testing - T Test 1------------------------------------------------
#Calculate total number of family members
cost_data$total_family_members <- cost_data$children_no + cost_data$parent_no + 1  # Adding 1 for the person mentioned in 'total_cost'

#Calculate average cost per person
cost_data$average_cost_per_person <- cost_data$total_cost / cost_data$total_family_members

# Print or view the updated dataset
print(cost_data)
#---------------------------------------------T Test 2-------------------------------------------------------------------
#Define family size categories (e.g., based on median)
median_family_size <- median(cost_data$total_family_members)
cost_data$family_size_category <- ifelse(cost_data$total_family_members <= median_family_size, "Smaller", "Larger")

# Perform t-test
t_test_result <- t.test(average_cost_per_person ~ family_size_category, data = cost_data)

# Print the results
print(t_test_result)




#Perform a t-test
t_test_result <- t.test(average_cost_per_person ~ isMetroTRUE, data = cost_data)

# Print the results
print(t_test_result)
#-------------------------------------------------T test 3 ----------------------------------------------------
#Separate data into two groups based on the number of parents
one_parent_group <- cost_data[cost_data$parent_no == 1, ]
two_parents_group <- cost_data[cost_data$parent_no == 2, ]

#Perform t-test for total cost
t_test_total_cost<- t.test(one_parent_group$total_cost, two_parents_group$total_cost)
print(t_test_total_cost)

#Perform t-test for median_family_savings
t_test_savings <- t.test(one_parent_group$median_family_savings, two_parents_group$median_family_savings)
print(t_test_savings)

#--------------------------------------Correlation Analysis-------------------------------------------------------#
#Select relevant numeric columns for correlation analysis
numeric_columns <- cost_data %>%
  select(housing_cost, food_cost,	transportation_cost, healthcare_cost, 
         other_necessities_cost, childcare_cost, taxes, total_cost, median_family_income,median_total_cost)

#Calculate correlation matrix
correlation_matrix <- cor(numeric_columns)

#Print correlation matrix
print(correlation_matrix)

#Plot the correlation matrix as a heatmap with smaller font size
corrplot(correlation_matrix, method = "color", addCoef.col = "black", tl.cex = 0.7, tl.col = "black")
#Add a title above the corrplot
mtext("Correlation Matrix", side = 3, line = 1, cex = 1.2)

#A correlation test
core_test <- cor.test(cost_data$taxes, cost_data$housing_cost)
# Print the result
print(core_test)
#------------------------------------------------------------------------------------------------------------------
#Define the components of living costs
living_cost_components <- c("housing_cost", "food_cost", "transportation_cost", "healthcare_cost", "childcare_cost", "other_necessities_cost")

#Create a new column for living cost by summing the selected components
cost_data$living_cost <- rowSums(cost_data[, living_cost_components], na.rm = TRUE)

#Print or view the updated dataset
print(cost_data)
View(cost_data)
colnames(cost_data)

#------------------------------------Hypothesis Testing - ANOVA testing----------------------------------------
library(carData)

# Perform ANOVA for Living Cost
living_cost_aov <- aov(living_cost ~ county, data = cost_data)

# Print the ANOVA summary
summary(living_cost_aov)
#-------------------------------------The multiple linear regression model- Iteration 1 ----------------------------------------#
set.seed(123) #for reproducibility
#Splitting the data into train, and test sets (70:30)
sample <- sample.split(cost_data$taxes, SplitRatio = 0.7)
train <- subset(cost_data, sample == TRUE)
test <- subset(cost_data, sample == FALSE)


#Dimensions of the subsets
dim(train)
dim(test)

#Fit a linear regression model with 'taxes' as the target variable
model <- lm(taxes ~ housing_cost + food_cost + transportation_cost 
            + healthcare_cost + other_necessities_cost +
              childcare_cost + median_family_income + parent_no +
              children_no,
            data = train)

# Print the summary of the regression model
summary(model)

#-----------------------------Evaluate the performance of the model - Iteration 1------------------------------------------------#
#Calculating the MSE
predictions <- predict(model, newdata = test)  # Generate predictions on test data
# Calculate Mean Squared Error (MSE)
mse <- mean((test$taxes - predictions)^2)
# Print or use the MSE value as needed
print(paste("Mean Squared Error (MSE):", mse))

#Calculating the RMSE
rmse <- sqrt(mse)
print(paste("Root Mean Squared Error (RMSE):", rmse))


#Calculate the Residuals 
residuals <- test$taxes - predictions
#plot the residuals 
plot(predictions, residuals, main = "Residuals vs. Fitted", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at y = 0

#Histogram of residuals 
hist(residuals, main = "Histogram of Residuals", 
     xlab = "Residuals", col = "blue", border = "white")

qqnorm(residuals, main = "Q-Q Plot of Residuals", col = "blue")
qqline(residuals, col = "red", lty = 2)

plot(residuals, main = "Residuals", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at y = 0

#----------------------------Linear Regression Model - Iteration 2--------------------------------------------
#Fit a linear regression model with 'Savings' as the target variable
model_2 <- lm(taxes ~ housing_cost + transportation_cost 
            + healthcare_cost + childcare_cost + parent_no + children_no,
            data = train)

# Print the summary of the regression model
summary(model_2)

#----------------------------Evaluate the performance of the iteration 2 -------------------------------------#
#Calculating the MSE
predictions <- predict(model_2, newdata = test)  # Generate predictions on test data
# Calculate Mean Squared Error (MSE)
mse <- mean((test$taxes - predictions)^2)
# Print or use the MSE value as needed
print(paste("Mean Squared Error (MSE):", mse))

#Calculating the RMSE
rmse <- sqrt(mse)
print(paste("Root Mean Squared Error (RMSE):", rmse))


#Calculate the Residuals 
residuals <- test$taxes - predictions
#plot the residuals 
plot(predictions, residuals, main = "Residuals vs. Fitted", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at y = 0

#Histogram of residuals 
hist(residuals, main = "Histogram of Residuals", 
     xlab = "Residuals", col = "blue", border = "white")

qqnorm(residuals, main = "Q-Q Plot of Residuals", col = "blue")
qqline(residuals, col = "red", lty = 2)

plot(residuals, main = "Residuals", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at y = 0

#----------------------------------------------The End Of the Code--------------------------------------------------

