#Importing Libraries

install.packages("ggplot2")
install.packages("psych")
install.packages("lmtest")
install.packages("moments")
install.packages("gridExtra")
install.packages("readr")
install.packages("tidyr")
install.packages("reshape2")
install.packages("dplyr")
install.packages("corrplot")
install.packages("car")
install.packages("TTR")
install.packages("forecast")
install.packages("tseries")
install.packages("urca")
install.packages("stats")

library(tseries)
library(urca) 
library(stats) 
library(forecast)
library(TTR)
library(car)
library(corrplot)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(lmtest)
library(forecast)
library(moments)
library(reshape2)
library(RColorBrewer)




#loading the data into R using the read.csv() function.
country_wealth <- read.csv("Countries_wealth.csv", header=TRUE)
country_wealth


#------------------------Data Exploration-------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

#inspecting the dataset

names(country_wealth)

head(country_wealth)

tail(country_wealth)

# checking the summary statistics for each of the numeric variables in the Data Frame
summary(country_wealth)

#checking the structure of the data using the str() function
str(country_wealth)

#checking the number of rows and columns in my dataset.
dimensions <- dim(country_wealth)

num_rows <- dimensions[1]  # Number of rows
num_cols <- dimensions[2]  # Number of columns

print(paste("Number of rows:", num_rows))
print(paste("Number of columns:", num_cols))

#using colnames function to rename the indicators(variables) to a more readable format

colnames(country_wealth)[4] <- "HC_em_f"      ## Human Capital, employed female
colnames(country_wealth)[5] <- "HC_em_m"      ## Human Capital, employed male
colnames(country_wealth)[6] <- "HC_sef_em_f"     ## Human Capital, self employed female
colnames(country_wealth)[7] <- "HC_sef_em_m"     ## Human Capital, self employed male
colnames(country_wealth)[8] <- "NC_Agric_land"     ## Natural Capital, Agricultural land
colnames(country_wealth)[9] <- "NC_fisheries"     ## Natural Capital, fisheries
colnames(country_wealth)[10] <- "NC_non_ren_Oil"    ## Natural Capital, non renewable, oil
colnames(country_wealth)[11] <- "N_foreign_asset"   ## Net foreign asset
colnames(country_wealth)[12] <- "Produced_C"     ## Produced capital
colnames(country_wealth)[13] <- "Total_Wealth"     ## Total wealth
print(country_wealth)


#-------------------------------checking for outliers of the objectives using Boxplot-------------------

#Objective 1: Gender Disparity in Human Capital

#Plotting the human capital value of employed males and females.

# Reshape data
country_wealth_long <- country_wealth %>%
  gather(key = "Gender", value = "Human Capital Value", HC_em_f, HC_em_m) %>%
  mutate(Gender = ifelse(Gender == "HC_em_f", "Female", "Male"))
# Plot
ggplot(country_wealth_long, aes(x = Gender, y = `Human Capital Value`, fill = Gender)) + 
  geom_boxplot(alpha = 0.7) +
  ggtitle("Human Capital by Gender - Employed Individuals") +
  xlab("") + ylab("Human Capital Value") +
  scale_fill_manual(values = c("Female" = "blue", "Male" = "red")) +
  theme_minimal()


#Objective 2: 2.	Impact of Natural Resources on Wealth: 

# Reshaping the data to long format for 'NC_Agric_land' and 'NC_fisheries'
country_wealth_long <- country_wealth %>%
  gather(key = "Category", value = "Value", NC_Agric_land, NC_fisheries) %>%
  mutate(Category = ifelse(Category == "NC_Agric_land", "Agricultural Land", "Fisheries"))

# Plotting the combined boxplot
ggplot(country_wealth_long, aes(x = Category, y = Value, fill = Category)) + 
  geom_boxplot(alpha = 0.7) +
  ggtitle("Natural Capital: Agricultural Land and Fisheries vs Total Wealth") +
  xlab("") + ylab("Value") +
  scale_fill_manual(values = c("Agricultural Land" = "green", "Fisheries" = "orange")) +
  theme_minimal()


#Objective 3: Influence of Nonrenewable Assets on Economic Stability

#Boxplot showing the relationship of nonrenewable assets (like oil) with net foreign assets.
# Combined boxplot for Net Foreign Assets and Nonrenewable Assets (Oil)
ggplot(country_wealth) + 
  geom_boxplot(aes(x = "Net Foreign Asset", 
                   y = N_foreign_asset, fill = "blue"), alpha = 0.7) +
  geom_boxplot(aes(x = "Non Renewable Oil",
                   y = NC_non_ren_Oil, fill = "red"), alpha = 0.7) +
  scale_fill_manual(values = c("blue", "red")) +
  ggtitle("Comparison of Net Foreign Assets and 
          Nonrenewable Assets (Oil)") +
  xlab("Category") + ylab("Value") +
  theme_minimal()

#Objective 4: Produced Capital and Overall Wealth
#Boxplot showing the correlation between produced capital and total wealth

# Combined boxplot for Produced Capital and Total Wealth
ggplot(country_wealth) + 
  geom_boxplot(aes(x = "Produced Capital", 
                   y = Produced_C, fill = "green"), alpha = 0.7) +
  geom_boxplot(aes(x = "Total Wealth", 
                   y = Total_Wealth, fill = "orange"), alpha = 0.7) +
  scale_fill_manual(values = c("green", "orange")) +
  ggtitle("Comparison of Produced Capital and Total Wealth") +
  xlab("Category") + ylab("Value") +
  theme_minimal()



#converting characters to numeric
# Selecting columns from index 4 to 13
cols_to_convert <- 4:13
# Converting columns to numeric format
for (col in cols_to_convert) {
  country_wealth[[col]] <- as.numeric(as.character(country_wealth[[col]]))
}

# Scaling the numeric columns to Trillion US dollars($)
country_wealth[, cols_to_convert] <- country_wealth[, cols_to_convert] / 1000000000000
# Printing the updated data frame
print(country_wealth)
# Checking for any remaining scientific notation
any(sapply(country_wealth[, 4:13], function(x) any(grepl("E", x))))



# Checking for missing values in the entire dataset
missing_values <- sum(is.na(country_wealth))
missing_values
# Outputting the number of missing values
print(paste("Number of missing values:", missing_values))





#checking the distribution of a variable and identifying any patterns or outliers using box plots and histograms

# Filtering only the numeric columns for outlier detection
numerics_cols <- sapply(country_wealth, is.numeric)

numerics_data <- country_wealth[, numerics_cols]






#-------------------------- Exploratory plots (Histograms)


# Creating a histogram for 'HC_sef_em_f' (Human Capital, self-employed female) column
hist(country_wealth$HC_sef_em_f,
     xlab = 'Human Capital Self Employed Female',
     main = 'Histogram of Self Employed Female',
     breaks = sqrt(nrow(country_wealth))) # Setting the number of bins

# Creating a histogram for 'HC_sef_em_m' (Human Capital, self-employed male) column
hist(country_wealth$HC_sef_em_m,
     xlab = 'Human Capital Self Employed Male',
     main = 'Histogram of Self Employed Male',
     breaks = sqrt(nrow(country_wealth))) # Setting the number of bins

# Creating a histogram for 'NC_Agric_land' (Natural Capital, Agricultural land) column
hist(country_wealth$NC_Agric_land,
     xlab = 'Natural Capital Agricultural Land',
     main = 'Histogram of Agricultural Land',
     breaks = sqrt(nrow(country_wealth))) # Setting the number of bins

# Creating a histogram for 'NC_fisherie' (Natural Capital, fisheries) column
hist(country_wealth$NC_fisheries,
     xlab = 'Natural Capital Fisheries',
     main = 'Histogram of Fisheries',
     breaks = sqrt(nrow(country_wealth))) # Setting the number of bins

# Creating a histogram for 'NC_non_ren_Oil' (Natural Capital, nonrenewable assets: oil) column
hist(country_wealth$NC_non_ren_Oil,
     xlab = 'Natural Capital Nonrenewable Assets: Oil',
     main = 'Histogram of Nonrenewable Assets: Oil',
     breaks = sqrt(nrow(country_wealth))) # Setting the number of bins

# Creating a histogram for 'N_foreign_asset' (Net foreign assets) column
hist(country_wealth$N_foreign_asset,
     xlab = 'Net Foreign Assets',
     main = 'Histogram of Net Foreign Assets',
     breaks = sqrt(nrow(country_wealth))) # Setting the number of bins

# Creating a histogram for 'Produced_C' (Produced capital) column
hist(country_wealth$Produced_C,
     xlab = 'Produced Capital',
     main = 'Histogram of Produced Capital',
     breaks = sqrt(nrow(country_wealth))) # Setting the number of bins

# Creating a histogram for 'Total_Wealth' (Total wealth) column
hist(country_wealth$Total_Wealth,
     xlab = 'Total Wealth',
     main = 'Histogram of Total Wealth',
     breaks = sqrt(nrow(country_wealth))) # Setting the number of bins



#-------------------------Using IQR method method for outlier detection-------------- 

# Filtering only the numeric columns for outlier detection
numeric_cols <- sapply(country_wealth, is.numeric)
numeric_data <- country_wealth[, numeric_cols]



# Function to identify outliers using IQR method on numeric columns
detect_outliers_numeric <- function(data) {
  Q1 <- apply(data, 2, quantile, probs = 0.25, na.rm = TRUE)
  Q3 <- apply(data, 2, quantile, probs = 0.75, na.rm = TRUE)
  IQR_values <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_values
  upper_bound <- Q3 + 1.5 * IQR_values
  outliers <- Map(function(x, lower, upper) x < lower | x > upper, data, 
                  lower_bound, upper_bound)
  outliers
}



# Detecting outliers in the numeric columns
outliers_detected_numeric <- detect_outliers_numeric(numeric_data)
outliers_detected_numeric



#detecting the total number of outliers, by use of the boxplot.stats() function to calculate the lower and upper bounds of the whiskers in a boxplot.

# Extracting the columns that you want to check for outliers
wealth_cols <- country_wealth[, 4:13]
# Creating an empty list to store the outliers
outliers <- list()
# Looping through each column and detect the outliers
for (col in names(wealth_cols)) {
  # Calculate the lower and upper bounds of the whiskers
  stats <- boxplot.stats(wealth_cols[[col]])
  lower <- stats$stats[1]
  upper <- stats$stats[5]
  #Subsetting the data to extract the outliers
  outliers[[col]] <- subset(country_wealth, country_wealth[[col]]
                            < lower | country_wealth[[col]] > upper)
}

#Counting the total number of outliers
total_outliers <- sum(sapply(outliers, nrow))
total_outliers
# Display the columns containing outliers and the number of outliers
for (col in names(outliers)) {
  if (nrow(outliers[[col]]) > 0) {
    cat("Column:", col, "\n")
    cat("Number of outliers:", nrow(outliers[[col]]), "\n\n")
  }
}



 #------------------------------#QUESTION 4.1---------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

#------------------DESCRIPTIVE STATISTICAL ANALYSIS-------------------------------


#CalculatING the mean, median, mode, standard deviation, skewness, and kurtosis for each indicator

numeric_cols <- sapply(country_wealth, is.numeric) & names(country_wealth) != "Year"

# Calculate summary statistics for numeric columns excluding 'Year'
summary_stats <- data.frame(
  Mean = colMeans(country_wealth[, numeric_cols], na.rm = TRUE),
  Median = sapply(country_wealth[, numeric_cols], median, na.rm = TRUE),
  Standard_Deviation = sapply(country_wealth[, numeric_cols], sd, na.rm = TRUE),
  Skewness = sapply(country_wealth[, numeric_cols], moments::skewness, na.rm = TRUE),
  Kurtosis = sapply(country_wealth[, numeric_cols], moments::kurtosis, na.rm = TRUE)
)

# Display the summary statistics
print(summary_stats)


# Grouping by Continent and calculating summary statistics for columns 4 to 13
summary_stats_continent <- country_wealth %>%
  group_by(Continent) %>%
  summarise(across(4:12, 
                   list(Mean = ~ mean(., na.rm = TRUE), 
                        Median = ~ median(., na.rm = TRUE), 
                        Standard_Deviation = ~ sd(., na.rm = TRUE), 
                        Skewness = ~ moments::skewness(., na.rm = TRUE), 
                        Kurtosis = ~ moments::kurtosis(., na.rm = TRUE)),
                   .names = "{.fn}_{.col}"))

# Displaying the summary statistics for each continent

# print(summary_stats_continent)


#----- Function to calculate mode

modeValue <- function(v) {
  uniqv <- unique(v)
  # Returning the value that appears most frequently
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}



#Analysis of Wealth Distribution by Year:
wealth_distribution_year <- country_wealth %>%
  group_by(Year) %>%
  summarise(Mean_Wealth = mean(Total_Wealth),
            Median_Wealth = median(Total_Wealth),
            Mode_Wealth = modeValue(Total_Wealth),
            SD_Wealth = sd(Total_Wealth),
            Skewness_Wealth = skewness(Total_Wealth),
            Kurtosis_Wealth = kurtosis(Total_Wealth))


#Analysis of Human Capital by Continent:
  human_capital_continent <- country_wealth %>%
  group_by(Continent) %>%
  summarise(Mean_HC_female = mean(HC_em_f),
            Median_HC_female = median(HC_em_f),
            Mode_HC_female = modeValue(HC_em_f),
            SD_HC_female = sd(HC_em_f),
            Skewness_HC_female = skewness(HC_em_f),
            Kurtosis_HC_female = kurtosis(HC_em_f),
            Mean_HC_male = mean(HC_em_m),
            Median_HC_male = median(HC_em_m),
            Mode_HC_male = modeValue(HC_em_m),
            SD_HC_male = sd(HC_em_m),
            Skewness_HC_male = skewness(HC_em_m),
            Kurtosis_HC_male = kurtosis(HC_em_m))
  
  
#Analysis of Net Foreign Assets by Continent:
net_foreign_assets_continent <- country_wealth %>%
  group_by(Continent) %>%
  summarise(Mean_NFA = mean(N_foreign_asset),
              Median_NFA = median(N_foreign_asset),
              Mode_NFA = modeValue(N_foreign_asset),
              SD_NFA = sd(N_foreign_asset),
              Skewness_NFA = skewness(N_foreign_asset),
              Kurtosis_NFA = kurtosis(N_foreign_asset))


#-----#Displaying summary statistics based on objective


# Comparative Wealth Analysis
wealth_analysis <- country_wealth %>% group_by(Country) %>% 
  summarise(Total_Wealth_Mean=mean(Total_Wealth),
            Total_Wealth_Median=median(Total_Wealth),
            Total_Wealth_Mode=modeValue(Total_Wealth),
            Total_Wealth_SD=sd(Total_Wealth),
            Total_Wealth_Skewness=skewness(Total_Wealth),
            Total_Wealth_Kurtosis=kurtosis(Total_Wealth))


# Gender Disparity in Human Capital for employed males and females
gender_disparity <- country_wealth %>% 
  summarise(Mean_HC_em_f=mean(HC_em_f),
            Median_HC_em_f=median(HC_em_f),
            Mode_HC_em_f=modeValue(HC_em_f),
            SD_HC_em_f=sd(HC_em_f),
            Skewness_HC_em_f=skewness(HC_em_f),
            Kurtosis_HC_em_f=kurtosis(HC_em_f),
            Mean_HC_em_m=mean(HC_em_m),
            Median_HC_em_m=median(HC_em_m),
            Mode_HC_em_m=modeValue(HC_em_m),
            SD_HC_em_m=sd(HC_em_m),
            Skewness_HC_em_m=skewness(HC_em_m),
            Kurtosis_HC_em_m=kurtosis(HC_em_m))


# Impact of Natural Resources on Wealth
natural_resources <- country_wealth %>%
  summarise(Mean_NC_Agric_land=mean(NC_Agric_land),
            Median_NC_Agric_land=median(NC_Agric_land),
            Mode_NC_Agric_land=modeValue(NC_Agric_land),
            SD_NC_Agric_land=sd(NC_Agric_land),
            Skewness_NC_Agric_land=skewness(NC_Agric_land),
            Kurtosis_NC_Agric_land=kurtosis(NC_Agric_land),
            Mean_NC_fisherie=mean(NC_fisheries),
            Median_NC_fisheries=median(NC_fisheries),
            Mode_NC_fisheries=modeValue(NC_fisheries),
            SD_NC_fisherie=sd(NC_fisheries),
            Skewness_NC_fisheries=skewness(NC_fisheries),
            Kurtosis_NC_fisheries=kurtosis(NC_fisheries))


# Influence of Nonrenewable Assets on Economic Stability
nonrenewable_assets <- country_wealth %>%
  summarise(Mean_NC_non_ren_Oil=mean(NC_non_ren_Oil),
            Median_NC_non_ren_Oil=median(NC_non_ren_Oil),
            Mode_NC_non_ren_Oil=modeValue(NC_non_ren_Oil),
            SD_NC_non_ren_Oil=sd(NC_non_ren_Oil),
            Skewness_NC_non_ren_Oil=skewness(NC_non_ren_Oil),
            Kurtosis_NC_non_ren_Oil=kurtosis(NC_non_ren_Oil),
            Mean_N_foreign_asset=mean(N_foreign_asset),
            Median_N_foreign_asset=median(N_foreign_asset),
            Mode_N_foreign_asset=modeValue(N_foreign_asset),
            SD_N_foreign_asset=sd(N_foreign_asset),
            Skewness_N_foreign_asset=skewness(N_foreign_asset),
            Kurtosis_N_foreign_asset=kurtosis(N_foreign_asset))


# Produced Capital and Overall Wealth
produced_capital <- country_wealth %>%
  summarise(Mean_Produced_C=mean(Produced_C),
            Median_Produced_C=median(Produced_C),
            Mode_Produced_C=modeValue(Produced_C),
            SD_Produced_C=sd(Produced_C),
            Skewness_Produced_C=skewness(Produced_C),
            Kurtosis_Produced_C=kurtosis(Produced_C),
            Mean_Total_Wealth=mean(Total_Wealth),
            Median_Total_Wealth=median(Total_Wealth),
            Mode_Total_Wealth=modeValue(Total_Wealth),
            SD_Total_Wealth=sd(Total_Wealth),
            Skewness_Total_Wealth=skewness(Total_Wealth),
            Kurtosis_Total_Wealth=kurtosis(Total_Wealth))


# Wealth Distribution Analysis by continent 
wealth_distribution <- country_wealth %>% group_by(Continent) %>%
  summarise(Mean_Wealth=mean(Total_Wealth),
            Median_Wealth=median(Total_Wealth),
            Mode_Wealth=modeValue(Total_Wealth),
            SD_Wealth=sd(Total_Wealth),
            Skewness_Wealth=skewness(Total_Wealth),
            Kurtosis_Wealth=kurtosis(Total_Wealth))


# Printing results for each analysis
print(wealth_analysis)
print(gender_disparity)
print(natural_resources)
print(nonrenewable_assets)
print(produced_capital)
print(wealth_distribution)



# Select columns 4 to 13
selected_columns <- country_wealth[, 4:13]


# Calculate mean, median, standard deviation, kurtosis, and skewness for selected columns
mean_values <- colMeans(selected_columns, na.rm = TRUE)
median_values <- apply(selected_columns, 2, median, na.rm = TRUE)
sd_values <- apply(selected_columns, 2, sd, na.rm = TRUE)
kurtosis_values <- apply(selected_columns, 2, kurtosis, na.rm = TRUE)
skewness_values <- apply(selected_columns, 2, skewness, na.rm = TRUE)


# Create a dataframe for plotting
plot_data <- data.frame(
  Indicator = colnames(selected_columns),
  Mean = mean_values,
  Median = median_values,
  SD = sd_values,
  Kurtosis = kurtosis_values,
  Skewness = skewness_values
)


# Melting the data for easier plotting with ggplot
plot_data_melted <- melt(plot_data, id.vars = 'Indicator')

# Plotting using ggplot
ggplot(plot_data_melted, aes(x = Indicator, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle('Comparison of Mean, Median, SD, Kurtosis, and Skewness Across Indicators') +
  xlab('Indicators') +
  ylab('Value') +
  theme_minimal()





#--------------------------------CORRELATION ANALYSIS---------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------                              
                                      #QUESTION 4.2

#Calculating the correlation matrix using the cor() function

correlation_matrix <- cor(country_wealth[,4:13])

correlation_matrix


#


# Renaming columns to remove spaces (if any)
names(country_wealth) <- gsub(" ", "_", names(country_wealth))

# Function to plot histogram for a given column
plot_histogram <- function(data, column) {
  p <- ggplot(data, aes(x = !!sym(column))) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
    geom_density(color = "red", size = 1) +
    labs(title = paste("Histogram of", column), x = column, y = "Density") +
    theme_minimal()
  print(p)
}

plot_qq <- function(data, column) {
  qq <- ggplot(data, aes(sample = !!sym(column))) +
    geom_qq() +
    geom_qq_line() +
    labs(title = paste("Q-Q Plot of", column)) +
    theme_minimal()
  print(qq)
}

# Iterating over columns 4 to 13 
for (col in names(country_wealth)[4:13]) {
  cat("Plots for:", col, "\n")
  plot_histogram(country_wealth, col)
  plot_qq(country_wealth, col)
}


# Subsetting the dataset for developed countries
developed_countries_data <- subset(country_wealth, Country %in% c(
  'United States', 'United Kingdom', 'Germany', 'Japan')) 
# Subsetting the dataset for developing countries
developing_countries_data <- subset(country_wealth, Country %in% c(
  'Nigeria', 'India', 'Brazil', 'Vietnam')) 



# Calculating the correlation matrix for developed countries
cor_matrix_developed <- cor(developed_countries_data[, c(
  "NC_Agric_land", "NC_fisheries", "Total_Wealth")], use="complete.obs")


# Generating the correlation plot for developed countries
corrplot(cor_matrix_developed, method = "number", 
         type = "upper", title = "Correlation: Natural Capital vs Total Wealth (Developed Countries)")




# Calculating the correlation matrix for developing countries
cor_matrix_developing <- cor(developing_countries_data[, c(
  "NC_Agric_land", "NC_fisheries", "Total_Wealth")], use="complete.obs")

# Generating the correlation plot for developing countries
corrplot(cor_matrix_developing, method = "number", 
         type = "upper", title = "Correlation: Natural Capital vs Total Wealth (Developing Countries)")





                            #QUESTION 4.3
#---------------------------Hypothesis Testing-------------------------------
#----------------------------------------------------------------------------
#-----------------------------------------------------------------------------


# Filtering data for the United Kingdom

uk_data <- filter(country_wealth, Country == "United Kingdom")

# Checking for normal distribution using Shapiro-Wilk test
columns_to_test <- c("HC_em_f", "HC_em_m", "NC_Agric_land", 
                     "NC_fisheries", "NC_non_ren_Oil", "N_foreign_asset", "Total_Wealth")
uk_normality_test_results <- lapply(uk_data[columns_to_test], function(x) shapiro.test(x))

# Print the results of the normality test
print("Normality Test Results for UK Data:")
print(uk_normality_test_results) 

#checking normality for the two groups

# Extract the variables for the two groups
group1 <- uk_data$HC_em_f
group2 <- uk_data$HC_em_m

# Create Q-Q plot for group 1
qq_plot_group1 <- ggplot(data.frame(Data = group1), aes(sample = Data)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Q-Q Plot - Group 1",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

# Create Q-Q plot for group 2
qq_plot_group2 <- ggplot(data.frame(Data = group2), aes(sample = Data)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Q-Q Plot - Group 2",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

# Print the Q-Q plots
print(qq_plot_group1)
print(qq_plot_group2)
      

# Checking variances between the two groups

# Perform variance comparison test
var_test <- var.test(group1, group2)

# Print the result
print("Variance Comparison Test:")
print(var_test)



#Hypothesis 1: Gender Disparity in Human Capital
# Independent Two Sample T-Test
t_test_result <- t.test(uk_data$HC_em_f, uk_data$HC_em_m, alternative = "two.sided", var.equal = TRUE)
print("Independent Two Sample T-Test result for Gender Disparity in Human Capital:")
print(t_test_result)





# Hypothesis 2: Impact of Natural Resources on Wealth
# Pearson Correlation for NC_fisheries and Total_Wealth
pearson_result_fisheries <- cor.test(uk_data$NC_fisheries, uk_data$Total_Wealth, method = "pearson")
print("Pearson Correlation result for Natural Capital Fisheries and Total Wealth:")
print(pearson_result_fisheries)

# Spearman's Rank Correlation for NC_Agric_land and Total_Wealth
spearman_result_agriculture <- cor.test(uk_data$NC_Agric_land, uk_data$Total_Wealth, method = "spearman")
print("Spearman's Rank Correlation result for Natural Capital Agricultural Land and Total Wealth:")
print(spearman_result_agriculture)

# Hypothesis 3: Influence of Nonrenewable Assets on Economic Stability
# Pearson Correlation for NC_non_ren_Oil and N_foreign_asset
pearson_result_oil <- cor.test(uk_data$NC_non_ren_Oil, uk_data$N_foreign_asset, method = "pearson")
print("Pearson Correlation result for Nonrenewable Assets (Oil) and Net Foreign Assets:")
print(pearson_result_oil)





  #-----------------------------#REGRESSION ANALYSIS-------------------------------------
##------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------

#-------------------Using GLM (Generalized linear Model)------------------------------

#OBJECTIVE 1:Relationship between Produced Capital and Overall Wealth in Nigeria
  


# Filter 'country_wealth' dataframe for Nigeria
nigeria_data <- subset(country_wealth, Country == "Nigeria")



# Fit the GLM model
glm_result4_nigeria <- glm(Total_Wealth ~ Produced_C, data=nigeria_data, family=gaussian())
glm_result4_nigeria
# Summarize the GLM model
summary(glm_result4_nigeria)



# Assumption 1: Linearity - Scatter plot with model's fitted values vs residuals
plot(glm_result4_nigeria$fitted.values, resid(glm_result4_nigeria), 
     xlab="Fitted Values", ylab="Residuals", main="Linearity Check for Nigeria")
abline(h=0, col="red")



# Assumption 2: Independence - Durbin-Watson test
dwtest(glm_result4_nigeria)



# Additional diagnostic plots
par(mfrow=c(2,2))
plot(glm_result4_nigeria, which=1)  # Residuals vs Fitted
plot(glm_result4_nigeria, which=2)  # Normal Q-Q
plot(glm_result4_nigeria, which=3)  # Scale-Location
plot(glm_result4_nigeria, which=5)  # Residuals vs Leverage




#---------------------------------------------------------------------

 
  # -------------------------------USING MULTIPLE REGRESSION------------

#OBJECTIVE 2 : Impact of Natural Resources and human capital on Wealth in Canada


# Filtering 'country_wealth' dataframe for Canada

canada_data <- subset(country_wealth, Country == "Canada")



numeric_data <- country_wealth[sapply(country_wealth, is.numeric)]

# calculating the correlation matrix
cor_matrix <- cor(numeric_data)

# View the correlation matrix
View(cor_matrix)

corrplot(cor_matrix)



# Fitting the multiple regression model for Canada

model_canada1 <- lm(Total_Wealth ~ NC_Agric_land + NC_fisheries, data=canada_data)
model_canada1

# Printing the summary of the regression model for Canada
summary.lm(model_canada1)


#adding more variable 2
model_canada2 <- lm(Total_Wealth ~ NC_Agric_land + NC_fisheries + NC_non_ren_Oil , 
                    data=canada_data)
model_canada2

# Printing the summary of the regression model for Canada
summary.lm(model_canada2)


#adding more variable 3
model_canada3 <- lm(Total_Wealth ~ NC_Agric_land + NC_fisheries + NC_non_ren_Oil + 
                      HC_em_m , data=canada_data)
model_canada3

# Printing the summary of the regression model for Canada
summary.lm(model_canada3)


# Check for linearity by plotting the observed vs predicted values for Canada
ggplot(canada_data, aes(x=predict(model_canada3), y=Total_Wealth)) +
  geom_point() +
  geom_smooth(method='lm', col='blue') +
  labs(title='Linearity Check: Observed vs Predicted for Canada', x='Predicted', y='Observed')


#printing out the column names
data.frame(colnames(canada_data))



#Check this linearity assumption by examining a scatterplot of x and y.

pairs(canada_data[,c(13,8,9,10,5)], lower.panel = NULL, pch = 19,cex = 0.2)



# Checking for independence of residuals using Durbin-Watson test for Canada
dwtest(model_canada3)



# Checking for homoscedasticity of residuals by plotting residuals vs fitted values for Canada
plot(model_canada3, which=3)


# Checking for normality of residuals using Q-Q plot for Canada
qqPlot(model_canada3, main="Q-Q Plot for Canada")



# Checking for multicollinearity using Variance Inflation Factor (VIF) for Canada
vif(model_canada3)








#----------------------------------------------------------------------------------
  
  
  
#addressing the violated assumptions

# Example: Log transformation of predictor variables
canada_data$Log_NC_Agric_land <- log(canada_data$NC_Agric_land + 1)  
canada_data$Log_NC_fisheries <- log(canada_data$NC_fisheries + 1)


# Re-fitting the model with transformed variables
model_canada4 <- lm(Total_Wealth ~ Log_NC_Agric_land + Log_NC_fisheries + 
                      NC_non_ren_Oil + HC_em_m, data=canada_data)
summary(model_canada4)



#Addressing multicolinearity

# Installing and loading the necessary package for VIF
install.packages("car")
library(car)

# Calculating VIF
vif_model4 <- vif(model_canada4)
print(vif_model4)

# Removing variables with high VIF and re-fitting the model if needed



#Addressing Lack of Independence

# Adding a lagged dependent variable (example: 1-period lag)
canada_data$Total_Wealth_lag <- c(NA, canada_data$Total_Wealth[-length(canada_data$Total_Wealth)])

# Re-fitting the model with lagged variable
model_canada5 <- lm(Total_Wealth ~ Log_NC_Agric_land + Log_NC_fisheries + NC_non_ren_Oil +
                      HC_em_m + Total_Wealth_lag, data=canada_data, na.action = na.exclude)
summary(model_canada5)


# Log transformation of the response variable
canada_data$Log_Total_Wealth <- log(canada_data$Total_Wealth)

# Re-fitting the model with transformed response variable
model_canada6 <- lm(Log_Total_Wealth ~ Log_NC_Agric_land + Log_NC_fisheries + NC_non_ren_Oil + 
                      HC_em_m, data=canada_data)
summary(model_canada6)

#Addressing Heteroscedasticity

# Log transformation of the response variable
canada_data$Log_Total_Wealth <- log(canada_data$Total_Wealth)

# Re-fitting the model with transformed response variable
model_canada7 <- lm(Log_Total_Wealth ~ Log_NC_Agric_land + Log_NC_fisheries + 
                      NC_non_ren_Oil + HC_em_m, data=canada_data)
summary(model_canada7)





#Revaluating the assumptions









# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# -----------------------------TIME SERIES ANALYSIS-------------------------

                            #QUESTION 4.4


#------------------------------------------------------------------------

  #OBJECTIVE 
# USING HOLTWINTERS MODEL

# Subset for USA and Nigeria
usa_data <- subset(country_wealth, Country == 'United States')

nigeria_data <- subset(country_wealth, Country == 'Nigeria')

# Time series analysis for produced capital in the USA
usa_produced_capital_ts <- ts(usa_data$Produced_C, start=2009)

# Time series analysis for produced capital in Nigeria
nigeria_produced_capital_ts <- ts(nigeria_data$Produced_C, 
                                  start=2009)

# Plotting time series for produced capital in the USA
plot(usa_produced_capital_ts, main="Time Series of Produced Capital in the USA",
     xlab="Year", ylab="Produced Capital", type="l")

# Plotting time series for produced capital in Nigeria
plot(nigeria_produced_capital_ts, main="Time Series of Produced Capital in Nigeria",
     xlab="Year", ylab="Produced Capital", type="l")



# USING ADDICTIVE MODEL SIMPLE MOVING AVERAGE (SMA) TO ESTIMATE TREND


#  #Applying SMA smoothing with a window of 3 for produced capital in the USA
usa_produced_capital_sma3 <- SMA(ts(usa_data$Produced_C, 
                                     start=2009), n=3)
 
#usa_produced_capital_sma3

#Plotting the smoothed time series for produced capital in the USA
plot.ts(usa_produced_capital_sma3, main="SMA(3) of Produced Capital in the USA", 
        xlab="Year", ylab="SMA Produced Capital")
 
 # Applying SMA smoothing with a window of 3 for produced capital in Nigeria
  nigeria_produced_capital_sma3 <- SMA(ts(nigeria_data$Produced_C, 
                                        start=2009),  n=3)
  
# # Plotting the smoothed time series for produced capital in Nigeria
 plot.ts(nigeria_produced_capital_sma3, main="SMA(3) of Produced Capital in Nigeria", 
         xlab="Year", ylab="SMA Produced Capital")



#-----Forcasting using Holt-Winters Exponential Smoothing--------------

#View Data
usa_produced_capital_ts
nigeria_produced_capital_ts

# Fitting holtwinters Exponential Smoothing models using HoltWinters for produced capital
usa_produced_capital_hw <- HoltWinters(usa_produced_capital_ts,
                                       gamma=FALSE,l.start = 77.0, b.start = 0.7)

usa_produced_capital_hw

nigeria_produced_capital_hw <- HoltWinters(nigeria_produced_capital_ts,
                                           gamma=FALSE, l.start=0.719, b.start=0.044)
nigeria_produced_capital_hw



#------Extracting the fitted values--

usa_produced_capital_hw$fitted

nigeria_produced_capital_hw$fitted


#---#plotting the original time series against the forecasts

plot(usa_produced_capital_hw)

plot(nigeria_produced_capital_hw)


#---------#Calculating sum of squared errors
usa_produced_capital_hw$SSE

nigeria_produced_capital_hw$SSE

#-----------#forcasting usa_produced_capital for future times by using forecast()

usa_produced_capital_hw_forcast <- forecast(usa_produced_capital_hw, h=20)
usa_produced_capital_hw_forcast

plot(usa_produced_capital_hw_forcast)

#-----------#forcasting nigeria_produced_capital for future times by using forecast()
  
nigeria_produced_capital_hw_forcast <- forecast(nigeria_produced_capital_hw, h=20)
nigeria_produced_capital_hw_forcast
plot(nigeria_produced_capital_hw_forcast)


#-------- making a correlogram and carrying out the Ljung-Box test

residuals_without_na1 <- na.omit(nigeria_produced_capital_hw_forcast$residuals)
residuals_without_na1

# Use a lag value less than or equal to the number of non-missing residuals
max_lag1 <- length(residuals_without_na1)-1 

# ACF analysis
acf(residuals_without_na1)

# Ljung-Box test with adjusted lag
Box.test(residuals_without_na1, lag = max_lag1, type = "Ljung-Box")




#  function to plot forecast errors and compare them with a normal distribution.

plotForecastErrors <- function(forecasterrors) {
  # Calculating the interquartile range (IQR) and standard deviation (sd) of the forecast errors
  mybinsize <- IQR(forecasterrors, na.rm = TRUE) / 4  # Added na.rm = TRUE to handle NA values
  mysd <- sd(forecasterrors, na.rm = TRUE)  # Added na.rm = TRUE to handle NA values
  
  # Determining the minimum and maximum values for the histogram
  mymin <- min(forecasterrors, na.rm = TRUE) - mysd * 5  # Added na.rm = TRUE to handle NA values
  mymax <- max(forecasterrors, na.rm = TRUE) + mysd * 3  # Added na.rm = TRUE to handle NA values
  
  # Generating normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  
  # Adjusting the minimum and maximum values based on the generated normal distribution
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  
  # Making a red histogram of the forecast errors with the normally distributed data overlaid
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins)  # freq = FALSE ensures the area under the histogram equals 1
  
  # Generating the histogram for the normally distributed data
  myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
  
  # Plotting the normal curve as a blue line on top of the histogram of forecast errors
  points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
}



#checking that the forecast errors have constant variance over time,and are normally distributed with mean zero.

#plot of forcast errors for nigeria produced capital


plot.ts(residuals_without_na1)

forecast_errors <- na.omit(residuals_without_na1)

#histogram of the distribution of forecast errors with an overlaid normal curve
plotForecastErrors(forecast_errors)

#--------------------

#plot of forcast errors for usa produced capital
residuals_without_na2 <- na.omit(usa_produced_capital_hw_forcast$residuals)
residuals_without_na2

# Use a lag value less than or equal to the number of non-missing residuals
max_lag2 <- length(residuals_without_na2)-1

# ACF analysis
acf(residuals_without_na2)

# Ljung-Box test with adjusted lag
Box.test(residuals_without_na2, lag = max_lag2, type = "Ljung-Box")


#plot of forcast errors for usa produced capital

plot.ts(residuals_without_na2)

forecast_errors2 <- na.omit(residuals_without_na2)

#histogram of the distribution of forecast errors with an overlaid normal curve
plotForecastErrors(forecast_errors2)





#-------------------ARIMA MODEL------------------------------------------------------------
#------------------------------------------------------------------------------------

#--------------------


#CHECKING for stationarity using Augmented Dickey-Fuller test
# Augmented Dickey-Fuller test function in R
adf_test <- function(timeseries) {
  result <- adf.test(timeseries)
  cat('ADF Statistic:', result$statistic, "\n")
  cat('p-value:', result$p.value, "\n")
  cat('Critical Values:\n')
  for(key in names(result$critical.values)) {
    cat(key, ':', result$critical.values[[key]], "\n")
  }
}





#OBJECTIVE 

#Time series analysis for employed male and female human capital in the USA
usa_male_ts <- ts(usa_data$HC_em_m, start=2009)
usa_female_ts <- ts(usa_data$HC_em_f, start=2009)




# # Plotting time series for the USA
# plot(usa_male_ts, main="Time Series of Employed Male Human Capital in the USA",
#      xlab="Year", ylab="Human Capital", type="l")
# 
# plot(usa_female_ts, main="Time Series of Employed Female Human Capital in the USA",
#      xlab="Year", ylab="Human Capital", type="l")

# Plot time series for Nigeria
plot(nigeria_male_ts, main="Time Series of Employed Male Human Capital in Nigeria",
     xlab="Year", ylab="Human Capital", type="l")

plot(nigeria_female_ts, main="Time Series of Employed Female Human Capital in Nigeria", 
     xlab="Year", ylab="Human Capital", type="l")

------
  

adf_test(nigeria_male_ts)

adf_test(nigeria_female_ts)

# My timeseries is non stationary.
#Differencing the mean
# Difference the series
differenced_series_female <- diff(nigeria_female_ts, differences=1)
                            
differenced_series_male <- diff(nigeria_male_ts, differences=1)

plot(differenced_series_female)

plot(differenced_series_male)



# reChecking for stationarity using Augmented Dickey-Fuller test
adf_test_diff_female <- adf.test(differenced_series_female)
adf_test_diff_male <- adf.test(differenced_series_male)

# Print the test results
print(adf_test_diff_female)

print(adf_test_diff_male)


# MODEL IDENTIFICATION
# Plot ACF and PACF of the differenced series
par(mfrow = c(2, 1))
acf(differenced_series_female, lag.max = 20, plot=FALSE)
pacf(differenced_series_female, lag.max = 20, plot=FALSE)

acf(differenced_series_male, lag.max = 30, plot=FALSE)
pacf(differenced_series_male, lag.max = 30, PLOT=FALSE)

auto.arima(differenced_series_female)


# Fitting ARIMA model for the female series
model_female <- arima(differenced_series_female, order = c(0, 0, 0))  
summary(model_female)
# Check residuals for autocorrelation and normality
residuals_female <- residuals(model_female)

# Autocorrelation of residuals
acf(residuals_female, lag.max = 30)

Box.test(residuals_female, lag=20, type="Ljung-Box")


# Normality test of residuals
jarque.bera.test(residuals_female)

#-----
auto.arima(differenced_series_male)


# Fitting ARIMA model for the male series
model_male <- arima(differenced_series_male, order = c(0, 0, 0))  # Replace p, d, q with the identified model order
summary(model_male)

# Check residuals for autocorrelation and normality
residuals_male <- residuals(model_male)

# Autocorrelation of residuals
acf(residuals_male, lag.max = 20)

Box.test(residuals_male, lag=20, type="Ljung-Box")

# Normality test of residuals
jarque.bera.test(residuals_male)



# Making forecasts for female
forecast_length <- 5  # Number of periods to forecast

forecast_female <- forecast(model_female, h = forecast_length)
forecast_female

# Making forecasts for male
forecast_male <- forecast(model_male, h = forecast_length)
forecast_male

plot(forecast_female)

plot(forecast_male)



plotForecastErrors <- function(forecasterrors) {
  # Calculating the interquartile range (IQR) and standard deviation (sd) of the forecast errors
  mybinsize <- IQR(forecasterrors, na.rm = TRUE) / 4  # Added na.rm = TRUE to handle NA values
  mysd <- sd(forecasterrors, na.rm = TRUE)  # Added na.rm = TRUE to handle NA values
  
  # Determining the minimum and maximum values for the histogram
  mymin <- min(forecasterrors, na.rm = TRUE) - mysd * 5  
  mymax <- max(forecasterrors, na.rm = TRUE) + mysd * 3  
  
  # Generating normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  
  # Adjusting the minimum and maximum values based on the generated normal distribution
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2}
  if (mymax2 > mymax) { mymax <- mymax2}
  
  # Making a histogram of the forecast errors with the normally distributed data overlaid
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins)  # freq = FALSE ensures the area under the histogram equals 1
  
  # Generating the histogram for the normally distributed data
  myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
  
  # Plotting the normal curve as a blue line on top of the histogram of forecast errors
  points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
}

# Plotting forecast errors for female series
plotForecastErrors(forecast_female$residuals)

# Plotting forecast errors for male series
plotForecastErrors(forecast_male$residuals)


# Checking  mean of forecast errors
mean(forecast_female$residuals)

mean(forecast_male$residuals)




