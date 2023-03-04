#Avi Milan Jani
#ALY6015 Module 1 - Regression Diagnostics with R
#Loading and installing the libraries and packages
library(dplyr)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(RColorBrewer)
install.packages("leaps")
library(leaps)
install.packages("mice")
library(mice)
install.packages("caret")
library(caret)

#Importing the data set
ameshousing <- read.csv("C:\\Users\\avija\\Downloads\\AmesHousing.csv", header=TRUE)

#REMOVE UNWANTED COLUMNS
ameshousing <- ameshousing %>%
  select(-c(Alley, Low.Qual.Fin.SF))

install.packages("openxlsx")
library(openxlsx)
write.xlsx(ameshousing, file = "ameshosuing.xlsx", sheetName = "Sheet1")


#1.Performing exploratory analysis and descriptive statistics
str(ameshousing)
str(na.omit(ameshousing))

# Store the output of str() in a variable
str_output <- capture.output(str(ameshousing))

# Create a data frame from the str output
str_ameshousing <- data.frame(str_output)

# View the resulting data frame
str_ameshousing



summary(ameshousing, na.rm = TRUE)

# Store the output of str() in a variable
summary_output1 <- capture.output(summary(ameshousing))

# Create a data frame from the str output
summary_ameshousing <- data.frame(summary_output1)

# View the resulting data frame
summary_ameshousing


output <- capture.output(str(ameshousing))

#Clean the data set
na_count <-sapply(ameshousing,function(y) sum(length(which(is.na(y)))))
na_count

#Input missing values by mean method
input <- mice(ameshousing, method ="mean", m =1, maxit=1)
input

# Change color values
color_values <- c("#E87653", "#5E4FA2", "#F4A259", "#4D9AC4", "#94B447", "#DC4C46", "#0072B2", "#FFD700")


# Create a histogram of SalePrice
library(scales)

ggplot(ameshousing, aes(x = SalePrice, fill = ..count..)) +
  geom_histogram(binwidth = 50000, color = "black", alpha = 0.8) +
  scale_fill_gradient(low = "white", high = color_values[4]) +
  xlab("Sale Price") + ylab("Count") +
  ggtitle("Sale Price Distribution") +
  xlim(c(0, 600000)) +
  scale_x_continuous(labels = comma) +
  theme_minimal()

mean(ameshousing$SalePrice)


# Create a boxplot of SalePrice by Neighborhood
library(RColorBrewer)

my_palette <- colorRampPalette(c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#084594"), space = "rgb")(28)
ggplot(ameshousing, aes(x = Neighborhood, y = SalePrice, fill = Neighborhood)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = my_palette) +
  xlab("") + ylab("Sale Price") +
  ggtitle("Price of Houses by Neighborhood") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), 
                     labels = scales::dollar_format(prefix = "$"))



# Check for missing values in numerical variables
numeric_vars <- sapply(ameshousing, is.numeric)
summary(ameshousing[,numeric_vars])

# Check for infinite values in numerical variables
is_inf <- apply(ameshousing[,numeric_vars], 2, function(x) any(!is.finite(x)))
ameshousing[,numeric_vars][,is_inf]




###################################################################################
# Select only the numeric columns
numeric.var <- sapply(ameshousing, is.numeric)
numeric.df <- ameshousing[, numeric.var]

# Remove rows with missing or infinite values
numeric.df <- na.omit(numeric.df)

# Create the correlation matrix
corr.matrix <- cor(numeric.df)

# Find highly correlated variables
highlyCorrelated <- findCorrelation(corr.matrix, cutoff = 0.6)

# Subset the correlation matrix to include only highly correlated variables
corr.matrix_subset <- corr.matrix[highlyCorrelated, highlyCorrelated]


# Plot the correlation matrix
corrplot(corr.matrix_subset, 
         method = "circle",
         tl.col = "dark red",
         tl.srt = 45,
         tl.cex = 0.8, 
         cl.cex = 0.8,
         font = 2, 
         addCoef.col = "transparent",
         title = "\n\nCorrelation Plot for Highly Correlated Numerical Variables")

#6. 
##########################################
# Get correlations of numeric columns with SalePrice
correlations <- cor(ameshousing[, sapply(ameshousing, is.numeric)], ameshousing$SalePrice)

# Sort correlations in descending order
sorted_corr <- sort(correlations, decreasing = FALSE)
View(correlations)
# Print correlations
print(correlations)
################################################################################################
install.packages("car")
library(car)

# Get correlations of numeric columns with SalePrice
correlations <- cor(ameshousing[, sapply(ameshousing, is.numeric)], ameshousing$SalePrice)

# Sort correlations in descending order
sort(correlations, decreasing = TRUE)

## scatterplot to identify highest correlation(1)
cor(ameshousing$SalePrice, ameshousing$Gr.Liv.Area)
scatterplot(ameshousing$SalePrice ~ ameshousing$Gr.Liv.Area, data = ameshousing, 
            main = "Highest correlation between Above ground living area and Sale Price",
            xlab = "Above Ground Area",
            ylab = "Sale Price", 
            col = "steelblue3")
options(scipen = 999)

### scatterplot to identify lowest correlation
cor(ameshousing$SalePrice, ameshousing$Enclosed.Porch)
scatterplot(ameshousing$SalePrice ~ ameshousing$Enclosed.Porch, data = ameshousing, 
            main = "Lowest correlation between Enclosed Porch square area  and Sale Price",
            xlab = "Enclosed Porch Square Area ",
            ylab = "Sale Price", 
            col = "darkgreen")
options(scipen = 999)

## scatterplot closest to 0.5 correlation
cor(ameshousing$SalePrice, ameshousing$X1st.Flr.SF)
scatterplot(ameshousing$SalePrice ~ ameshousing$X1st.Flr.SF, data = ameshousing, 
            main = "Sale price with First Floor in square feet",
            xlab = "First Floor in square feet",
            ylab = "Sale Price", 
            col = "steelblue4")
options(scipen = 999)


#7. Fit a multiple linear regression model with three continuous predictors
regression_model <- lm(SalePrice ~ Gr.Liv.Area + Wood.Deck.SF + Lot.Area, data = ameshousing)

# Print the summary of the model
summary(regression_model)
plot(regression_model)

#-------------------------------------------------------------------------------------------------------------------------------
#8.
library(broom)

# Fit the model
regression_model <- lm(SalePrice ~ Gr.Liv.Area + Wood.Deck.SF + Lot.Area, data = ameshousing)

# Extract the coefficients from the model
intercept <- regression_model$coefficients[1]
coef1 <- regression_model$coefficients[2]
coef2 <- regression_model$coefficients[3]
coef3 <- regression_model$coefficients[4]

# Print the equation of the model
cat("SalePrice = ", intercept, " + ", coef1, " * Gr.Liv.Area + ", coef2, " * Wood.Deck.SF + ", coef3, " * Lot.Area")

# Fit a multiple linear regression model with three continuous predictors
regression_model <- lm(SalePrice ~ Gr.Liv.Area + Wood.Deck.SF + Lot.Area, data = ameshousing)

#10. Check for multicollinearity using VIF
library(car)
install.packages("olsrr")
library(olsrr)
vif(regression_model)
ols_vif_tol(regression_model)

# If VIF is greater than 5 or 10 for any variable, it indicates the presence of multicollinearity.
# To correct for multicollinearity, one can consider dropping one of the correlated variables or using dimensionality reduction techniques such as principal component analysis.


## checking for outliers
ggplot(ameshousing, 
       aes( x=Gr.Liv.Area+Wood.Deck.SF+Lot.Area,
            y=SalePrice,color=SalePrice)) + 
  geom_boxplot(fill ="red"
  )

## removing outliers
p1<-boxplot(ameshousing$SalePrice)$out 
p2<-boxplot(ameshousing$Gr.Liv.Area)$out 
p3<-boxplot(ameshousing$Wood.Deck.SF)$out 
p4<-boxplot(ameshousing$Lot.Area)$out 


install.packages("leaps")
library(leaps)

ameshousing$SalePrice<-as.factor(ameshousing$SalePrice)
model <- regsubsets(SalePrice ~ ., data = ameshousing)
# Fit all possible models
regfits <- regsubsets(SalePrice ~ ., data = ameshousing)

# Find the best model using adjusted R-squared
summary(regfits)$adjr2

# Get the coefficients of the best model
best_model <- regfits$which[which.max(summary(regfits)$adjr2), ]
coef(regfits, best_model)

# run all subsets regression
regfit.full <- regsubsets(SalePrice ~ ., data = ameshousing)

# summary of results
summary(regfit.full)

# the best model with lowest Cp statistic
which.min(regfit.full$cp)


