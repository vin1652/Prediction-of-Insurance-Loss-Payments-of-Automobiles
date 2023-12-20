library(tidyverse)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(tidyr)
library(gridExtra)
library(randomForest)
library(caret)
library(readr)
library(caret)
library(corrplot)
# Load Data
auto_data <- read_csv("filepath")
#View(auto_data)
attach(auto_data)


###############################################################################################
#################################### Phase:1 Data Preprocessing ###############################
###############################################################################################
auto_data <- auto_data %>%
  mutate(`num-of-doors` = ifelse(`num-of-doors` == "two", 2, ifelse(`num-of-doors` == "four", 4, NA)))
auto_data <- auto_data %>%
  mutate(`num-of-cylinders` = case_when(
    `num-of-cylinders` == "two" ~ 2,
    `num-of-cylinders` == "three" ~ 3,
    `num-of-cylinders` == "four" ~ 4,
    `num-of-cylinders` == "five" ~ 5,
    `num-of-cylinders` == "six" ~ 6,
    `num-of-cylinders` == "eight" ~ 8,
    `num-of-cylinders` == "twelve" ~ 12,
    TRUE ~ NA_integer_  # Default case if none of the above match
  ))

#Change - to _
auto_data <- auto_data %>% 
  rename_with(~gsub("-", "_", .x))

# Replace '?' with NA in character columns only
auto_data <- auto_data %>%
  mutate(across(where(is.character), ~na_if(.x, "?"))) %>%
  mutate(across(where(is.character), ~na_if(.x, "")))
#Get Numeric column names
num_col <- c("normalized_losses", "symboling", "num_of_doors", "wheel_base", "length", "width", 
             "height", "curb_weight", "num_of_cylinders", "engine_size", "bore", "stroke", 
             "compression_ratio", "horsepower", "peak_rpm", "city_mpg", "highway_mpg", "price")

# Drop rows with any NAs
auto_data <- drop_na(auto_data)

# List of categorical columns to be dummified
columns_to_dummify <- c("make", "fuel_type", "aspiration","body_style",
                        "drive_wheels", "engine_location", "engine_type",
                        "fuel_system")

for (colName in columns_to_dummify) {
  # Check if the column has at least two unique levels
  if (length(unique(auto_data[[colName]])) > 1) {
    formula <- reformulate(colName)
    dv <- dummyVars(formula, data = auto_data, fullRank = FALSE)
    dv_data <- predict(dv, newdata = auto_data)

    auto_data <- auto_data %>% select(-all_of(colName))
    auto_data <- cbind(auto_data, dv_data)
  }
}

# Replace NAs with 0s for dummy variables
auto_data[is.na(auto_data)] <- 0
#Change - to _
auto_data <- auto_data %>% 
  rename_with(~gsub("-", "_", .x))
# Make data numeric 
auto_data <- auto_data %>%
  mutate(
    normalized_losses = as.numeric(normalized_losses),
    symboling = as.numeric(symboling),
    num_of_doors = as.numeric(num_of_doors),
    wheel_base = as.numeric(wheel_base),
    length = as.numeric(length),
    width = as.numeric(width),
    height = as.numeric(height),
    curb_weight = as.numeric(curb_weight),
    num_of_cylinders = as.numeric(num_of_cylinders),
    engine_size = as.numeric(engine_size),
    bore = as.numeric(bore),
    stroke = as.numeric(stroke),
    compression_ratio = as.numeric(compression_ratio),
    horsepower = as.numeric(horsepower),
    peak_rpm = as.numeric(peak_rpm),
    city_mpg = as.numeric(city_mpg),
    highway_mpg = as.numeric(highway_mpg),
    price = as.numeric(price)
  )

# Boxplot for the numerical columns

auto_data_num <- auto_data %>%
pivot_longer(
    cols = c(normalized_losses, symboling, num_of_doors, wheel_base, length, width, height, curb_weight, 
               num_of_cylinders, engine_size, bore, stroke, compression_ratio, horsepower, peak_rpm, 
               city_mpg, highway_mpg, price),
      names_to = "variable",
      values_to = "value"
    )
# Plot boxplots
ggplot(auto_data_num, aes(x = variable, y = value)) + 
  geom_boxplot(aes(fill = variable)) + 
  facet_wrap(~ variable, scales = "free", ncol = 6) + 
  labs(y = "Value", x = "") + 
  theme_minimal() + 
  theme(legend.position = "none")

#cols = c(symboling, num_of_doors, wheel_base, length, width, height, curb_weight, 
         #num_of_cylinders, engine_size, bore, stroke, compression_ratio, horsepower, peak_rpm, 
         #city_mpg, highway_mpg, price)

# Histograms plotting for required columns
auto_data_num2 <- auto_data %>%
  pivot_longer(
    cols = c(symboling,length, width,
              engine_size, compression_ratio, horsepower, peak_rpm, 
             city_mpg, highway_mpg, price),
    names_to = "variable",
    values_to = "value"
  )
# Plot histograms
ggplot(auto_data_num2, aes(x = value)) + 
  geom_histogram(aes(fill = variable), bins = 30, position = "identity", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_x", ncol = 2) + 
  labs(x = "Value", y = "Count") + 
  theme_minimal() + 
  theme(legend.position = "none")

# FINDING THE STRONGEST POSITIVE AND NEGATIVE CORRELATIONS TO AVG LOSS PAYMENTS

quantvars <- auto_data[,num_col]
corr_matrix=cor(quantvars)
# Extract the correlation values for 'normalized_losses' only
auto_corr = corr_matrix['normalized_losses',]
# Round the values
rounded_auto_corr = round(auto_corr, 3)
View(rounded_auto_corr)

#correlation plot
corrplot(corr_matrix, method = "square", type = "upper", tl.col = "black", tl.srt = 45)

# Sort the values to get top positive correlations, and exclude the correlation of imdb_score with itself (which is 1)
top_positive = sort(auto_corr[auto_corr < 1], decreasing = TRUE)[1:10]
# Sort the values to get top negative correlations
top_negative = sort(auto_corr)[1:10]

top_positive
top_negative

# SCATTERPLOTS FOR ALL NUMERIC VARIABLES (NOT DUMMY COLUMNS)
# Create scatter plots using a for loop
plot_list <- list()
for (col in c("symboling","height","length","width","engine_size","horsepower","peak_rpm","city_mpg","highway_mpg","price")) {
  if (col %in% names(auto_data)) {
    plot <- ggplot(auto_data, aes_string(x = col, y = "normalized_losses")) +
      geom_point(alpha = 0.5) +
      labs(title = paste("normalized_losses vs", col), x = col, y = "avg_losses")
    plot_list[[col]] <- plot
  }
}

# Arrange the plots in a grid
do.call("grid.arrange", c(plot_list, ncol = 3))

# Perform PCA
pca_auto <- prcomp(auto_data[, num_col], scale. = TRUE)

# Enhanced PCA plot with dark blue loading lines
enhanced_plot <- autoplot(pca_auto, data = auto_data, loadings = TRUE, colour = 'grey', 
                          loadings.label = TRUE, loadings.label.size = 4, loadings.colour = 'black') +
  labs(title = "PCA - Principal Component Analysis",
       x = "Principal Component 1 (46.12%)",
       y = "Principal Component 2 (16.67%)") +
  theme_minimal() +
  theme(
    title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"
  ) +
  scale_color_discrete(name = "") +
  coord_fixed(ratio = 1)

# Print the enhanced PCA plot
print(enhanced_plot)


############# RANDOM FOREST ################################################
auto_data$normalized_losses <- as.numeric(as.character(auto_data$normalized_losses))
names(auto_data)
# List of all columns for the model
cols <- c("symboling", "num_of_doors", "wheel_base", "length", "width", "height", "curb_weight", 
           "num_of_cylinders", "engine_size", "bore", "stroke", "compression_ratio", 
          "horsepower", "peak_rpm", "city_mpg", "highway_mpg", "price", "makeaudi", "makebmw",
          "makechevrolet", "makedodge", "makehonda","makemazda", "makemercedes_benz", 
           "makemitsubishi", "makenissan", "makepeugot", "makeplymouth", "makeporsche", 
          "makesaab", "makesubaru", "maketoyota", "makevolkswagen", "makevolvo", "fuel_typediesel", "fuel_typegas", 
          "aspirationstd", "aspirationturbo", "body_styleconvertible", "body_stylehardtop", "body_stylehatchback", 
          "body_stylesedan", "body_stylewagon", "drive_wheels4wd", "drive_wheelsfwd", "drive_wheelsrwd", 
           "engine_typedohc","engine_typel", 
          "engine_typeohc", "engine_typeohcf", "engine_typeohcv", "fuel_system1bbl", "fuel_system2bbl", "fuel_systemidi", "fuel_systemmfi", "fuel_systemmpfi", "fuel_systemspdi")

#Single regression tree
mytree=rpart(formula,data=auto_data,control=rpart.control(cp=0.01))
rpart.plot(mytree)

# Construct the formula for the randomForest model
formula_str <- paste("normalized_losses ~", paste(cols, collapse = " + "))
formula <- as.formula(formula_str)
formula
# Build the randomForest model
myforest <- randomForest(formula, ntree=500, data=auto_data, importance=TRUE, na.action=na.omit, do.trace=50,keep.forest=TRUE)
myforest
importance(myforest)
varImpPlot(myforest)

# Comparing the results of the original model to a model without symboling score for interpretation
# Remove 'symboling' from 'cols' and create a formula string
formula_alt_str <- paste("normalized_losses ~", paste(setdiff(cols, "symboling"), collapse = " + "))
formula_alt_str <- gsub("symboling +", "", formula_alt_str)
formula_noscore<-as.formula(formula_alt_str)
formula_noscore
myforest_noscore <- randomForest(formula_noscore, ntree=500, data=auto_data, importance=TRUE, na.action=na.omit, do.trace=50,keep.forest=TRUE)
myforest_noscore
importance(myforest_noscore)
varImpPlot(myforest_noscore)


