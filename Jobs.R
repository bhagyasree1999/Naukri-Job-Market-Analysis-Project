# Install the required packages and load the libraries
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("rpart")) install.packages("rpart", dependencies = TRUE)
library(tidyverse) # For data manipilation and Visualization
library(dplyr) # Used to perform data operations
library(ggplot2) # Used for creating graphs
library(tidytext) # For text mining and analysis of textual data
library(plotly) # For creating interactive plots and visualizations
library(readr)  # For efficient data reading and parsing
library(caret)   # For modeling and data partitioning
library(rpart)   # For decision tree models
library(corrplot) # For visualizing correlation matrices
library(scales)  # For formatting scales

### Step1: Data Loading

# Setting the correct path for the dataset
filepath <- "C:/r-workshop/data/raw/naukri.csv"

# Loading the dataset
Jobs <- read_csv(filepath)

### Step2: Data Inspection

# Initial data inspection
view(Jobs) # Used to view the dataset
glimpse (Jobs) # Gives the concise overview of the dataset

nrow(Jobs) # Gives the No.OfRows in the dataset
ncol(Jobs) # Gives the No.OfColumns in the dataset
str(Jobs) # Gives the detailed structure of the dataset

### Step3: Data Cleaning

# All the columns in the dataset are assigned to new_column_names
new_column_names <- c("JobTitle",
                      "JobTitleLink",
                      "Company",
                      "CompanyLink",
                      "RatingLink",
                      "CompanyRating",
                      "No.ofReviews",
                      "YearsOfExperience",
                      "LPA",
                      "JobDescription",
                      "Requirement1",
                      "Requirement2",
                      "Requirement3",
                      "Requirement4",
                      "Requirement5",
                      "Requirement6",
                      "Requirement7",
                      "Requirement8",
                      "Requirement9")

# Assign the new column names to the dataset
colnames(Jobs) <- new_column_names

view(Jobs)

# Remove the rows with any missing values.
Jobs <- na.omit(Jobs)

# Removes duplicate entries.
Jobs <- Jobs %>% distinct()

Jobs$JobTitle <- as.character(Jobs$JobTitle)
Jobs$CompanyRating <- as.numeric(Jobs$CompanyRating) #Company rating is coverted in to numeric datatype.

# Combine "Requirement1" to "Requirement9" into one column
Jobs <- Jobs %>%
  unite("Requirements", c(Requirement1,
                          Requirement2,
                          Requirement3,
                          Requirement4,
                          Requirement5,
                          Requirement6,
                          Requirement7,
                          Requirement8,
                          Requirement9),
        sep = ", ", na.rm = TRUE, remove = TRUE)

Jobs <- Jobs %>%
  select(-JobTitleLink)

# Extract the numerical part from the "No.ofReviews" column
Jobs <- Jobs %>% 
  mutate(No.ofReviews = parse_number(No.ofReviews))

#Converting character to numeric
Jobs$No.ofReviews <- as.numeric(Jobs$No.ofReviews)

# Extract just the numeric part of the range and keep it as a string
Jobs <- Jobs %>%
  mutate(YearsOfExperience = str_extract(YearsOfExperience, "\\d+-\\d+"))

# Selecting required columns that are required for visualizing the data.
Company_information <- Jobs %>%
  select(Company, 
         CompanyRating,
         No.ofReviews,
         JobTitle) %>%
  mutate(
    # Clean non-numeric characters from reviews
    No.ofReviews = as.numeric(gsub("\\D", "", No.ofReviews)), 
    
    # Standardize company names
    Company = tolower(Company), 
    
    # Ensure numeric ratings
    CompanyRating = as.numeric(CompanyRating), 
    
    # Standardize job titles
    JobTitle = tolower(JobTitle) 
  ) %>%
  na.omit() %>%  # Handle missing values
  distinct()

# Handle rare factors by grouping them into 'Other'
count_companies <- table(Company_information$Company)
count_titles <- table(Company_information$JobTitle)

# Threshold for considering a factor level rare
threshold <- 5

# Grouping the rare categories and job titles into 'Other'
Company_information$Company <- 
  ifelse(count_companies[Company_information$Company] 
         < threshold, 'Other', Company_information$Company)
Company_information$JobTitle <- 
  ifelse(count_titles[Company_information$JobTitle] 
         < threshold, 'Other', Company_information$JobTitle)

# Refactorize to ensure consistency
Company_information$Company <- factor(Company_information$Company)
Company_information$JobTitle <- factor(Company_information$JobTitle)

### Step4: Data Visualization

# Histogram of distribution of company ratings
rating_dist <- ggplot(Company_information, aes(x = CompanyRating)) +
  geom_histogram(binwidth = 0.5, fill = "#0073C2FF", color = "black") + 
  # Format x-axis labels
  scale_x_continuous(breaks = pretty_breaks(n = 10), limits = c(0, 5)) +
  # Format y-axis labels
  scale_y_continuous(labels = comma) +
  labs(title = "Distribution of Company Ratings", 
       subtitle = "Histogram showing the frequency distribution of company ratings",
       x = "Rating", 
       y = "Frequency") +
  theme_minimal() +  # Clean and minimal theme
  theme(
    plot.title = element_text(face = "bold", size = 14),  # Bold and larger title
    plot.subtitle = element_text(size = 12),  # Subtitle
    axis.title = element_text(size = 12),  # Axis titles
    axis.text = element_text(size = 10),  # Axis text
    panel.grid.major = element_line(color = "gray80"),  # Lighter grid lines
    panel.grid.minor = element_blank(),  # No minor grid lines
    legend.position = "none"  # Remove legend if not necessary
  )

print(rating_dist)

# Assuming 'Company_information' has columns 'Company' and 'avg_rating'
aggregated_data <- Company_information %>% 
  group_by(Company) %>% 
  summarize(avg_rating = mean(CompanyRating, na.rm = TRUE))

# Select the top 10 companies based on average rating
top_companies <- head(aggregated_data, 10)

Company_Avg_Rating <- ggplot(top_companies, 
            aes(x = reorder(Company,
                            avg_rating), 
                y = avg_rating)) +
  geom_bar(stat = "identity", 
           fill = "skyblue") +
  labs(title = "Top 10 Companies by Average Rating",
       x = "Company",
       y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplotly(Company_Avg_Rating)

# Scatter plot of total reviews vs. company rating
reviews_vs_rating <- ggplot(Company_information, 
                              aes(x = CompanyRating, 
                                  y = No.ofReviews)) +
  geom_point() +
  labs(title = "Total Reviews vs. Company Rating",
       x = "Company Rating", 
       y = "Total Reviews")

print(reviews_vs_rating)

top_companies_reviews <- Company_information %>%
  group_by(Company) %>%
  summarize(TotalReviews = sum(No.ofReviews, na.rm = TRUE)) %>%
  arrange(desc(TotalReviews)) %>%
  slice_head(n = 10)  # Select the top 10 companies with the most reviews

# Plotting the top 10 companies by total reviews
ggplot(top_companies_reviews, aes(x = reorder(Company, TotalReviews), y = TotalReviews, fill = Company)) +
  geom_col() +  # or geom_bar(stat = "identity") depending on your preference
  labs(title = "Top 10 Companies by Total Reviews", 
       x = "Company", 
       y = "Total Reviews") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Review counts are categorized into groups
Company_information$ReviewGroup <- cut(Company_information$No.ofReviews,
                                       breaks = quantile(Company_information$No.ofReviews, probs = 0:4 / 4, na.rm = TRUE),
                                       include.lowest = TRUE,
                                       labels = c("Very Low", "Low","Medium", "High"))

# Boxplot of Company Ratings by Review Groups
boxplot_rating_reviews <- ggplot(Company_information, aes(x = ReviewGroup, y = CompanyRating)) +
  geom_boxplot(aes(fill = ReviewGroup)) +
  labs(title = "Boxplot of Company Ratings by Review Group",
       x = "Review Group",
       y = "Company Rating") +
  theme_minimal() +
  theme(legend.position = "none")

print(boxplot_rating_reviews)

### Step5: Data Modeling
## Decision Trees

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
training_rows <- createDataPartition(Company_information$CompanyRating,
                                     p = 0.8, 
                                     list = FALSE)
training_data <- Company_information[training_rows, ]
testing_data <- Company_information[-training_rows, ]

# Fit a decision tree model
model_tree <- rpart(CompanyRating ~ ., data = training_data, method = "anova")
summary(model_tree)

# Predict and evaluate the model
predictions <- predict(model_tree, newdata = testing_data)
result <- data.frame(Actual = testing_data$CompanyRating, 
                     Predicted = predictions)

# Display the first six rows of the result dataframe to preview the actual vs predicted values
head(result)

# Calculating RMSE
rmse <- sqrt(mean((result$Actual - result$Predicted)^2))
print(paste("Root Mean Square Error:", rmse))

## Linear Regression
# Fit a linear regression model
linear_model <- lm(CompanyRating ~ ., data = training_data)
summary(linear_model)

# Predict and evaluate the linear regression model
linear_predictions <- predict(linear_model, newdata = testing_data)
linear_result <- data.frame(Actual = testing_data$CompanyRating, 
                            Predicted = linear_predictions)
head(linear_result)

# Calculate RMSE for the linear regression model
linear_rmse <- sqrt(mean((linear_result$Actual - linear_result$Predicted)^2))
print(paste("Root Mean Square Error for Linear Regression:", linear_rmse))

