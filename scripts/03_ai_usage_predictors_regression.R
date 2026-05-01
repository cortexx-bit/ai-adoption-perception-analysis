library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(car)
data <- read_csv(here("data", "cleaned_sentiments.csv"))


# Define the order of responses
likert_levels <- c(
  "Strongly disagree",
  "Disagree",
  "Neither agree/disagree",
  "Agree",
  "Strongly agree"
)

likert_columns <- c(
  "AIEasyToUse",
  "AIEnjoyableToUse",
  "AIReliableInfo",
  "AIComplexToUse",
  "AIHelpsMeLearn",
  "UniShouldShowMeHowToUseAI"
)

data <- data %>%
  mutate(
    across(
      all_of(likert_columns),
      ~ factor(.x, levels = likert_levels, ordered = TRUE)
    )
  )

# Segment the data into two groups: users who have used AI systems and those who haven't
ai_users <- data %>% filter(UsedAIPreviously == "Yes")
non_ai_users <- data %>% filter(UsedAIPreviously == "No")

# Function to compare distributions of a specific column between two groups
compare_distributions <- function(column_name, group1, group2, title, x_label) {
  # Combine the two groups into a single data frame for plotting
  combined_data <- data.frame(
    Group = c(rep("AI Users", nrow(group1)), rep("Non-AI Users", nrow(group2))),
    Response = c(as.character(group1[[column_name]]), as.character(group2[[column_name]]))
  )
  # Ensure the Response column is in the desired order
  combined_data$Response <- factor(combined_data$Response, levels = likert_levels, ordered=TRUE)
  
  ggplot(combined_data, aes(x = Response, fill = Group)) +
    geom_bar(position = "dodge") +
    labs(
      title = title,
      x = x_label,
      y = "Number of Users"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Compare ease of use of AI systems
compare_distributions(
  "AIEasyToUse",
  ai_users,
  non_ai_users,
  "Ease of Use of AI Systems: AI Users vs Non-AI Users",
  "Ease of Use"
)
# Compare whether AI helps users learn
compare_distributions(
  "AIHelpsMeLearn",
  ai_users,
  non_ai_users,
  "AI Helps Me Learn: AI Users vs Non-AI Users",
  "Level of Agreement"
)
# Compare opinions on universities teaching AI usage
compare_distributions(
  "UniShouldShowMeHowToUseAI",
  ai_users,
  non_ai_users,
  "Opinions on Universities Teaching AI Usage: AI Users vs Non-AI Users",
  "Level of Agreement"
)

data_model <- data %>%
  select(
    HowManyTimesUsedLastMonth,
    Year,
    AIEasyToUse,
    AIEnjoyableToUse,
    AIReliableInfo,
    AIComplexToUse,
    AIHelpsMeLearn
  ) %>%
  mutate(
    across(
      c(AIEasyToUse, AIEnjoyableToUse, AIReliableInfo, AIComplexToUse, AIHelpsMeLearn),
      ~ as.numeric(factor(.x, levels = likert_levels))
    )
  ) %>%
  na.omit()

summary(data_model)
# Correlation matrix
cor(data_model[, c("HowManyTimesUsedLastMonth", "Year", "AIEasyToUse", "AIEnjoyableToUse", "AIReliableInfo", "AIComplexToUse", "AIHelpsMeLearn")], use = "complete.obs")

# Scatterplot matrix
pairs(data_model[, c("HowManyTimesUsedLastMonth", "Year", "AIEasyToUse", "AIEnjoyableToUse", "AIReliableInfo", "AIComplexToUse", "AIHelpsMeLearn")])


# Fit the initial model
model <- lm(HowManyTimesUsedLastMonth ~ Year + AIEasyToUse + AIEnjoyableToUse + AIReliableInfo + AIHelpsMeLearn, data = data_model)
summary(model)

# Standardize the predictors to calculate standardized coefficients
data_model_standardized <- data_model %>%
  mutate(
    across(
      c(Year, AIEasyToUse, AIEnjoyableToUse, AIReliableInfo, AIHelpsMeLearn), 
      ~ as.numeric(scale(.x))))

# Fit the standardized model
standardized_model <- lm(HowManyTimesUsedLastMonth ~ Year + AIEasyToUse + AIEnjoyableToUse + AIReliableInfo + AIHelpsMeLearn, data = data_model_standardized)
summary(standardized_model)

# Extract standardized coefficients
standardized_coefficients <- coef(standardized_model)[-1]
print(standardized_coefficients)

# Identify the strongest predictor
strongest_predictor <- names(which.max(abs(standardized_coefficients)))
print(paste("The strongest predictor is:", strongest_predictor))

support_summary <- table(data$LecturersUseAISupportTeaching, useNA="ifany")
cat("Student Opinions on Lecturers Using AI to Support Teaching:\n")
print(support_summary)

# Check for multicollinearity using VIF
car::vif(model)

# Diagnostic plot for influential observations
plot(model, which = 5)
std_resid <- rstandard(model)
plot(fitted(model), std_resid, xlab= "Fitted values", ylab = "Standardized residuals", main = "Standardized Residuals vs Fitted Values")
abline(h = c(-2, 2), col = "red")
# Find the indices of the largest absolute standardized residuals using a threshold of 2
outlier_indices <- which(abs(std_resid) > 2)
print(outlier_indices)
# observation 113, 155, 185 - influential
data_model_no_outliers <- data_model[-outlier_indices, ]
# Refit the model without outliers
new_model <- lm(
  HowManyTimesUsedLastMonth ~ Year +
    AIEasyToUse +
    AIEnjoyableToUse +
    AIReliableInfo +
    AIComplexToUse +
    AIHelpsMeLearn,
  data = data_model_no_outliers
)

summary(new_model)





