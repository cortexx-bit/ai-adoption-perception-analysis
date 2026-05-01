# Segment the data into two groups: users who have used AI systems and those who haven't
ai_users <- data %>% filter(UsedAIPreviously == "Yes")
non_ai_users <- data %>% filter(UsedAIPreviously == "No")
# Define the order of responses
response_order <- c("Strongly agree", "Agree", "Neither agree/disagree", "Disagree", "Strongly disagree", "NA")
# Convert columns to factors with the desired order
data$AIReliableInfo <- factor(data$AIReliableInfo, levels = response_order)
data$AIEasyToUse <- factor(data$AIEasyToUse, levels = response_order)
data$AIHelpsMeLearn <- factor(data$AIHelpsMeLearn, levels = response_order)


# Function to compare distributions of a specific column between two groups
compare_distributions <- function(column_name, group1, group2, title, x_label) {
  # Combine the two groups into a single data frame for plotting
  combined_data <- data.frame(
    Group = c(rep("AI Users", nrow(group1)), rep("Non-AI Users", nrow(group2))),
    Response = c(group1[[column_name]], group2[[column_name]])
  )
  # Ensure the Response column is in the desired order
  combined_data$Response <- factor(combined_data$Response, levels = response_order)
  
  # Plot the distributions
  ggplot(combined_data, aes(x = Response, fill = Group)) +
    geom_bar(position = "dodge") +
    labs(title = title,
         x = x_label,
         y = "Number of Users") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# Compare ease of use of AI systems
compare_distributions("AIEasyToUse", ai_users, non_ai_users, "Ease of Use of AI Systems: AI Users vs Non-AI Users", "Ease of Use")

# Compare whether AI helps users learn
compare_distributions("AIHelpsMeLearn", ai_users, non_ai_users, "AI Helps Me Learn: AI Users vs Non-AI Users","Level of Agreement")

# Compare opinions on universities teaching AI usage
compare_distributions("UniShouldShowMeHowToUseAI", ai_users, non_ai_users,"Opinions on Universities Teaching AI Usage: AI Users vs Non-AI Users", "Level of Agreement")







# Convert categorical variables to factors (if needed)
data$AIReliableInfo <- factor(data$AIReliableInfo, levels = c("Strongly disagree", "Disagree", "Neither agree/disagree", "Agree", "Strongly agree"))
data$AIComplexToUse <- factor(data$AIComplexToUse, levels = c("Strongly disagree", "Disagree", "Neither agree/disagree", "Agree", "Strongly agree"))
data$AIEnjoyableToUse <- factor(data$AIEnjoyableToUse, levels = c("Strongly disagree", "Disagree", "Neither agree/disagree", "Agree", "Strongly agree"))
data$AIEasyToUse <- factor(data$AIEasyToUse, levels = c("Strongly disagree", "Disagree", "Neither agree/disagree", "Agree", "Strongly agree"))
# Convert the sentiment columns to numeric (assuming they are Likert scale responses)
data <- data %>%
  mutate(across(c(AIEasyToUse, AIEnjoyableToUse, AIReliableInfo, AIComplexToUse), 
                ~ as.numeric(factor(., levels = c("Strongly disagree", "Disagree", "Neither agree/disagree", "Agree", "Strongly agree")))))

# Summary statistics
summary(data)

# Correlation matrix
cor(data[, c("HowManyTimesUsedLastMonth", "Year", "AIEasyToUse", "AIEnjoyableToUse", "AIReliableInfo", "AIComplexToUse")], use = "complete.obs")

# Scatterplot matrix
pairs(data[, c("HowManyTimesUsedLastMonth", "Year", "AIEasyToUse", "AIEnjoyableToUse", "AIReliableInfo", "AIComplexToUse")])

# Build the model
model <- lm(HowManyTimesUsedLastMonth ~ Year + AIEasyToUse + AIEnjoyableToUse + AIReliableInfo + AIComplexToUse, data = data)

# Summary of the model
summary(model)





# Build the multiple linear regression model
model <- lm(HowManyTimesUsedLastMonth ~ Year + AIEasyToUse + AIEnjoyableToUse + AIReliableInfo + AIComplexToUse, data = data)

# Summarize the model
summary(model)










# Remove rows with missing values in the chosen columns
data_clean <- data %>%
  select(HowManyTimesUsedLastMonth, Year, AIEasyToUse, AIEnjoyableToUse, AIReliableInfo, AIHelpsMeLearn) %>%
  na.omit()

# Convert Likert scale to numeric
likert_to_numeric <- function(x) {
  factor(x, levels = c("Strongly disagree", "Disagree", "Neither agree/disagree", "Agree", "Strongly agree"), labels = c(1, 2, 3, 4, 5))
}

data_clean <- data_clean %>%
  mutate(
    AIEasyToUse = as.numeric(likert_to_numeric(AIEasyToUse)),
    AIEnjoyableToUse = as.numeric(likert_to_numeric(AIEnjoyableToUse)),
    AIReliableInfo = as.numeric(likert_to_numeric(AIReliableInfo)),
    AIHelpsMeLearn = as.numeric(likert_to_numeric(AIHelpsMeLearn))
  )



# Fit the initial model
model <- lm(HowManyTimesUsedLastMonth ~ Year + AIEasyToUse + AIEnjoyableToUse + AIReliableInfo + AIHelpsMeLearn, data = data_clean)
summary(model)






# Standardize the predictors to calculate standardized coefficients
data_clean_standardized <- data_clean %>%
  mutate(across(c(Year, AIEasyToUse, AIEnjoyableToUse, AIReliableInfo, AIHelpsMeLearn), scale))

# Fit the standardized model
standardized_model <- lm(HowManyTimesUsedLastMonth ~ Year + AIEasyToUse + AIEnjoyableToUse + AIReliableInfo + AIHelpsMeLearn, data = data_clean_standardized)
summary(standardized_model)

# Extract standardized coefficients
standardized_coefficients <- coef(standardized_model)[-1]  # Exclude the intercept
print(standardized_coefficients)

# Identify the strongest predictor
strongest_predictor <- names(which.max(abs(standardized_coefficients)))
print(paste("The strongest predictor is:", strongest_predictor))


support_summary <- table(data$LecturersUseAISupportTeaching)
cat("Student Opinions on Lecturers Using AI to Support Teaching:\n")
print(support_summary)








# Check for multicollinearity using VIF
vif(model)


# 3. leverage
plot(model, which = 5)
std_resid <- rstandard(model)
plot(fitted(model), std_resid)
abline(h = c(-2, 2), col = "red")
# Find the indices of the largest absolute standardized residuals using a threshold of 2
outlier_indices <- which(abs(std_resid) > 2)
print(outlier_indices)
# observation 113, 155, 185 - influential
outliers <- c(91, 109, 113, 155, 185)
non_influential <- setdiff(1:nrow(data_clean), outliers)
n <- nrow(data_clean[non_influential, ]) # removes 4 records correctly, total 190
new_model <- lm(HowManyTimesUsedLastMonth ~ ., data = data_clean[non_influential, ])
summary(new_model)






