# Ensure the AI tool columns are numeric
ai_tool_columns <- c("ChatGPT", "Dalle", "ChatSonic", "Claude", "Bard.AI", "LaMDA", "Jasper.chat", "Socratic", "Bing.AI", "Other")
data[ai_tool_columns] <- lapply(data[ai_tool_columns], as.numeric)

# Count the number of individuals who have used AI systems, excluding NA values
ai_users <- sum(data$UsedAIPreviously == "Yes", na.rm = TRUE)
cat("Number of individuals who have used AI systems:", ai_users, "\n")

# Calculate the usage of specific AI tools using colSums
ai_tools <- colSums(data[, ai_tool_columns], na.rm = TRUE)

# Print the usage of specific AI tools
print(ai_tools)


# Summary statistics for frequency of AI tool usage in the past month
frequency_summary <- summary(data$HowManyTimesUsedLastMonth)

# Print the summary statistics
cat("Summary of Frequency of AI Tool Usage in the Past Month:\n")
print(frequency_summary)

# Plot the frequency distribution
ggplot(data, aes(x = HowManyTimesUsedLastMonth)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Frequency of AI Tool Usage in the Past Month",
       x = "Number of Times Used",
       y = "Number of Users") +
  theme_minimal()





# Summarize user perception of AI tools
benefit_summary <- table(data$AIIsValuableToMe)
enjoyment_summary <- table(data$AIEnjoyableToUse)

# Print the summaries
cat("User Perception of AI Tools (Beneficial):\n")
print(benefit_summary)

cat("\nUser Perception of AI Tools (Enjoyable):\n")
print(enjoyment_summary)

# Plot user perception of AI tools (Beneficial)
ggplot(data, aes(x = AIIsValuableToMe)) +
  geom_bar(fill = "green") +
  labs(title = "Perceived Value of AI Tools",
       x = "Perceived Value",
       y = "Number of Users") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot user perception of AI tools (Enjoyable)
ggplot(data, aes(x = AIEnjoyableToUse)) +
  geom_bar(fill = "orange") +
  labs(title = "Enjoyment of Using AI Tools",
       x = "Enjoyment",
       y = "Number of Users") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Summarize institutional support for AI usage
support_summary <- table(data$LecturersUseAISupportTeaching)

# Print the summary
cat("Institutional Support for AI Usage:\n")
print(support_summary)

# Plot institutional support
ggplot(data, aes(x = LecturersUseAISupportTeaching)) +
  geom_bar(fill = "purple") +
  labs(title = "Student Opinions on Lecturers Using AI to Support Teaching",
       x = "Level of Agreement",
       y = "Number of Users") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

