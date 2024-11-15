
#############################Question 1
# Impoting the data set and loading neccesary packages.

library(ggplot2)
library(dplyr)
library(readxl)
library(psych)
df<-read_excel("D:/Big_Data_Files.xlsx",sheet = "TechSales_Reps")
# View the structure of the data frame
str(df)
df<-df[,-1]
df$Female<-factor(df$Female)

######################### Question 2

# Sumary statistics

# Using summary() to get basic statistics for each column
summary(df)
table(df$Female)
table(df$College)
table(df$Personality)
table(df$Business)


#########Female######################### Question 3

# Graphical visualization

# Graph for 'Business'
ggplot(df, aes(x = Business)) + geom_bar(fill="steelblue") +
  labs(title = "Distribution of Business Type", x = "Business Type", y = "Count")

# Graph for 'Age'
ggplot(df, aes(x = Age)) + geom_histogram(bins = 30, fill="darkgreen") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")

# Graph for 'Female'
ggplot(df, aes(x = as.factor(Female))) + geom_bar(fill="purple") +
  labs(title = "Gender Distribution", x = "Gender (0=Male, 1=Female)", y = "Count")

# Graph for 'Years'
ggplot(df, aes(x = Years)) + geom_histogram(bins = 30, fill="orange") +
  labs(title = "Distribution of Years in Position", x = "Years", y = "Frequency")

# Graph for 'College'
ggplot(df, aes(x = College)) + geom_bar(fill="red") +
  labs(title = "Distribution of College Education", x = "College (Yes/No)", y = "Count")

# Graph for 'Personality'
ggplot(df, aes(x = Personality)) + geom_bar(fill="blue") +
  labs(title = "Distribution of Personality Types", x = "Personality Type", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Graph for 'Certificates'
ggplot(df, aes(x = Certficates)) + geom_histogram(bins = max(df$Certficates) - min(df$Certficates), fill="brown") +
  labs(title = "Distribution of Certificates", x = "Certificates", y = "Frequency")

# Graph for 'Feedback'
ggplot(df, aes(x = Feedback)) + geom_histogram(bins = 30, fill="cyan") +
  labs(title = "Distribution of Feedback Scores", x = "Feedback", y = "Frequency")

# Graph for 'Salary'
ggplot(df, aes(x = Salary)) + geom_histogram(bins = 30, fill="magenta") +
  labs(title = "Distribution of Salary", x = "Salary", y = "Frequency")

# Graph for 'NPS'
ggplot(df, aes(x = NPS)) + geom_histogram(bins = max(df$NPS) - min(df$NPS), fill="gray") +
  labs(title = "Distribution of Net Promoter Score (NPS)", x = "NPS", y = "Frequency")

####################### Question 4

# Counting missing values for each column
sapply(df, function(x) sum(is.na(x)))

describe(df[,-c(1,3,5,6)])

# Summary of quantitative variables

prop.table(table(df$Business))*100
prop.table(table(df$College))*100
prop.table(table(df$Personality))*100
prop.table(table(df$Female))*100



############### Question 5


# NPS vs. Age
ggplot(df, aes(x = Age, y = NPS)) +
  geom_point(alpha = 0.5,col=9) +
  labs(title = "Scatterplot of NPS vs Age", x = "Age", y = "Net Promoter Score (NPS)") +
  theme_minimal()
#NPS vs. Years
ggplot(df, aes(x = Years, y = NPS)) +
  geom_point(alpha = 0.5,col=222) +
  labs(title = "Scatterplot of NPS vs Years of Experience", x = "Years of Experience", y = "Net Promoter Score (NPS)") +
  theme_minimal()
#NPS vs. Certificates
ggplot(df, aes(x = Certficates, y = NPS)) +
  geom_point(alpha = 0.5,col=89) +
  labs(title = "Scatterplot of NPS vs Number of Certificates", x = "Number of Certificates", y = "Net Promoter Score (NPS)") +
  theme_minimal()
#NPS vs. Feedback
ggplot(df, aes(x = Feedback, y = NPS)) +
  geom_point(alpha = 0.5,col=53) +
  labs(title = "Scatterplot of NPS vs Feedback Score", x = "Feedback Score", y = "Net Promoter Score (NPS)") +
  theme_minimal()
#NPS vs. Salary
ggplot(df, aes(x = Salary, y = NPS)) +
  geom_point(alpha = 0.5,col=111) +
  labs(title = "Scatterplot of NPS vs Salary", x = "Salary", y = "Net Promoter Score (NPS)") +
  theme_minimal()




# Correlation MAtrix

quantitative_vars <- df[, c("Age", "Years", "Certficates", "Feedback", "Salary", "NPS")]
correlation_matrix <- cor(quantitative_vars, use = "complete.obs")
library(reshape2)  # for melt function

# Transform the correlation matrix into a long format
cor_melt <- melt(correlation_matrix)
names(cor_melt) <- c("Variable1", "Variable2", "Correlation")
# Plotting
ggplot(data = cor_melt, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +  # creates the tiles
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black") +  # adds correlation values
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_minimal() +  # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # adjust text angle for x-axis labels
        axis.title = element_blank())  + ggtitle("Correlation MAtrix")


# Boxplots
ggplot(df, aes(x = Business, y = NPS, fill = Business)) +
  geom_boxplot() +
  labs(title = "NPS Distribution by Business Type", x = "Business Type", y = "Net Promoter Score (NPS)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(df, aes(x = as.factor(Female), y = NPS, fill = as.factor(Female))) +
  geom_boxplot() +
  labs(title = "NPS Distribution by Gender", x = "Gender (0 = Male, 1 = Female)", y = "Net Promoter Score (NPS)")
ggplot(df, aes(x = College, y = NPS, fill = College)) +
  geom_boxplot() +
  labs(title = "NPS Distribution by College Education", x = "College Education (Yes/No)", y = "Net Promoter Score (NPS)")
ggplot(df, aes(x = Personality, y = NPS, fill = Personality)) +
  geom_boxplot() +
  labs(title = "NPS Distribution by Personality Type", x = "Personality Type", y = "Net Promoter Score (NPS)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Taply

nps_business <- tapply(df$NPS, df$Business, function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Count = length(na.omit(x)))
})
print(nps_business)
nps_female <- tapply(df$NPS, df$Female, function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Count = length(na.omit(x)))
})
print(nps_female)
nps_college <- tapply(df$NPS, df$College, function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Count = length(na.omit(x)))
})
print(nps_college)
nps_personality <- tapply(df$NPS, df$Personality, function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Count = length(na.omit(x)))
})
print(nps_personality)


