# Load libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(purrr)
library(VIM)
library(corrplot)


# credit <- read.csv("credit_application.csv", stringsAsFactors = FALSE)
credit <- readRDS("credit_application.rds")

#Data preparation
credit <- credit %>%
  mutate(
    AGE = -DAYS_BIRTH / 365.25,
    DAYS_EMPLOYED_CLEAN = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED),
    EMPLOYED_STATUS = ifelse(DAYS_EMPLOYED == 365243, "Unemployed", "Employed")
  )


cat("Missing values per column:\n")
print(colSums(is.na(credit)))
aggr(credit, col = c('navyblue','red'), numbers = TRUE, sortVars = TRUE, labels = names(credit), cex.axis = .7, gap = 3, ylab = c("Missing data","Pattern"))

library(purrr)
library(ggplot2)
library(gridExtra)

plot_numerical_distributions <- function(df) {
  num_vars <- c("CNT_CHILDREN", "AMT_INCOME_TOTAL", "AGE", "DAYS_EMPLOYED_CLEAN", "CNT_FAM_MEMBERS")
  
  plots <- map(num_vars, function(var) {
    # Extract column vector for calculations
    col_vector <- df[[var]]
    
    ggplot(df, aes(x = .data[[var]])) +
      geom_histogram(
        bins = 30, 
        fill = "skyblue", 
        color = "white", 
        alpha = 0.7, 
        na.rm = TRUE
      ) +
      geom_density(
        aes(y = after_stat(density) * 
              diff(range(col_vector, na.rm = TRUE)) * 
              length(na.omit(col_vector)) / 30),
        color = "red", 
        size = 1, 
        na.rm = TRUE
      ) +
      labs(
        title = paste("Distribution of", var), 
        x = var, 
        y = "Frequency"
      ) +
      theme_minimal()
  })
  
  do.call(grid.arrange, c(plots, ncol = 2))
}


plot_numerical_distributions(credit)

#Boxplots for Outlier Detection
boxplot_vars <- c("CNT_CHILDREN", "AMT_INCOME_TOTAL", "AGE", "DAYS_EMPLOYED_CLEAN", "CNT_FAM_MEMBERS")
plots_box <- map(boxplot_vars, function(var) {
  ggplot(credit, aes(y = .data[[var]])) +
    geom_boxplot(fill = "lightgreen", outlier.color = "red", na.rm = TRUE) +
    labs(title = paste("Boxplot of", var), y = var) +
    theme_minimal()
})
do.call(grid.arrange, c(plots_box, ncol = 2))

#Categorical Distributions
cat_vars <- c("CODE_GENDER", "FLAG_OWN_CAR", "FLAG_OWN_REALTY", "NAME_INCOME_TYPE",
              "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "OCCUPATION_TYPE", "EMPLOYED_STATUS")
for (var in cat_vars) {
  print(
    ggplot(credit, aes_string(x = var)) +
      geom_bar(fill = 'tomato') +
      theme_minimal() +
      labs(title = paste("Distribution of", var), x = var, y = "Count") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  )
}

#STATUS Class Balance
ggplot(credit, aes(x = STATUS)) +
  geom_bar(fill = "#2ab7c6") +
  labs(title = "Class Balance: STATUS", x = "STATUS", y = "Count") +
  theme_minimal(base_size = 16)

#Correlation Matrix for Numeric Variables
num_data <- credit %>%
  select(CNT_CHILDREN, AMT_INCOME_TOTAL, AGE, DAYS_EMPLOYED_CLEAN, CNT_FAM_MEMBERS) %>%
  mutate_all(as.numeric)
cor_matrix <- cor(num_data, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

#Summary Table for Numeric Variables
summary(num_data)

#STATUS by EMPLOYED_STATUS
ggplot(credit, aes(x = STATUS, fill = EMPLOYED_STATUS)) +
  geom_bar(position = "dodge") +
  labs(title = "STATUS by Employment Status", x = "STATUS", y = "Count") +
  theme_minimal(base_size = 14)

#STATUS by CODE_GENDER
ggplot(credit, aes(x = STATUS, fill = CODE_GENDER)) +
  geom_bar(position = "dodge") +
  labs(title = "STATUS by Gender", x = "STATUS", y = "Count") +
  theme_minimal(base_size = 14)
