## ==============================================================================
## 1. Load Libraries & Read Data
## ==============================================================================
## Install packages if not already installed:## install.packages(c("tidyverse", "caret", "pROC", "car"))
    library(tidyverse)
    library(caret)
    library(pROC)
    library(car)

## Load the dataset
    data <- read.csv("D:/Data Analysis/customers_churn/Telco-Customer-Churn.csv")

    ## Preview the first few rows
head(data)

## ==============================================================================
## 2. Data Cleaning & Pre-processing
## ==============================================================================
  data_cleaned <- data %>%
  mutate(
  # Convert character columns to statistical factors
  Churn = as.factor(Churn),
  Contract = as.factor(Contract),
  InternetService = as.factor(InternetService),
  TechSupport = as.factor(TechSupport),
  PaymentMethod = as.factor(PaymentMethod)
  ) %>%
  
## Drop missing values in Monthly Charges if any exist
filter(!is.na(MonthlyCharges))

## ==============================================================================
## 3. Exploratory Visualizations (EDA)
## ==============================================================================
## Plot 1: Churn Rate Distribution by Contract Type
    ggplot(data_cleaned, aes(x = Contract, fill = Churn)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
    labs(title = "Churn Rate by Contract Type", x = "Contract", y = "Percentage") +
    theme_minimal()
    
## WHAT THIS ANALYSIS DOES & WHY IT MATTERS:
## This visual computes the proportional churn rate across different contract types.
## It proves whether short-term contracts have a higher risk of leaving than long-term ones,
## allowing the business to identify structural retention weaknesses.
## Plot 2: Monthly Charges Distribution by Churn Status (Boxplot)
    ggplot(data_cleaned, aes(x = Churn, y = MonthlyCharges, fill = Churn)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#3498db", "#e67e22")) +
    labs(title = "Monthly Charges vs Churn", x = "Churn", y = "Monthly Charges ($)") +
    theme_minimal()
    
## WHAT THIS ANALYSIS DOES & WHY IT MATTERS:## This boxplot shows the distribution, median,
## and spread of monthly bills for churned vs retained customers.
## It helps detect if price sensitivity exists by showing if lost customers were paying more on average.
## Statistical Summary: Investigate the relationship between Churn and Monthly Charges
    data_cleaned %>%
    group_by(Churn) %>%
    summarise(
    Customer_Count = n(),
    Average_Monthly_Charges = mean(MonthlyCharges, na.rm = TRUE),
    Median_Monthly_Charges = median(MonthlyCharges, na.rm = TRUE)
    )
    
## WHAT THIS ANALYSIS DOES & WHY IT MATTERS:
## This calculates the exact mathematical average and median spend for both customer groups.
## It converts the visual signals from the boxplot into hard baseline numbers to present to stakeholders.
## Plot 3: Monthly Charges Density by Churn Status (Density Plot)
ggplot(data_cleaned, aes(x = MonthlyCharges, fill = Churn)) +
geom_density(alpha = 0.5) +
scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
labs(
title = "Monthly Charges Density by Churn Status",
x = "Monthly Charges ($)",
y = "Density",
fill = "Churn Status"
) +
theme_minimal()

## WHAT THIS ANALYSIS DOES & WHY IT MATTERS:
## This continuous density curve pinpoints the exact price points where customer loss is heaviest.
## It reveals the "danger zones" (e.g., bills over $70) where churn concentration peaks,
## guiding pricing adjustments.
## ==============================================================================
## 4. Data Splitting & Imbalance Handling
## ==============================================================================
set.seed(123)
train_index <- createDataPartition(data_cleaned$Churn, p = 0.8, list = FALSE)
train_data <- data_cleaned[train_index, ]
test_data <- data_cleaned[-train_index, ]
## Handle class imbalance using Down-sampling on Training Data
train_features <- train_data %>% select(tenure, MonthlyCharges, Contract, InternetService, TechSupport)
train_target <- train_data$Churn
train_data_balanced <- downSample(x = train_features, y = train_target, yname = "Churn")
## WHAT THIS ANALYSIS DOES & WHY IT MATTERS:## In real-world data, the number of loyal customers heavily outweighs the number of churners.## This down-sampling balances the dataset to prevent the Machine Learning model from being biased,## ensuring it learns how to catch actual churners accurately instead of just predicting "No Churn" for everyone.## ==============================================================================## 5. Predictive Modeling & Statistical Diagnostics## ==============================================================================## Train the Logistic Regression Model
churn_model <- glm(
Churn ~ tenure + MonthlyCharges + Contract + InternetService + TechSupport,
data = train_data_balanced,
family = binomial(link = "logit")
)

## Print model summary
summary(churn_model)

## WHAT THIS ANALYSIS DOES & WHY IT MATTERS:
## This outputs the mathematical coefficients, log-odds, and p-values for each feature.
## It shows which factors are statistically significant drivers of churn and which ones are negligible.
## Smart Multicollinearity Check (VIF)
aliased_check <- alias(churn_model)$Complete
if (!is.null(aliased_check)) {
cat("Multicollinearity Warning: Some factors are perfectly correlated. Calculating VIF for non-aliased terms:\n")
print(vif(glm(Churn ~ tenure + MonthlyCharges, data = train_data_balanced, family = binomial)))
} else {
print(vif(churn_model))
}

## WHAT THIS ANALYSIS DOES & WHY IT MATTERS:
## VIF (Variance Inflation Factor) checks if any independent variables are heavily correlating with each other.
## High correlation (VIF > 5) destabilizes the model; this safety check ensures the predictors provide clean, independent insights.
## Plot 4: Feature Importance based on Model z-values (Fixed Names)
importance <- as.data.frame(summary(churn_model)$coefficients)
colnames(importance) <- c("Estimate", "Std_Error", "z_value", "p_value")
importance$Feature <- rownames(importance)
importance <- importance[-1, ]
ggplot(importance, aes(x = reorder(Feature, abs(z_value)), y = z_value, fill = z_value > 0)) +
geom_bar(stat = "identity", width = 0.6) +
coord_flip() +
scale_fill_manual(values = c("#2ecc71", "#e74c3c"), labels = c("Reduces Churn", "Increases Churn")) +
labs(
title = "Key Predictors of Customer Churn",
x = "Features",
y = "Impact Score (z-value)",
fill = "Effect Type"
) +
theme_minimal()
## WHAT THIS ANALYSIS DOES & WHY IT MATTERS:## This translates complex model weights into an intuitive, actionable executive chart.## It ranks drivers by impact: Red bars increase churn probability (risk factors), while green bars reduce it (retention hooks).## Model Evaluation
test_preds <- predict(churn_model, newdata = test_data, type = "response")
test_pred_class <- as.factor(ifelse(test_preds >= 0.5, "Yes", "No"))
conf_matrix <- confusionMatrix(test_pred_class, test_data$Churn, positive = "Yes")
print(conf_matrix$table)
roc_curve <- roc(test_data$Churn, test_preds)
cat("Model Accuracy:", round(conf_matrix$overall['Accuracy'] * 100, 2), "%\n")
cat("Model Sensitivity (True Churn Catch Rate):", round(conf_matrix$byClass['Sensitivity'] * 100, 2), "%\n")
cat("Area Under Curve (AUC):", round(auc(roc_curve), 2), "\n")
## WHAT THIS ANALYSIS DOES & WHY IT MATTERS:## This tests the model's predictive power on completely unseen validation data (test set).## It outputs critical metrics: Sensitivity tells us what percentage of real churners the model successfully caught,## and AUC validates the model's overall capability to distinguish between a leaving customer and a staying one.## ==============================================================================## 6. Risk Predictions & Validation## ==============================================================================## Generate churn probabilities for the entire dataset
data_cleaned$Churn_Probability <- predict(churn_model, newdata = data_cleaned, type = "response")
## Categorize customers into actionable risk tiers
data_cleaned <- data_cleaned %>%
mutate(
Risk_Level = case_when(
Churn_Probability >= 0.7 ~ "High Risk",
Churn_Probability >= 0.3 ~ "Medium Risk",
TRUE ~ "Low Risk"
)
)
## Risk Tiers Validation Summary
data_cleaned %>%
group_by(Risk_Level) %>%
summarise(
Total_Customers = n(),
Actual_Churn_Count = sum(Churn == "Yes"),
Actual_Churn_Rate = round((Actual_Churn_Count / Total_Customers) * 100, 2)
)
## WHAT THIS ANALYSIS DOES & WHY IT MATTERS:## This cross-references our model's predictions with actual historical outcomes to validate the risk segments.## Proving that the "High Risk" tier has a drastically higher actual churn rate than the "Low Risk" tier## gives stakeholders absolute confidence to back your marketing retention budget.## Export final enriched dataset for Power BI
write.csv(data_cleaned, "D:/Data Analysis/customers_churn/customers_with_predictions2.csv", row.names = FALSE)



