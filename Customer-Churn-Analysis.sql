use churn_db;
SELECT * FROM customerID LIMIT 10;

-- 1. Data Exploration
-- Total customers count
SELECT COUNT(*) AS total_customers 
FROM customers_data;

-- View first 10 rows
SELECT * 
FROM customers_data 
LIMIT 10;


-- 2. KPIs Calculation
-- Calculate total churned customers and churn rate percentage
SELECT 
    COUNT(*) AS total_customers,
    SUM(CASE WHEN Churn = 'Yes' THEN 1 ELSE 0 END) AS churned_customers,
    ROUND(SUM(CASE WHEN Churn = 'Yes' THEN 1 ELSE 0 END) * 100.0 / COUNT(*), 2) AS churn_rate_percentage
FROM customers_data;


-- 3. Churn Drivers Analysis
-- A) Churn Rate by Contract Type
SELECT 
    Contract,
    COUNT(*) AS total_customers,
    SUM(CASE WHEN Churn = 'Yes' THEN 1 ELSE 0 END) AS churned_customers,
    ROUND(SUM(CASE WHEN Churn = 'Yes' THEN 1 ELSE 0 END) * 100.0 / COUNT(*), 2) AS churn_rate_percentage
FROM customers_data
GROUP BY Contract
ORDER BY churn_rate_percentage DESC;


-- B) Average Tenure and Monthly Charges by Churn Status
SELECT 
    Churn,
    COUNT(*) AS total_customers,
    ROUND(AVG(tenure), 1) AS avg_tenure_months,
    ROUND(AVG(MonthlyCharges), 2) AS avg_monthly_charges
FROM customers_data
GROUP BY Churn;


-- C) Churn Rate by Internet Service and Tech Support
SELECT 
    InternetService,
    TechSupport,
    COUNT(*) AS total_customers,
    SUM(CASE WHEN Churn = 'Yes' THEN 1 ELSE 0 END) AS churned_customers,
    ROUND(SUM(CASE WHEN Churn = 'Yes' THEN 1 ELSE 0 END) * 100.0 / COUNT(*), 2) AS churn_rate_percentage
FROM customers_data 
GROUP BY InternetService, TechSupport
ORDER BY churn_rate_percentage DESC;

-- D) Churn Rate by MonthlyCharge
SELECT 
    CASE 
        WHEN MonthlyCharges < 30 THEN 'Low ($0-$30)'
        WHEN MonthlyCharges >= 30 AND MonthlyCharges < 70 THEN 'Medium ($30-$70)'
        WHEN MonthlyCharges >= 70 AND MonthlyCharges < 100 THEN 'High ($70-$100)'
        ELSE 'Very High ($100+)'
    END AS Charge_Tier,
    COUNT(*) AS Total_Customers,
    SUM(CASE WHEN Churn = 'Yes' THEN 1 ELSE 0 END) AS Churn_Count,
    ROUND(SUM(CASE WHEN Churn = 'Yes' THEN 1 ELSE 0 END) * 100.0 / COUNT(*), 2) AS Churn_Rate_Percentage
FROM customers_data
GROUP BY 
    CASE 
        WHEN MonthlyCharges < 30 THEN 'Low ($0-$30)'
        WHEN MonthlyCharges >= 30 AND MonthlyCharges < 70 THEN 'Medium ($30-$70)'
        WHEN MonthlyCharges >= 70 AND MonthlyCharges < 100 THEN 'High ($70-$100)'
        ELSE 'Very High ($100+)'
    END
ORDER BY MIN(MonthlyCharges);
