# E-Commerce Analytics Project
# Step 1: Install required packages

packages <- c(
  "tidyverse",
  "lubridate",
  "janitor",
  "plotly",
  "shiny"
)

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

print("Package installation completed!")
install.packages("readxl")
# Step 1: Load the required library
library(readxl)

# Step 2: Import the dataset
retail_data <- read_excel("Online Retail.xlsx")

# Step 3: Display the first 6 rows
head(retail_data)

# Step 4: Check dataset size
dim(retail_data)

# Step 5: Display column names
colnames(retail_data)

# Step 6: Check data types
str(retail_data)

# Step 7: Generate a summary
summary(retail_data)
dim(retail_data)
# STEP 2: DATA QUALITY CHECKS

# Check missing values in each column
colSums(is.na(retail_data))

# Check duplicate rows
sum(duplicated(retail_data))

# Check negative quantities
sum(retail_data$Quantity < 0, na.rm = TRUE)

# Check zero or negative prices
sum(retail_data$UnitPrice <= 0, na.rm = TRUE)

# Check cancelled invoices
sum(grepl("^C", retail_data$InvoiceNo), na.rm = TRUE)

# Check the date range
range(retail_data$InvoiceDate, na.rm = TRUE)
# STEP 3: CLEAN THE DATA

# Keep the original dataset unchanged
retail_clean <- retail_data

# Remove exact duplicate rows
retail_clean <- retail_clean[!duplicated(retail_clean), ]

# Remove rows with missing product descriptions
retail_clean <- retail_clean[
  !is.na(retail_clean$Description), ]

# Remove rows with missing or blank descriptions
retail_clean <- retail_clean[
  trimws(retail_clean$Description) != "", ]

# Create a column for transaction value
retail_clean$Sales <- retail_clean$Quantity *
  retail_clean$UnitPrice

# Check the cleaned dataset
dim(retail_clean)

# Check missing values remaining
colSums(is.na(retail_clean))

# Check the first six cleaned records
head(retail_clean)

# STEP 4: OVERALL SALES ANALYSIS

# Calculate total sales value
total_sales <- sum(retail_clean$Sales, na.rm = TRUE)

# Count unique invoices
total_orders <- length(unique(retail_clean$InvoiceNo))

# Count unique products
total_products <- length(unique(retail_clean$StockCode))

# Count unique customers (excluding missing IDs)
total_customers <- length(unique(
  na.omit(retail_clean$CustomerID)
))

# Display the results
cat("Total Sales:", round(total_sales, 2), "\n")
cat("Total Orders:", total_orders, "\n")
cat("Unique Products:", total_products, "\n")
cat("Unique Customers:", total_customers, "\n")


# STEP 5: TOP 10 PRODUCTS BY REVENUE

library(dplyr)
library(ggplot2)

# Calculate revenue for each product
top_products <- retail_clean %>%
  group_by(Description) %>%
  summarise(
    Total_Revenue = sum(Sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 10)

# Display the top 10 products
print(top_products)

# Create a bar chart
ggplot(top_products,
       aes(x = reorder(Description, Total_Revenue),
           y = Total_Revenue)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Products by Revenue",
    x = "Product",
    y = "Revenue (£)"
  ) +
  theme_minimal()

# STEP 6: CUSTOMER SEGMENTATION USING RFM

library(dplyr)

# Remove rows without customer IDs
customer_data <- retail_clean %>%
  filter(!is.na(CustomerID))

# Set the reference date for recency calculation
reference_date <- max(customer_data$InvoiceDate, na.rm = TRUE) +
  1

# Calculate RFM metrics for each customer
rfm_data <- customer_data %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(
      reference_date - max(InvoiceDate)
    ),
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(Sales, na.rm = TRUE),
    .groups = "drop"
  )

# Display the first six customers
head(rfm_data)

# Check the number of customers
nrow(rfm_data)

# Summarise RFM metrics
summary(rfm_data)

# STEP 7: PREPARE RFM DATA FOR CLUSTERING

# Keep customers with valid positive monetary values
rfm_valid <- rfm_data %>%
  filter(
    Recency >= 0,
    Frequency > 0,
    Monetary > 0
  )

# Select the three RFM metrics
rfm_features <- rfm_valid %>%
  select(Recency, Frequency, Monetary)

# Apply log transformation to reduce skewness
rfm_log <- log1p(rfm_features)

# Standardize the metrics
rfm_scaled <- as.data.frame(scale(rfm_log))

# Check the prepared data
dim(rfm_scaled)

# Check the first six rows
head(rfm_scaled)

# Check whether any missing values remain
colSums(is.na(rfm_scaled))


# STEP 8: ELBOW METHOD

set.seed(123)

# Test different numbers of clusters
wss <- numeric(8)

for (k in 1:8) {
  model <- kmeans(
    rfm_scaled,
    centers = k,
    nstart = 10
  )
  
  wss[k] <- model$tot.withinss
}

# Create a data frame for plotting
elbow_data <- data.frame(
  Clusters = 1:8,
  WSS = wss
)

# Display the results
print(elbow_data)

# Plot the elbow chart
ggplot(elbow_data, aes(x = Clusters, y = WSS)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "Elbow Method for Customer Segmentation",
    x = "Number of Clusters (K)",
    y = "Within-Cluster Sum of Squares"
  ) +
  theme_minimal()

# STEP 9: CUSTOMER SEGMENTATION USING K-MEANS

# Set reproducible results
set.seed(123)

# Create 3 customer clusters
kmeans_model <- kmeans(
  rfm_scaled,
  centers = 3,
  nstart = 25
)

# Add cluster labels to customer data
rfm_valid$Cluster <- as.factor(kmeans_model$cluster)

# Count customers in each cluster
table(rfm_valid$Cluster)

# Summarise the RFM metrics by cluster
cluster_summary <- rfm_valid %>%
  group_by(Cluster) %>%
  summarise(
    Customers = n(),
    Avg_Recency = round(mean(Recency), 2),
    Avg_Frequency = round(mean(Frequency), 2),
    Avg_Monetary = round(mean(Monetary), 2),
    .groups = "drop"
  )

# Display the cluster summary
print(cluster_summary)

# STEP 10: DISPLAY COMPLETE CLUSTER RESULTS

print(cluster_summary, width = Inf)

# Display the average RFM values by cluster
cluster_summary %>%
  select(
    Cluster,
    Customers,
    Avg_Recency,
    Avg_Frequency,
    Avg_Monetary
  ) %>%
  print(width = Inf)

# STEP 11: VISUALIZE CUSTOMER SEGMENTS

# Count customers in each cluster
cluster_counts <- rfm_valid %>%
  count(Cluster, name = "Customers")

# Create a bar chart
ggplot(cluster_counts,
       aes(x = Cluster, y = Customers)) +
  geom_col() +
  geom_text(
    aes(label = Customers),
    vjust = -0.5
  ) +
  labs(
    title = "Customer Segmentation Using K-Means",
    x = "Customer Cluster",
    y = "Number of Customers"
  ) +
  theme_minimal()

# STEP 12: PREPARE VALID SALES TRANSACTIONS

library(dplyr)
library(ggplot2)

# Exclude cancelled invoices and invalid sales records
valid_sales <- retail_clean %>%
  filter(
    !is.na(CustomerID),
    !grepl("^C", InvoiceNo),
    Quantity > 0,
    UnitPrice > 0,
    Sales > 0
  ) %>%
  mutate(InvoiceDate = as.Date(InvoiceDate))

# Check the cleaned sales data
cat("Valid sales records:", nrow(valid_sales), "\n")

cat("Unique customers:",
    n_distinct(valid_sales$CustomerID), "\n")

cat("Unique invoices:",
    n_distinct(valid_sales$InvoiceNo), "\n")

# STEP 13: RECALCULATE RFM

# Use the day after the last valid transaction
reference_date <- max(valid_sales$InvoiceDate, na.rm = TRUE) + 1

rfm_data <- valid_sales %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(
      reference_date - max(InvoiceDate)
    ),
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(Sales, na.rm = TRUE),
    .groups = "drop"
  )

# Prepare the RFM features
rfm_log <- rfm_data %>%
  select(Recency, Frequency, Monetary) %>%
  mutate(across(everything(), log1p))

# Standardize the features
rfm_scaled <- as.data.frame(scale(rfm_log))

# Display the updated results
cat("Customers ready for segmentation:",
    nrow(rfm_scaled), "\n")

print(head(rfm_data))
print(summary(rfm_data))

# STEP 14: RECHECK THE ELBOW METHOD

set.seed(123)

wss <- numeric(8)

for (k in 1:8) {
  model <- kmeans(
    rfm_scaled,
    centers = k,
    nstart = 10
  )
  
  wss[k] <- model$tot.withinss
}

elbow_data <- data.frame(
  Clusters = 1:8,
  WSS = wss
)

ggplot(elbow_data, aes(x = Clusters, y = WSS)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "Elbow Method - Clean Sales Data",
    x = "Number of Clusters",
    y = "Within-Cluster Sum of Squares"
  ) +
  theme_minimal()
# STEP 15: FINAL CUSTOMER SEGMENTATION USING CLEAN DATA

set.seed(123)

# Create 3 customer clusters
kmeans_model <- kmeans(
  rfm_scaled,
  centers = 3,
  nstart = 25
)

# Attach cluster labels to customer data
rfm_data$Cluster <- as.factor(kmeans_model$cluster)

# Count customers in each cluster
print(table(rfm_data$Cluster))

# Summarise each customer group
cluster_summary <- rfm_data %>%
  group_by(Cluster) %>%
  summarise(
    Customers = n(),
    Avg_Recency = round(mean(Recency), 2),
    Avg_Frequency = round(mean(Frequency), 2),
    Avg_Monetary = round(mean(Monetary), 2),
    .groups = "drop"
  )

# Display all results
print(cluster_summary, width = Inf)

# Visualise the number of customers in each group
cluster_counts <- rfm_data %>%
  count(Cluster, name = "Customers")

ggplot(cluster_counts,
       aes(x = Cluster, y = Customers)) +
  geom_col() +
  geom_text(
    aes(label = Customers),
    vjust = -0.5
  ) +
  labs(
    title = "Customer Segmentation Using Clean Sales Data",
    x = "Customer Cluster",
    y = "Number of Customers"
  ) +
  theme_minimal()
print(cluster_summary, width = Inf)
# STEP 17: CUSTOMER INACTIVITY ANALYSIS

library(dplyr)
library(ggplot2)

# Classify customers using a 90-day threshold
inactivity_data <- rfm_data %>%
  mutate(
    Inactivity_Status = ifelse(
      Recency >= 90,
      "No purchase in 90+ days",
      "Purchase within 90 days"
    )
  )

# Count customers in each group
inactivity_summary <- inactivity_data %>%
  count(Inactivity_Status, name = "Customers") %>%
  mutate(
    Percentage = round(
      Customers / sum(Customers) * 100, 2
    )
  )

# Display the results
print(inactivity_summary)

# Visualize the groups
ggplot(
  inactivity_summary,
  aes(x = Inactivity_Status, y = Customers)
) +
  geom_col() +
  geom_text(
    aes(label = Customers),
    vjust = -0.5
  ) +
  labs(
    title = "Customer Inactivity Analysis",
    subtitle = "Based on a 90-day threshold in the historical dataset",
    x = "Customer Group",
    y = "Number of Customers"
  ) +
  theme_minimal()
print(inactivity_summary)
# STEP 19: MARKET BASKET ANALYSIS

# Install arules if it is not already installed
if (!requireNamespace("arules", quietly = TRUE)) {
  install.packages("arules")
}

library(arules)
library(dplyr)

# Prepare invoice-product combinations
basket_data <- valid_sales %>%
  filter(
    !is.na(Description),
    trimws(Description) != "",
    !grepl("POSTAGE", Description, ignore.case = TRUE)
  ) %>%
  distinct(InvoiceNo, Description)

# Group products by invoice
basket_list <- split(
  basket_data$Description,
  basket_data$InvoiceNo
)

# Convert the data into transactions
transactions <- as(basket_list, "transactions")

# Check the number of transactions and products
cat("Number of transactions:", length(transactions), "\n")
cat("Number of unique products:", length(itemLabels(transactions)), "\n")

# Display a summary
summary(transactions)
# STEP 20: GENERATE PRODUCT ASSOCIATION RULES

library(arules)

# Generate association rules
set.seed(123)

rules <- apriori(
  transactions,
  parameter = list(
    supp = 0.005,
    conf = 0.30,
    minlen = 2,
    maxlen = 3
  )
)

# Check how many rules were found
cat("Total association rules:", length(rules), "\n")

# Sort rules by lift
if (length(rules) > 0) {
  
  rules_sorted <- sort(
    rules,
    by = "lift",
    decreasing = TRUE
  )
  
  # Display the top 10 rules
  inspect(head(rules_sorted, 10))
  
} else {
  cat("No rules found. We will adjust the parameters.\n")
}
# STEP 20: PRODUCT ASSOCIATION RULES

library(arules)

# Generate association rules
set.seed(123)

rules <- apriori(
  transactions,
  parameter = list(
    supp = 0.005,
    conf = 0.30,
    minlen = 2,
    maxlen = 3
  )
)

# Display the total number of rules
cat("Total association rules:", length(rules), "\n")

# Display the top 10 rules by lift
if (length(rules) > 0) {
  
  rules_sorted <- sort(
    rules,
    by = "lift",
    decreasing = TRUE
  )
  
  inspect(head(rules_sorted, 10))
  
} else {
  cat("No rules found. We need to adjust the parameters.\n")
}
cat("Total association rules:", length(rules), "\n")
inspect(head(rules_sorted, 10))
# STEP 22: MONTHLY SALES ANALYSIS

library(dplyr)
library(ggplot2)

# Calculate monthly revenue and order count
monthly_sales <- valid_sales %>%
  mutate(
    Month = format(InvoiceDate, "%Y-%m")
  ) %>%
  group_by(Month) %>%
  summarise(
    Revenue = sum(Sales, na.rm = TRUE),
    Orders = n_distinct(InvoiceNo),
    .groups = "drop"
  ) %>%
  arrange(Month)

# Display monthly sales results
print(monthly_sales, n = Inf)

# Plot monthly revenue
ggplot(
  monthly_sales,
  aes(x = Month, y = Revenue, group = 1)
) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Monthly Sales Trend",
    subtitle = "Historical revenue from valid sales transactions",
    x = "Month",
    y = "Revenue (£)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
# STEP 24: SALES FORECASTING

# Install forecast package if needed
if (!requireNamespace("forecast", quietly = TRUE)) {
  install.packages("forecast")
}

library(forecast)
library(dplyr)
library(ggplot2)

# Exclude incomplete December 2011
sales_complete <- monthly_sales %>%
  filter(Month < "2011-12") %>%
  arrange(Month)

# Extract monthly revenue values
sales_values <- sales_complete$Revenue

# Use the first 9 months for training
train_values <- head(sales_values, length(sales_values) - 3)

# Use the last 3 complete months for testing
test_values <- tail(sales_values, 3)

# Create the training time series
train_ts <- ts(
  train_values,
  start = c(2010, 12),
  frequency = 12
)

# Fit a trend-based exponential smoothing model
sales_model <- ets(train_ts, model = "AAN")

# Forecast the next 3 months
test_forecast <- forecast(sales_model, h = 3)

# Compare predicted revenue with actual revenue
evaluation <- data.frame(
  Month = tail(sales_complete$Month, 3),
  Actual_Revenue = round(test_values, 2),
  Predicted_Revenue = round(as.numeric(test_forecast$mean), 2)
)

# Calculate evaluation metrics
MAE <- mean(abs(
  evaluation$Actual_Revenue -
    evaluation$Predicted_Revenue
))

RMSE <- sqrt(mean(
  (evaluation$Actual_Revenue -
     evaluation$Predicted_Revenue)^2
))

MAPE <- mean(abs(
  (evaluation$Actual_Revenue -
     evaluation$Predicted_Revenue) /
    evaluation$Actual_Revenue
)) * 100

# Display evaluation results
print(evaluation)

cat("MAE:", round(MAE, 2), "\n")
cat("RMSE:", round(RMSE, 2), "\n")
cat("MAPE:", round(MAPE, 2), "%\n")

# Refit the model using all 12 completed months
full_ts <- ts(
  sales_values,
  start = c(2010, 12),
  frequency = 12
)

final_model <- ets(full_ts, model = "AAN")

# Forecast the next 3 months in the historical series
future_forecast <- forecast(final_model, h = 3)

# Display future forecast
forecast_table <- data.frame(
  Month = c("2011-12", "2012-01", "2012-02"),
  Forecast_Revenue = round(
    as.numeric(future_forecast$mean), 2
  )
)

print(forecast_table)

# Plot the historical series and forecast
plot(
  forecast(final_model, h = 3),
  main = "Historical Monthly Sales Forecast",
  xlab = "Time",
  ylab = "Revenue (£)"
)
# Display actual versus predicted revenue
print(evaluation)

# Display forecasting errors
cat("MAE:", round(MAE, 2), "\n")
cat("RMSE:", round(RMSE, 2), "\n")
cat("MAPE:", round(MAPE, 2), "%\n")

# Display forecast for the next 3 months
print(forecast_table)
# STEP 26: CUSTOMER VALUE ANALYSIS

library(dplyr)
library(ggplot2)

# Calculate each customer's total spending and order count
customer_value <- valid_sales %>%
  group_by(CustomerID) %>%
  summarise(
    Total_Spend = sum(Sales, na.rm = TRUE),
    Total_Orders = n_distinct(InvoiceNo),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Spend)) %>%
  mutate(
    Revenue_Percentage =
      Total_Spend / sum(Total_Spend) * 100,
    Cumulative_Percentage =
      cumsum(Revenue_Percentage)
  )

# Display the top 10 customers
top_customers <- customer_value %>%
  slice_head(n = 10)

print(top_customers, width = Inf)

# Display the combined revenue share of the top 10
top_10_share <- sum(top_customers$Revenue_Percentage)

cat(
  "Top 10 customers' revenue share:",
  round(top_10_share, 2),
  "%\n"
)

# Visualize the top 10 customers
ggplot(
  top_customers,
  aes(
    x = reorder(factor(CustomerID), Total_Spend),
    y = Total_Spend
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Customers by Spending",
    x = "Customer ID",
    y = "Total Spending (£)"
  ) +
  theme_minimal()
# Display the top 10 customers
print(top_customers, width = Inf)

# Display the top 10 customers' revenue share
cat(
  "Top 10 customers' revenue share:",
  round(top_10_share, 2),
  "%\n"
)
# STEP 27: AUTOMATIC BUSINESS-INSIGHT GENERATOR

library(dplyr)

# Create a list to store business insights
business_insights <- c()

# INSIGHT 1: Highest-revenue month
best_month <- monthly_sales %>%
  slice_max(Revenue, n = 1, with_ties = FALSE)

business_insights <- c(
  business_insights,
  paste0(
    "Highest-revenue month: ",
    best_month$Month,
    " with revenue of £",
    format(round(best_month$Revenue, 2), big.mark = ",")
  )
)

# INSIGHT 2: Best-selling product by revenue
product_revenue <- valid_sales %>%
  group_by(Description) %>%
  summarise(
    Revenue = sum(Sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Revenue))

best_product <- product_revenue %>% slice_head(n = 1)

business_insights <- c(
  business_insights,
  paste0(
    "Highest-revenue product: ",
    best_product$Description,
    " (£",
    format(round(best_product$Revenue, 2), big.mark = ","),
    ")"
  )
)

# INSIGHT 3: Customer inactivity
inactive_customers <- inactivity_summary %>%
  filter(Inactivity_Status == "No purchase in 90+ days")

business_insights <- c(
  business_insights,
  paste0(
    "Customers with no purchase in 90+ days: ",
    inactive_customers$Customers,
    " (",
    inactive_customers$Percentage,
    "%)"
  )
)

# INSIGHT 4: Highest-value customer segment
best_cluster <- cluster_summary %>%
  slice_max(Avg_Monetary, n = 1, with_ties = FALSE)

business_insights <- c(
  business_insights,
  paste0(
    "Highest-spending customer segment: Cluster ",
    as.character(best_cluster$Cluster),
    ", with average recorded spending of £",
    format(round(best_cluster$Avg_Monetary, 2), big.mark = ",")
  )
)

# INSIGHT 5: Top 10 customer revenue contribution
business_insights <- c(
  business_insights,
  paste0(
    "Top 10 customers contribute ",
    round(top_10_share, 2),
    "% of total customer spending."
  )
)

# Display all business insights
cat("\n===== SHOPSENSE BUSINESS INSIGHTS =====\n\n")

for (i in seq_along(business_insights)) {
  cat(i, ". ", business_insights[i], "\n", sep = "")
}

cat("\n===== END OF INSIGHTS =====\n")
# ==========================================
# SHOPSENSE - INTERACTIVE ANALYTICS DASHBOARD
# ==========================================

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)
library(arules)


# ==========================================
# 1. LOAD AND CLEAN THE DATA
# ==========================================

retail_data <- read_excel("Online Retail.xlsx")

retail_clean <- retail_data %>%
  distinct() %>%
  filter(
    !is.na(Description),
    trimws(Description) != ""
  ) %>%
  mutate(
    Sales = Quantity * UnitPrice
  )

valid_sales <- retail_clean %>%
  filter(
    !is.na(CustomerID),
    !grepl("^C", InvoiceNo),
    Quantity > 0,
    UnitPrice > 0,
    Sales > 0
  ) %>%
  mutate(
    InvoiceDate = as.Date(InvoiceDate)
  )


# ==========================================
# 2. SALES ANALYSIS
# ==========================================

monthly_sales <- valid_sales %>%
  mutate(
    Month = format(InvoiceDate, "%Y-%m")
  ) %>%
  group_by(Month) %>%
  summarise(
    Revenue = sum(Sales),
    Orders = n_distinct(InvoiceNo),
    .groups = "drop"
  ) %>%
  arrange(Month)


top_products <- valid_sales %>%
  filter(
    !grepl("POSTAGE", Description, ignore.case = TRUE)
  ) %>%
  group_by(Description) %>%
  summarise(
    Revenue = sum(Sales),
    .groups = "drop"
  ) %>%
  slice_max(
    Revenue,
    n = 10,
    with_ties = FALSE
  )


# ==========================================
# 3. CUSTOMER RFM ANALYSIS
# ==========================================

reference_date <- max(valid_sales$InvoiceDate) + 1

rfm_data <- valid_sales %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(
      reference_date - max(InvoiceDate)
    ),
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(Sales),
    .groups = "drop"
  )


# ==========================================
# 4. CUSTOMER SEGMENTATION
# ==========================================

rfm_log <- rfm_data %>%
  select(Recency, Frequency, Monetary) %>%
  mutate(
    across(everything(), log1p)
  )

rfm_scaled <- as.data.frame(scale(rfm_log))

set.seed(123)

kmeans_model <- kmeans(
  rfm_scaled,
  centers = 3,
  nstart = 25,
  iter.max = 100
)

rfm_data$Cluster <- as.factor(
  kmeans_model$cluster
)

cluster_summary <- rfm_data %>%
  group_by(Cluster) %>%
  summarise(
    Customers = n(),
    Avg_Recency = round(mean(Recency), 2),
    Avg_Frequency = round(mean(Frequency), 2),
    Avg_Monetary = round(mean(Monetary), 2),
    .groups = "drop"
  )


# ==========================================
# 5. CUSTOMER INACTIVITY
# ==========================================

inactivity_summary <- rfm_data %>%
  mutate(
    Inactivity_Status = ifelse(
      Recency >= 90,
      "No purchase in 90+ days",
      "Purchase within 90 days"
    )
  ) %>%
  count(
    Inactivity_Status,
    name = "Customers"
  ) %>%
  mutate(
    Percentage = round(
      Customers / sum(Customers) * 100,
      2
    )
  )


# ==========================================
# 6. CUSTOMER VALUE ANALYSIS
# ==========================================

customer_value <- rfm_data %>%
  arrange(desc(Monetary))

top_customers <- customer_value %>%
  slice_head(n = 10)


# ==========================================
# 7. SALES FORECASTING
# ==========================================

sales_complete <- monthly_sales %>%
  filter(Month < "2011-12") %>%
  arrange(Month)

sales_values <- sales_complete$Revenue

# Hold out the final 3 complete months
train_values <- head(
  sales_values,
  length(sales_values) - 3
)

test_values <- tail(sales_values, 3)

train_ts <- ts(
  train_values,
  start = c(2010, 12),
  frequency = 12
)

sales_model <- ets(
  train_ts,
  model = "AAN"
)

test_forecast <- forecast(
  sales_model,
  h = 3
)

forecast_evaluation <- data.frame(
  Month = tail(sales_complete$Month, 3),
  Actual_Revenue = test_values,
  Predicted_Revenue = as.numeric(test_forecast$mean)
)

forecast_MAE <- mean(
  abs(
    forecast_evaluation$Actual_Revenue -
      forecast_evaluation$Predicted_Revenue
  )
)

forecast_RMSE <- sqrt(
  mean(
    (
      forecast_evaluation$Actual_Revenue -
        forecast_evaluation$Predicted_Revenue
    )^2
  )
)

forecast_MAPE <- mean(
  abs(
    (
      forecast_evaluation$Actual_Revenue -
        forecast_evaluation$Predicted_Revenue
    ) /
      forecast_evaluation$Actual_Revenue
  )
) * 100

full_ts <- ts(
  sales_values,
  start = c(2010, 12),
  frequency = 12
)

final_model <- ets(
  full_ts,
  model = "AAN"
)

future_forecast <- forecast(
  final_model,
  h = 3
)

forecast_table <- data.frame(
  Month = c("2011-12", "2012-01", "2012-02"),
  Forecast_Revenue = round(
    as.numeric(future_forecast$mean),
    2
  )
)


# ==========================================
# 8. SHINY DASHBOARD INTERFACE
# ==========================================

ui <- fluidPage(

  titlePanel(
    "ShopSense: E-Commerce Analytics Dashboard"
  ),

  p(
    "Explore historical sales, customers, products,",
    "and purchasing patterns."
  ),

  tabsetPanel(

    # OVERVIEW TAB
    tabPanel(
      "Overview",

      br(),

      h3("Business Overview"),

      fluidRow(
        column(
          3,
          wellPanel(
            h4("Recorded Revenue"),
            textOutput("total_sales")
          )
        ),

        column(
          3,
          wellPanel(
            h4("Invoices"),
            textOutput("total_orders")
          )
        ),

        column(
          3,
          wellPanel(
            h4("Products"),
            textOutput("total_products")
          )
        ),

        column(
          3,
          wellPanel(
            h4("Customers"),
            textOutput("total_customers")
          )
        )
      ),

      h4("Top Products by Revenue"),

      plotOutput("overview_products")
    ),


    # SALES TAB
    tabPanel(
      "Sales Trends",

      br(),

      plotOutput("monthly_plot"),

      h4("Monthly Revenue and Orders"),

      tableOutput("monthly_table")
    ),


    # PRODUCTS TAB
    tabPanel(
      "Products",

      br(),

      h3("Top 10 Products by Revenue"),

      plotOutput("products_plot"),

      tableOutput("products_table")
    ),


    # SEGMENTATION TAB
    tabPanel(
      "Customer Segments",

      br(),

      h3("RFM Customer Segmentation"),

      plotOutput("segments_plot"),

      tableOutput("segments_table")
    ),


    # INACTIVITY TAB
    tabPanel(
      "Customer Inactivity",

      br(),

      h3("Customer Inactivity Analysis"),

      p(
        "Customers are classified using a 90-day",
        "historical recency threshold."
      ),

      plotOutput("inactivity_plot"),

      tableOutput("inactivity_table")
    ),


    # CUSTOMER VALUE TAB
    tabPanel(
      "Customer Value",

      br(),

      h3("Top 10 Customers by Spending"),

      plotOutput("customer_value_plot"),

      tableOutput("customer_value_table")
    ),


    # FORECAST TAB
    tabPanel(
      "Sales Forecast",

      br(),

      h3("Historical Sales Forecast"),

      p(
        "Forecasts are historical demonstrations,",
        "not predictions of present-day sales."
      ),

      plotOutput("forecast_plot"),

      h4("Forecast Evaluation"),

      tableOutput("forecast_evaluation_table"),

      verbatimTextOutput("forecast_errors"),

      h4("Forecasted Revenue"),

      tableOutput("forecast_table")
    ),


    # MARKET BASKET TAB
    tabPanel(
      "Product Associations",

      br(),

      h3("Market Basket Analysis"),

      p(
        "Click the button to discover products",
        "that frequently appear together in invoices."
      ),

      actionButton(
        "run_rules",
        "Discover Product Associations"
      ),

      br(),
      br(),

      tableOutput("association_table")
    )
  )
)


# ==========================================
# 9. SHINY SERVER
# ==========================================

server <- function(input, output, session) {


  # OVERVIEW

  output$total_sales <- renderText({
    paste0(
      "\u00A3",
      format(
        round(sum(valid_sales$Sales), 2),
        big.mark = ","
      )
    )
  })

  output$total_orders <- renderText({
    format(
      n_distinct(valid_sales$InvoiceNo),
      big.mark = ","
    )
  })

  output$total_products <- renderText({
    format(
      n_distinct(valid_sales$StockCode),
      big.mark = ","
    )
  })

  output$total_customers <- renderText({
    format(
      n_distinct(valid_sales$CustomerID),
      big.mark = ","
    )
  })

  output$overview_products <- renderPlot({
    ggplot(
      top_products,
      aes(
        x = reorder(Description, Revenue),
        y = Revenue
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Top Products by Revenue",
        x = "Product",
        y = "Revenue (\u00A3)"
      ) +
      theme_minimal()
  })


  # SALES TRENDS

  output$monthly_plot <- renderPlot({
    ggplot(
      monthly_sales,
      aes(
        x = Month,
        y = Revenue,
        group = 1
      )
    ) +
      geom_line() +
      geom_point() +
      labs(
        title = "Monthly Revenue",
        x = "Month",
        y = "Revenue (\u00A3)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
  })

  output$monthly_table <- renderTable({
    monthly_sales
  })


  # PRODUCTS

  output$products_plot <- renderPlot({
    ggplot(
      top_products,
      aes(
        x = reorder(Description, Revenue),
        y = Revenue
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Top 10 Products",
        x = "Product",
        y = "Revenue (\u00A3)"
      ) +
      theme_minimal()
  })

  output$products_table <- renderTable({
    top_products
  })


  # CUSTOMER SEGMENTS

  output$segments_plot <- renderPlot({
    cluster_counts <- rfm_data %>%
      count(Cluster, name = "Customers")

    ggplot(
      cluster_counts,
      aes(
        x = Cluster,
        y = Customers
      )
    ) +
      geom_col() +
      geom_text(
        aes(label = Customers),
        vjust = -0.5
      ) +
      labs(
        title = "Number of Customers per Segment",
        x = "Cluster",
        y = "Customers"
      ) +
      theme_minimal()
  })

  output$segments_table <- renderTable({
    cluster_summary
  })


  # CUSTOMER INACTIVITY

  output$inactivity_plot <- renderPlot({
    ggplot(
      inactivity_summary,
      aes(
        x = Inactivity_Status,
        y = Customers
      )
    ) +
      geom_col() +
      geom_text(
        aes(label = Customers),
        vjust = -0.5
      ) +
      labs(
        title = "Customer Inactivity",
        x = "Customer Group",
        y = "Customers"
      ) +
      theme_minimal()
  })

  output$inactivity_table <- renderTable({
    inactivity_summary
  })


  # CUSTOMER VALUE

  output$customer_value_plot <- renderPlot({
    ggplot(
      top_customers,
      aes(
        x = reorder(
          factor(CustomerID),
          Monetary
        ),
        y = Monetary
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Top Customers by Spending",
        x = "Customer ID",
        y = "Recorded Spending (\u00A3)"
      ) +
      theme_minimal()
  })

  output$customer_value_table <- renderTable({
    top_customers
  })


  # SALES FORECAST

  output$forecast_plot <- renderPlot({
    plot(
      future_forecast,
      main = "Historical Sales Forecast",
      xlab = "Time",
      ylab = "Revenue (\u00A3)"
    )
  })

  output$forecast_evaluation_table <- renderTable({
    forecast_evaluation
  })

  output$forecast_errors <- renderText({
    paste0(
      "MAE: \u00A3",
      round(forecast_MAE, 2),
      "\nRMSE: \u00A3",
      round(forecast_RMSE, 2),
      "\nMAPE: ",
      round(forecast_MAPE, 2),
      "%"
    )
  })

  output$forecast_table <- renderTable({
    forecast_table
  })


  # MARKET BASKET ANALYSIS
  # Rules are calculated only when the user clicks the button.

  association_results <- eventReactive(
    input$run_rules,
    {
      basket_data <- valid_sales %>%
        filter(
          !is.na(Description),
          trimws(Description) != "",
          !grepl(
            "POSTAGE",
            Description,
            ignore.case = TRUE
          )
        ) %>%
        distinct(InvoiceNo, Description)

      basket_list <- split(
        basket_data$Description,
        basket_data$InvoiceNo
      )

      transactions <- as(
        basket_list,
        "transactions"
      )

      rules <- apriori(
        transactions,
        parameter = list(
          supp = 0.005,
          conf = 0.30,
          minlen = 2,
          maxlen = 3
        )
      )

      if (length(rules) == 0) {
        return(
          data.frame(
            Message = "No association rules found."
          )
        )
      }

      rules_sorted <- sort(
        rules,
        by = "lift",
        decreasing = TRUE
      )

      head(
        as(rules_sorted, "data.frame"),
        10
      )
    }
  )

  output$association_table <- renderTable({
    association_results()
  })
}


# ==========================================
# 10. START THE DASHBOARD
# ==========================================

shinyApp(
  ui = ui,
  server = server
)

#website : http://127.0.0.1:7505/

# ==========================================
# STEP 30: SAVE SHOPSENSE VISUALIZATIONS
# ==========================================

library(ggplot2)
library(dplyr)

# Create a folder for the images
dir.create("visualizations", showWarnings = FALSE)

# 1. TOP 10 PRODUCTS BY REVENUE
products_plot_data <- valid_sales %>%
  filter(!grepl("POSTAGE", Description, ignore.case = TRUE)) %>%
  group_by(Description) %>%
  summarise(
    Revenue = sum(Sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_max(Revenue, n = 10, with_ties = FALSE)

p1 <- ggplot(
  products_plot_data,
  aes(x = reorder(Description, Revenue), y = Revenue)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Products by Revenue",
    x = "Product",
    y = "Revenue (£)"
  ) +
  theme_minimal()

ggsave("visualizations/01_top_products.png",
       p1, width = 10, height = 7, dpi = 300)


# 2. ELBOW METHOD
p2 <- ggplot(elbow_data, aes(x = Clusters, y = WSS)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = elbow_data$Clusters) +
  labs(
    title = "Elbow Method for Customer Segmentation",
    x = "Number of Clusters",
    y = "Within-Cluster Sum of Squares"
  ) +
  theme_minimal()

ggsave("visualizations/02_elbow_method.png",
       p2, width = 9, height = 6, dpi = 300)


# 3. CUSTOMER SEGMENTS
cluster_counts <- rfm_data %>%
  count(Cluster, name = "Customers")

p3 <- ggplot(
  cluster_counts,
  aes(x = Cluster, y = Customers)
) +
  geom_col() +
  geom_text(aes(label = Customers), vjust = -0.5) +
  labs(
    title = "Customer Segmentation Using K-Means",
    x = "Cluster",
    y = "Number of Customers"
  ) +
  theme_minimal()

ggsave("visualizations/03_customer_segments.png",
       p3, width = 9, height = 6, dpi = 300)


# 4. CUSTOMER INACTIVITY
p4 <- ggplot(
  inactivity_summary,
  aes(x = Inactivity_Status, y = Customers)
) +
  geom_col() +
  geom_text(aes(label = Customers), vjust = -0.5) +
  labs(
    title = "Customer Inactivity Analysis",
    subtitle = "Based on a 90-day historical threshold",
    x = "Customer Group",
    y = "Number of Customers"
  ) +
  theme_minimal()

ggsave("visualizations/04_customer_inactivity.png",
       p4, width = 10, height = 6, dpi = 300)


# 5. MONTHLY SALES
monthly_complete <- monthly_sales %>%
  filter(Month < "2011-12")

p5 <- ggplot(
  monthly_complete,
  aes(x = Month, y = Revenue, group = 1)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Sales Trend",
    subtitle = "Completed months only",
    x = "Month",
    y = "Revenue (£)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("visualizations/05_monthly_sales.png",
       p5, width = 10, height = 6, dpi = 300)


# 6. SALES FORECAST
png(
  "visualizations/06_sales_forecast.png",
  width = 1200,
  height = 800,
  res = 120
)

plot(
  forecast::forecast(final_model, h = 3),
  main = "Historical Monthly Sales Forecast",
  xlab = "Time",
  ylab = "Revenue (£)"
)

dev.off()


# 7. TOP CUSTOMERS
p7 <- ggplot(
  top_customers,
  aes(
    x = reorder(factor(CustomerID), Monetary),
    y = Monetary
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Customers by Spending",
    x = "Customer ID",
    y = "Total Spending (£)"
  ) +
  theme_minimal()

ggsave("visualizations/07_top_customers.png",
       p7, width = 10, height = 7, dpi = 300)


# 8. SAVE MARKET BASKET RULES AS A TABLE
if (exists("rules_sorted") && length(rules_sorted) > 0) {
  write.csv(
    as(rules_sorted, "data.frame"),
    "visualizations/product_association_rules.csv",
    row.names = FALSE
  )
}

# CONFIRM THE FILES
cat("\nShopSense visualization files:\n")
print(list.files("visualizations"))
