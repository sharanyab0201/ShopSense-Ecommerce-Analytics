# ==========================================
# SHOPSENSE: SAVE ALL VISUALIZATIONS
# ==========================================

library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)

# STEP 1: CHECK THE DATASET

if (!file.exists("Online Retail.xlsx")) {
  stop("Online Retail.xlsx was not found in the project folder.")
}

retail_data <- read_excel("Online Retail.xlsx")

# STEP 2: CLEAN THE DATA

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

# STEP 3: CREATE A FOLDER FOR GRAPHS

dir.create("visualizations", showWarnings = FALSE)

# ==========================================
# GRAPH 1: TOP 10 PRODUCTS
# ==========================================

top_products <- valid_sales %>%
  filter(!grepl("POSTAGE", Description, ignore.case = TRUE)) %>%
  group_by(Description) %>%
  summarise(
    Revenue = sum(Sales),
    .groups = "drop"
  ) %>%
  slice_max(Revenue, n = 10, with_ties = FALSE)

p1 <- ggplot(
  top_products,
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

ggsave(
  "visualizations/01_top_products.png",
  p1, width = 10, height = 7, dpi = 300
)

# ==========================================
# GRAPH 2: ELBOW METHOD
# ==========================================

reference_date <- max(valid_sales$InvoiceDate) + 1

rfm_data <- valid_sales %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(reference_date - max(InvoiceDate)),
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(Sales),
    .groups = "drop"
  )

rfm_log <- rfm_data %>%
  select(Recency, Frequency, Monetary) %>%
  mutate(across(everything(), log1p))

rfm_scaled <- as.data.frame(scale(rfm_log))

set.seed(123)

wss <- sapply(1:8, function(k) {
  kmeans(
    rfm_scaled,
    centers = k,
    nstart = 10,
    iter.max = 100
  )$tot.withinss
})

elbow_data <- data.frame(
  Clusters = 1:8,
  WSS = wss
)

p2 <- ggplot(elbow_data, aes(x = Clusters, y = WSS)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "Elbow Method for Customer Segmentation",
    x = "Number of Clusters",
    y = "Within-Cluster Sum of Squares"
  ) +
  theme_minimal()

ggsave(
  "visualizations/02_elbow_method.png",
  p2, width = 9, height = 6, dpi = 300
)

# ==========================================
# GRAPH 3: CUSTOMER SEGMENTS
# ==========================================

set.seed(123)

kmeans_model <- kmeans(
  rfm_scaled,
  centers = 3,
  nstart = 25,
  iter.max = 100
)

rfm_data$Cluster <- as.factor(kmeans_model$cluster)

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
    x = "Customer Cluster",
    y = "Number of Customers"
  ) +
  theme_minimal()

ggsave(
  "visualizations/03_customer_segments.png",
  p3, width = 9, height = 6, dpi = 300
)

# ==========================================
# GRAPH 4: CUSTOMER INACTIVITY
# ==========================================

inactivity_summary <- rfm_data %>%
  mutate(
    Inactivity_Status = ifelse(
      Recency >= 90,
      "No purchase in 90+ days",
      "Purchase within 90 days"
    )
  ) %>%
  count(Inactivity_Status, name = "Customers")

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

ggsave(
  "visualizations/04_customer_inactivity.png",
  p4, width = 10, height = 6, dpi = 300
)

# ==========================================
# GRAPH 5: MONTHLY SALES
# ==========================================

monthly_sales <- valid_sales %>%
  mutate(Month = format(InvoiceDate, "%Y-%m")) %>%
  group_by(Month) %>%
  summarise(
    Revenue = sum(Sales),
    .groups = "drop"
  ) %>%
  arrange(Month)

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

ggsave(
  "visualizations/05_monthly_sales.png",
  p5, width = 10, height = 6, dpi = 300
)

# ==========================================
# GRAPH 6: SALES FORECAST
# ==========================================

sales_values <- monthly_complete$Revenue

full_ts <- ts(
  sales_values,
  start = c(2010, 12),
  frequency = 12
)

final_model <- ets(full_ts, model = "AAN")

png(
  "visualizations/06_sales_forecast.png",
  width = 1200,
  height = 800,
  res = 120
)

plot(
  forecast(final_model, h = 3),
  main = "Historical Monthly Sales Forecast",
  xlab = "Time",
  ylab = "Revenue (£)"
)

dev.off()

# ==========================================
# GRAPH 7: TOP 10 CUSTOMERS
# ==========================================

customer_value <- valid_sales %>%
  group_by(CustomerID) %>%
  summarise(
    Total_Spend = sum(Sales),
    Total_Orders = n_distinct(InvoiceNo),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Spend))

top_customers <- customer_value %>%
  slice_head(n = 10)

p7 <- ggplot(
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

ggsave(
  "visualizations/07_top_customers.png",
  p7, width = 10, height = 7, dpi = 300
)

# ==========================================
# FINISH
# ==========================================

cat("\nShopSense visualizations saved!\n\n")

print(list.files("visualizations"))