# ---- Libraries ----
library(openxlsx)
library(ggplot2)
library(dplyr)
library(forcats)

# -------------------------
# 1) PARAMETERS
# -------------------------
toggleChurn <- "Active and Churn Cohort"
# toggleChurn <- "Churn Only Cohort"

file_path <- "../Data/telcoRds_1.xlsx"
set.seed(123)

# -------------------------
# 2) LOAD + CLEAN DATA
# -------------------------
if (!file.exists(file_path)) {
  stop("Excel file not found. Check file_path: ", file_path)
}

telcoRds <- openxlsx::read.xlsx(file_path)

colnames(telcoRds) <- c(
  "Subscriber_CustomerID","Subscriber_Gender","Subscriber_Senior_Citizen","Subscriber_Partner",
  "Subscriber_Dependents","Subscriber_TenureInMonths","Subscriber_PhoneService","Subscriber_MultipleLines",
  "Subscriber_Internet_Service","Subscriber_Online_Security","Subscriber_Online_Backup","Subscriber_Device_Protection",
  "Subscriber_Technical_Support","Subscriber_Streaming_TV_Online","Subscriber_Streaming_Movies_Online",
  "Subscriber_Contract_Type","Subscriber_Paperless_Billing","Subscriber_Payment_Method_Type",
  "Subscriber_Monthly_AccessFee","Subscriber_Total_Charges","churn","ChurnValue",
  "Subscriber_Churn_Score_Value","CLV","Subscriber_DisconnectionReason"
)

# Force valid, non-empty, unique names
nm <- names(telcoRds)
nm[is.na(nm) | nm == ""] <- paste0("X", which(is.na(nm) | nm == ""))
names(telcoRds) <- make.unique(nm)

telcoRds <- as.data.frame(telcoRds)

# Recode specific columns
telcoRds <- telcoRds |>
  mutate_at(7, \(x) as.factor(case_when(
    x == "No phone service" ~ "No",
    x == "No" ~ "No",
    x == "Yes" ~ "Yes",
    TRUE ~ as.character(x)
  ))) |>
  mutate_at(9:14, \(x) as.factor(case_when(
    x == "No internet service" ~ "No",
    x == "No" ~ "No",
    x == "Yes" ~ "Yes",
    TRUE ~ as.character(x)
  )))

# -------------------------
# 3) FILTER
# -------------------------
filteredData <- telcoRds
if (toggleChurn == "Churn Only Cohort") {
  filteredData <- filteredData |> filter(churn == "Yes")
}

cat("Rows total:", nrow(telcoRds), "\n")
cat("Rows after filter:", nrow(filteredData), "\n")
cat("Unique churn values:\n")
print(unique(telcoRds$churn))

if (nrow(filteredData) == 0) {
  stop("No rows after filtering. Your churn column may not contain 'Yes' exactly. Check printed unique values.")
}

# -------------------------
# 4) HELPERS
# -------------------------
safe_sample <- function(df, n = 1000) {
  df |> dplyr::sample_n(min(n, nrow(df)))
}

safe_mean <- function(x) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

# Ensure churn colors map to the levels present
churn_cols <- c("No" = "lightblue", "Yes" = "lightgray")

theme_common <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text  = element_text(size = 10)
  )

# -------------------------
# 5) PLOTS (STATIC)
# -------------------------

# Plot 1: Average Tenure by churn (bar of means)
df1 <- filteredData |>
  group_by(churn) |>
  summarise(avg_tenure = safe_mean(Subscriber_TenureInMonths), .groups = "drop")

g1 <- ggplot(df1, aes(x = churn, y = avg_tenure, fill = churn)) +
  geom_col() +
  geom_text(aes(label = paste0("Avg: ", round(avg_tenure, 0), " months")), vjust = -0.5) +
  labs(title = "Average Subscriber_TenureInMonths", x = "Churn", y = "Avg Tenure (Months)") +
  scale_fill_manual(values = churn_cols, drop = FALSE) +
  theme_common

# Plot 2: Average Monthly Charges by churn
df2 <- filteredData |>
  group_by(churn) |>
  summarise(avg_monthly = safe_mean(Subscriber_Monthly_AccessFee), .groups = "drop")

g2 <- ggplot(df2, aes(x = churn, y = avg_monthly, fill = churn)) +
  geom_col() +
  geom_text(aes(label = paste0("Avg: ", round(avg_monthly, 0))), vjust = -0.5) +
  labs(title = "Average Monthly Charges", x = "Churn", y = "Avg Monthly Charges") +
  scale_fill_manual(values = churn_cols, drop = FALSE) +
  theme_common

# Plot 3: Churn by Contract Type
g3 <- safe_sample(filteredData, 1000) |>
  ggplot(aes(x = fct_rev(Subscriber_Contract_Type), fill = fct_rev(churn))) +
  geom_bar(alpha = 1) +
  labs(title = "Customer Churn by Subscriber Contract Type", x = "Contract Type", y = "Count") +
  scale_fill_manual(values = churn_cols) +
  theme_common

# Plot 4: Churn by Gender
g4 <- filteredData |>
  mutate(Subscriber_Gender = as.factor(Subscriber_Gender)) |>
  ggplot(aes(x = Subscriber_Gender, fill = fct_rev(churn))) +
  geom_bar(alpha = 1) +
  labs(title = "Customer Churn on Subscriber Gender", x = "Gender", y = "Count") +
  scale_fill_manual(values = churn_cols) +
  theme_common

# Plot 5: Churn by Dependents
g5 <- filteredData |>
  mutate(Subscriber_Dependents = as.factor(Subscriber_Dependents)) |>
  ggplot(aes(x = Subscriber_Dependents, fill = fct_rev(churn))) +
  geom_bar(alpha = 1) +
  labs(title = "Customer Churn on Subscriber Dependents", x = "Dependents", y = "Count") +
  scale_fill_manual(values = churn_cols) +
  theme_common

# Plot 6: Churn by Partner
g6 <- filteredData |>
  mutate(Subscriber_Partner = as.factor(Subscriber_Partner)) |>
  ggplot(aes(x = Subscriber_Partner, fill = fct_rev(churn))) +
  geom_bar(alpha = 1) +
  labs(title = "Customer Churn on Subscriber Partner", x = "Partner", y = "Count") +
  scale_fill_manual(values = churn_cols) +
  theme_common

# Plot 7: Churn by Senior Citizen
g7 <- filteredData |>
  mutate(Subscriber_Senior_Citizen = as.factor(Subscriber_Senior_Citizen)) |>
  ggplot(aes(x = Subscriber_Senior_Citizen, fill = fct_rev(churn))) +
  geom_bar(alpha = 1) +
  labs(title = "Customer Churn on Subscriber Senior Citizen", x = "Senior Citizen", y = "Count") +
  scale_fill_manual(values = churn_cols) +
  theme_common

# Plot 8: Average CLV by churn
df8 <- filteredData |>
  group_by(churn) |>
  summarise(avg_clv = safe_mean(CLV), .groups = "drop")

g8 <- ggplot(df8, aes(x = churn, y = avg_clv, fill = churn)) +
  geom_col() +
  geom_text(aes(label = paste0("Avg: ", round(avg_clv, 0), " Euros")), vjust = -0.5) +
  labs(title = "Average Customer Lifetime Value", x = "Churn", y = "Avg CLV") +
  scale_fill_manual(values = churn_cols, drop = FALSE) +
  theme_common

# Plot 9: Static replacement for your 3-plot subplot
fd9 <- filteredData |>
  mutate(
    Subscriber_TenureInMonths = suppressWarnings(as.numeric(as.character(Subscriber_TenureInMonths))),
    CLV = suppressWarnings(as.numeric(as.character(CLV))),
    Subscriber_Monthly_AccessFee = suppressWarnings(as.numeric(as.character(Subscriber_Monthly_AccessFee)))
  )

g9a <- safe_sample(fd9, 1000) |>
  ggplot(aes(x = Subscriber_TenureInMonths, y = CLV, color = churn)) +
  geom_point(alpha = 0.6) +
  labs(title = "Tenure vs CLV", x = "Tenure (Months)", y = "CLV") +
  scale_color_manual(values = churn_cols) +
  theme_common

g9b <- safe_sample(fd9, 1000) |>
  ggplot(aes(x = Subscriber_TenureInMonths, y = Subscriber_Monthly_AccessFee, color = churn)) +
  geom_point(alpha = 0.6) +
  labs(title = "Tenure vs Monthly Charges", x = "Tenure (Months)", y = "Monthly Charges") +
  scale_color_manual(values = churn_cols) +
  theme_common

g9c <- fd9 |>
  ggplot(aes(x = Subscriber_TenureInMonths, fill = churn)) +
  geom_histogram(bins = 30, alpha = 0.8, position = "identity") +
  labs(title = "Distribution of Tenure (Months)", x = "Tenure (Months)", y = "Count") +
  scale_fill_manual(values = churn_cols) +
  theme_common

# Plot 10: Disconnection Drivers (bar chart - most robust for categorical)
g10 <- filteredData |>
  ggplot(aes(x = Subscriber_DisconnectionReason, fill = churn)) +
  geom_bar() +
  labs(title = "Histogram of Disconnection Drivers", x = "Disconnection Reason", y = "Count") +
  scale_fill_manual(values = churn_cols) +
  theme_common +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------
# 6) RENDER (Plots pane)
# -------------------------

print(g1)
print(g2)
print(g3)
print(g4)
print(g5)
print(g6)
print(g7)
print(g8)
print(g9a)
print(g9b)
print(g9c)
print(g10)

