---
title: "MIS748 Times Series Fall 2023"
author: "Group Project"
date: "`r Sys.Date()`"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(zoo)
library(forecast)
library(fable)
library(prophet)
```

## Overview

This project's goals include developing predictive models to estimate sales as well as analyzing and extracting insights from a dataset of sales and customer information from a hypothetical superstore. The study is  particularly interested in understanding the patterns in total monthly sales as well as the number of unique clients each month from 2014 to 2017. Businesses may make data-driven decisions and improve business strategies through the  usage of these derived insights.

The following contributed to the project success:
[Group Member 1]: Responsible for data acquisition and initial data exploration and Conducted data preprocessing and assisted in data exploration
[Group Member 2]: Performed descriptive analysis and visualizations and Built and evaluated predictive models for sales in 2017.

## Data Source and Data Preparation (Part I)
### Data Source
Sales information from a fictitious superstore makes up the raw dataset that was utilized for this research, which was obtained via Kaggle. The dataset is containing Information related to Sales, Profits and other interesting facts of a Superstore giant. The data is composed of the following features or variables:

Row ID => Unique ID for each row. Order 

ID => Unique Order ID for each Customer. 

Order Date => Order Date of the product. 

Ship Date => Shipping Date of the Product. 

Ship Mode=> Shipping Mode specified by the Customer. 

Customer ID => Unique ID to identify each Customer. 

Customer Name => Name of the Customer. 

Segment => The segment where the Customer belongs. 

Country => Country of residence of the Customer. 

City => City of residence of of the Customer. 

State => State of residence of the Customer. 

Postal Code => Postal Code of every Customer. 

Region => Region where the Customer belong. 

Product ID => Unique ID of the Product. 

Category => Category of the product ordered. 

Sub-Category => Sub-Category of the product ordered. 

Product Name => Name of the Product 

Sales => Sales of the Product. 

Quantity => Quantity of the Product. 

Discount => Discount provided. 

Profit => Profit/Loss incurred.


### Data preparation

In this section, data wrangling/preparation is carried out. By definition, data preparation is the process of preparing raw data so that it is suitable for further processing and analysis. Key steps include collecting, cleaning, and labeling raw data into a form suitable for modeling/regression algorithms and then exploring and visualizing the data. To assure data quality, consistency, and fit for the project's goals, multiple processes were performed to prepare the raw data for additional analysis.

Data Import: The raw dataset was obtained from Kaggle in CSV format and imported into the R programming environment using the read.csv function. This initial step allowed us to load the data for further processing.

```{r superstore}
# Load the dataset
superstore <- read.csv("superstore.csv", header = TRUE)

head(superstore)
```


```{r, echo=FALSE}
# Number of records
dim(superstore)
```

Rename columns: To remove white spaces and other characters in column names,  the colnames() function function together with the gsub() function was used.

```{r, echo=FALSE}
# Rename columns by replacing "." with "_"
colnames(superstore) <- gsub("\\.", "", colnames(superstore))
head(superstore)
```

Date Transformation: The OrderDate column was converted to a proper date format, and additional columns for Year and Month were created for time-based analysis.
```{r, echo=FALSE}
# Convert "OrderDate" to a proper date format
superstore$OrderDate <- as.Date(superstore$OrderDate, format="%m/%d/%Y")

# Create new columns for Year and Month
superstore$Year <- format(superstore$OrderDate, "%Y")
superstore$Month <- format(superstore$OrderDate, "%m")
# Select only the necessary columns for further analysis
selected_columns <- c("OrderID", "OrderDate", "CustomerID", "Segment", "Country", 
                      "City", "State", "Region", "Category", "SubCategory", 
                      "ProductName", "Sales", "Quantity", "Discount", "Profit","Year","Month")

df <- superstore[, selected_columns]

head(df)
```

Drop unnecessary variables: In this section, some of the variables that are not useful for the subsequent analysis are eliminated. This is not the same as parameter tuning, it is general elimination based on preliminary information derived from the data.

### Timeseries 
```{r, echo=FALSE}
# Calculate monthly sales without grouping by Year
monthly_sales <- df %>%
  group_by(Year, Month) %>%
  summarize(TotalSales = sum(Sales, na.rm = TRUE), .groups = "drop")

# Calculate distinct customer count without grouping by Year
monthly_customers <- df %>%
  group_by(Year, Month) %>%
  summarize(DistinctCustomers = n_distinct(CustomerID, na.rm = TRUE), .groups = "drop")

# Filter data for the years 2014-2017
monthly_sales2014_2017 <- monthly_sales %>% filter(Year %in% c("2014", "2015", "2016", "2017"))
monthly_customers2014_2017 <- monthly_customers %>% filter(Year %in% c("2014", "2015", "2016", "2017"))
```

```{r, echo=FALSE}
monthly_customers2014_2017
```

```{r, echo=FALSE}
monthly_sales2014_2017
```
## Descriptive Analysis (Part II)

The section performs Exploratory Data Analysis (EDA) on the time series data. This involves understanding the characteristics of the time series, including trends, seasonality and outliers.  Descriptive analysis involves examining the data to summarize its main characteristics, uncover patterns, and identify relationships between variables. This project performed univariate, bivariate, and multivariate descriptive analysis on the resulting summary_data dataframe.

### Plot the Time Series using Line Graphs
In this section line graphs are used to visualize the time series data for both overall monthly sales and overall monthly count of distinct customers. This will give a preliminary understanding of the data's patterns and trends.

```{r, echo=FALSE}
# Create a line plot for monthly sales
ggplot(monthly_sales2014_2017, aes(x = Month, y = TotalSales, group = Year, color = Year)) +
  geom_line() +
  labs(title = "Monthly Sales Trends (2014-2017)",
       x = "Month",
       y = "Total Sales") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()
```
```{r, echo=FALSE}
# Create a line plot for monthly customer count
ggplot(monthly_customers2014_2017, aes(x = Month, y = DistinctCustomers, group = Year, color = Year)) +
  geom_line() +
  labs(title = "New Monthly Customer Trends (2014-2017)",
       x = "Month",
       y = "Distinct Customers") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()
```

### Explore Characteristics of the Time Series

#### Overall Monthly Customer Counts (2014-2017)

Analysis of Trends: Over the years, there seem to be some variations in the total monthly client numbers. Customer numbers increased considerably between 2014 and 2015, peaking in November of that year. Indicating probable patterns or seasonality, the statistics demonstrate fluctuations in client counts across time.

Analysis of Seasonality: There may be some seasonality in the data, with sporadic maxima appearing in particular months, such November and September. Seasonality is the term for recurring patterns that happen on a predictable schedule throughout the year.

Outliers: Some months, including September 2016, November 2016, and November 2017, had a disproportionately high number of customers. These could be a result of holidays, sales, or other circumstances.

#### Overall Monthly Sales (2014-2017)

Trend Analysis: The overall monthly sales also exhibit fluctuations throughout the years, with varying levels of sales in different months. There seems to be an increase in sales from 2014 to 2015, with fluctuations in subsequent years.

Seasonality Analysis: Similar to customer counts, there may be seasonality in sales, with peaks occurring in specific months, such as November and September. Seasonality in sales could be influenced by factors like holidays or promotions.

Outliers: There are some months with exceptionally high sales, such as December 2014, November 2015, and November 2017. These could be due to holiday season shopping.

### Explore Moving Averages of the Time Series
```{r,echo=FALSE}
# Convert the YearMonth column to a Date object for proper plotting
monthly_customers2014_2017$YearMonth <- as.Date(paste(monthly_customers2014_2017$Year, monthly_customers2014_2017$Month, "01", sep = "-"))
monthly_sales2014_2017$YearMonth <- as.Date(paste(monthly_sales2014_2017$Year, monthly_sales2014_2017$Month, "01", sep = "-"))

# Fill missing values with zeros before calculating moving averages
monthly_customers2014_2017$DistinctCustomers[is.na(monthly_customers2014_2017$DistinctCustomers)] <- 0
monthly_sales2014_2017$TotalSales[is.na(monthly_sales2014_2017$TotalSales)] <- 0


# Calculate and visualize 3-month moving averages for customer counts
monthly_customers2014_2017$MovingAvg3 <- rollmean(monthly_customers2014_2017$DistinctCustomers, k = 3, fill = NA)

# Calculate and visualize 3-month moving averages for sales
monthly_sales2014_2017$MovingAvg3 <- rollmean(monthly_sales2014_2017$TotalSales, k = 3, fill = NA)

# Calculate and visualize 12-month moving averages for customer counts
monthly_customers2014_2017$MovingAvg12 <- rollmean(monthly_customers2014_2017$DistinctCustomers, k = 12, fill = NA)

# Calculate and visualize 12-month moving averages for sales
monthly_sales2014_2017$MovingAvg12 <- rollmean(monthly_sales2014_2017$TotalSales, k = 12, fill = NA)
```

```{r, echo=FALSE}
# Plot customer counts with 3-month and 12-month moving averages
ggplot(monthly_customers2014_2017, aes(x = YearMonth)) +
  geom_line(aes(y = DistinctCustomers), color = "blue", size = 1, linetype = "solid") +
  geom_line(aes(y = MovingAvg3), color = "red", size = 1, linetype = "dashed") +
  geom_line(aes(y = MovingAvg12), color = "green", size = 1, linetype = "dotted") +
  labs(title = "Monthly Customer Counts with Moving Averages",
       x = "Year-Month",
       y = "Distinct Customers") +
  scale_color_manual(values = c("blue" = "blue", "red" = "red", "green" = "green")) +
  theme_minimal()
```
```{r, echo=FALSE}
# Plot sales with 3-month and 12-month moving averages
ggplot(monthly_sales2014_2017, aes(x = YearMonth)) +
  geom_line(aes(y = TotalSales), color = "blue", size = 1, linetype = "solid") +
  geom_line(aes(y = MovingAvg3), color = "red", size = 1, linetype = "dashed") +
  geom_line(aes(y = MovingAvg12), color = "green", size = 1, linetype = "dotted") +
  labs(title = "Monthly Sales with Moving Averages",
       x = "Year-Month",
       y = "Total Sales") +
  scale_color_manual(values = c("blue" = "blue", "red" = "red", "green" = "green")) +
  theme_minimal()
```

### Explore ACFs (Auto-Correlation Functions) of the Time Series

This study used ACF plots to understand the autocorrelation within the time series data. ACF plots show how each observation in the time series is related to previous observations. The acf() function is used for this analysis.

```{r, echo=FALSE}
# Calculate ACF for customer counts
acf_customers <- acf(monthly_customers2014_2017$DistinctCustomers, lag.max = 12, main = "ACF for Monthly Customer Counts")

# Plot ACF for customer counts
plot(acf_customers)
```



```{r, echo=FALSE}
# Calculate ACF for sales
acf_sales <- acf(monthly_sales2014_2017$TotalSales, lag.max = 12, main = "ACF for Monthly Sales")

# Plot ACF for sales
plot(acf_sales)
```

### Explore CCF (Cross-Correlation Functions) of Sales vs. Customers
Investigate the cross-correlation between sales and customer count to understand if changes in one variable are associated with changes in the other. The ccf() function is used to create cross-correlation plots.

```{r, echo=FALSE}
# Calculate CCF between sales and customer counts
ccf_sales_customers <- ccf(monthly_sales2014_2017$TotalSales, monthly_customers$DistinctCustomers, lag.max = 12, main = "CCF between Sales and Customer Counts")

# Plot CCF between sales and customer counts
plot(ccf_sales_customers)
```



At lag -12, the autocorrelation is 0.623, indicating a moderately strong positive correlation between 'X' and itself with a 12-month (or time period) lag.

At lag -11, the autocorrelation is 0.204, indicating a weaker positive correlation with an 11-month lag.

At lag -10, the autocorrelation is -0.022, indicating a very weak negative correlation with a 10-month lag.

The pattern continues for different lags up to lag 12, where the autocorrelation is 0.568, indicating a moderately strong positive correlation with a 12-month lead.

This observation reveals that there is a significant positive autocorrelation at lag 0 (no lag), and there are also some notable positive and negative correlations at other lags. These patterns can provide insights into the underlying time series structure, such as seasonality or trends.

```{r, echo=FALSE}
ccf_sales_customers
```

```{r}
# Assuming you have the cross-correlation values in 'ccf_sales_customers'
# You can create a cross-correlation plot

# Create a cross-correlation plot
plot(ccf_sales_customers, main = "Cross-Correlation between Sales and Customers")
```

```{r}
# Calculate PACF for sales
pacf_sales <- pacf(monthly_sales2014_2017$TotalSales, lag.max = 12, main = "PACF for Monthly Sales")

# Plot PACF for sales
plot(pacf_sales)
```

```{r}
# Calculate PACF for customer counts
pacf_customers <- pacf(monthly_customers2014_2017$DistinctCustomers, lag.max = 12, main = "PACF for Monthly Customer Counts")

# Plot PACF for customer counts
plot(pacf_customers)
```



## Part III: Modeling and Forecasting

```{r, echo=FALSE}
# Split data into training (2014-2016) and testing (2017) periods
train_sales <- monthly_sales %>% filter(Year < 2017)
test_sales <- monthly_sales %>% filter(Year == 2017)

train_customers <- monthly_customers %>% filter(Year < 2017)
test_customers <- monthly_customers %>% filter(Year == 2017)

# Define a function to calculate RMSE
calculate_rmse <- function(observed, forecasted) {
  sqrt(mean((observed - forecasted)^2, na.rm = TRUE))
}
```

### ARIMA (AutoRegressive Integrated Moving Average) Model

ARIMA is a commonly used time series forecasting method that can capture trends, seasonality, and autocorrelation.

```{r, echo=FALSE}
# Model 1: ARIMA for Sales
sales_arima <- auto.arima(train_sales$TotalSales)
sales_forecast_arima <- forecast(sales_arima, h = nrow(test_sales))
sales_rmse_arima <- calculate_rmse(test_sales$TotalSales, sales_forecast_arima$mean)
```

```{r, echo=FALSE}
# Model 2: ARIMA for Customers
customers_arima <- auto.arima(train_customers$DistinctCustomers)
customers_forecast_arima <- forecast(customers_arima, h = nrow(test_customers))
customers_rmse_arima <- calculate_rmse(test_customers$DistinctCustomers, customers_forecast_arima$mean)
```

```{r}
# Plot historical sales data and ARIMA forecast
plot(sales_forecast_arima, main = "Historical Sales Data and ARIMA Forecast")
```
```{r}
sales_point_forecasts <- sales_forecast_arima$mean
sales_point_forecasts
```

```{r}
sales_lower_prediction_intervals <- sales_forecast_arima$lower
sales_upper_prediction_intervals <- sales_forecast_arima$upper
sales_lower_prediction_intervals
sales_upper_prediction_intervals
```







### Exponential Smoothing (ETS) Model
ETS is another method for time series forecasting that can handle data with trend and seasonality.

```{r, echo=FALSE}
# Model 3: ETS for Sales
sales_train_ts <- ts(train_sales$TotalSales, start = c(2014, 1), frequency = 12)
sales_ets <- ets(sales_train_ts)
sales_forecast_ets <- forecast(sales_ets, h = 12)  # Forecast for 12 months
sales_rmse_ets <- calculate_rmse(test_sales$TotalSales, sales_forecast_ets$mean)
```

```{r, echo=FALSE}
# Model 4: ETS for Customers
customers_train_ts <- ts(train_customers$DistinctCustomers, start = c(2014, 1), frequency = 12)
customers_ets <- ets(customers_train_ts)
customers_forecast_ets <- forecast(customers_ets, h = 12)  # Forecast for 12 months
customers_rmse_ets <- calculate_rmse(test_customers$DistinctCustomers, customers_forecast_ets$mean)
```

This plot will display the historical data and the ETS forecast for the next 12 months. It typically includes the observed data, point forecasts, and prediction intervals.
```{r}
# Plot historical data and ETS forecast
plot(customers_forecast_ets, main = "Historical Data and ETS Forecast for Distinct Customers")
```

```{r}
# Plot point forecast with prediction intervals
plot(customers_forecast_ets$mean, main = "ETS Point Forecast with Prediction Intervals")
```
```{r}
# Plot historical data (observed values)
plot(customers_train_ts, main = "Historical Data for Distinct Customers")

```

```{r}
point_forecasts <- customers_forecast_ets$mean
point_forecasts 
```


```{r}
lower_prediction_intervals <- customers_forecast_ets$lower
upper_prediction_intervals <- customers_forecast_ets$upper
lower_prediction_intervals
upper_prediction_intervals
```





### Prophet Model
Prophet is a forecasting tool developed by Facebook that is particularly useful for time series data with seasonality and holiday effects.

```{r, echo=FALSE}
# Model 5: Prophet for Sales
sales_prophet_data <- data.frame(
  ds = as.Date(paste(train_sales$Year, train_sales$Month, "01", sep = "-")),
  y = train_sales$TotalSales
)
sales_prophet <- prophet(sales_prophet_data, yearly.seasonality = TRUE)
future_sales <- make_future_dataframe(sales_prophet, periods = 12)  # Forecast for 12 months
sales_forecast_prophet <- predict(sales_prophet, future_sales)
sales_rmse_prophet <- calculate_rmse(test_sales$TotalSales, sales_forecast_prophet$yhat)
```
```{r}
# Plot historical sales data
prophet_plot_components(sales_prophet, sales_forecast_prophet)

```
```{r}
lower_uncertainty <- sales_forecast_prophet$yhat_lower
upper_uncertainty <- sales_forecast_prophet$yhat_upper
lower_uncertainty
upper_uncertainty
```







```{r, echo=FALSE}
# Model 6: Prophet for Customers
customers_prophet_data <- data.frame(
  ds = as.Date(paste(train_customers$Year, train_customers$Month, "01", sep = "-")),
  y = train_customers$DistinctCustomers
)

customers_prophet <- prophet(customers_prophet_data, yearly.seasonality = TRUE)
future_customers <- make_future_dataframe(customers_prophet, periods = 12)  # Forecast for 12 months
customers_forecast_prophet <- predict(customers_prophet, future_customers)
customers_rmse_prophet <- calculate_rmse(test_customers$DistinctCustomers, customers_forecast_prophet$yhat)

```

## SARIMA Model

```{r, echo=FALSE}

# Model 7: SARIMA for Sales
sales_sarima <- auto.arima(train_sales$TotalSales, seasonal = TRUE)
sales_forecast_sarima <- forecast(sales_sarima, h = nrow(test_sales))
sales_rmse_sarima <- calculate_rmse(test_sales$TotalSales, sales_forecast_sarima$mean)


```

```{r, echo=FALSE}
# Model 8: SARIMA for Customers
customers_sarima <- auto.arima(train_customers$DistinctCustomers, seasonal = TRUE)
customers_forecast_sarima <- forecast(customers_sarima, h = nrow(test_customers))
customers_rmse_sarima <- calculate_rmse(test_customers$DistinctCustomers, customers_forecast_sarima$mean)

```

```{r, echo=FALSE}
# SARIMA diagnostic plots
checkresiduals(sales_sarima)
```

```{r, echo=FALSE}
# Plot historical sales data and SARIMA forecast
plot(sales_forecast_sarima, main = "Historical Sales Data and SARIMA Forecast")

```
```{r, echo=FALSE}
# Point forecasts for SARIMA
sales_point_forecasts_sarima <- sales_forecast_sarima$mean
sales_point_forecasts_sarima
```
```{r, echo=FALSE}
# Prediction intervals for SARIMA
sales_lower_prediction_intervals_sarima <- sales_forecast_sarima$lower
sales_upper_prediction_intervals_sarima <- sales_forecast_sarima$upper

sales_lower_prediction_intervals_sarima
sales_upper_prediction_intervals_sarima

```

```{r, echo=FALSE}
# Create a data frame to compare RMSE values

results_df <- data.frame(
  Model = c("ARIMA", "ETS", "Prophet", "SARIMA"),
  Sales_RMSE = c(sales_rmse_arima, sales_rmse_ets, sales_rmse_prophet, sales_rmse_sarima),
  Customers_RMSE = c(customers_rmse_arima, customers_rmse_ets, customers_rmse_prophet, customers_rmse_sarima)
)


# Print the RMSE values for sales and customers
print(results_df)
```

```{r, echo=FALSE}
# Select the best models
best_sales_model <- results_df[which.min(results_df$Sales_RMSE), "Model"]
best_customers_model <- results_df[which.min(results_df$Customers_RMSE), "Model"]

# Print the best models
cat("Best Sales Model:", best_sales_model, "\n")
cat("Best Customers Model:", best_customers_model, "\n")

```

Due to its low RMSE values in both situations, it appears that the ETS (Exponential Smoothing State Space Model) fared the best for projecting sales and customer counts. ETS would thus be the preferable forecasting model in this case based on RMSE.
```{r, echo=FALSE}
### Create Time series objects
ts_sales <- ts(monthly_sales2014_2017$TotalSales, start = c(2014, 1), frequency = 12)
ts_customers <- ts(monthly_customers2014_2017$DistinctCustomers, start = c(2014, 1), frequency = 12)
### Regression Modeling with ARIMA Errors
# Fit a regression model with ARIMA errors
fit <- Arima(ts_sales, xreg = ts_customers, order = c(1, 0, 2))
checkresiduals(fit)

# Forecast for the next 12 periods with regression model
# Make forecasts
forecasted_values <- forecast(fit, xreg = ts_customers, h = 12)

# Plot the forecasted values
plot(forecasted_values)

```

## Summary

The capacity to foresee future trends and consumer behavior is a useful skill in the commercial sector. We set out on a trip with this project to examine and predict sales and customer data from the years 2014 to 2017. Our intention was to learn from the past and apply what we learned to plan for the future. This essay addresses the relevance of our discoveries, summarizes our findings, and talks about the difficulties that were encountered.

The Exploratory Data investigation (EDA) stage served as the core of the study. The study explored the extensive nature of the dataset. It also explored time series data visually and discovered fascinating patterns. Sales began to exhibit seasonality, with clear peaks and valleys over time. Even just one realization has the power to completely alter company tactics. Additionally, the observed a consistent upward trend in the number of clients over time, which is encouraging for any company.

Beyond display, we developed moving averages to remove data fluctuation and more clearly illustrate underlying trends. The study was able to comprehend the intricate dynamics of our dataset by viewing how each data point linked to the ones before it using the Auto-Correlation Functions (ACF) and Cross-Correlation Functions (CCF).

During the study, three distinct models for both sales and customer forecasting: ARIMA, ETS, and Prophet were built. These models were trained on historical data and equipped to forecast future values. 

The Root Mean Square Error (RMSE) statistic was used to evaluate our models' performance critically. More accurate projections were reflected by lower RMSE values. With the lowest RMSE for both sales and customer forecasts in the study, ETS came out on top. This result highlights the significance of selecting the appropriate forecasting model—a choice that can have a significant effect on the bottom line.

