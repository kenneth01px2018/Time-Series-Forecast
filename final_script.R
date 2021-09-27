# Read in data and load packages (do NOT read this data in again)
library(ggplot2)
library(dplyr)
library(car)
library(forecast)
library(tseries)
library(astsa)
library(Metrics)
library(grid)
library(gridExtra)
library(imputeTS)
new_df <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
covid_df <- read.csv("Desktop/Stats Courses/Summer '21/SRP 199/Data/cases.csv")

# Plot covid cases for each country (as of 7/25)
usa_covid <- covid_df[covid_df["location"] == "United States", ]
usa_covid$date <- as.Date(usa_covid$date)
c1 <- ggplot(usa_covid, aes(x = date, y = new_cases)) +
  geom_line(color = "steelblue") + 
  xlab("Date") + ggtitle("Daily New Cases in the USA") + ylab("Number of Cases") + 
  scale_x_date(date_labels = "%b %d %Y")
jp_covid <- covid_df[covid_df["location"] == "Japan", ]
jp_covid$date <- as.Date(jp_covid$date)
c2 <- ggplot(jp_covid, aes(x = date, y = new_cases)) +
  geom_line(color = "steelblue") + 
  xlab("Date") + ggtitle("Daily New Cases in Japan") + ylab("Number of Cases") + 
  scale_x_date(date_labels = "%b %d %Y")
tw_covid <- covid_df[covid_df["location"] == "Taiwan", ]
tw_covid$date <- as.Date(tw_covid$date)
c3 <- ggplot(tw_covid, aes(x = date, y = new_cases)) +
  geom_line(color = "steelblue") + 
  xlab("Date") + ggtitle("Daily New Cases in Taiwan") + ylab("Number of Cases") + 
  scale_x_date(date_labels = "%b %d %Y")
cn_covid <- covid_df[covid_df["location"] == "China", ]
cn_covid$date <- as.Date(cn_covid$date)
c4 <- ggplot(cn_covid, aes(x = date, y = new_cases)) +
  geom_line(color = "steelblue") + 
  xlab("Date") + ggtitle("Daily New Cases in China") + ylab("Number of Cases") + 
  scale_x_date(date_labels = "%b %d %Y")
grid.arrange(c1, c2, c3, c4, ncol = 2)

# Save American data
usa_new <- new_df[new_df["location"] == "United States", ]
dim(usa_new)
usa_new$date <- as.Date(usa_new$date)
# Add a percent vaccination column
usa_pop <- 331449281
usa_new$percent_vaccinated <- usa_new$people_fully_vaccinated / usa_pop
dim(usa_new)
# Training data: every observation until 7/19
usa_df <- usa_new[1:212, ]
dim(usa_df)
# Summary statistics
summary(usa_df$percent_vaccinated)
sd(usa_df$percent_vaccinated, na.rm = T)

# Save Japanese data
jp_new <- new_df[new_df["location"] == "Japan", ]
jp_new$date <- as.Date(jp_new$date)
dim(jp_new)
# Add a percent vaccination column
jp_pop <- 126264931
jp_new$percent_vaccinated <- jp_new$people_fully_vaccinated / jp_pop
dim(jp_new)
# Training data: every observation until 7/19
jp_df <- jp_new[1:153, ]
dim(jp_df)
# Summary statistics
summary(jp_df$percent_vaccinated)
sd(jp_df$percent_vaccinated, na.rm = T)

# Save Taiwanese data
tw_new <- new_df[new_df["location"] == "Taiwan", ]
tw_new$date <- as.Date(tw_new$date)
dim(tw_new)
# Training data: every observation until 7/19
tw_df <- tw_new[1:121, ]
dim(tw_df)
# Summary statistics
summary(tw_df$total_vaccinations)
sd(tw_df$total_vaccinations, na.rm = T)

# Save Chinese data
cn_new <- new_df[new_df["location"] == "China", ]
cn_new$date <- as.Date(cn_new$date)
dim(cn_new)
# Training data: every observation until 7/19
cn_df <- cn_new[1:217, ]
dim(cn_df)
# Summary statistics
summary(cn_df$total_vaccinations)
sd(cn_df$total_vaccinations, na.rm = T)

# Create table of summary statistics
sum_stats <- data.frame(format(round(rbind(
  summary(usa_df$percent_vaccinated),
  summary(jp_df$percent_vaccinated),
  summary(tw_df$total_vaccinations),
  summary(cn_df$total_vaccinations)
), 5), scientific = F))
sum_stats$Country <- c("USA", "Japan", "Taiwan", "China")
sum_stats <- sum_stats[, c(8, 1:6)]
kable(sum_stats, caption = "Summary statistics for outcome variables", "pipe")

# Plot each time series
p1 <- ggplot(na.omit(usa_df[, c("date", "percent_vaccinated")]), aes(x = date, y = percent_vaccinated)) +
  geom_line(color = "steelblue") + ylim(0, 1) + 
  xlab("Date") + ggtitle("Percent (Fully) Vaccinated in the USA") + ylab("Percent") + 
  scale_x_date(date_labels = "%b %d %Y")
p2 <- ggplot(na.omit(jp_df[, c("date", "percent_vaccinated")]), aes(x = date, y = percent_vaccinated)) +
  geom_line(color = "steelblue") + ylim(0, 1) + 
  xlab("Date") + ggtitle("Percent (Fully) Vaccinated in Japan") + ylab("Percent") + 
  scale_x_date(date_labels = "%b %d %Y")
p3 <- ggplot(na.omit(tw_df[, c(3, 4)]), aes(x = date, y = total_vaccinations)) +
  geom_line(color = "steelblue") +  
  xlab("Date") + ggtitle("Total Doses in Taiwan") + ylab("Percent") + 
  scale_x_date(date_labels = "%b %d %Y")
p4 <- ggplot(na.omit(cn_df[, c(3, 4)]), aes(x = date, y = total_vaccinations)) +
  geom_line(color = "steelblue") +  
  xlab("Date") + ggtitle("Total Doses in China") + ylab("Percent") + 
  scale_x_date(date_labels = "%b %d %Y")
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Model Japanese time series with linear regression
jp_lm <- lm(percent_vaccinated ~ date, data = jp_df)
inverseResponsePlot(jp_lm)
jp_irp <- lm(I(percent_vaccinated^0.23) ~ date, data = jp_df)
summary(jp_irp)
plot(jp_irp)
# Modeling American time series with linear regression
usa_lm <- lm(percent_vaccinated ~ date, data = usa_df)
summary(usa_lm)
inverseResponsePlot(usa_lm)
# Modeling Taiwanese time series with linear regression
tw_lm <- lm(total_vaccinations ~ date, data = tw_df[-1, ])
summary(tw_lm)
inverseResponsePlot(tw_lm)
tw_irp <- lm(I(total_vaccinations^(.08)) ~ date, data = tw_df)
summary(tw_irp)
# Modeling Chinese time series with linear regression
cn_lm <- lm(total_vaccinations ~ date, data = cn_df)
summary(cn_lm)
inverseResponsePlot(cn_lm)
cn_irp <- lm(I(total_vaccinations^(1/8)) ~ date, data = cn_df)
summary(cn_irp)

# Plot Residuals vs Fitted for SLR models
par(mfrow = c(2, 2))
plot(usa_lm, which = 1, main = "Residuals vs Fitted (USA)")
plot(jp_irp, which = 1, main = "Residuals vs Fitted (Japan)")
plot(tw_irp, which = 1, main = "Residuals vs Fitted (Taiwan)")
plot(cn_irp, which = 1, main = "Residuals vs Fitted (China)")

# Print summary statistics for missing values
usa_missing <- usa_df$percent_vaccinated[26:212]
statsNA(usa_missing)
jp_missing <- jp_df$percent_vaccinated[22:153]
statsNA(jp_missing)
statsNA(tw_df$total_vaccinations)
statsNA(cn_df$total_vaccinations)

# Create table for statistics
ts_length <- c(187, 132, 121, 217)
ts_missing_count <- c(6, 10, 23, 85)
ts_percent <- c("3.21%", "7.58%", "19%", "39.2%")
bin1 <- c("4 NAs", "10 NAs", "2 NAs", "47 NAs")
bin2 <- c("0 NAs", "0 NAs", "5 NAs", "38 NAs")
bin3 <- c("1 NAs", "0 NAs", "12 NAs", "0 NAs")
bin4 <- c("1 NAs", "0 NAs", "4 NAs", "0 NAs")
missing_stats <- data.frame(
  "Country" = c("USA", "Japan", "Taiwan", "China"),
  "Length" = ts_length,
  "Missing Values" = ts_missing_count,
  "% of Total" = ts_percent,
  "Bin 1" = bin1,
  "Bin 2" = bin2,
  "Bin 3" = bin3,
  "Bin 4" = bin4
)
xtable2kable <- function(x) {
  out <- capture.output(print(x, table.placement = NULL))[-(1:2)]
  out <- paste(out, collapse = "\n")
  structure(out, format = "latex", class = "knitr_kable")
}
xtable(missing_stats, caption = "Test") %>%
  xtable2kable() %>%
  kable_styling(position = "center")
kable(missing_stats, "pipe")

# Create function to test imputations
test_imp <- function(df, col, p, d, q, is.auto = FALSE) {
  train_mape <- numeric(14)
  train_mae <- numeric(14)
  test_mape <- numeric(14)
  test_mae <- numeric(14)
  # Save number of rows for train-test-split
  n <- nrow(df)
  t_rows <- round(0.8 * n)
  temp_col <- df[, col]
  idx <- 1
  na_idx <- which(is.na(temp_col))
  # Loop through interpolations
  for (i in c("linear", "spline", "stine")) {
    # Impute missing values
    clean_col <- na_interpolation(temp_col, option = i)
    # Create training  and testing data
    clean_train <- data.frame(data = df$date[1:t_rows], col = clean_col[1:t_rows])
    colnames(clean_train)[2] <- col
    clean_test <- data.frame(data = df$date[(t_rows + 1):n], col = clean_col[(t_rows + 1):n])
    colnames(clean_test)[2] <- col
    if (is.auto) {
      mdl <- auto.arima(ts(clean_train[, col]))
      fitted_vals <- mdl$fitted
    } else {
      mdl <- arima(ts(clean_train[, col]), order = c(p, d, q))
      fitted_vals <- clean_train[, col] + mdl$residuals
    }
    train_mape[idx] <- mape(fitted_vals, clean_train[, col])
    train_mae[idx] <- mae(fitted_vals, clean_train[, col])
    future_vals <- forecast(mdl, h = nrow(clean_test))$mean
    test_mape[idx] <- mape(future_vals, clean_test[, col])
    test_mae[idx] <- mae(future_vals, clean_test[, col])
    idx <- idx + 1
  }
  # Loop through Kalman interpolations
  for (i in c("auto.arima", "StructTS")) {
    # Impute missing values
    clean_col <- na_kalman(temp_col, model = i)
    # Create training  and testing data
    clean_train <- data.frame(data = df$date[1:t_rows], col = clean_col[1:t_rows])
    colnames(clean_train)[2] <- col
    clean_test <- data.frame(data = df$date[(t_rows + 1):n], col = clean_col[(t_rows + 1):n])
    colnames(clean_test)[2] <- col
    if (is.auto) {
      mdl <- auto.arima(ts(clean_train[, col]))
      fitted_vals <- mdl$fitted
    } else {
      mdl <- arima(ts(clean_train[, col]), order = c(p, d, q))
      fitted_vals <- clean_train[, col] + mdl$residuals
    }
    train_mape[idx] <- mape(fitted_vals, clean_train[, col])
    train_mae[idx] <- mae(fitted_vals, clean_train[, col])
    future_vals <- forecast(mdl, h = nrow(clean_test))$mean
    test_mape[idx] <- mape(future_vals, clean_test[, col])
    test_mae[idx] <- mae(future_vals, clean_test[, col])
    idx <- idx + 1
  }
  # Loop through seadec interpolations
  for (i in c("interpolation", "kalman")) {
    # Impute missing values
    clean_col <- na_seadec(temp_col, algorithm = i)
    # Create training  and testing data
    clean_train <- data.frame(data = df$date[1:t_rows], col = clean_col[1:t_rows])
    colnames(clean_train)[2] <- col
    clean_test <- data.frame(data = df$date[(t_rows + 1):n], col = clean_col[(t_rows + 1):n])
    colnames(clean_test)[2] <- col
    if (is.auto) {
      mdl <- auto.arima(ts(clean_train[, col]))
      fitted_vals <- mdl$fitted
    } else {
      mdl <- arima(ts(clean_train[, col]), order = c(p, d, q))
      fitted_vals <- clean_train[, col] + mdl$residuals
    }
    train_mape[idx] <- mape(fitted_vals, clean_train[, col])
    train_mae[idx] <- mae(fitted_vals, clean_train[, col])
    future_vals <- forecast(mdl, h = nrow(clean_test))$mean
    test_mape[idx] <- mape(future_vals, clean_test[, col])
    test_mae[idx] <- mae(future_vals, clean_test[, col])
    idx <- idx + 1
  }
  # Loop through seasplit interpolations
  for (i in c("interpolation", "kalman")) {
    # Impute missing values
    clean_col <- na_seasplit(temp_col, algorithm = i)
    # Create training  and testing data
    clean_train <- data.frame(data = df$date[1:t_rows], col = clean_col[1:t_rows])
    colnames(clean_train)[2] <- col
    clean_test <- data.frame(data = df$date[(t_rows + 1):n], col = clean_col[(t_rows + 1):n])
    colnames(clean_test)[2] <- col
    if (is.auto) {
      mdl <- auto.arima(ts(clean_train[, col]))
      fitted_vals <- mdl$fitted
    } else {
      mdl <- arima(ts(clean_train[, col]), order = c(p, d, q))
      fitted_vals <- clean_train[, col] + mdl$residuals
    }
    train_mape[idx] <- mape(fitted_vals, clean_train[, col])
    train_mae[idx] <- mae(fitted_vals, clean_train[, col])
    future_vals <- forecast(mdl, h = nrow(clean_test))$mean
    test_mape[idx] <- mape(future_vals, clean_test[, col])
    test_mae[idx] <- mae(future_vals, clean_test[, col])
    idx <- idx + 1
  }
  for (i in c("locf", "nocb")) {
    # Impute missing values
    clean_col <- na_locf(temp_col, option = i)
    # Create training  and testing data
    clean_train <- data.frame(data = df$date[1:t_rows], col = clean_col[1:t_rows])
    colnames(clean_train)[2] <- col
    clean_test <- data.frame(data = df$date[(t_rows + 1):n], col = clean_col[(t_rows + 1):n])
    colnames(clean_test)[2] <- col
    if (is.auto) {
      mdl <- auto.arima(ts(clean_train[, col]))
      fitted_vals <- mdl$fitted
    } else {
      mdl <- arima(ts(clean_train[, col]), order = c(p, d, q))
      fitted_vals <- clean_train[, col] + mdl$residuals
    }
    train_mape[idx] <- mape(fitted_vals, clean_train[, col])
    train_mae[idx] <- mae(fitted_vals, clean_train[, col])
    future_vals <- forecast(mdl, h = nrow(clean_test))$mean
    test_mape[idx] <- mape(future_vals, clean_test[, col])
    test_mae[idx] <- mae(future_vals, clean_test[, col])
    idx <- idx + 1
  }
  for (i in c("simple", "linear", "exponential")) {
    # Impute missing values
    clean_col <- na_ma(temp_col, weighting = i)
    # Create training  and testing data
    clean_train <- data.frame(data = df$date[1:t_rows], col = clean_col[1:t_rows])
    colnames(clean_train)[2] <- col
    clean_test <- data.frame(data = df$date[(t_rows + 1):n], col = clean_col[(t_rows + 1):n])
    colnames(clean_test)[2] <- col
    if (is.auto) {
      mdl <- auto.arima(ts(clean_train[, col]))
      fitted_vals <- mdl$fitted
    } else {
      mdl <- arima(ts(clean_train[, col]), order = c(p, d, q))
      fitted_vals <- clean_train[, col] + mdl$residuals
    }
    train_mape[idx] <- mape(fitted_vals, clean_train[, col])
    train_mae[idx] <- mae(fitted_vals, clean_train[, col])
    future_vals <- forecast(mdl, h = nrow(clean_test))$mean
    test_mape[idx] <- mape(future_vals, clean_test[, col])
    test_mae[idx] <- mae(future_vals, clean_test[, col])
    idx <- idx + 1
  }
  imp_methods <- c("Linear", "Spline", "Stineman", "Kalman (ARIMA)", "Kalman (Structural)", "SeaDec (Interpolation)", "SeaDec (Kalman)",
                   "SeaSplit (Interpolation)", "SeaSplit (Kalman)", "LOCF", "NOCB", "Simple MA", 
                   "Linear MA", "Exponential MA")
  data.frame(Method = imp_methods, "Training MAPE" = train_mape, "Training MAE" = train_mae, "Test MAPE" = test_mape, "Test MAE" = test_mae)
}

# Test imputations
usa_clean <- usa_df[26:212, ]
jp_clean <- jp_df[22:153, ]
tw_clean <- tw_df
cn_clean <- cn_df
kable(test_imp(usa_clean, "percent_vaccinated", 3, 2, 3, T), "pipe") # NOCB
kable(test_imp(jp_clean, "percent_vaccinated", 7, 2, 0, T), "pipe") # Spline Interpolation
kable(test_imp(tw_clean[2:121, ], "total_vaccinations", 0, 2, 2, T), "pipe") # Simple MA
kable(test_imp(cn_clean, "total_vaccinations", 8, 2, 0, T), "pipe") # Linear MA

# Create function to tune parameters
tune_imp <- function(df, col, p, d, q, is.auto = FALSE, algorithm = "spline") {
  train_mape <- numeric(4)
  train_mae <- numeric(4)
  test_mape <- numeric(4)
  test_mae <- numeric(4)
  # Save number of rows for train-test-split
  n <- nrow(df)
  t_rows <- round(0.8 * n)
  temp_col <- df[, col]
  idx <- 1
  na_idx <- which(is.na(temp_col))
  # If tuning spline interpolation
  if (algorithm == "spline") {
    # Loop through interpolations
    for (i in c("fmm", "periodic", "natural", "hyman")) {
      # Impute missing values
      clean_col <- na_interpolation(temp_col, option = algorithm, method = i)
      # Create training  and testing data
      clean_train <- data.frame(data = df$date[1:t_rows], col = clean_col[1:t_rows])
      colnames(clean_train)[2] <- col
      clean_test <- data.frame(data = df$date[(t_rows + 1):n], col = clean_col[(t_rows + 1):n])
      colnames(clean_test)[2] <- col
      if (is.auto) {
        mdl <- auto.arima(ts(clean_train[, col]))
        fitted_vals <- mdl$fitted
      } else {
        mdl <- arima(ts(clean_train[, col]), order = c(p, d, q))
        fitted_vals <- clean_train[, col] + mdl$residuals
      }
      train_mape[idx] <- mape(fitted_vals, clean_train[, col])
      train_mae[idx] <- mae(fitted_vals, clean_train[, col])
      future_vals <- forecast(mdl, h = nrow(clean_test))$mean
      test_mape[idx] <- mape(future_vals, clean_test[, col])
      test_mae[idx] <- mae(future_vals, clean_test[, col])
      idx <- idx + 1
    }
    imp_methods <- c("fmm", "periodic", "natural", "hyman")
    data.frame(Method = imp_methods, "Training MAPE" = train_mape, "Training MAE" = train_mae, "Test MAPE" = test_mape, "Test MAE" = test_mae)
    # tuning moving average 
  } else {
    # Loop through interpolations
    for (i in c(2, 4, 6, 8)) {
      # Impute missing values
      clean_col <- na_ma(temp_col, weighting = algorithm, k = i)
      # Create training  and testing data
      clean_train <- data.frame(data = df$date[1:t_rows], col = clean_col[1:t_rows])
      colnames(clean_train)[2] <- col
      clean_test <- data.frame(data = df$date[(t_rows + 1):n], col = clean_col[(t_rows + 1):n])
      colnames(clean_test)[2] <- col
      if (is.auto) {
        mdl <- auto.arima(ts(clean_train[, col]))
        fitted_vals <- mdl$fitted
      } else {
        mdl <- arima(ts(clean_train[, col]), order = c(p, d, q))
        fitted_vals <- clean_train[, col] + mdl$residuals
      }
      train_mape[idx] <- mape(fitted_vals, clean_train[, col])
      train_mae[idx] <- mae(fitted_vals, clean_train[, col])
      future_vals <- forecast(mdl, h = nrow(clean_test))$mean
      test_mape[idx] <- mape(future_vals, clean_test[, col])
      test_mae[idx] <- mae(future_vals, clean_test[, col])
      idx <- idx + 1
    }
    params <- c("k = 2", "k = 4", "k = 6", "k = 8")
    data.frame(Parameter = params, "Training MAPE" = train_mape, "Training MAE" = train_mae, "Test MAPE" = test_mape, "Test MAE" = test_mae)
  }
}

# Tune parameters
kable(tune_imp(jp_clean, "percent_vaccinated", 7, 2, 0, T, algorithm = "spline"), "pipe") # Spline Interpolation
kable(tune_imp(tw_clean[2:121, ], "total_vaccinations", 0, 2, 2, T, algorithm = "simple"), "pipe") # Simple MA
kable(tune_imp(cn_clean, "total_vaccinations", 8, 2, 0, T, algorithm = "linear"), "pipe") # Linear MA

# Graph best imputations for each country
i1 <- ggplot_na_imputations(usa_missing, na_locf(usa_missing, option = "nocb"), title = "Imputed Values (USA - NOCB)")
i2 <- ggplot_na_imputations(jp_missing, na_interpolation(jp_missing, option = "spline"), title = "Imputed Values (Japan - Spline Interpolation)")
grid.arrange(i1, i2, ncol = 1)
i3 <- ggplot_na_imputations(tw_df$percent_vaccinated, na_ma(tw_df$percent_vaccinated, weighting = "simple", k = 6), title = "Imputed Values (Taiwan - Simple MA)")
i4 <- ggplot_na_imputations(cn_df$percent_vaccinated, na_ma(cn_df$percent_vaccinated, weighting = "linear"), title = "Imputed Values (China - Linear MA)")
grid.arrange(i3, i4, ncol = 1)
grid.arrange(i1, i2, i3, i4, ncol = 2)

# copy "clean" dataframes to new variable names
usa_cleaned <- usa_clean
jp_cleaned <- jp_clean
tw_cleaned <- tw_clean
cn_cleaned <- cn_clean

# Impute NAs
usa_cleaned$percent_vaccinated <- na_locf(usa_clean$percent_vaccinated, option = "nocb")
jp_cleaned$percent_vaccinated <- na_interpolation(jp_clean$percent_vaccinated, option = "spline")
tw_cleaned$total_vaccinations <- na_ma(tw_clean$total_vaccinations, weighting = "simple", k = 6)
cn_cleaned$total_vaccinations <- na_ma(cn_clean$total_vaccinations, weighting = "linear")

# Graph ACF and PACF for USA
par(mfrow = c(2, 2))
acf(usa_cleaned$percent_vaccinated, main = "USA ACF")
acf(diff(diff(usa_cleaned$percent_vaccinated)), main = "Differenced USA ACF")
pacf(usa_cleaned$percent_vaccinated, main = "USA PACF")
pacf(diff(diff(usa_cleaned$percent_vaccinated)), main = "Differenced USA PACF")

# Graph ACF and PACF for Japan
acf(jp_cleaned$percent_vaccinated, main = "Japan ACF")
acf(diff(diff(jp_cleaned$percent_vaccinated)), main = "Differenced Japan ACF")
pacf(jp_cleaned$percent_vaccinated, main = "Japan PACF")
pacf(diff(diff(jp_cleaned$percent_vaccinated)), main = "Differenced Japan PACF")

# Graph ACF and PACF for Taiwan
acf(tw_cleaned$total_vaccinations, main = "Taiwan ACF")
acf(diff(diff(tw_cleaned$total_vaccinations)), main = "Differenced Taiwan ACF")
pacf(tw_cleaned$total_vaccinations, main = "Taiwan PACF")
pacf(diff(diff(tw_cleaned$total_vaccinations)), main = "Differenced Taiwan PACF")

# Graph ACF and PACF for China
acf(cn_cleaned$total_vaccinations, main = "China ACF")
acf(diff(diff(cn_cleaned$total_vaccinations)), main = "Differenced China ACF")
pacf(cn_cleaned$total_vaccinations, main = "China PACF")
pacf(diff(diff(cn_cleaned$total_vaccinations)), main = "Differenced China PACF")

# Convert to ts object
clean_usa_ts <- ts(usa_cleaned[, c("date", "percent_vaccinated")])
clean_jp_ts <- ts(jp_cleaned[, c("date", "percent_vaccinated")])
clean_tw_ts <- ts(tw_cleaned[, c("date", "total_vaccinations")])
clean_cn_ts <- ts(cn_cleaned[, c("date", "total_vaccinations")])

# Graph cleaned and twice differenced data
par(mfrow = c(2, 2))
plot.ts(
  diff(diff(clean_usa_ts[, 2])),
  ylab = "Value",
  main = "USA Time Series Cleaned and Twice Differenced"
)
plot.ts(
  diff(diff(clean_jp_ts[, 2])),
  ylab = "Value",
  main = "Japan Time Series Cleaned and Twice Differenced"
)
plot.ts(
  diff(diff(clean_tw_ts[, 2])),
  ylab = "Value",
  main = "Taiwan Time Series Cleaned and Twice Differenced"
)
plot.ts(
  diff(diff(clean_cn_ts[, 2])),
  ylab = "Value",
  main = "China Time Series Twice Differenced"
)

# Augmented Dickey-Fuller Test for Stationarity
adf.test(diff(diff(clean_usa_ts[, 2])), alternative = "stationary", k = 0)
adf.test(diff(diff(clean_jp_ts[, 2])), alternative = "stationary", k = 0)
adf.test(diff(diff(clean_tw_ts[, 2])), alternative = "stationary", k = 0)
adf.test(diff(diff(clean_cn_ts[, 2])), alternative = "stationary", k = 0)

kable(data.frame(
  "Country" = c("USA", "Japan", "Taiwan", "China"),
  "Test Statistic" = c(-13.822, -12.559, -23.239, -20.799),
  "P-value" = c("< 0.01", "< 0.01", "< 0.01", "< 0.01")
), "pipe")

# Auto ARIMA for USA and diagnostics
m_usa_clean <- auto.arima(clean_usa_ts[, 2])
m_usa_clean
tsdisplay(m_usa_clean$residuals,
          main = "USA Model (Cleaned) Residual Diagnostics - ARIMA(3, 2, 2)"
)
adf.test(m_usa_clean$residuals, alternative = "stationary", k = 0)
sarima(clean_usa_ts[, 2], 3, 2, 2)
# Manual ARIMA for USA and diagnostics
m_usa_clean <- arima(clean_usa_ts[, 2], order = c(7, 2, 4))
m_usa_clean
tsdisplay(m_usa_clean$residuals,
          main = "USA Model (Cleaned) Residual Diagnostics - ARIMA(3, 2, 2)"
)
adf.test(m_usa_clean$residuals, alternative = "stationary", k = 0)
sarima(clean_usa_ts[, 2], 7, 2, 4)

# Split into training and test
clean_usa_train <- clean_usa_ts[1:149, 2]
clean_usa_test <- clean_usa_ts[150:187, 2]
# Fit model and forecast
m_usa_temp <- arima(clean_usa_train, order = c(7, 2, 4))
test_forecast_usa <- forecast(m_usa_temp, h = 38)
# Training RMSE and MAPE
usa_fitted <- clean_usa_train + m_usa_temp$residuals
mae(clean_usa_train, usa_fitted)
mape(clean_usa_train, usa_fitted)
# Test RMSE and MAPE
mae(clean_usa_test, test_forecast_usa$mean)
mape(clean_usa_test, test_forecast_usa$mean)

# Auto ARIMA for Japan and diagnostics
m_jp_clean <- auto.arima(clean_jp_ts[, 2])
m_jp_clean
tsdisplay(m_jp_clean$residuals,
          main = "Japan Model (Cleaned) Residual Diagnostics - ARIMA(2, 2, 2)"
)
adf.test(m_jp_clean$residuals, alternative = "stationary", k = 0)
sarima(clean_jp_ts[, 2], 2, 2, 2) 

# Manual ARIMA for Japan and diagnostics
m_jp_clean <- arima(clean_jp_ts[, 2], order = c(7, 2, 2))
m_jp_clean
tsdisplay(m_jp_clean$residuals,
          main = "Japan Model (Cleaned) Residual Diagnostics - ARIMA(7, 2, 7)"
)
adf.test(m_jp_clean$residuals, alternative = "stationary", k = 0)
sarima(clean_jp_ts[, 2], 7, 2, 0) 

# Split into training and test
clean_jp_train <- clean_jp_ts[1:105, 2]
clean_jp_test <- clean_jp_ts[106:132, 2]
# Fit model and forecast
m_jp_test <- arima(clean_jp_train, order = c(7, 2, 2))
test_forecast_jp <- forecast(m_jp_test, h = 27)
# Training RMSE and MAPE
jp_fitted <- clean_jp_train + m_jp_test$residuals
mae(clean_jp_train, jp_fitted)
mape(clean_jp_train, jp_fitted)
# Test RMSE and MAPE
mae(clean_jp_test, test_forecast_jp$mean)
mape(clean_jp_test, test_forecast_jp$mean)
# Create dataframe to compare model choices
model_type <- c("ARIMA(7, 2, 0)", "ARIMA(7, 2, 7)", "ARIMA(7, 2, 2)")
jp_train_mape <- c(0.03036902, 0.03013898, 0.03056263)
jp_train_mae <- c(0.0001965875, 0.0001889329, 0.0001943532)
jp_test_mape <- c(0.01868673, 0.02169291, 0.01746194)
jp_test_mae <- c(0.003328125, 0.004371528, 0.003066149)
kable(data.frame(
  Model = model_type, 
  "Training MAPE" = jp_train_mape,
  "Training MAE" = jp_train_mae,
  "Test MAPE" = jp_test_mape,
  "Test MAE" = jp_test_mae
), "pipe")

# Auto ARIMA for Taiwan and diagnostics
m_tw_clean <- auto.arima(clean_tw_ts[, 2])
m_tw_clean
tsdisplay(m_tw_clean$residuals,
          main = "Taiwan Model (Cleaned) Residual Diagnostics - ARIMA(0, 2, 2)"
)
adf.test(m_tw_clean$residuals, alternative = "stationary", k = 0)
sarima(clean_tw_ts[, 2], 0, 2, 3) 

# Split into training and test
clean_tw_train <- clean_tw_ts[1:97, 2]
clean_tw_test <- clean_tw_ts[98:121, 2]
# Fit model and forecast
m_tw_test <- arima(clean_tw_train, order = c(0, 2, 3))
test_forecast_tw <- forecast(m_tw_test, h = 24)
# Training RMSE and MAPE
tw_fitted <- clean_tw_train + m_tw_test$residuals
mae(clean_tw_train, tw_fitted)
mape(clean_tw_train[2:97], tw_fitted[2:97])
# Test RMSE and MAPE
mae(clean_tw_test, test_forecast_tw$mean)
mape(clean_tw_test, test_forecast_tw$mean)

# Auto ARIMA for China and diagnostics
m_cn_clean <- auto.arima(clean_cn_ts[, 2])
m_cn_clean
tsdisplay(m_cn_clean$residuals,
          main = "China Model (Cleaned) Residual Diagnostics - ARIMA(2, 2, 2)"
)
adf.test(m_cn_clean$residuals, alternative = "stationary", k = 0)
sarima(clean_cn_ts[, 2], 0, 2, 2) 

# Manual ARIMA for China and diagnostics
m_cn_clean <- arima(clean_cn_ts[, 2], order = c(0, 2, 9))
m_cn_clean
tsdisplay(m_cn_clean$residuals,
          main = "China Model (Cleaned) Residual Diagnostics - ARIMA(8, 2, 0)"
)
adf.test(m_cn_clean$residuals, alternative = "stationary", k = 0)
sarima(clean_cn_ts[, 2], 0, 2, 9)

# Split into training and test
clean_cn_train <- clean_cn_ts[1:174, 2]
clean_cn_test <- clean_cn_ts[175:217, 2]
# Fit model and forecast
m_cn_test <- arima(clean_cn_train, order = c(0, 2, 9))
test_forecast_cn <- forecast(m_cn_test, h = 43)
# Training RMSE and MAPE
cn_fitted <- clean_cn_train + m_cn_test$residuals
mae(clean_cn_train, cn_fitted)
mape(clean_cn_train, cn_fitted)
# Test RMSE and MAPE
mae(clean_cn_test, test_forecast_cn$mean)
mape(clean_cn_test, test_forecast_cn$mean)

# Plot forecasts
par(mfrow = c(2, 2))
plot(
  forecast(m_usa_clean, h = 30), 
  main = "USA Forecasts (Next 30 Days)",
  xlab = "Time",
  ylab = "Percent (Fully) Vaccinated"
)
plot(
  forecast(m_jp_clean, h = 30), 
  main = "Japan Forecasts (Next 30 Days)",
  xlab = "Time",
  ylab = "Percent (Fully) Vaccinated"
)
plot(
  forecast(m_tw_clean, h = 30), 
  main = "Taiwan Forecasts (Next 30 Days)",
  xlab = "Time",
  ylab = "Total Doses"
)
plot(
  forecast(m_cn_clean, h = 30), 
  main = "China Forecasts (Next 30 Days)",
  xlab = "Time",
  ylab = "Total Doses"
)

# Read in updated data starting from 07/19
new_df <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")

# Read in test data
test_data <- read.csv("Desktop/Stats Courses/Summer '21/SRP 199/Data/test_data.csv")
new_df <- test_data


# Find new values for USA data
usa_new <- new_df[new_df["location"] == "United States", ]
usa_new <- usa_new[(nrow(usa_df) + 1):(nrow(usa_df) + 30),]
dim(usa_new)
# Add percent vaccinated column
usa_new$percent_vaccinated <- usa_new$people_fully_vaccinated / usa_pop 
# Remove missing values from new data
clean_idx <- which(!is.na(usa_new$total_vaccinations))
# Find MAPE and RMSE for cleaned model
usa_pred <- forecast(m_usa_clean, h = nrow(usa_new))$mean[clean_idx]
usa_actual <- na.omit(usa_new$percent_vaccinated)
mape(usa_pred, usa_actual)
mae(usa_pred, usa_actual)
# Test accuracy of SLR models
usa_slr_preds <- predict(usa_lm, data.frame(date = as.Date(usa_new$date)))[clean_idx]
mape(usa_slr_preds, usa_actual)
mae(usa_slr_preds, usa_actual)
# Plot predicted vs actual
d1 <- ggplot(data.frame(Predicted = usa_pred, Actual = usa_actual), aes(x = Predicted, y = Actual)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0, col = "blue") + ggtitle("Predicted vs Actual (USA)")
d1

# Find new values for Japan data
jp_new <- new_df[new_df["location"] == "Japan", ]
jp_new <- jp_new[(nrow(jp_df) + 1):(nrow(jp_df) + 30),]
dim(jp_new)
# Add percent vaccinated column
jp_new$percent_vaccinated <- jp_new$people_fully_vaccinated / jp_pop 
# Remove missing values from new data
clean_idx <- which(!is.na(jp_new$total_vaccinations))
# Find MAPE and RMSE for cleaned model
jp_pred <- forecast(m_jp_clean, h = nrow(jp_new))$mean[clean_idx]
jp_actual <- na.omit(jp_new$percent_vaccinated)
mape(jp_pred, jp_actual)
mae(jp_pred, jp_actual)
# Test accuracy of SLR models
jp_slr_preds <- predict(jp_irp, data.frame(date = as.Date(jp_new$date)))^(1 / 0.23)
jp_slr_preds <- jp_slr_preds[clean_idx]
mape(jp_slr_preds, jp_actual)
mae(jp_slr_preds, jp_actual)
# Plot predicted vs actual
d2 <- ggplot(data.frame(Predicted = jp_pred, Actual = jp_actual), aes(x = Predicted, y = Actual)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0, col = "blue") + ggtitle("Predicted vs Actual (Japan)")
d2

# Find new values for Taiwan data
tw_new <- new_df[new_df["location"] == "Taiwan", ]
tw_new <- tw_new[(nrow(tw_df) + 1):(nrow(tw_df) + 30),]
dim(tw_new)
# Remove missing values from new data
clean_idx <- which(!is.na(tw_new$total_vaccinations))
# Find MAPE and RMSE for cleaned model
tw_pred <- forecast(m_tw_clean, h = nrow(tw_new))$mean[clean_idx]
tw_actual <- na.omit(tw_new$total_vaccinations)
mape(tw_pred, tw_actual)
mae(tw_pred, tw_actual)
# Test accuracy of SLR models
tw_slr_preds <- predict(tw_irp, data.frame(date = as.Date(tw_new$date)))^(1 / 0.08)
tw_slr_preds <- tw_slr_preds[clean_idx]
mape(tw_slr_preds, tw_actual)
mae(tw_slr_preds, tw_actual)
# Plot predicted vs actual
d3 <- ggplot(data.frame(Predicted = tw_pred, Actual = tw_actual), aes(x = Predicted, y = Actual)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0, col = "blue") + ggtitle("Predicted vs Actual (Taiwan)")
d3

# Find new values for China data
cn_new <- new_df[new_df["location"] == "China", ]
cn_new <- cn_new[(nrow(cn_df) + 1):(nrow(cn_df) + 30),]
dim(cn_new)
# Remove missing values from new data
clean_idx <- which(!is.na(cn_new$total_vaccinations))
# Find MAPE and RMSE for cleaned model
cn_pred <- forecast(m_cn_clean, h = nrow(cn_new))$mean[clean_idx]
cn_actual <- na.omit(cn_new$total_vaccinations)
mape(cn_pred, cn_actual)
mae(cn_pred, cn_actual)
# Test accuracy of SLR models
cn_slr_preds <- predict(cn_irp, data.frame(date = as.Date(cn_new$date)))^8
cn_slr_preds <- cn_slr_preds[clean_idx]
mape(cn_slr_preds, cn_actual)
mae(cn_slr_preds, cn_actual)
# Plot predicted vs actual
d4 <- ggplot(data.frame(Predicted = cn_pred, Actual = cn_actual), aes(x = Predicted, y = Actual)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0, col = "blue") + ggtitle("Predicted vs Actual (China)")
d4
grid.arrange(d1, d2, d3, d4, ncol = 2)

# Create function to track accuracy over 30 days
progress_plot <- function(predicted, actual, country) {
  clean_idx <- which(!is.na(actual))
  predicted <- predicted[clean_idx]
  actual <- na.omit(actual)
  n <- length(predicted) 
  mapes <- numeric(n)
  MAEs <- numeric(n)
  for (i in 1:n) {
    mapes[i] <- mape(predicted[1:i], actual[1:i])
    MAEs[i] <- mae(predicted[1:i], actual[1:i])
  }
  g1 <- ggplot(data.frame(Day = 1:n, MAPE = mapes), aes(x = Day, y = MAPE)) +
    geom_line(color = "steelblue") + 
    xlab("Day") + ggtitle(paste("MAPE Over 30 Days For", country)) + ylab("MAE")
  g2 <- ggplot(data.frame(Day = 1:n, MAE = MAEs), aes(x = Day, y = MAE)) +
    geom_line(color = "steelblue") + 
    xlab("Day") + ggtitle(paste("MAE Over 30 Days For", country)) + ylab("MAE")
  list(g1, g2)
}

# Plot accuracy over 30 days
plots_usa <- progress_plot(usa_pred, usa_actual, "USA")
plots_jp <- progress_plot(jp_pred, jp_actual, "Japan")
grid.arrange(plots_usa[[1]], plots_usa[[2]], plots_jp[[1]], plots_jp[[2]], ncol = 2)
plots_tw <- progress_plot(tw_pred, tw_actual, "Taiwan")
plots_cn <- progress_plot(cn_pred, cn_actual, "China")
grid.arrange(plots_tw[[1]], plots_tw[[2]], plots_cn[[1]], plots_cn[[2]], ncol = 2)

# Create plot to compare accuracy
compare_accuracy <- function(predicted, actual, slr, country, response, yrange, yrange2) {
  n <- length(predicted)
  plot(
    1:n, 
    predicted, 
    type = "l", 
    col = "steelblue", 
    xlab = "Days",
    ylab = response,
    main = paste("Comparison of ARIMA and SLR for", country),
    ylim = yrange,
    lwd = 2
    )
  lines(1:n, actual, col = "red", lwd = 2)
  lines(1:n, slr, col = "orange", lwd = 2)
  legend(c(2.5, 12), yrange2, 
         legend = c("SLR", "ARIMA", "Actual"), 
         col = c("orange", "steelblue", "red"), 
         lty = rep(1, 3),
         lwd = rep(2, 3)
         )
}

pdf("test.pdf", width = 9, height = 8)
par(mfrow = c(2, 2))
compare_accuracy(usa_pred, usa_actual, usa_slr_preds, "USA", "Percent Fully Vaccinated", c(.48, .65), c(0.6, 0.65))
compare_accuracy(jp_pred, jp_actual, jp_slr_preds, "Japan", "Percent Fully Vaccinated", c(.2, .5), c(0.41, 0.5))
compare_accuracy(tw_pred, tw_actual, tw_slr_preds, "Taiwan", "Total Doses", c(min(tw_pred), max(tw_slr_preds)), c(0.75 * max(tw_slr_preds), max(tw_slr_preds)))
compare_accuracy(cn_pred, cn_actual, cn_slr_preds, "China", "Total Doses", c(min(cn_pred), max(cn_slr_preds)), c(0.825 * max(cn_slr_preds), max(cn_slr_preds)))
dev.off()

# Forecast for 30 days from August 19 to September 17 for USA
usa_temp <- new_df[new_df["location"] == "United States", ][1:(nrow(usa_df) + 30), ]
usa_temp$percent_vaccinated <- usa_temp$people_fully_vaccinated / usa_pop 
usa_temp$percent_vaccinated <- na_locf(usa_temp$percent_vaccinated, option = "nocb")
m_usa_temp <- arima(usa_temp$percent_vaccinated, order = c(7, 2, 4))
temp_usa_forecast <- forecast(m_usa_temp, h = 30)
tail(temp_usa_forecast$mean)

# Forecast for 30 days from August 19 to September 17 for Japan
jp_temp <- new_df[new_df["location"] == "Japan", ][1:(nrow(jp_df) + 30), ]
jp_temp$percent_vaccinated <- jp_temp$people_fully_vaccinated / jp_pop 
jp_temp$percent_vaccinated <- na_interpolation(jp_temp$percent_vaccinated, option = "spline")
m_jp_temp <- arima(jp_temp$percent_vaccinated, order = c(7, 2, 2))
temp_jp_forecast <- forecast(m_jp_temp, h = 30)
tail(temp_jp_forecast$mean)

# Forecast for 30 days from August 19 to September 17 for Taiwan
tw_temp <- new_df[new_df["location"] == "Taiwan", ][1:(nrow(tw_df) + 30), ]
tw_temp$total_vaccinations <- na_ma(tw_temp$total_vaccinations, weighting = "simple", k = 6)
m_tw_temp <- arima(tw_temp$total_vaccinations, order = c(0, 2, 3))
temp_tw_forecast <- forecast(m_tw_temp, h = 30)
tail(temp_tw_forecast$mean)

# Forecast for 30 days from August 19 to September 17 for China
cn_temp <- new_df[new_df["location"] == "China", ][1:(nrow(cn_df) + 30), ]
cn_temp$total_vaccinations <- na_ma(cn_temp$total_vaccinations, weighting = "linear")
m_cn_temp <- arima(cn_temp$total_vaccinations, order = c(0, 2, 9))
temp_cn_forecast <- forecast(m_cn_temp, h = 30)
tail(temp_cn_forecast$mean)

# Plot forecasts
par(mfrow = c(2, 2))
plot(
  temp_usa_forecast, 
  main = "USA Forecasts From 8/19 to 9/17",
  xlab = "Time",
  ylab = "Percent (Fully) Vaccinated"
)
plot(
  temp_jp_forecast, 
  main = "Japan Forecasts From 8/19 to 9/17",
  xlab = "Time",
  ylab = "Percent (Fully) Vaccinated"
)
plot(
  temp_tw_forecast, 
  main = "Taiwan Forecasts From 8/19 to 9/17",
  xlab = "Time",
  ylab = "Total Doses"
)
plot(
  temp_cn_forecast, 
  main = "China Forecasts From 8/19 to 9/17",
  xlab = "Time",
  ylab = "Total Doses"
)
