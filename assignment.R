# Read data
data <- read.csv("E:/Users/jorda/Desktop/Assignment/Y3 S2/BDA/2018Floor2_aggregatedweather_data.csv")
# Inspect variable in the dataset
names(data)
head(data, 5)

# Check for missing values
dim(data)
missing_values <- colSums(is.na(data))
barplot(missing_values, main = "Missing Value Counts", ylab = "Number of Missing Values", las = 2)

# Inspecting data
summary(data)

# Line chart to view AC energy usage
library(ggplot2)
library(tidyr)

# Convert Just_Date to Date format
data$Just_Date <- as.Date(data$Just_Date)

# Select columns related to AC energy consumption
ac_data <- data[, c("Just_Date", "z1_AC1.kW.", "z2_AC1.kW.", "z2_AC2.kW.", "z2_AC3.kW.", "z2_AC4.kW.", "z2_AC5.kW.", 
                    "z2_AC6.kW.", "z2_AC7.kW.", "z2_AC8.kW.", "z2_AC9.kW.", "z2_AC10.kW.", "z2_AC11.kW.", "z2_AC12.kW.", 
                    "z2_AC13.kW.", "z2_AC14.kW.", "z4_AC1.kW.")]

# Melt the data to long format for ggplot
ac_data_long <- tidyr::gather(ac_data, key = "AC_Unit", value = "Energy_Consumption", -Just_Date)

# Create separate line charts for each AC unit
ggplot(ac_data_long, aes(x = Just_Date, y = Energy_Consumption, color = AC_Unit)) +
  geom_line() +
  labs(title = "AC Energy Consumption Over Time",
       x = "Date",
       y = "Energy Consumption (kW)",
       color = "AC Unit") +
  theme_minimal() +
  facet_wrap(~AC_Unit, scales = "free_y", nrow = 4)

# Line chart to view temperature over month
# Select the columns related to temperature
temp <- data[, c("Just_Date", "temp")]

# Create the line chart using ggplot2
ggplot(temp, aes(x = Just_Date, y = temp)) +
  geom_line() +
  labs(title = "Temperature Over Month",
       x = "Month",
       y = "Temperature (°C)") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")




# Data Preparation
# Select needed column
ext_data <- data[, c("Just_Date", "z1_AC1.kW.", "z2_AC1.kW.", "z2_AC2.kW.", "z2_AC3.kW.", "z2_AC4.kW.", "z2_AC5.kW.", 
                    "z2_AC6.kW.", "z2_AC7.kW.", "z2_AC8.kW.", "z2_AC9.kW.", "z2_AC10.kW.", "z2_AC11.kW.", "z2_AC12.kW.", 
                    "z2_AC13.kW.", "z2_AC14.kW.", "z4_AC1.kW.", "temp")]
# Load needed library
library(dplyr)

# Apply the custom function to variable names using rename_with() and rename other column to clearer name
ext_data <- ext_data %>% 
  rename(Date = Just_Date,
         Temperature = temp)

# Convert Date from character to Date format
ext_data$Date <- as.Date(ext_data$Date)

ext_data$Z2_AC <- rowSums(ext_data[, c("z2_AC1.kW.", "z2_AC2.kW.", "z2_AC3.kW.", "z2_AC4.kW.", "z2_AC5.kW.", 
                                       "z2_AC6.kW.", "z2_AC7.kW.", "z2_AC8.kW.", "z2_AC9.kW.", "z2_AC10.kW.", 
                                       "z2_AC11.kW.", "z2_AC12.kW.", "z2_AC13.kW.", "z2_AC14.kW.")])

# Data Modelling
# Load required library
library(forecast)

# Condense parts of day into one day
averages <- aggregate(. ~ Date, data = ext_data, FUN = mean)

# Convert Date from character to Date format
averages$Day <- as.Date(averages$Day)

# Rename Day back to Date
averages <- averages %>% 
  rename(Date = Day)


ACZ1 <- averages[, c("Z1_AC1(kW)", "Temperature")]
# Create time series object for AC energy consumption
AC_energy_ts <- ts(ACZ1[, 1], start = c(2018, 7), frequency = 184)
temperature_ts <- ts(ACZ1[, 2], start = c(2018, 7), frequency = 184)


# Fit ARIMA model with external regressor using auto.arima
model <- auto.arima(AC_energy_ts, xreg = temperature_ts)
# Generate forecasts
forecast_AC_Z1 <- forecast(model, xreg = temperature_ts)
plot(forecast_AC_Z1)
