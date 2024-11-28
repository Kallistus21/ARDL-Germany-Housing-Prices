# ARDL Model for Germany's Housing Prices

This project investigates the factors influencing Germany's housing prices from 1995 Q2 to 2023 Q4. The analysis uses an **Auto Regressive Distributed Lag (ARDL)** model to identify both short-term and long-term relationships between real house prices (adjusted for inflation) and several macroeconomic indicators. Additionally, an **ARIMA** model is employed to benchmark the predictive performance of the ARDL model.

The idea for the project was due to my, at the moment, interest in Housing in the world. Real estate prices are soaring almost everywhere - Germany was chosen due to availability of data (which is not ideal as well).
Once again, the data ranges from 1995 to 2023 - it's a quarterly data to increase the number of observations. Yearly data would have resulted in only 28 observations which is too low for a time series analysis.
I tried to obtain monthly data but for some variables it was available and for some not and I didn't want to do interpolation to avoid skewing of the results.

---

## Features

### Variables:
- **Dependent Variable**:
  - `Real_House_Prices`: House prices adjusted for inflation, indexed with the year 2015 as the reference point (=100).

- **Independent Variables**:
  - `GDP_per_capita`: Gross Domestic Product per capita.
  - `Unemployment`: Unemployment rate for individuals aged 15 and over (in percentages).
  - `Inflation_rate`: Consumer Price Index (CPI) inflation rate (in percentages).
  - `Interest_rate`: Long-term interest rates (in percentages).
  - `Population`: Total population (transformed to individual counts from thousands).
  - `Permits_issued`: Permits issued for dwellings, construction of buildings (indexed, 2015=100).

---

## Methodology

### Data Preprocessing:
1. **Renaming Variables**: Standardized column names for consistency.
2. **Transformations**:
   - Logarithmic transformation of all variables.
   - Adjusted for non-positive values by adding a small constant `1e-2`. Without it, logarithmic transformation would not have been possible
   - First differenced all variables to address non-stationarity.
3. **Time Series Conversion**: Converted the dataset into a quarterly time series format starting in Q2 1994.

### Stationarity Testing:
- **Augmented Dickey-Fuller (ADF) Test**:
  - Custom function `testdf2()` applied to identify stationarity and determine necessary augmentations.
- **Phillips-Perron (PP) Test**:
  - Cross-validated with ADF results to confirm stationarity.
- **KPSS Test**:
  - Used to detect stationarity against a trend.

### ARDL Model:
- Conducted lag selection using the **BIC Criterion**.
- General-to-Specific modeling approach:
  - Iteratively removed insignificant variables and tested for joint significance using Wald Tests.
- Final ARDL Model includes:
  - `Real_House_Prices` (lags 1 and 2), `GDP_per_capita`, `Interest_rate`, and `Permits_issued` (lag 1).

### ARIMA Benchmark Model:
- Optimal model selection using `auto.arima()` (AIC and BIC criterion).
- Final ARIMA model: ARIMA(3,1,4).

---

## Statistical Tests and Diagnostics:
- **Model Performance**:
  - Compared ARDL and ARIMA using AIC and BIC scores.
- **Diagnostic Tests**:
  - **RESET Test**: Checked for functional form misspecification.
  - **Breusch-Pagan Test**: Tested for heteroscedasticity in residuals.
  - **Jarque-Bera Test**: Verified normality of residuals.
  - **Ljung-Box Test**: Assessed autocorrelation in ARIMA residuals.

---

## Visualizations
- Plots of logged variables and their first differences.
- Diagnostic plots to validate the stationarity and suitability of models.

---

## Results
- The ARDL model outperforms the ARIMA model in explaining Germany's real housing prices, offering both predictive and interpretative power.
- Key determinants of housing prices:
  - Positive correlation with GDP per capita and population growth.
  - Negative correlation with interest rates.
- Permit issuance has a lagged effect, suggesting that construction activity impacts prices with a delay.

---

## Tools and Packages:
- **R Packages Used**:
  - `lmtest`, `tseries`, `sandwich`, `zoo`, `dplyr`, `urca`, `dynlm`, `car`, `forecast`, `ARDL`.

- **Custom Function**:
  - `testdf2()`: Automates the Augmented Dickey-Fuller testing procedure.

---

## Visualizations
- Plots of logged variables and their first differences.
- Diagnostic plots to validate the stationarity and suitability of models.

---

## How to Run
1. Clone the repository:
   ```bash
   git clone https://github.com/Kallistus21/ARDL-Germany-Housing-Prices.git
   ```
2. Install packages
   ```
   requiredPackages <- c("lmtest", "tseries", "sandwich", "zoo", "dplyr", "urca", "dynlm", "car", "forecast", "ARDL")
   for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
   for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) }
   ```
