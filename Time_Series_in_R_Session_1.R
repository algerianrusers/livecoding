
# 1- References

# download data here: https://forecasters.org/resources/time-series-data/m3-competition/

# Forecasting: Principles and Practice ebook - https://otexts.com/fpp2/
# Forecasting Best Practices : https://github.com/microsoft/forecasting
# Most used packages for Time Series Analysis:  https://cran.r-project.org/web/views/TimeSeries.html
# R packages for forecast combinations: https://robjhyndman.com/hyndsight/forecast-combinations/
# Rob J Hyndman website: https://robjhyndman.com/ 


# 2- Most common challenges in time series analysis and forecasting (real-life challenges)

# How to fit time series models and forecast?
# How to assess the forecastability of time series ?
# How to clean outliers in time series ?
# How to do time series clustering/segmentation ?
# Does Machine Learning algorithms like RNNs and LSTM lead always to more accurate forecast ?


# 3- load required libraries


library(readxl)
library(tidyverse)
library(lubridate)
library(forecast)
library(forecastHybrid)
library(imputeTS)
library(timeSeries)


library(plotly)
library(highcharter) # Note that Highcharts is free for non-commercial use but otherwise requires a license.
#-> for more HTML widgets packages, see here : https://www.htmlwidgets.org/showcase_highcharts.html


# 4 - data import

M3_Import <- read_excel("../data/data/M3C.xls" , sheet = 3)


# 5 - data sanity check after import

glimpse(select(M3_Import, 1:17))
skimr::skim(select(M3_Import , 1:17))


# 6 - data wrangling : make data ready for analysis


ts_long <-
 M3_Import %>% 
  filter(Category == "INDUSTRY") %>% 
  select(-N:-Category) %>% 
  gather(obs , qty , -Series , -`Starting Year` , -`Starting Month`) %>% 
  arrange(Series , `Starting Year` , `Starting Month`)


# 7- select one time series randomly


ts_example <-
  ts_long %>% 
  filter(Series == sample(.$Series , 1)) %>% 
  {
    vec <- .$qty
    Year <- unique(.$`Starting Year`)
    Month <- unique(.$`Starting Month`)
    ts_example <- ts(vec , start = c(Year,Month) , frequency = 12) # create monthly time series object in R
    ts_example
  } %>% 
  timeSeries::na.contiguous() # Find the longest non-missing values in a time series object
  
# 8- Basics of visualizing time series 

# 8.1 - time plots

p1 <- ggplot2::autoplot(ts_example)
p1
ggplotly(p1) # render the chart p1 interactive with plotly 

# 8.2- Seasonal plots 

# a seasonal plot allows the underlying seasonal pattern to be seen more clearly, 
# and is especially useful in identifying years in which the pattern changes.

ggseasonplot(ts_example) 


# 8.3 - Seasonal subseries plots

ggsubseriesplot(ts_example)

# The horizontal lines indicate the means for each month. 
# This form of plot enables the underlying seasonal pattern to be seen clearly, 
# and also shows the changes in seasonality over time. 
# It is especially useful in identifying changes within particular seasons


# 8.4 - more efficiency to inspect time series visually

hchart(ts_example) %>% 
  hc_add_theme(hc_theme_ffx()) %>% 
  hc_chart(zoomType = "xy")


# 9 - time series decomposition

hchart(stl(ts_example , s.window = "periodic"))
autoplot(stl(ts_example , s.window = "periodic"))



# 10- time series modeling and forecasting

# 10.1- fit different time series models 

ts_without_last_18 <- head(ts_example, -18)

ARIMA <- auto.arima(ts_without_last_18)
ETS <- ets(ts_without_last_18)
STLM <- stlm(ts_without_last_18)
NNAR <- nnetar(ts_without_last_18)
TBATS <- tbats(ts_without_last_18)
TSLM <- tslm(ts_without_last_18 ~ trend + season)
HYBRID <- hybridModel(ts_without_last_18 , models = "aefnst")

# 10.2 forecast from fitted models
  
FC_ARIMA <- forecast(ARIMA , h=18)
FC_ETS <- forecast(ETS , h=18)
FC_STLM <- forecast(STLM , h=18)
FC_TSLM <- forecast(TSLM , h=18)
FC_HYBRID <- forecast(HYBRID , h=18)
  
hchart(FC_ARIMA)
hchart(FC_ETS)
hchart(FC_STLM)


# 10.3 compare forecasts vs. actuals during holdout period

FC_ARIMA_points <- data.frame(FC_ARIMA = as.numeric(FC_ARIMA$mean))
FC_ETS_points <- data.frame(FC_ETS = as.numeric(FC_ETS$mean))
FC_STLM_points <- data.frame(FC_STLM = as.numeric(FC_STLM$mean))
FC_HYBRID_points <- data.frame(FC_HYBRID = as.numeric(FC_HYBRID$mean))


Actuals <-
  tail(ts_example , 18) %>% 
  data.frame() %>% 
  setNames("Act")

year_month <- as_date(date_decimal(as.vector(time(FC_ETS$mean))))

compare_FC <-
  cbind(year_month , FC_ARIMA_points , FC_ETS_points , FC_STLM_points, FC_HYBRID_points , Actuals) %>% 
  gather(Model , qty , -year_month)

plot_ly(compare_FC , x = ~year_month , y = ~qty , color = ~Model , type = "scatter",
        mode  = "lines+markers")


# 11- Generate the FC for all time series

M3_FC_generation <-  function(ts_input){

  Y <-
  ts_input %>% 
  filter(!is.na(qty))

ts_name <- unique(Y$Series)
Year <- unique(Y$`Starting Year`)
Month <- unique(Y$`Starting Month`)

ts_qty <- unique(Y$qty) %>% ts(start = c(Year, Month) , frequency = 12)
ts_fit <- head(ts_qty , -18)

ETS <- ets(ts_fit)
ARIMA <- auto.arima(ts_fit)
Hybrid <- hybridModel(ts_fit , models = "ae")

FC_ETS <- forecast(ETS , h = 18)
FC_ARIMA <- forecast(ARIMA , h = 18)
FC_Hybrid <- forecast(Hybrid , h = 18)

FC_points_ETS <- data.frame(ets_fc = as.numeric(FC_ETS$mean))
FC_points_ARIMA <- data.frame(arima_fc = as.numeric(FC_ARIMA$mean))
FC_points_Hybrid <- data.frame(hybrid_fc = as.numeric(FC_Hybrid$mean))

Actuals <-
  tail(ts_qty , 18) %>% 
  as.numeric() %>% 
  data.frame(Act = .)

year_month <- as_date(date_decimal(as.vector(time(FC_ETS$mean))))

FCA <-
  cbind(ts_name , year_month , FC_points_ETS , FC_points_ARIMA , FC_points_Hybrid ,  Actuals) %>% 
  group_by(ts_name) %>% 
  mutate(MAE_ets = sum(abs(ets_fc - Act)) ,
         MAE_arima = sum(abs(arima_fc - Act)) ,
         MAE_hybrid = sum(abs(hybrid_fc - Act)),
         
         MAPE_ets = sum(abs(ets_fc - Act) / sum(Act)),
         MAPE_arima = sum(abs(arima_fc - Act) / sum(Act)),
         MAPE_hybrid = sum(abs(hybrid_fc - Act) / sum(Act)),
         
         FCA_ets = (1-MAPE_ets)*100 , 
         FCA_arima = (1-MAPE_arima)*100 , 
         FCA_hybrid = (1-MAPE_hybrid)*100 ) %>% 
  ungroup()

FCA

         
}
         
    
M3_FC_output <-
  ts_long %>% 
  split(.$Series) %>% # break a tibble/data.frame into a list of data frames 
  head(50) %>% 
  map_dfr(M3_FC_generation) # apply M3_FC_generation across the list 


         
  







  

