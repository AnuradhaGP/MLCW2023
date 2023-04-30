install.packages("dplyr")
library(dplyr)             #install first the package before calling it via the library command
tm_series <- c(1:15)  # a time series with 15 data as an example
str(tm_series)
# see here how we can create 4 columns in this I/O matrix. Check the time-delayed variables!
time_lagged_data <- bind_cols(G_previous2 = lag(tm_series,3),
                              G_previous = lag(tm_series,2),
                              G_current = lag(tm_series,1),
                              G_pred = tm_series)          
# see here the existence of NA values due to that shifting
time_lagged_data 
# see here the use of complete.cases function to remove those rows with NA
time_lagged_data <- time_lagged_data[complete.cases(time_lagged_data),]
# see this time-delayed I/O configuration from these rows
head (time_lagged_data)

str(time_lagged_data)
# see here the length of this I/O matrix. It is l-m (i.e. m: number of input variables)
time_lagged_data
time_lagged_data$G_previous
