
library(olsrr)

taxifare = read.csv('processed_taxi_fare.csv')

model <- lm(fare_amount ~ dist_km + passenger_count + weekend + 
            hour_of_the_day, data = taxifare)

#best subset regression
ols_step_best_subset(model)
#dist_km, passenger_count, hour_of_the_day have highest R-square but dist_km & passenger_count have lower AIC

