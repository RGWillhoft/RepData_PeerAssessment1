## ----setup, include=FALSE------------------------------------------------
# Make it so I don't have to put echo=TRUE on each chunk
knitr::opts_chunk$set(echo = TRUE)


## ----load----------------------------------------------------------------
unzip("activity.zip")
step_data <- read.csv("activity.csv", stringsAsFactors = FALSE)
str(step_data)


## ----preprocess----------------------------------------------------------
step_data$date <- as.Date(step_data$date, "%Y-%m-%d")
range(step_data$date)


## ----explore-------------------------------------------------------------
summary(step_data$steps)
summary(step_data$interval)


## ----total_steps---------------------------------------------------------
steps_per_day <- with(step_data, tapply(steps, date, sum, na.rm = TRUE))
str(steps_per_day)


## ----hist----------------------------------------------------------------
hist(steps_per_day,
     xlab = "Steps per Day",
     main = "Histogram of Total Steps per Day"
)


## ----mean_median---------------------------------------------------------
mean_steps_per_day <- mean(steps_per_day)
median_steps_per_day <- median(steps_per_day)


## ----daily_activity------------------------------------------------------
steps_per_interval <- with(step_data, tapply(steps, interval, mean, na.rm = TRUE))
str(steps_per_interval)


## ----plot----------------------------------------------------------------
plot(x = names(steps_per_interval),
     y = steps_per_interval,
     xlab = "Interval",
     main = "Average Steps by 5 Minute Interval",
     type = "l"
)


## ----missing_data--------------------------------------------------------
na_values = sum(is.na(step_data$steps))
na_values
na_percent = 100 * na_values / nrow(step_data)
na_percent


## ------------------------------------------------------------------------
all(table(step_data$interval) == 61)
all(table(step_data$interval[is.na(step_data$steps)]) == 8)


## ------------------------------------------------------------------------
all(table(step_data$date) == 288)
table(step_data$date[is.na(step_data$steps)])


## ----impute--------------------------------------------------------------
step_data_imputed <- step_data
bad <- is.na(step_data$steps)
interval_index <- match(step_data$interval[bad],names(steps_per_interval))
step_data_imputed$steps[bad] <- steps_per_interval[interval_index] 


## ----total_steps_imputed-------------------------------------------------
steps_per_day_imputed <- with(step_data_imputed, tapply(steps, date, sum, na.rm = TRUE))
par(mfrow = c(1,2))
hist(steps_per_day,
     xlab = "Steps per Day",
     main = "Histogram of Total Steps per Day"
)
hist(steps_per_day_imputed,
     xlab = "Steps per Day",
     main = "Histogram of Total Steps per Day"
)


## ----per_interval_imputed------------------------------------------------
steps_per_interval_imputed <- with(step_data_imputed, tapply(steps, interval, mean, na.rm = TRUE))
plot(x = names(steps_per_interval),
     y = steps_per_interval,
     xlab = "Interval",
     main = "Average Steps by 5 Minute Interval",
     type = "l",
     col = "blue"
)
lines(x = names(steps_per_interval_imputed),
     y = steps_per_interval,
     col = "red"
)


## ----add_weekdays--------------------------------------------------------
weekend <- weekdays(step_data$date) %in% c("Saturday", "Sunday")
step_data$weekday <- factor( weekdays(step_data$date) %in% c("Saturday", "Sunday") )
levels(step_data$weekend) <- c("weekday", "weekend")

