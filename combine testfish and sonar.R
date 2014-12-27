library(lubridate)
library(ggplot2)
library(tidyr)

test_fish <- read.csv("data/offshore_test_fish.csv")

test_fish$Year <- paste0(ifelse(nchar(test_fish$Year) == 1, "0", ""), test_fish$Year)

test_fish$Year <- paste0(ifelse(test_fish$Year < 15, 20, 19), test_fish$Year)

test_fish$date <- ymd(paste(test_fish$Year, test_fish$Month, test_fish$Day))

test_fish <- data.frame(date = test_fish$date, station = test_fish$Station, catch = test_fish$Catch, index = test_fish$Index, year = test_fish$Year)
test_fish$year <- as.numeric(as.character(test_fish$year))
test_fish$index <- as.numeric(as.character(test_fish$index))
test_fish$station <- as.factor(as.numeric(as.character(test_fish$station)))

save(test_fish, file = "data/test_fish.rda")

test_fish<- test_fish[!test_fish$index >= 400,]
test_fish <-test_fish[!test_fish$station == ".",]
     
year(test_fish$date) <- 2013

ggplot(data = test_fish, aes(x = date,  y = index, fill = station)) + geom_bar(stat = "identity") +
    facet_wrap(~ year) 

year(test_fish$date) <- test_fish$year


p_values <- data.frame(days_lagged = numeric(), model_type = numeric(), intercept = numeric(), parameter = numeric())
plot_list_obj <- list()


for(i in 0:20) {
    plotting_obj <- inner_join(test_fish, sonar, by = "date")
    plot_list_obj[1+1] <- ggplot( data = plotting_obj, aes(x = log(n), y = index, color)) + geom_point() +
                                facet_wrap( ~ station) + labs(title = paste("Staggered", i, "Days"), 
                                                              x = "Offshore Catch Index", 
                                                              y = "Sonar Count") 
    p_values[i+1,] <- c(i,"log",summary(lm(plotting_obj$index ~ log(plotting_obj$n)))$coefficients[,4])
    
    day(test_fish$date) <- day(test_fish$date) + 1 
}


p_values <- gather(p_values, days_lagged, model_type)
colnames(p_values)[3] <- "value"
colnames(p_values)[4] <- "p"
p_values$p <- as.numeric(p_values$p)
p_values$days_lagged <- as.numeric(p_values$days_lagged)


ggplot(data = p_values, aes(y = -log(p), x = days_lagged)) + geom_bar(stat = "identity") +
    facet_wrap( ~ value) + 
    labs(title = "Negative Log Of Parameter P-Values", 
         x = "Days Lagged Of Offshore Test Fishing", 
         y = "-log(p value)")


