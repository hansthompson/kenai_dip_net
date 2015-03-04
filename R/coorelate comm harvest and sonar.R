library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(scales)
library(animation)
source("R/multiplot.r")
load("data/harvest_survey.rda")
load("data/sonar.rda")
load("data/test_fish.rda")
load("data/comm_catch.rda")

comm_catch_temp <- filter(comm_catch, stat_code == 24421)


p_values <- data.frame(days_lagged = numeric(), model_type = numeric(), intercept = numeric(), parameter = numeric())
plot_list_obj <- list()
comm_and_sonar <- inner_join(na.omit(filter(comm_catch, stat_code == 24421)), sonar, by = "date")






for(i in 0:25) {
    plotting_obj <- inner_join(comm_catch_temp, sonar, by = "date")
    plot_list_obj [[i+1]] <- ggplot(data = plotting_obj, 
                                    aes(x = sockeye, y = log(n), color = factor(stat_code))) + geom_point() + geom_smooth() +
        labs(title = paste("Staggered", i -10, "Days"), 
                                        x = "Commericial Catch", 
                                        y = "log(Sonar Count)") 
     p_values[i+1,] <- c(i,"log",summary(lm(plotting_obj$sockeye ~ log(plotting_obj$n)))$coefficients[,4])
                                            
    day(comm_catch_temp$date) <- day(comm_catch_temp$date) + 1 
}

p_values <- gather(p_values, days_lagged, model_type)
colnames(p_values)[3] <- "value"
colnames(p_values)[4] <- "p"
p_values$p <- as.numeric(p_values$p)
p_values$days_lagged <- as.numeric(p_values$days_lagged)



ggplot(data = p_values, aes(y = -log(p), x = days_lagged)) + geom_bar(stat = "identity") +
    facet_wrap( ~ value) + 
    labs(title = "Negative Log Of Log Model P-Values", 
         x = "Days Lagged Of Offshore Test Fishing", 
         y = "-log(p value)")
