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


year(harvest_survey$date) <- 2013

limits <- aes(ymax = harvest_survey$Mean + harvest_survey$SE, 
              ymin = harvest_survey$Mean - harvest_survey$SE)

ggplot(data = harvest_survey, aes(x = date, y = Mean)) + geom_point(stat= "identity", fill = "white", color = "black") + geom_line() +
    geom_errorbar(limits,  color = "red") +  facet_wrap(~ year) +
    labs(title = "Harvest Survey Results Across Years", 
         x = "Date", 
         y = "Mean") 

year(harvest_survey$date) <- harvest_survey$year

test_fish<- test_fish[!test_fish$index >= 400,]
test_fish <-test_fish[!test_fish$station == ".",]
test_fish <-test_fish[!is.na(test_fish$year),]

year(test_fish$date) <- 2013

test_fish_bar <- test_fish %>%
    group_by(date, station) %>%
    summarize(index = sum(index))

ggplot(data = test_fish_bar, 
       aes(x = date,  y = index, fill = station)) + 
    geom_bar(stat = "identity") #+
    facet_wrap(~ year) 

year(test_fish$date) <- test_fish$year

year(sonar$date) <- 2013

ggplot(data = sonar, aes(x = date, y = n)) + geom_line(stat = "identity", fill = "white", color = "black") +
    facet_wrap(~ year) +
    labs(title = "Sonar Count Results Across Years", 
         x = "Date", 
         y = "Sockeye Counted") 

year(sonar$date) <- sonar$year

harv_surv_tmp <- harvest_survey

p_values <- data.frame(days_lagged = numeric(), model_type = numeric(), intercept = numeric(), parameter = numeric())

## NO STAGGER
p0<- ggplot(data = inner_join(sonar, harv_surv_tmp, by = "date"), aes(x = n, y = Mean)) + geom_point() +
    labs(title = "No Staggering", 
         x = "Sonar Count", 
         y = "Mean Harvest") +
    scale_x_continuous(breaks=pretty_breaks(n=3)) 
p_values[1,] <- c(0,"log",summary(lm(inner_join(sonar, harv_surv_tmp, by = "date")$Mean ~ log(inner_join(sonar, harv_surv_tmp, by = "date")$n)))$coefficients[,4])
p_values[2,] <- c(0,"linear",summary(lm(inner_join(sonar, harv_surv_tmp, by = "date")$Mean ~ inner_join(sonar, harv_surv_tmp, by = "date")$n))$coefficients[,4])

## ONE DAY STAGGER
day(harv_surv_tmp$date) <- day(harv_surv_tmp$date) + 1
p1 <- ggplot(data = inner_join(sonar, harv_surv_tmp, by = "date"), aes(x = n, y = Mean)) + geom_point() +
    labs(title = "Staggered One Day", 
         x = "Sonar Count", 
         y = "Mean Harvest") +
    scale_x_continuous(breaks=pretty_breaks(n=3)) 
p1_best <- ggplot(data = inner_join(sonar, harv_surv_tmp, by = "date"), aes(x = n, y = Mean)) + geom_point() +
    labs(title = "Sonar Counts By Mean Catch - Staggered For One Day", 
         x = "Sonar Count", 
         y = "Mean Harvest") + 
    stat_smooth(formula = "y ~ log(x)", method = "lm")
p_values[3,] <- c(1,"log",summary(lm(inner_join(sonar, harv_surv_tmp, by = "date")$Mean ~ log(inner_join(sonar, harv_surv_tmp, by = "date")$n)))$coefficients[,4])
p_values[4,] <- c(1,"linear",summary(lm(inner_join(sonar, harv_surv_tmp, by = "date")$Mean ~ inner_join(sonar, harv_surv_tmp, by = "date")$n))$coefficients[,4])

## TWO DAY STAGGER
day(harv_surv_tmp$date) <- day(harv_surv_tmp$date) + 1
p2 <- ggplot(data = inner_join(sonar, harv_surv_tmp, by = "date"), aes(x = n, y = Mean)) + geom_point() +
    labs(title = "Staggered Two Days", 
         x = "Sonar Count", 
         y = "Mean Harvest") +
    scale_x_continuous(breaks=pretty_breaks(n=3)) 
p_values[5,] <- c(2,"log",summary(lm(inner_join(sonar, harv_surv_tmp, by = "date")$Mean ~ log(inner_join(sonar, harv_surv_tmp, by = "date")$n)))$coefficients[,4])
p_values[6,] <- c(2,"linear",summary(lm(inner_join(sonar, harv_surv_tmp, by = "date")$Mean ~ inner_join(sonar, harv_surv_tmp, by = "date")$n))$coefficients[,4])

## THREE DAY STAGGER
day(harv_surv_tmp$date) <- day(harv_surv_tmp$date) + 1
p3<- ggplot(data = inner_join(sonar, harv_surv_tmp, by = "date"), aes(x = n, y = Mean)) + geom_point() +
    labs(title = "Staggered Three Days", 
         x = "Sonar Count", 
         y = "Mean Harvest") +
    scale_x_continuous(breaks=pretty_breaks(n=3)) 
p_values[7,] <- c(3,"log",summary(lm(inner_join(sonar, harv_surv_tmp, by = "date")$Mean ~ log(inner_join(sonar, harv_surv_tmp, by = "date")$n)))$coefficients[,4])
p_values[8,] <- c(3,"linear",summary(lm(inner_join(sonar, harv_surv_tmp, by = "date")$Mean ~ inner_join(sonar, harv_surv_tmp, by = "date")$n))$coefficients[,4])

## FOUR DAY STAGGER
day(harv_surv_tmp$date) <- day(harv_surv_tmp$date) + 1
p4 <- ggplot(data = inner_join(sonar, harv_surv_tmp, by = "date"), aes(x = n, y = Mean)) + geom_point() +
    labs(title = "Staggered Four Days", 
         x = "Sonar Count", 
         y = "Mean Harvest") +
    scale_x_continuous(breaks=pretty_breaks(n=3)) 

p_values[9,] <- c(4,"log",summary(lm(inner_join(sonar, harv_surv_tmp, by = "date")$Mean ~ log(inner_join(sonar, harv_surv_tmp, by = "date")$n)))$coefficients[,4])
p_values[10,] <- c(4,"linear",summary(lm(inner_join(sonar, harv_surv_tmp, by = "date")$Mean ~ inner_join(sonar, harv_surv_tmp, by = "date")$n))$coefficients[,4])


p_values <- gather(p_values, days_lagged, model_type)
colnames(p_values)[3] <- "value"
colnames(p_values)[4] <- "p"
p_values$p <- as.numeric(p_values$p)

ggplot(data = p_values, aes(y = -log(p), x = days_lagged)) + geom_bar(stat = "identity") +
    facet_wrap(value ~ model_type) + 
    labs(title = "Negative Log Of Parameter P-Values", 
         x = "Days Lagged Of Harvest Survey", 
         y = "-log(p value)")

multiplot(p0, p1, p2, p3, p4, cols=3)



p_values <- data.frame(days_lagged = numeric(), model_type = numeric(), intercept = numeric(), parameter = numeric())
plot_list_obj <- list()


for(i in 0:15) {
    plotting_obj <- inner_join(test_fish, sonar, by = "date")
    plot_list_obj[[i+1]] <- ggplot( data = plotting_obj, aes(x = log(n), y = index, color)) + geom_point() +
        facet_wrap( ~ station) + labs(title = paste("Staggered", i, "Days"), 
                                      x = "Offshore Catch Index", 
                                      y = "log(Sonar Count)") 
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
    labs(title = "Negative Log Of Log Model P-Values", 
         x = "Days Lagged Of Offshore Test Fishing", 
         y = "-log(p value)")

#Add eight days forward in time to the test_fish in a temporary object
test_fish_tmp <- test_fish
day(test_fish_tmp$date) <- day(test_fish_tmp$date) + 8
test_fish_to_sonar <- inner_join(filter(test_fish_tmp, as.numeric(station) >= 4, as.numeric(station) <= 9),
                                 sonar,
                                 by = "date")

#Look at coorelation between test fishery and sonar count.  Not very informative. I think genetics would 
#really improve this as I can't account for how many sockeye in the test fishery are not going to the Kenai river.
ggplot(data = test_fish_to_sonar,
       aes(x = index, y = log(n), color = station)) + 
    geom_point() + 
    geom_smooth(method = "lm", formula = "y ~ log(x)") + 
    labs(title = "Daily counts of sonar and test fishery lagged eight days",
         y = "log(Sonar Count)",
         x = "Test Fishery Count")

ggplot(data = filter(test_fish_to_sonar, index > 50),
       aes(x = index, y = log(n), color = station)) + 
    geom_point() + 
    geom_smooth(method = "lm", formula = "y ~ log(x)") + 
    labs(title = "Daily counts of sonar and test fishery lagged eight days and over 50 fish per station.",
         y = "log(Sonar Count)",
         x = "Test Fishery Count")


#Try to group test fish numbers by week to get bigger numbers. 
test_fish_tmp$week <- week(test_fish_tmp$date)
sonar$week <- week(sonar$date)
test_fish_to_sonar <- inner_join(filter(test_fish_tmp, as.numeric(station) >= 4, as.numeric(station) <= 9, index != 0),
                                 sonar,
                                 by = c("week", "year"))
test_fish_to_sonar_by_week <- test_fish_to_sonar %>%
    group_by(week, station, year) %>%
    summarize(index = sum(index),n = sum(n))
ggplot(data = test_fish_to_sonar_by_week,
       aes(x = index, y = n)) + 
    geom_point() + 
    facet_wrap( ~ station) + 
    geom_smooth(method = "lm", formula = "y ~ log(x)") + 
    labs(title = "Weekly Groupings")
#Give up on that and start using the original test fishery object. 

#Try to find teh stagger by stat code for the commerical harverst. Not very sucessful.  
#Assume (big one) that is a one day lag from Ninilchik  
comm_catch_temp <- comm_catch
comm_catch_temp$date <- comm_catch_temp$date + days(1)
comm_and_sonar <- inner_join(na.omit(filter(comm_catch, stat_code == 24421)), sonar, by = "date")
#Some semplence of a pattern?
ggplot(data = comm_and_sonar, aes(x = sockeye, y = n)) + geom_point()


#Comm_catch and sonar coorelation. I can't find very good coorelation between commercial catch and sonar by stat code with
#any staggering but maybe I can come back and look again. 
#Spread out by stat code. 
comm_catch_spread <- spread(comm_catch, stat_code, sockeye)
p_values <- data.frame(days_lagged = numeric(), model_type = numeric(), intercept = numeric(), parameter = numeric())
plot_list_obj <- list()

#Try to find the right stagger for each stat code
comm_catch_tmp <- comm_catch
day(comm_catch_tmp$date) <- day(comm_catch_tmp$date) - 10
for(i in 0:25) {
    plotting_obj <- inner_join(comm_catch_tmp, sonar, by = "date")
    plot_list_obj[[i+1]] <- ggplot(data = plotting_obj, aes(x = sockeye, y = log(n), color = factor(stat_code))) + geom_point() +
        #geom_smooth(method = "lm") +
        labs(title = paste("Staggered", i - 10, "Days - Daily Counts"), 
                                      x = "Commericial Catch", 
                                      y = "log(Sonar Count)") 
    #Not working and probably not very insightful. 
    #p_values[i+1,] <- c(i,"log",summary(lm(plotting_obj$n  ~ plotting_obj$`24421` + plotting_obj$`24422` + 
    #                                                         plotting_obj$`24422` + plotting_obj$`24431` + 
    #                                                         plotting_obj$`24432` + plotting_obj$`24441` + 
    #                                                         plotting_obj$`24442` + plotting_obj$`24450` + 
    #                                                         plotting_obj$`24450` + plotting_obj$`24451` +
    #                                                         plotting_obj$`24460` + plotting_obj$`24461` + 
    #                                                         plotting_obj$`24470`))$coefficients[,4])
    
    day(comm_catch_tmp$date) <- day(comm_catch_tmp$date) + 1 
}
#The resulting object shows all plots but is not very interesting
#plot_list_obj

#p_values <- gather(p_values, days_lagged, model_type)
#colnames(p_values)[3] <- "value"
#colnames(p_values)[4] <- "p"
#p_values$p <- as.numeric(p_values$p)
#p_values$days_lagged <- as.numeric(p_values$days_lagged)
#ggplot(data = p_values, aes(y = -log(p), x = days_lagged)) + geom_bar(stat = "identity") +
#    facet_wrap( ~ value) + 
#    labs(title = "Negative Log Of Log Model P-Values", 
#         x = "Days Lagged Of Offshore Test Fishing", 
#         y = "-log(p value)")

