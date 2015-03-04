library(dplyr)
library(ggplot2)
library(lubridate)

commercial_catch <- read.csv("data\\commercial_catch.csv", skip = 3)
colnames(commercial_catch) <- c("year", "date", "stat_area_name", "stat_code", "chinook", "chum",
                                "coho", "pink", "sockeye")
commercial_catch$date <- mdy(paste(commercial_catch$date, commercial_catch$year))
commercial_catch$sockeye <- as.numeric(as.character(commercial_catch$sockeye))
commercial_catch$sockeye[is.na(commercial_catch$sockeye)] <- NA
levels(commercial_catch$stat_area_name)

year(commercial_catch$date) <- 2000
ggplot(data = commercial_catch, aes(x = date, y = sockeye, color = factor(stat_code))) + geom_point() + 
    facet_wrap( ~ stat_code)
year(commercial_catch$date) <- commercial_catch$year


summed_catch_stat <- commercial_catch %>%
    group_by(year, stat_code) %>%
    summarize(sum(sockeye)) 
colnames(summed_catch_stat)[3] <- "sockeye"

ggplot(data = summed_catch_stat, aes(x = year, y = sockeye, color = factor(stat_code))) + geom_point() +
    facet_wrap( ~ stat_code)

subset_of_comm_catch <- commercial_catch[as.character(commercial_catch$stat_code) %in% c("24421", "24422", "24431", "24432", "24442", "24470", "24460", "24450", "24461"),]

year(subset_of_comm_catch$date) <- 2000
ggplot(data = subset_of_comm_catch, aes(x = date, y = sockeye, fill = factor(stat_code))) + geom_bar(stat = "identity") +
    facet_wrap( ~ stat_code)
year(subset_of_comm_catch$date) <- subset_of_comm_catch$year

comm_catch <- select(subset_of_comm_catch, year, date, stat_area_name, stat_code, sockeye)

save(comm_catch, file = "data/comm_catch.rda")
       

