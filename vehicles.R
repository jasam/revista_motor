library(plyr)
library(ggplot2)
library(reshape2)
# http://www.fueleconomy.gov/feg/ws/index.shtml#vehicle.
vehicles <- read.csv("vehicles.csv", stringsAsFactors = F)
head(vehicles)
labels <- do.call(rbind, strsplit(readLines("varlabels.txt"), " - "))
head(labels)
nrow(vehicles)
ncol(vehicles)
names(vehicles)
length(unique(vehicles[, "year"]))
first_year <- min(vehicles[, "year"])
last_year <- max(vehicles[, "year"])
# Count group by for column
table(vehicles$fuelType1)
vehicles$trany[vehicles$trany == ""] <- NA
vehicles$trany2 <- ifelse(substr(vehicles$trany, 1, 4) == "Auto", "Auto", "Manual")
vehicles$trany <- as.factor(vehicles$trany)
table(vehicles$trany2)
with(vehicles, table(sCharger, year))
mpgByYr <- ddply(vehicles, ~year, summarise, avgMPG =
                   mean(comb08), avgHghy = mean(highway08), avgCity =
                   mean(city08))

ggplot(mpgByYr, aes(year, avgMPG)) + geom_point() +
  geom_smooth() + xlab("Year") + ylab("Average MPG") +
  ggtitle("All cars")

gasCars <- subset(vehicles, fuelType1 %in% c("Regular
Gasoline", "Premium Gasoline", "Midgrade Gasoline") &
                    fuelType2 == "" & atvType != "Hybrid")

mpgByYr_Gas <- ddply(gasCars, ~year, summarise, avgMPG =
                       mean(comb08))

ggplot(mpgByYr_Gas, aes(year, avgMPG)) + geom_point() +
  geom_smooth() + xlab("Year") + ylab("Average MPG") +
  ggtitle("Gasoline cars")

gasCars$displ <- as.numeric(gasCars$displ)

ggplot(gasCars, aes(displ, comb08)) + geom_point() +
  geom_smooth()


avgCarSize <- ddply(gasCars, ~year, summarise, avgDispl =
                      mean(displ))

ggplot(avgCarSize, aes(year, avgDispl)) + geom_point() +
  geom_smooth() + xlab("Year") + ylab("Average engine
displacement (l)")

byYear <- ddply(gasCars, ~year, summarise, avgMPG =
                  mean(comb08), avgDispl = mean(displ))

byYear2 = melt(byYear, id = "year")
levels(byYear2$variable) <- c("Average MPG", "Avg engine
displacement")


ggplot(byYear2, aes(year, value)) + geom_point() +
  geom_smooth() + facet_wrap(~variable, ncol = 1, scales =
                               "free_y") + xlab("Year") + ylab("")

gasCars4 <- subset(gasCars, cylinders == "4")

ggplot(gasCars4, aes(factor(year), comb08)) + geom_boxplot()
+ facet_wrap(~trany2, ncol = 1) + theme(axis.text.x = element_
                                        text(angle = 45)) + labs(x = "Year", y = "MPG")

ggplot(gasCars4, aes(factor(year), fill = factor(trany2))) +
  geom_bar(position = "fill") + labs(x = "Year", y = "Proportion
of cars", fill = "Transmission") + theme(axis.text.x =
                                           element_text(angle = 45)) + geom_hline(yintercept = 0.5,
                                                                                  linetype = 2)

carsMake <- ddply(gasCars4, ~year, summarise, numberOfMakes =
                    length(unique(make)))

