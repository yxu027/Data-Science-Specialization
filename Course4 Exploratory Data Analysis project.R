setwd("X:/Git/Data-Science-Specialization/exdata_data_NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#plot1
plot1 <- with(NEI, tapply(Emissions, year, sum))
png("plot1.png",width=5,height=5,units="in",res=150)
barplot(plot1, ylab = "Emissions (tons)", xlab = "Year")
dev.off()


#plot2
library(tidyverse)
plot2 <- NEI %>%
  filter(fips == "24510")
plot2 <- with(plot2, tapply(Emissions, year, sum))
png("plot2.png",width=5,height=5,units="in",res=150)
barplot(plot2, ylab = "Baltimore City Emissions (tons)", xlab = "Year")
dev.off()

#plot3
library(tidyverse)
plot3 <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year, type) %>%
  summarise(Emissions.sum = sum(Emissions))
plot3 
png("plot3.png",width=5,height=5,units="in",res=150)
ggplot(plot3, aes(x=factor(year), y=Emissions.sum, color=type, group=type)) +
  geom_line() +
  geom_point() +
  xlab("Year") +
  ylab("Emissions (tons)")
dev.off()


#plot4
head(SCC)
coal_SCC <- SCC %>%
  select(1,4) %>%
  filter(., grepl("Coal", EI.Sector))

plot4 <- merge(NEI, coal_SCC, by= "SCC") %>%
  group_by(year) %>%
  summarise(Emissions.sum = sum(Emissions))

png("plot4.png",width=5,height=5,units="in",res=150)
ggplot(plot4, aes(x=factor(year), y=Emissions.sum)) +
  geom_line(aes(group=1)) +
  geom_point() +
  xlab("Year") +
  ylab("Coal Emissions (tons)")
dev.off()


#plot5
plot5 <- NEI %>%
  filter(fips == "24510" & type == "ON-ROAD") %>%
  group_by(year) %>%
  summarise(Emissions.sum = sum(Emissions))

png("plot5.png",width=5,height=5,units="in",res=150)
ggplot(plot5, aes(x=factor(year), y=Emissions.sum)) +
  geom_line(aes(group=1)) +
  geom_point() +
  xlab("Year") +
  ylab("Baltimore Motor Vehicle Emissions (tons)")
dev.off()

#plot6
plot6 <- NEI %>%
  filter(type == "ON-ROAD") %>%
  filter(fips == "24510" | fips == "06037" ) %>%
  group_by(year, fips) %>%
  summarise(Emissions.sum = sum(Emissions))

plot6$city[plot6$fips == "24510"] <- "Baltimore"
plot6$city[plot6$fips == "06037"] <- "Los Angeles"

plot6

png("plot6.png",width=5,height=5,units="in",res=150)
ggplot(plot6, aes(x=factor(year), y=Emissions.sum, color=city, group=city)) +
  geom_line(aes(group=fips)) +
  geom_point() +
  xlab("Year") +
  ylab("Motor Vehicle Emissions (tons)")
dev.off()
