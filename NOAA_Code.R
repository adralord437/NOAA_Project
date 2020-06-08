#                             Course Project 2 - NOAA
# ==========================================================================================================================

# Loading dependencies
 
if(!require(dplyr))(install.packages("dplyr"))         ; library(dplyr)
if(!require(ggplot2))(install.packages("ggplot2"))     ; library(ggplot2)
if(!require(gridExtra))(install.packages("gridExtra")) ; library(gridExtra)
       
# Downloading the data
if(!dir.exists("./data"))(dir.create("./data"))

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, destfile = "./data/Storm_Data.bz2")  ## 2020-06-07 17:36:05 CST

# Importing the data
storm_data <- read.csv("./data/Storm_Data.bz2")
View(storm_data)

# Tranforming variable BGN_DATE to Date format
storm_data$BGN_DATE <- as.Date(storm_data$BGN_DATE, format = "%m/%d/%Y %H:%M:%S")
with(storm_data, range(BGN_DATE)) # Range: 1950 - 2011

## What types of events are most harmful with respect to population health

# Creating df summarizing info
harmful_events <- storm_data %>%
        group_by(EVTYPE) %>%
        summarise(
                Fatal = sum(FATALITIES),
                Injured = sum(INJURIES)) %>%
        mutate(Totalppl_affected = Fatal + Injured) %>%
        arrange(desc(Totalppl_affected))


# Plotting
win.graph(width = 1080, height = 720) ## opening a new window to use it as graph device

a <- ggplot(head(harmful_events, n = 20), aes(Fatal, reorder(EVTYPE, Fatal))) +  # Fatalities plot (National level)
        geom_bar(stat =  "identity", aes(fill = )) +
        labs(title = "Most Deathly Events in the US", subtitle = "1950 - 2011") +
        xlab("Total Deaths") +
        ylab("Event Type") +
        scale_x_continuous(breaks = seq(from = 0, to = 7000, by = 500))


b <- ggplot(head(harmful_events, n = 20), aes(Injured, reorder(EVTYPE, Injured))) +  # Fatalities plot (National level)
        geom_bar(stat =  "identity", aes(fill = )) +
        labs(title = "Most Harmful events in the US", subtitle = "1950 - 2011") +
        xlab("Total People Injured") +
        ylab("Event Type") +
        scale_x_continuous(breaks = seq(from = 0, to = 100000, by = 10000))

grid.arrange(a, b) ## Arranging together the two plots
