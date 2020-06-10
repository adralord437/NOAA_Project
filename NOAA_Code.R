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
str(storm_data)
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
        labs(title = "Deaths", subtitle = "1950 - 2011") +
        xlab("Total Deaths") +
        ylab("Event Type") +
        scale_x_continuous(breaks = seq(from = 0, to = 7000, by = 500))


b <- ggplot(head(harmful_events, n = 20), aes(Injured, reorder(EVTYPE, Injured))) +  # Fatalities plot (National level)
        geom_bar(stat =  "identity", aes(fill = )) +
        labs(title = "Injuries", subtitle = "1950 - 2011") +
        xlab("Total People Injured") +
        ylab("Event Type") +
        scale_x_continuous(breaks = seq(from = 0, to = 100000, by = 10000))

grid.arrange(a, b, top = "Most Harmful Events in the US") ## Arranging together the two plots


## Economic impact of the Events
Economic_events <- storm_data %>%
        filter(STATE__:EVTYPE, PROPDMG, CROPDMG) %>%
        group_by(EVTYPE) %>%
        summarize(
                PROP_DAMAGE = sum(PROPDMG),
                CROP_DAMAGE = sum(CROPDMG),
                TOTAL_DAMAGE = PROP_DAMAGE + CROP_DAMAGE) %>%
        arrange(desc(TOTAL_DAMAGE))

ggplot(head(Economic_events, n =  20), aes(TOTAL_DAMAGE, reorder(EVTYPE, TOTAL_DAMAGE))) +
        geom_bar(stat = "identity") +
        ylab("Event Type") +
        xlab("Millions of Dollars") +
        labs(title = "Events by Economic Damage in the US", subtitle = "1950 - 2011") +
scale_x_continuous(breaks = seq(0,600000, by = 50000)) +
        theme_classic()

## Which STATE has been the most damaged by natural events
storm_data$total_economic_dmg <- with(storm_data, PROPDMG) + with(storm_data, CROPDMG) ## Creating new variable total dmg
        
interest_variables = c("STATE__","BGN_DATE","TIME_ZONE", "COUNTYNAME", "STATE", "EVTYPE", "FATALITIES", 
                       "INJURIES", "PROPDMG", "CROPDMG", "total_economic_dmg")

df1 <- storm_data[, interest_variables]  ## Eliminating unnecessary variables

df1$total_ppl_dmg <- with(df1, FATALITIES) + with(df1, INJURIES)

STATES <- unique(df1$STATE)
subset(df1, STATE == STATES[[1]])

## State with the greatest number of natural events in record
states <- unique(df1$STATE)
n <- vector("integer", length = length(states))
for(i in seq_along(states))
n[[i]] <- length(which(df1$STATE == states[[i]]))

N_events_State <- data.frame(
        state = states,
        N_events = n) %>%
        arrange(desc(N_events))

N_events_State[which(N_events_State$N_events == max(N_events_State$N_events)), ] ## The state with the greatest number of natural events is texas

## What are the most common natural events in TEXAS?
df1
df2 <- subset(df1, STATE == "TX")
events_TX <- unique(df2$EVTYPE)

n1 <- vector("integer", length = length(events_TX))
for(i in seq_along(events_TX)){
n1[[i]] <- length(which(df2$EVTYPE == events_TX[[i]]))
}
evnts_TX <- data.frame(
        Event = events_TX,
        Ocurrency = n1
) %>%
        arrange(desc(Ocurrency))
rm(events_TX)

## What has been the economic damage made to TEXAS by this events?

common_evnts_TX <- evnts_TX[1:10,1] ## 10 most common natural events in TEXAS

a <- subset(df2, EVTYPE %in% common_evnts_TX)
sum(a$total_economic_dmg)                        ## Joint Economic losses of this events since 1950 to 2011


eco_dmg <- vector("double", length(common_evnts_TX)) ## Economic losses by event
for(i in seq_along(common_evnts_TX)){ 
eco_dmg[[i]] <- with(a[which(a$EVTYPE == common_evnts_TX[[i]]), ], sum(total_economic_dmg)) 
}
Common_evnts_TX <- data.frame(
        Event = common_evnts_TX,
        Econ_dmg = eco_dmg
) %>%
        arrange(desc(Econ_dmg))


## Which counties in Texas are the most damaged 1950 - 2011
a %>% 
        group_by(COUNTYNAME) %>%
        summarise(
                Economic_damage = sum(total_economic_dmg)
        ) %>%
        arrange(desc(Economic_damage))

## What events cause the greatest damage in Harris County (TX)
a[a$COUNTYNAME == "HARRIS", ] %>%
        group_by(EVTYPE) %>%
        summarise(Economic_Damage = sum(total_economic_dmg)) %>%
        arrange(desc(Economic_Damage))

