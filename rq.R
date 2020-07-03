## Corona data from European CDC

library(tidyverse)
library(cowplot)
library(forecast)
library(gridExtra)
library(directlabels)
library(reshape2)

data <- read.csv(url("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"))
# colnames(data)[1] <- "dateRep"
#data <- read.csv("C:/Users/Lenovo/Google Drive/R files/DataProjects/Corona/corona/ecdc_20JunCovid.csv")
data$countriesAndTerritories <- gsub("United_States_of_America", "USA", data$countriesAndTerritories)
data$countriesAndTerritories <- gsub("United_Kingdom", "UK", data$countriesAndTerritories)
d <- data  %>% 
    mutate(ReportingDate = as.Date(as.character(dateRep), format = "%d/%m/%Y")) %>%
    group_by(countriesAndTerritories) %>%
    arrange(ReportingDate, .by_group = TRUE) %>%
    mutate(CasesToDate = cumsum(cases), 
           DeathsToDate = cumsum(deaths)) %>%
    #select(ReportingDate, countriesAndTerritories, cases, deaths, 
    # CasesToDate, DeathsToDate) %>%
    mutate(CasesDoublingTime = log(2)/log(CasesToDate/lag(CasesToDate)),
           DeathsDoublingTime = log(2)/log(DeathsToDate/lag(DeathsToDate)) ) %>%
    ungroup() %>%
    rename(Country = countriesAndTerritories,
           CountryCode = countryterritoryCode,
           Continent = continentExp
    )

sickest <- d %>%
    group_by(Country) %>%
    summarise(CasesToDate = max(CasesToDate),
              DeathsToDate = max(DeathsToDate),
              DeathRate = round(100 * DeathsToDate/CasesToDate, 1),
              Population = last(popData2019),
              CountryCode = last(CountryCode)
    ) %>%
    arrange(desc(CasesToDate)) %>%
    ungroup()

## To extract the most recet reporting date for the title in the charts
RepDate <- d %>% 
    group_by(Country) %>%
    
    summarise(latest = last(ReportingDate))
Date.Upto <- filter(RepDate, Country == "UK")$latest  ## the most recent date for the UK    

top16countries <- as.character(sickest$Country[1:16])
top8countries <- as.character(sickest$Country[1:8])
#1. Lollipop chart of top 25 countries tally of cases to date
## to reorder the levels of the Country names
sickest$Country <- reorder(sickest$Country, sickest$CasesToDate, FUN = identity)
summary.text <- paste("Worldwide there have been \n", 
                      format(sum(sickest$CasesToDate), big.mark= ","),
                      "reported cases and ", 
                      format(sum(sickest$DeathsToDate, na.rm = TRUE), big.mark= ","),
                      "deaths. \nIndia has had", format( subset(sickest, Country == "India")[[1,2]], big.mark=","),
                      "cases and", subset(sickest, Country == "India")[[1,3]],
                      "deaths"
)


sickest <- mutate(sickest, ToHighlight = ifelse(Country == "India", "yes", "No"))

##**************************
#function to calculate the recency quotient
recentcumsum <- function(x, n) { # x is a numeric vectior, n is a number less than length(x)
    # returns a vector of length x os the sum mof last n values of x
    # padding with leading NAs as necessary
    z <- numeric(length = length(x))
    for (i in 1:length(x)) {
        x.sofar <- x[1:i]
        if (i <= n) {
            z[i] <-  NA  
        }
        else { z[i] <- sum(lead(x.sofar, length(x.sofar)-n), na.rm = TRUE)
        
        }
    }
    return(z)
}

##*********************
recburd <- d %>%  ## recburd calculates recent burden of cases 
    group_by(Country) %>%
    arrange(ReportingDate, .by_group = TRUE) %>%
    mutate(CasesLast14Days = recentcumsum(cases, 14) ) %>%
    ungroup()
topcountries <- sickest$Country[1:25]
#recburd$Country <- reorder(recburd$Country, recburd$last14days/recburd$cases, FUN = identity)
stillgrowing <- recburd %>%
    filter(Country %in% topcountries) %>%
    group_by(Country) %>%
    summarise(last14days = last(CasesLast14Days),
              cases = last(CasesToDate) ) 
stillgrowing$Country <- reorder(stillgrowing$Country, 
                                stillgrowing$last14days/stillgrowing$cases, 
                                FUN = identity)
p9 <- stillgrowing %>%
    ggplot(aes(x=Country, 
               y = last14days/cases)) +
    
    geom_point(stat="identity",position="identity", size=3.0, colour = "red") +
    geom_col(fill= "blue", width = 0.11) +
    coord_flip() +
    scale_x_discrete("") +
    scale_y_continuous("%age of all cases that occurred in last 14 days",
                       breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                       labels = c("0", "10%", "20%", "30%", "40%", "50%", "60%")) +
    labs(title = "Covid19 across the world; the top 25 countries", 
         subtitle = "By the proportion of total infections that occurred in last 14 days",
         caption = paste("data source: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv, 
                         © JayEnAar ", as.character(Date.Upto, format = "%d %b %Y") ) )


p9
# ggsave(plot = p9, file=paste0("RecencyQuotient", as.character(Date.Upto, format = "%d%m%Y"), ".png" ), 
#        width=10, height =12, units = "in", 
#        path = "CovidTrackerV2")
##**************************************
India.RQ <- recburd %>% filter(Country == "India" & ReportingDate > as.Date("2020/04/15")) %>%
    mutate(RQ = CasesLast14Days/CasesToDate) %>%
    select(ReportingDate, RQ, CountryCode)
countrycolors <- c("India" = "black")
countrylines <- c("India" = 1)
top16recburd <- recburd %>% 
    filter(Country %in% sickest$Country[1:8] & 
               ReportingDate > as.Date("2020/04/15")) %>%
    mutate(RQ = CasesLast14Days/CasesToDate)  %>%
    group_by(CountryCode) %>%
    mutate(label1 = ifelse(ReportingDate == last(ReportingDate), as.character(CountryCode), ""),
           label2 = ifelse(ReportingDate == as.Date("2020/05/05"), as.character(CountryCode), "") ) %>%
    ungroup() %>%
    select(ReportingDate, RQ, CountryCode, label1, label2)

plot.rq <- top16recburd %>%
    ggplot(aes(x = ReportingDate, y = RQ), colour = CountryCode) + 
    geom_line(aes(colour = CountryCode)) +
    geom_text(aes(label = label1), hjust = "outward") +
    geom_text(aes(label = label2)) +
    geom_line(data = India.RQ, aes(y=RQ), colour= "black", size = 1.1) +
    #scale_colour_brewer(palette = "") +
    #scale_colour_viridis_d() 
    theme(legend.position = "none") +
    labs(title = "Recency Quotient over time in the 12 countries with the most cases", 
         subtitle = "RQ is the proportion of total infections that occurred in last 14 days",
         caption = paste("data source: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv, 
                         © JayEnAar ", as.character(Date.Upto, format = "%d %b %Y") ),
         y= "RQ - recency quotient") +
    scale_y_continuous("RQ - Recency quotient", 
                       breaks = c(0, 0.25, 0.5, 0.75),
                       labels = c("0", "25%", "50%", "75%")) +
    scale_x_date(name = "", 
                 breaks = as.Date(c("2020/04/15", "2020/05/01", "2020/05/15", 
                                    "2020/06/01", "2020/06/15", "2020/07/01")),
                 labels = c("15\nApr", "01\nMay", "15\nMay", "01\nJun", "15\nJun", "01\nJul")
    )


plot.rq



##*******************************************
## india states data
d.states <- read.csv(url("https://t.co/lfRdu7epRj?amp=1")) %>%
    melt(id = c(1:2)) %>% 
    dcast(formula = Date + variable ~ Status, 
          value.var = "value") %>%
    mutate(Date = as.Date(Date, format = "%d-%b-%y")) %>%
    arrange(Date)
colnames(d.states) <- c("Date", "StateCode","Cases", "Deaths", "Recovered") 
d.states <- d.states %>%
    group_by(StateCode) %>%
    arrange(Date, .by_group = TRUE) %>%
    mutate(CasesToDate = cumsum(Cases), 
           DeathsToDate = cumsum(Deaths),
           RecoveriesToDate = cumsum(Recovered),
           CFR = DeathsToDate / (DeathsToDate + RecoveriesToDate)) %>%
    ungroup()

SickestState <- d.states %>%
    group_by(StateCode) %>%
    arrange(Date, .by_group = TRUE) %>%
    summarise(CasesToDate = max(CasesToDate),
              DeathsToDate = max(DeathsToDate),
              RecoveredToDate = last(RecoveriesToDate),
              DeathRate = last(CFR),
              State.Population = last(state.population)) %>%
    arrange(desc(CasesToDate)) %>%
    ungroup()
Date.Latest <- d.states[nrow(d.states),]$Date

states.RQ <- d.states %>%  ## recburd calculates recent burden of cases 
    filter(!(StateCode %in% c("TT", "UN"))) %>%
    group_by(StateCode) %>%
    arrange(Date, .by_group = TRUE) %>%
    mutate(CasesLast14Days = recentcumsum(Cases, 14) ) %>%
    ungroup()

top10states <- states.RQ %>% 
    filter(StateCode %in% SickestState$StateCode[2:9] & 
               Date > as.Date("2020/04/15")) %>%
    mutate(RQ = CasesLast14Days/CasesToDate)  %>%
    group_by(StateCode) %>%
    mutate(label1 = ifelse(Date == as.Date("2020/05/07"), as.character(StateCode), ""),
           label2 = ifelse(Date == last(Date), as.character(StateCode), ""),
           label3 = ifelse(Date == as.Date("2020/05/17"), as.character(StateCode), "")) %>%
    
    ungroup() %>%
    select(Date, RQ, StateCode, label1, label2, label3)
states.rqplot <- top10states %>%
    ggplot(aes(x = Date, y = RQ), colour = StateCode) + 
    geom_line(aes(colour = StateCode)) +
    geom_text(aes(label = label1), hjust = "outward") +
    geom_text(aes(label = label2), hjust = "outward") +
    geom_text(aes(label = label3), hjust = "outward") +
    #geom_line(data = India.RQ, aes(y=RQ), colour= "black", size = 1.1) +
    #scale_colour_brewer(palette = "") +
    #scale_colour_viridis_d() 
    theme(legend.position = "none") +
    labs(title = "Recency Quotient over time in the 8 states with the most cases", 
         subtitle = "RQ is the proportion of total infections that occurred in last 14 days",
         caption = paste("data source: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv, 
                         © JayEnAar ", as.character(Date.Upto, format = "%d %b %Y") ),
         y= "RQ - recency quotient") +
    scale_x_date(name = "", 
                 breaks = as.Date(c("2020/04/15", "2020/05/01", "2020/05/15", 
                                    "2020/06/01", "2020/06/15", "2020/07/01")),
                 labels = c("15\nApr", "01\nMay", "15\nMay", "01\nJun", "15\nJun", "01\nJul")
    )
states.rqplot



