#pulling dataset
library(ggplot2) #used for plotting
library(magrittr)
library(lubridate)
library(dplyr) 
library(anytime) #anytime pacakge. converts interger/factor/etc to
library(zoo)
library(shinyjs)
airdata = read.csv("Data/2015_Air_quality_in_northern_Taiwan.csv", na.strings = c("","NA"))

Tamsui = airdata %>% subset(station == "Tamsui")
data = as.POSIXct(as.character(Tamsui$time), format = "%Y/%m/%d %H:%M") 
#or, simpler, data = anydate(Tamsui$time)

Tamsui$Hour <- format(data, "%T")
Tamsui$Month <- months(as.Date(data))

Tamsui2 <- Tamsui %>% subset(select = c("Month", "CO", "NOx", "O3", "CH4"))


#converst factor to numeric, in order to Omit NA's. 
Tamsui2$CO <- as.numeric(as.character(Tamsui2$CO))
Tamsui2$NOx <- as.numeric(as.character(Tamsui2$NOx))
Tamsui2$O3 <- as.numeric(as.character(Tamsui2$O3))
Tamsui2$CH4 <- as.numeric(as.character(Tamsui2$CH4))


#as.numeric(levels(Tamsui2$CO)[Tamsui2$CO])
Tamsui2$Month <- factor(Tamsui2$Month, levels = month.name, labels = month.abb)

#Since our date is factor, we need to change it to a date. 
#Tamsui2$Month <-anydate(Tamsui2$Month) #- This line works, however, makes the year 1400. strange.

#Adding the year back in because can't read it properly with only month displayed. We will removed year 
Tamsui2$Month <-as.Date(as.yearmon(format(paste(Tamsui2$Month,2015)), "%b%Y"))

ggplot(Tamsui2, aes(Month, O3, color = O3)) +
  geom_jitter(alpha = 1, height = 5) +
  geom_smooth(method = "loess", color = "#581845", size = 1.3, span = .3, alpha = .4, se = FALSE) +
  scale_x_date(date_labels="%b", date_breaks = "1 month") +
  scale_y_continuous(breaks = c(30,60,90,110)) +
  scale_color_gradient2(low = "#07D570", mid = "#D59007", high = "#FE0000", midpoint = 50, guide = "colourbar")+
  ggtitle("Ozone Levels in Tamsui") +
  labs(x = "Month", y = "Ozone (ppb)", caption = "
      In general, Ozone is within acceptable levels (0-50).
      However, there is a sharp uptick in Ozone levels in September and August (100+). 
      These levels are unhealthy - affecting children and older adults." ) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), plot.caption = element_text(size=12, hjust = .5), plot.title = element_text(size = 20, hjust = .5)) 

ggsave("tamsui_plot2.png", plot = tamsui_plot, path = "Images", device = "png", scale = 1)



readinteger <- function(x){
  print(paste(unique(x$station)))
  n <- readline(prompt = "Enter the city you would like to analyze: ")
  
  City = x %>% subset(x$station == n)
  
  City = City[,colSums(is.na(City)) < nrow(City) * .50]
  
  City2 = as.POSIXct(as.character(City$time), format = "%Y/%m/%d %H:%M") 
  
  City$Month <- months(as.Date(City2))
  
  City$Month <- factor(City$Month, levels = month.name, labels = month.abb)
  
  City$Month <- as.Date(as.yearmon(format(paste(City$Month,2015)), "%b%Y"))
  
  print(colnames(City))
  Yvar <- readline(prompt = "Enter the Y variable you would like to analyze: ")
  City[,Yvar] <- as.numeric(as.character(City[,Yvar]))
  class(City[,Yvar])}
  ggplot(City, aes(Month, Yvar, color = Yvar)) +
    geom_jitter(alpha = 1, height = 5) +
    geom_smooth(method = "loess", color = "#581845", size = 1.3, span = .3, alpha = .4, se = FALSE) +
    scale_y_log10()
  


readinteger(airdata)



