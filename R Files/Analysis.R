#pulling dataset
library(ggplot2)
library(magrittr)
library(lubridate)
airdata = read.csv("Data/2015_Air_quality_in_northern_Taiwan.csv")

Tamsui = airdata %>% subset(station == "Tamsui")
data = as.POSIXct(as.character(Tamsui$time), format = "%Y/%m/%d %H:%M")


Tamsui$Hour <- format(data, "%T")
Tamsui$Month <-months(as.Date(data))

Tamsui2 <- Tamsui %>% subset(select = c("Month", "CO"))
Tamsui2$CO <- as.numeric(Tamsui2$CO)

  
ggplot(Tamsui2, aes(Month, CO)) +
  geom_line() +
  scale_y_discrete(breaks = seq(0,.3,1))

month(Tamsui$time)
       