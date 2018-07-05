
require(openxlsx)
require(xts)
require(zoo)
require(tidyr)
require(dplyr)
require(quantmod)
require(kernlab)
require(PerformanceAnalytics)


source('./functions/functions_sit.R')
source('./functions/momentum_models.R')
source('./functions/pos_momentum.R')

model_choices <- list("SPY", "EW", "MOM", "PMOM", "MV", "RPMOM", "RPPMOM", "TMNG", "MOMTV", "PMOMTV", "TMNGTV")

xl.file <- "ETF Data.xlsx"
xl.data <- read.xlsx(paste0('./data/', xl.file))
xl.data$Date <- as.Date(xl.data$Date, origin = "1899-12-30")

# # omit data until all series have price
# idx <- !is.na(rowSums(xl.data[,-1]))
# xl.data <- xl.data[idx,]
# 
# # create long format
# lfd <- gather(xl.data, Security, Price, -Date  )
# 
# # rebase time series
# xl.data <- lfd %>% arrange(., Security, Date) %>%
#   group_by(., Security) %>%
#   mutate(., value = log(Price) - log(lag(Price, 1)),
#          value = ifelse(is.na(value), 0, value),
#          cum.value = exp(cumsum(value)) * 100
#          ) %>%
#   select(., -c(value, Price)) %>%
#   rename(., value = cum.value) %>%
#   spread(Security, value) %>%
#   as.data.frame()

# dynamic start and end date
xts.ts <- xts(xl.data[,-1], order.by = xl.data$Date)
start.date <- first(index(xts.ts))
end.date   <- last(index(xts.ts))

max_assets <- ncol(xts.ts)

