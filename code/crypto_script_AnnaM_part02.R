

############################# PART II ##########################################

library(readxl)
library(quantmod)
library(dplyr)
library(lubridate)
library(tidyquant)
library(tidyverse)
library(patchwork)
library(moments)
library(ggplot2)
library(dplyr)
library(broom)
library(purrr)
library(knitr)
library(kableExtra)


crypto_data_part2 <- read_excel("crypto_data_2.xlsx")
str(crypto_data_part2)

tickers <- c("BTC-USD", "ETH-USD", "XRP-USD", "DOGE-USD", "SHIB-USD")
getSymbols(tickers, src = "yahoo", from = "2021-01-01", auto.assign = TRUE)
str(`BTC-USD`)
str(`ETH-USD`)
str(`XRP-USD`)
str(`DOGE-USD`)
str(`SHIB-USD`)

crypto_data_part21 <- data.frame(
        date = index(`BTC-USD`),
        BTC_Close = Cl(`BTC-USD`),
        BTC_Volume = Vo(`BTC-USD`),
        
        ETH_Close = Cl(`ETH-USD`),
        ETH_Volume = Vo(`ETH-USD`),
        
        XRP_Close = Cl(`XRP-USD`),
        XRP_Volume = Vo(`XRP-USD`),
        
        DOGE_Close = Cl(`DOGE-USD`),
        DOGE_Volume = Vo(`DOGE-USD`),
        
        SHIB_Close = Cl(`SHIB-USD`),
        SHIB_Volume = Vo(`SHIB-USD`)
) %>%
        na.omit()  


crypto_data_part21 <- crypto_data_part21 %>%
        mutate(
                market_cap_BTC  = BTC.USD.Close * BTC.USD.Volume,
                market_cap_ETH  = ETH.USD.Close * ETH.USD.Volume,
                market_cap_XRP  = XRP.USD.Close * XRP.USD.Volume,
                market_cap_DOGE = DOGE.USD.Close * DOGE.USD.Volume,
                market_cap_SHIB = SHIB.USD.Close * SHIB.USD.Volume
        )


crypto_data_part21 <- crypto_data_part21 %>%
        arrange(date) %>%
        mutate(
                ret_BTC  = (BTC.USD.Close - lag(BTC.USD.Close))  / lag(BTC.USD.Close),
                ret_ETH  = (ETH.USD.Close - lag(ETH.USD.Close))  / lag(ETH.USD.Close),
                ret_XRP  = (XRP.USD.Close - lag(XRP.USD.Close))  / lag(XRP.USD.Close),
                ret_DOGE = (DOGE.USD.Close - lag(DOGE.USD.Close))  / lag(DOGE.USD.Close),
                ret_SHIB = (SHIB.USD.Close - lag(SHIB.USD.Close))  / lag(SHIB.USD.Close)
        ) %>%
        na.omit()  

crypto_data_part21 <- crypto_data_part21 %>%
        mutate(total_market_cap = market_cap_BTC + market_cap_ETH + market_cap_XRP + market_cap_DOGE + market_cap_SHIB)

crypto_data_part21 <- crypto_data_part21 %>%
        mutate(
                market_return = (
                        (ret_BTC  * market_cap_BTC) +
                                (ret_ETH  * market_cap_ETH) +
                                (ret_XRP  * market_cap_XRP) +
                                (ret_DOGE * market_cap_DOGE) +
                                (ret_SHIB * market_cap_SHIB)
                ) / total_market_cap
        )

crypto_weekly <- crypto_data_part21 %>%
        group_by(week = floor_date(date, "week")) %>%
        summarise(
                weekly_return_BTC = mean(ret_BTC, na.rm = TRUE),
                weekly_return_ETH = mean(ret_ETH, na.rm = TRUE),
                weekly_return_XRP = mean(ret_XRP, na.rm = TRUE),
                weekly_return_DOGE = mean(ret_DOGE, na.rm = TRUE),
                weekly_return_SHIB = mean(ret_SHIB, na.rm = TRUE),
                weekly_market_return = mean(market_return, na.rm = TRUE)
        )

str(crypto_weekly)

crypto_monthly <- crypto_data_part21 %>%
        group_by(month = floor_date(date, "month")) %>%
        summarise(
                weekly_return_BTC = mean(ret_BTC, na.rm = TRUE),
                weekly_return_ETH = mean(ret_ETH, na.rm = TRUE),
                weekly_return_XRP = mean(ret_XRP, na.rm = TRUE),
                weekly_return_DOGE = mean(ret_DOGE, na.rm = TRUE),
                weekly_return_SHIB = mean(ret_SHIB, na.rm = TRUE),
                weekly_market_return = mean(market_return, na.rm = TRUE)
        )
str(crypto_monthly)

crypto_data_part21 <- crypto_data_part21 %>%
        mutate(week = floor_date(date, "week", week_start = 7))


weekly_market_caps <- crypto_data_part21 %>%
        group_by(week) %>%
        summarise(
                market_cap_BTC = last(market_cap_BTC[!is.na(market_cap_BTC)]),
                market_cap_ETH = last(market_cap_ETH[!is.na(market_cap_ETH)]),
                market_cap_XRP = last(market_cap_XRP[!is.na(market_cap_XRP)]),
                market_cap_DOGE = last(market_cap_DOGE[!is.na(market_cap_DOGE)]),
                market_cap_SHIB = last(market_cap_SHIB[!is.na(market_cap_SHIB)])
        )


weekly_market_caps <- weekly_market_caps %>%
        mutate(week = week + days(2))

str(weekly_market_caps)

weekly_market_caps <- weekly_market_caps %>%
        mutate(week = week - days(2))

## Graph

normalized_data <- crypto_data_part21 %>%
        mutate(
                BTC_index = BTC.USD.Close / first(BTC.USD.Close),
                ETH_index = ETH.USD.Close / first(ETH.USD.Close),
                XRP_index = XRP.USD.Close / first(XRP.USD.Close),
                DOGE_index = DOGE.USD.Close / first(DOGE.USD.Close),
                SHIB_index = SHIB.USD.Close / first(SHIB.USD.Close),
                CoinMarket_index = total_market_cap / first(total_market_cap) 
        ) %>%
        select(date, BTC_index, ETH_index, XRP_index, CoinMarket_index) %>%
        pivot_longer(cols = -date, names_to = "Crypto", values_to = "Index")

btc_data <- normalized_data %>%
        filter(Crypto %in% c("BTC_index", "CoinMarket_index"))
eth_data <- normalized_data %>%
        filter(Crypto %in% c("ETH_index", "CoinMarket_index"))
xrp_data <- normalized_data %>%
        filter(Crypto %in% c("XRP_index", "CoinMarket_index"))

plot_btc <- ggplot(btc_data, aes(x = date, y = Index, color = Crypto)) +
        geom_line(linewidth = 1) +
        labs(
                title = "Coin Market vs Bitcoin",
                x = "Date",
                y = "$ Value of Investment",
                color = "Index"
        ) +
        theme_minimal() +
        theme(
                legend.position = "top",
                plot.title = element_text(size = 14, face = "bold")
        )

plot_eth <- ggplot(eth_data, aes(x = date, y = Index, color = Crypto)) +
        geom_line(size = 1) +
        labs(
                title = "Coin Market vs Ethereum",
                x = "Date",
                y = "$ Value of Investment",
                color = "Index"
        ) +
        theme_minimal() +
        theme(
                legend.position = "top",
                plot.title = element_text(size = 14, face = "bold")
        )

plot_xrp <- ggplot(xrp_data, aes(x = date, y = Index, color = Crypto)) +
        geom_line(size = 1) +
        labs(
                title = "Coin Market vs Ripple",
                x = "Date",
                y = "$ Value of Investment",
                color = "Index"
        ) +
        theme_minimal() +
        theme(
                legend.position = "top",
                plot.title = element_text(size = 14, face = "bold")
        )

plot_btc / plot_eth / plot_xrp

## Statistic

crypto_data <- crypto_data_part21
crypto_data <- crypto_data %>%
        filter(across(everything(), ~ !is.infinite(.)))

calculate_statistics <- function(data, freq) {
        aggregated_data <- data %>%
                group_by(period = floor_date(date, freq)) %>%
                summarise(
                        BTC = mean(ret_BTC, na.rm = TRUE),
                        ETH = mean(ret_ETH, na.rm = TRUE),
                        XRP = mean(ret_XRP, na.rm = TRUE),
                        CoinMarket = mean(market_return, na.rm = TRUE)
                ) %>%
                na.omit()
        
        stats <- aggregated_data %>%
                summarise(
                        Mean_BTC = mean(BTC) * 100,
                        SD_BTC = sd(BTC) * 100,
                        tStat_BTC = mean(BTC) / (sd(BTC) / sqrt(n())),
                        Sharpe_BTC = mean(BTC) / sd(BTC),
                        Skewness_BTC = skewness(BTC),
                        Kurtosis_BTC = kurtosis(BTC),
                        Percent_Positive_BTC = sum(BTC > 0) / n() * 100,
                        
                        Mean_ETH = mean(ETH) * 100,
                        SD_ETH = sd(ETH) * 100,
                        tStat_ETH = mean(ETH) / (sd(ETH) / sqrt(n())),
                        Sharpe_ETH = mean(ETH) / sd(ETH),
                        Skewness_ETH = skewness(ETH),
                        Kurtosis_ETH = kurtosis(ETH),
                        Percent_Positive_ETH = sum(ETH > 0) / n() * 100,
                        
                        Mean_XRP = mean(XRP) * 100,
                        SD_XRP = sd(XRP) * 100,
                        tStat_XRP = mean(XRP) / (sd(XRP) / sqrt(n())),
                        Sharpe_XRP = mean(XRP) / sd(XRP),
                        Skewness_XRP = skewness(XRP),
                        Kurtosis_XRP = kurtosis(XRP),
                        Percent_Positive_XRP = sum(XRP > 0) / n() * 100,
                        
                        Mean_CoinMarket = mean(CoinMarket) * 100,
                        SD_CoinMarket = sd(CoinMarket) * 100,
                        tStat_CoinMarket = mean(CoinMarket) / (sd(CoinMarket) / sqrt(n())),
                        Sharpe_CoinMarket = mean(CoinMarket) / sd(CoinMarket),
                        Skewness_CoinMarket = skewness(CoinMarket),
                        Kurtosis_CoinMarket = kurtosis(CoinMarket),
                        Percent_Positive_CoinMarket = sum(CoinMarket > 0) / n() * 100
                )
        return(stats)
}

daily_stats <- calculate_statistics(crypto_data, "day")
weekly_stats <- calculate_statistics(crypto_data, "week")
monthly_stats <- calculate_statistics(crypto_data, "month")

summary_stats <- bind_rows(
        daily_stats %>% mutate(Frequency = "Daily"),
        weekly_stats %>% mutate(Frequency = "Weekly"),
        monthly_stats %>% mutate(Frequency = "Monthly")
)
summary_stats
summary(crypto_data$market_return)

## Google searches and past returns



## CRYPTO

google_trends_crypto <- read.csv("multiTimelineCrypto.csv", skip = 1, header = TRUE)
str(google_trends_crypto)
colnames(google_trends_crypto) <- c("Week", "Crypto_Trend")
google_trends_crypto$Week <- as.Date(google_trends_crypto$Week)
str(google_trends_crypto)

ggplot(google_trends_crypto, aes(x = Week, y = Crypto_Trend)) +
        geom_line(color = "blue", size = 1) +
        labs(
                title = "Google Trends: Crypto",
                x = "date",
                y = "search interest"
        ) +
        theme_minimal()
str(crypto_weekly)

google_trends_crypto <- google_trends_crypto %>%
        rename(week = Week, Google_t = Crypto_Trend) %>%  
        mutate(Google_t = scale(Google_t))  

crypto_weekly_google <- crypto_weekly %>%
        inner_join(google_trends_crypto, by = "week")

crypto_weekly_google <- crypto_weekly_google %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_market_return, 1),
                R_t_2 = lag(weekly_market_return, 2),
                R_t_3 = lag(weekly_market_return, 3),
                R_t_4 = lag(weekly_market_return, 4)
        ) %>%
        na.omit()

crypto_weekly_google <- crypto_weekly_google %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_google)

models_google_crypto <- list(
        lm(Google_t ~ weekly_market_return, data = crypto_weekly_google),
        lm(Google_t ~ weekly_market_return + R_t_1, data = crypto_weekly_google),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2, data = crypto_weekly_google),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_google),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_google)
)


results <- map_dfr(models_google_crypto, ~ tidy(.x), .id = "Model")  # Коэффициенты
results
r_squared <- map_dbl(models_google_crypto, ~ summary(.x)$r.squared)  # R^2

r_squared_df <- tibble(
        term = "R^2",
        estimate = r_squared,
        statistic = NA,
        Model = as.character(1:5)
)


## BITCOIN

google_trends_bitcoin <- read.csv("multiTimelineBitcoin.csv", skip = 1, header = TRUE)
str(google_trends_bitcoin)
colnames(google_trends_bitcoin) <- c("Week", "Crypto_Trend")
google_trends_bitcoin <- google_trends_bitcoin[-1, ]
google_trends_bitcoin$Week <- as.Date(google_trends_bitcoin$Week, format = "%m/%d/%Y")
google_trends_bitcoin$Crypto_Trend <- as.numeric(google_trends_bitcoin$Crypto_Trend)

str(google_trends_bitcoin)

ggplot(google_trends_bitcoin, aes(x = Week, y = Crypto_Trend)) +
        geom_line(color = "blue", size = 1) +
        labs(
                title = "Google Trends: Bitcoin",
                x = "date",
                y = "search interest"
        ) +
        theme_minimal()

str(crypto_weekly)


google_trends_bitcoin <- google_trends_bitcoin %>%
        rename(week = Week, Google_t = Crypto_Trend) %>%  
        mutate(Google_t = scale(Google_t))  

crypto_weekly_google_bitcoin <- crypto_weekly %>%
        inner_join(google_trends_bitcoin, by = "week")

crypto_weekly_google_bitcoin <- crypto_weekly_google_bitcoin %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_market_return, 1),
                R_t_2 = lag(weekly_market_return, 2),
                R_t_3 = lag(weekly_market_return, 3),
                R_t_4 = lag(weekly_market_return, 4)
        ) %>%
        na.omit() 

crypto_weekly_google_bitcoin <- crypto_weekly_google_bitcoin %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_google_bitcoin)

models_google_crypto_bitcoin <- list(
        lm(Google_t ~ weekly_market_return, data = crypto_weekly_google_bitcoin),
        lm(Google_t ~ weekly_market_return + R_t_1, data = crypto_weekly_google_bitcoin),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2, data = crypto_weekly_google_bitcoin),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_google_bitcoin),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_google_bitcoin)
)


results <- map_dfr(models_google_crypto_bitcoin, ~ tidy(.x), .id = "Model")  # Коэффициенты
results
r_squared <- map_dbl(models_google_crypto_bitcoin, ~ summary(.x)$r.squared)  # R^2
r_squared

r_squared_df <- tibble(
        term = "R^2",
        estimate = r_squared,
        statistic = NA,
        Model = as.character(1:5)
)

## CryptoReg

google_trends_CryptoReg <- read.csv("multiTimelineCryptoReg.csv", skip = 1, header = TRUE)
str(google_trends_CryptoReg)
colnames(google_trends_CryptoReg) <- c("Week", "Crypto_Trend")
google_trends_CryptoReg$Week <- as.Date(google_trends_CryptoReg$Week)
google_trends_CryptoReg$Crypto_Trend <- as.numeric(google_trends_CryptoReg$Crypto_Trend)

str(google_trends_CryptoReg)


ggplot(google_trends_CryptoReg, aes(x = Week, y = Crypto_Trend)) +
        geom_line(color = "blue", size = 1) +
        labs(
                title = "Google Trends: Bitcoin",
                x = "date",
                y = "search interest"
        ) +
        theme_minimal()

str(crypto_weekly)


google_trends_CryptoReg <- google_trends_CryptoReg %>%
        rename(week = Week, Google_t = Crypto_Trend) %>%  
        mutate(Google_t = scale(Google_t))  

crypto_weekly_google_CryptoReg <- crypto_weekly %>%
        inner_join(google_trends_CryptoReg, by = "week")  

crypto_weekly_google_CryptoReg <- crypto_weekly_google_CryptoReg %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_market_return, 1),
                R_t_2 = lag(weekly_market_return, 2),
                R_t_3 = lag(weekly_market_return, 3),
                R_t_4 = lag(weekly_market_return, 4)
        ) %>%
        na.omit() 

crypto_weekly_google_CryptoReg <- crypto_weekly_google_CryptoReg %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_google_CryptoReg)

models_google_crypto_CryproReg <- list(
        lm(Google_t ~ weekly_market_return, data = crypto_weekly_google_CryptoReg),
        lm(Google_t ~ weekly_market_return + R_t_1, data = crypto_weekly_google_CryptoReg),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2, data = crypto_weekly_google_CryptoReg),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_google_CryptoReg),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_google_CryptoReg)
)


results <- map_dfr(models_google_crypto_CryproReg, ~ tidy(.x), .id = "Model")  # Коэффициенты
results
r_squared <- map_dbl(models_google_crypto_CryproReg, ~ summary(.x)$r.squared)  # R^2
r_squared

r_squared_df <- tibble(
        term = "R^2",
        estimate = r_squared,
        statistic = NA,
        Model = as.character(1:5)
)

## Hack

google_trends_Hack <- read.csv("multiTimelineHack.csv", skip = 1, header = TRUE)
str(google_trends_Hack)
colnames(google_trends_Hack) <- c("Week", "Crypto_Trend")
google_trends_Hack$Week <- as.Date(google_trends_Hack$Week)
google_trends_Hack$Crypto_Trend <- as.numeric(google_trends_Hack$Crypto_Trend)

str(google_trends_Hack)

ggplot(google_trends_Hack, aes(x = Week, y = Crypto_Trend)) +
        geom_line(color = "blue", size = 1) +
        labs(
                title = "Google Trends: Bitcoin",
                x = "date",
                y = "search interest"
        ) +
        theme_minimal()

str(crypto_weekly)


google_trends_Hack <- google_trends_Hack %>%
        rename(week = Week, Google_t = Crypto_Trend) %>%  
        mutate(Google_t = scale(Google_t))  

crypto_weekly_google_Hack <- crypto_weekly %>%
        inner_join(google_trends_Hack, by = "week") 

crypto_weekly_google_Hack <- crypto_weekly_google_Hack %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_market_return, 1),
                R_t_2 = lag(weekly_market_return, 2),
                R_t_3 = lag(weekly_market_return, 3),
                R_t_4 = lag(weekly_market_return, 4)
        ) %>%
        na.omit()  # Убираем строки с NA

crypto_weekly_google_Hack <- crypto_weekly_google_Hack %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_google_Hack)

models_google_crypto_Hack <- list(
        lm(Google_t ~ weekly_market_return, data = crypto_weekly_google_Hack),
        lm(Google_t ~ weekly_market_return + R_t_1, data = crypto_weekly_google_Hack),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2, data = crypto_weekly_google_Hack),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_google_Hack),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_google_Hack)
)


results <- map_dfr(models_google_crypto_Hack, ~ tidy(.x), .id = "Model")  # Коэффициенты
results
r_squared <- map_dbl(models_google_crypto_Hack, ~ summary(.x)$r.squared)  # R^2
r_squared

r_squared_df <- tibble(
        term = "R^2",
        estimate = r_squared,
        statistic = NA,
        Model = as.character(1:5)
)


## S&P 500, Dow Jones, Treasury Yield (10 лет), Gold Futures 

library(quantmod)
library(TTR)
library(jsonlite)
library(xts)
library(lubridate)

symbols <- c("^GSPC", "^DJI", "^TNX", "GC=F")  
getSymbols(symbols, src = "yahoo", from = "2000-01-01")

GSPC_Close <- Cl(GSPC)
DJI_Close <- Cl(DJI)
TNX_Close <- Cl(TNX)
Gold_Close <- Cl(`GC=F`)

colnames(GSPC_Close) <- "S&P500"
colnames(DJI_Close) <- "DowJones"
colnames(TNX_Close) <- "TreasuryYield"
colnames(Gold_Close) <- "GoldFutures"

GSPC_Close <- na.omit(GSPC_Close)
DJI_Close <- na.omit(DJI_Close)
TNX_Close <- na.omit(TNX_Close)
Gold_Close <- na.omit(Gold_Close)

market_data <- merge(GSPC_Close, DJI_Close, TNX_Close, Gold_Close, all = TRUE)
market_data <- na.locf(market_data) 
str(market_data)


library(quantmod)
library(TTR)
library(jsonlite)
library(xts)
library(lubridate)
library(xts)
library(tidyverse)
library(lubridate)
library(tseries)
library(vars)
library(ggplot2)
library(gridExtra)


fear_greed_url <- "https://api.alternative.me/fng/?limit=0"
fear_greed_data <- jsonlite::fromJSON(fear_greed_url)$data
fear_greed_timestamps <- as.numeric(fear_greed_data$timestamp)
fear_greed_dates <- as.Date(as.POSIXct(fear_greed_timestamps, origin = "1970-01-01", tz = "UTC"))
fear_greed_values <- as.numeric(fear_greed_data$value)
fear_greed_xts <- xts(fear_greed_values, order.by = fear_greed_dates)

if (!is.xts(fear_greed_xts)) {
        fear_greed_xts <- xts(fear_greed_xts$Fear_Greed_Index, order.by = fear_greed_dates)
}
fear_greed_df <- data.frame(Date = index(fear_greed_xts), 
                            Fear_Greed_Index = coredata(fear_greed_xts))
fear_greed_df$Date <- as.Date(fear_greed_df$Date)
fear_greed_filtered <- subset(fear_greed_df, Date >= as.Date("2021-01-01"))
str(fear_greed_filtered)
head(fear_greed_filtered)


market_data_tbl <- as_tibble(as.data.frame(market_data), rownames = "Date")
market_data_tbl$Date <- as.Date(market_data_tbl$Date)
market_weekly <- market_data_tbl %>%
        mutate(week = floor_date(Date, "week")) %>%
        group_by(week) %>%
        summarize(across(c(S.P500, DowJones, TreasuryYield, GoldFutures), \(x) mean(x, na.rm = TRUE)))

fear_greed_weekly <- fear_greed_filtered %>%
        mutate(week = floor_date(Date, "week")) %>%
        group_by(week) %>%
        summarize(Fear_Greed_Index = mean(Fear_Greed_Index, na.rm = TRUE))

data_weekly <- crypto_weekly %>%
        left_join(market_weekly, by = "week") %>%
        left_join(fear_greed_weekly, by = "week")

data_weekly <- data_weekly %>%
        mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .)))

data_weekly <- na.omit(data_weekly)
data_weekly <- as.data.frame(data_weekly)  
data_weekly_no_week <- data_weekly[, !(names(data_weekly) %in% "week")]  
data_xts <- xts(data_weekly_no_week, order.by = as.Date(data_weekly$week))


stationarity_tests <- apply(data_xts, 2, function(x) kpss.test(x)$p.value)
stationarity_results <- apply(data_xts, 2, function(x) {
        test <- kpss.test(x)
        return(test$p.value)
})
print(stationarity_results)

data_diff <- data_xts
data_diff[, c("S.P500", "DowJones", "TreasuryYield", "GoldFutures", "Fear_Greed_Index")] <- 
        diff(data_xts[, c("S.P500", "DowJones", "TreasuryYield", "GoldFutures", "Fear_Greed_Index")])
data_diff <- na.omit(data_diff)
stationarity_results_diff <- apply(data_diff, 2, function(x) kpss.test(na.omit(x))$p.value)
print(stationarity_results_diff)
data_diff <- na.omit(data_diff)
str(data_diff)
stationarity_results_diff <- apply(data_diff, 2, function(x) kpss.test(na.omit(x))$p.value)
print(stationarity_results_diff)


cor_matrix <- cor(data_diff, use = "pairwise.complete.obs")
print(cor_matrix)

n <- nrow(var_model) 
p_values <- outer(1:ncol(cor_matrix), 1:ncol(cor_matrix), Vectorize(function(i, j) {
        if (i == j) return(NA)  
        cor.test(data_diff[, i], data_diff[, j])$p.value
}))
cor_data <- melt(cor_matrix)
p_data <- melt(p_values)

p_data$significance <- cut(
        p_data$value,
        breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
        labels = c("***", "**", "*", ""),
        include.lowest = TRUE
)

cor_data$significance <- p_data$significance

cor_data$label <- ifelse(
        is.na(cor_data$significance),
        round(cor_data$value, 2),  
        paste0(round(cor_data$value, 2), cor_data$significance)  
)

ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +  
        geom_text(
                aes(label = label),  
                color = "black", size = 4
        ) + 
        scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) + 
        labs(
                title = "Heatmap of Correlations",
                x = "Variables",
                y = "Variables",
                fill = "Correlation"
        ) +
        theme_minimal() +
        theme(
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)  
        )


########################### Google Search - Doge Coin ##########################

google_trends_doge <- read.csv("multiTimeline_doge_coin.csv", skip = 1, header = TRUE)
str(google_trends_doge)
colnames(google_trends_doge) <- c("Week", "Doge")
google_trends_doge$Week <- as.Date(google_trends_doge$Week)
google_trends_doge$Doge <- as.numeric(google_trends_doge$Doge)
str(google_trends_doge)

library(ggplot2)
ggplot(google_trends_doge, aes(x = Week, y = Doge)) +
        geom_line(color = "blue", size = 1) +
        labs(
                title = "Google Trends: Doge",
                x = "date",
                y = "search interest"
        ) +
        theme_minimal()


library(dplyr)
library(broom)
library(purrr)

google_trends_doge <- google_trends_doge %>%
        rename(week = Week, Google_t = Doge) %>%  
        mutate(Google_t = scale(Google_t))  
str(google_trends_doge)


crypto_weekly_doge <- crypto_weekly %>%
        left_join(weekly_market_caps, by = "week") %>%
        left_join(google_trends_doge, by = "week")

crypto_weekly_doge <- crypto_weekly_doge %>%
        mutate(
                weekly_return_SHIB = ifelse(is.infinite(weekly_return_SHIB), NA, weekly_return_SHIB),
                weekly_market_return = ifelse(is.infinite(weekly_market_return), NA, weekly_market_return)
        )

crypto_weekly_doge <- crypto_weekly_doge %>%
        mutate(
                total_meme_cap = market_cap_DOGE + market_cap_SHIB,
                total_conventional_cap = market_cap_BTC + market_cap_ETH + market_cap_XRP,
                
                weight_DOGE = market_cap_DOGE / total_meme_cap,
                weight_SHIB = market_cap_SHIB / total_meme_cap,
                
                weight_BTC = market_cap_BTC / total_conventional_cap,
                weight_ETH = market_cap_ETH / total_conventional_cap,
                weight_XRP = market_cap_XRP / total_conventional_cap,
                
                weekly_return_meme = (weekly_return_DOGE * weight_DOGE) + (weekly_return_SHIB * weight_SHIB),
                weekly_return_conventional = (weekly_return_BTC * weight_BTC) + 
                        (weekly_return_ETH * weight_ETH) + 
                        (weekly_return_XRP * weight_XRP)
        )

summary(crypto_weekly_doge)
crypto_weekly_doge <- crypto_weekly_doge %>%
        filter(!is.na(weekly_return_meme) & !is.na(weekly_return_conventional))


## MEME-coins
crypto_weekly_doge_meme <- crypto_weekly_doge %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_meme, 1),
                R_t_2 = lag(weekly_return_meme, 2),
                R_t_3 = lag(weekly_return_meme, 3),
                R_t_4 = lag(weekly_return_meme, 4)
        ) %>%
        na.omit()

crypto_weekly_doge_meme <- crypto_weekly_doge_meme %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_doge_meme)


models_google_crypto_weekly_doge_meme <- list(
        lm(Google_t ~ weekly_return_meme, data = crypto_weekly_doge_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1, data = crypto_weekly_doge_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2, data = crypto_weekly_doge_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_doge_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_doge_meme)
)


results_meme <- map_dfr(models_google_crypto_weekly_doge_meme, ~ tidy(.x), .id = "Model")  
results_meme
r_squared_meme <- map_dbl(models_google_crypto_weekly_doge_meme, ~ summary(.x)$r.squared)  # R^2
r_squared_meme
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)


## conventional-coins
crypto_weekly_doge_conventional <- crypto_weekly_doge %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_conventional, 1),
                R_t_2 = lag(weekly_return_conventional, 2),
                R_t_3 = lag(weekly_return_conventional, 3),
                R_t_4 = lag(weekly_return_conventional, 4)
        ) %>%
        na.omit()

crypto_weekly_doge_conventional <- crypto_weekly_doge_conventional %>%
        filter(if_all(everything(), ~ !is.infinite(.)))

models_google_crypto_weekly_doge_conventional <- list(
        lm(Google_t ~ weekly_return_conventional, data = crypto_weekly_doge_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1, data = crypto_weekly_doge_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2, data = crypto_weekly_doge_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_doge_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_doge_conventional)
)


results_conventional <- map_dfr(models_google_crypto_weekly_doge_conventional, ~ tidy(.x), .id = "Model")  
results_conventional
r_squared_conventional <- map_dbl(models_google_crypto_weekly_doge_conventional, ~ summary(.x)$r.squared)  # R^2
r_squared_conventional
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)

## weekly_market_return
crypto_weekly_Cryptocurrency_market <- crypto_weekly_Cryptocurrency %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_market_return, 1),
                R_t_2 = lag(weekly_market_return, 2),
                R_t_3 = lag(weekly_market_return, 3),
                R_t_4 = lag(weekly_market_return, 4)
        ) %>%
        na.omit()

crypto_weekly_Cryptocurrency_market <- crypto_weekly_Cryptocurrency_market %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_Cryptocurrency_market)

models_google_crypto_weekly_Cryptocurrency_market <- list(
        lm(Google_t ~ weekly_market_return, data = crypto_weekly_Cryptocurrency_market),
        lm(Google_t ~ weekly_market_return + R_t_1, data = crypto_weekly_Cryptocurrency_market),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2, data = crypto_weekly_Cryptocurrency_market),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_Cryptocurrency_market),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_Cryptocurrency_market)
)


results_market <- map_dfr(models_google_crypto_weekly_Cryptocurrency_market, ~ tidy(.x), .id = "Model")  
results_market
r_squared_market <- map_dbl(models_google_crypto_weekly_Cryptocurrency_market, ~ summary(.x)$r.squared)  # R^2
r_squared_market

r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)


########################### Google Search - BITCOIN ############################

google_trends_bitcoin <- read.csv("multiTimelineBitcoin.csv", skip = 1, header = TRUE)
str(google_trends_bitcoin)
colnames(google_trends_bitcoin) <- c("Week", "Bitcoin")
google_trends_bitcoin <- google_trends_bitcoin[-1, ]
google_trends_bitcoin$Week <- as.Date(google_trends_bitcoin$Week, format = "%m/%d/%Y")
google_trends_bitcoin$Bitcoin <- as.numeric(google_trends_bitcoin$Bitcoin)
str(google_trends_bitcoin)

ggplot(google_trends_bitcoin, aes(x = Week, y = Bitcoin)) +
        geom_line(color = "blue", size = 1) +
        labs(
                title = "Google Trends: Bitcoin",
                x = "date",
                y = "search interest"
        ) +
        theme_minimal()

str(crypto_weekly)


google_trends_bitcoin <- google_trends_bitcoin %>%
        rename(week = Week, Google_t = Bitcoin) %>%  
        mutate(Google_t = scale(Google_t))  


crypto_weekly_google_bitcoin <- crypto_weekly %>%
        left_join(weekly_market_caps, by = "week") %>%
        left_join(google_trends_bitcoin, by = "week")


crypto_weekly_google_bitcoin <- crypto_weekly_google_bitcoin %>%
        mutate(
                weekly_return_BTC = ifelse(is.infinite(weekly_return_BTC), NA, weekly_return_BTC),
                weekly_market_return = ifelse(is.infinite(weekly_market_return), NA, weekly_market_return)
        )
summary(crypto_weekly_google_bitcoin)



crypto_weekly_google_bitcoin <- crypto_weekly_google_bitcoin %>%
        mutate(
                total_meme_cap = market_cap_DOGE + market_cap_SHIB,
                total_conventional_cap = market_cap_BTC + market_cap_ETH + market_cap_XRP,
                
                weight_DOGE = market_cap_DOGE / total_meme_cap,
                weight_SHIB = market_cap_SHIB / total_meme_cap,
                
                weight_BTC = market_cap_BTC / total_conventional_cap,
                weight_ETH = market_cap_ETH / total_conventional_cap,
                weight_XRP = market_cap_XRP / total_conventional_cap,
                
                weekly_return_meme = (weekly_return_DOGE * weight_DOGE) + (weekly_return_SHIB * weight_SHIB),
                weekly_return_conventional = (weekly_return_BTC * weight_BTC) + 
                        (weekly_return_ETH * weight_ETH) + 
                        (weekly_return_XRP * weight_XRP)
        )

summary(crypto_weekly_google_bitcoin)
crypto_weekly_google_bitcoin <- crypto_weekly_google_bitcoin %>%
        filter(!is.na(weekly_return_meme) & !is.na(weekly_return_conventional))


## MEME-coins
crypto_weekly_bitcoin_meme <- crypto_weekly_google_bitcoin %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_meme, 1),
                R_t_2 = lag(weekly_return_meme, 2),
                R_t_3 = lag(weekly_return_meme, 3),
                R_t_4 = lag(weekly_return_meme, 4)
        ) %>%
        na.omit()

crypto_weekly_bitcoin_meme <- crypto_weekly_bitcoin_meme %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_bitcoin_meme)


models_google_crypto_weekly_bitcoin_meme <- list(
        lm(Google_t ~ weekly_return_meme, data = crypto_weekly_bitcoin_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1, data = crypto_weekly_bitcoin_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2, data = crypto_weekly_bitcoin_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_bitcoin_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_bitcoin_meme)
)


results_meme <- map_dfr(models_google_crypto_weekly_bitcoin_meme, ~ tidy(.x), .id = "Model")  
results_meme
r_squared_meme <- map_dbl(models_google_crypto_weekly_bitcoin_meme, ~ summary(.x)$r.squared)  # R^2
r_squared_meme


## conventional-coins
crypto_weekly_bitcoin_conventional <- crypto_weekly_google_bitcoin %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_conventional, 1),
                R_t_2 = lag(weekly_return_conventional, 2),
                R_t_3 = lag(weekly_return_conventional, 3),
                R_t_4 = lag(weekly_return_conventional, 4)
        ) %>%
        na.omit()

crypto_weekly_bitcoin_conventional <- crypto_weekly_bitcoin_conventional %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_bitcoin_conventional)

models_google_crypto_weekly_bitcoin_conventional <- list(
        lm(Google_t ~ weekly_return_conventional, data = crypto_weekly_bitcoin_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1, data = crypto_weekly_bitcoin_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2, data = crypto_weekly_bitcoin_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_bitcoin_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_bitcoin_conventional)
)


results_conventional <- map_dfr(models_google_crypto_weekly_bitcoin_conventional, ~ tidy(.x), .id = "Model")  
results_conventional
r_squared_conventional <- map_dbl(models_google_crypto_weekly_bitcoin_conventional, ~ summary(.x)$r.squared)  # R^2
r_squared_conventional



## weekly_market_return
crypto_weekly_google_bitcoin <- crypto_weekly_google_bitcoin %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_market_return, 1),
                R_t_2 = lag(weekly_market_return, 2),
                R_t_3 = lag(weekly_market_return, 3),
                R_t_4 = lag(weekly_market_return, 4)
        ) %>%
        na.omit() 

crypto_weekly_google_bitcoin <- crypto_weekly_google_bitcoin %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_google_bitcoin)

models_google_crypto_bitcoin_market <- list(
        lm(Google_t ~ weekly_market_return, data = crypto_weekly_google_bitcoin),
        lm(Google_t ~ weekly_market_return + R_t_1, data = crypto_weekly_google_bitcoin),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2, data = crypto_weekly_google_bitcoin),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_google_bitcoin),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_google_bitcoin)
)


results <- map_dfr(models_google_crypto_bitcoin_market, ~ tidy(.x), .id = "Model")  # Коэффициенты
results
r_squared <- map_dbl(models_google_crypto_bitcoin_market, ~ summary(.x)$r.squared)  # R^2
r_squared



################################################################################

str(crypto_weekly_google_bitcoin)
crypto_subset_2 <- crypto_weekly_google_bitcoin %>%
        dplyr::select(weekly_return_BTC, weekly_return_ETH, weekly_return_XRP, 
                      weekly_return_DOGE, weekly_return_SHIB, weekly_market_return, 
                      weekly_return_conventional, weekly_return_meme)

str(crypto_subset_2)


library(psych)
describe(crypto_subset_2)

################################################################################

library(dplyr)
library(tidyr)
library(e1071)

selected_vars <- c("weekly_market_return", "weekly_return_meme", "weekly_return_conventional",
                   "weekly_return_BTC", "weekly_return_ETH", "weekly_return_XRP", 
                   "weekly_return_DOGE", "weekly_return_SHIB")

summary_stats <- crypto_weekly_Cryptocurrency %>%
        summarise(
                across(all_of(selected_vars), list(
                        Mean = ~ mean(.x, na.rm = TRUE),
                        Median = ~ median(.x, na.rm = TRUE),
                        SD = ~ sd(.x, na.rm = TRUE),
                        Skewness = ~ skewness(.x, na.rm = TRUE),
                        Kurtosis = ~ kurtosis(.x, na.rm = TRUE)
                ))
        )

summary_stats_t <- summary_stats %>%
        pivot_longer(cols = everything(), names_to = "Variable_Statistic", values_to = "Value") %>%
        separate(Variable_Statistic, into = c("Variable", "Statistic"), sep = "_(?=[^_]+$)") %>%
        pivot_wider(names_from = Statistic, values_from = Value)
summary_stats_t


## Hash rate
options(scipen=999)
library(jsonlite)
library(dplyr)
library(lubridate)
library(dplyr)
library(dplyr)
library(lubridate)
library(stats)

data_weekly_market <- data.frame(data_diff)
data_weekly_market$week <- crypto_weekly_Cryptocurrency$week[2:200]
crypto_weekly_Cryptocurrency <- crypto_weekly_Cryptocurrency %>%
        left_join(data_weekly_market %>%
                          dplyr::select(week, S.P500, DowJones, TreasuryYield, GoldFutures, Fear_Greed_Index), 
                  by = "week")
str(crypto_weekly_Cryptocurrency)
crypto_weekly_Cryptocurrency <- na.omit(crypto_weekly_Cryptocurrency)

hash_rate_data <- fromJSON("hash-rate.json")
hash_rate_df <- hash_rate_data$`hash-rate` %>%
        rename(timestamp = x, hash_rate = y) %>%
        mutate(week = as.Date(as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")))  # Исправленное преобразование
summary(hash_rate_df$week)

hash_rate_df <- hash_rate_df %>%
        mutate(week = floor_date(week, unit = "week", week_start = 7)) %>%
        group_by(week) %>%
        summarise(hash_rate = mean(hash_rate, na.rm = TRUE))  

crypto_weekly_Cryptocurrency <- crypto_weekly_Cryptocurrency %>%
        left_join(hash_rate_df, by = "week")

summary(crypto_weekly_Cryptocurrency$hash_rate)
crypto_weekly_Cryptocurrency <- crypto_weekly_Cryptocurrency %>%
        mutate(hash_rate = ifelse(is.na(hash_rate), 0, hash_rate))

str(crypto_weekly_Cryptocurrency)

unique(crypto_weekly_Cryptocurrency$hash_rate)
crypto_weekly_Cryptocurrency$log_hash_rate <- log(crypto_weekly_Cryptocurrency$hash_rate + 1)
unique(crypto_weekly_Cryptocurrency$log_hash_rate)



# weekly_return_meme
model_meme <- lm(weekly_return_meme ~ S.P500 + DowJones + TreasuryYield + GoldFutures + 
                         Fear_Greed_Index + log_hash_rate, 
                 data = crypto_weekly_Cryptocurrency)

# weekly_return_conventional
model_conventional <- lm(weekly_return_conventional ~ S.P500 + DowJones + TreasuryYield + 
                                 GoldFutures + Fear_Greed_Index + log_hash_rate, 
                         data = crypto_weekly_Cryptocurrency)
# weekly_return_conventional
model_market <- lm(weekly_market_return ~ S.P500 + DowJones + TreasuryYield + GoldFutures + 
                           Fear_Greed_Index + log_hash_rate, 
                   data = crypto_weekly_Cryptocurrency)
summary(model_meme)
summary(model_conventional)
summary(model_market)

str(crypto_weekly_Cryptocurrency)

# weekly_return_meme
model_BTC_hr <- lm(weekly_return_BTC ~ hash_rate, 
                   data = crypto_weekly_Cryptocurrency)
summary(model_BTC_hr)

model_ETH_hr <- lm(weekly_return_ETH ~ hash_rate, 
                   data = crypto_weekly_Cryptocurrency)
summary(model_ETH_hr)

model_XRP_hr <- lm(weekly_return_XRP ~ hash_rate, 
                   data = crypto_weekly_Cryptocurrency)
summary(model_XRP_hr)

model_DOGE_hr <- lm(weekly_return_DOGE ~ hash_rate, 
                    data = crypto_weekly_Cryptocurrency)
summary(model_DOGE_hr)

model_SHIB_hr <- lm(weekly_return_SHIB ~ hash_rate, 
                    data = crypto_weekly_Cryptocurrency)
summary(model_SHIB_hr)

str(crypto_weekly_Cryptocurrency)

cor_vars <- crypto_weekly_Cryptocurrency[, c("weekly_return_meme", "weekly_return_conventional", 
                                             "weekly_market_return", "S.P500", "DowJones", 
                                             "TreasuryYield", "GoldFutures", "Fear_Greed_Index")]

cor_matrix <- cor(cor_vars, use = "pairwise.complete.obs")
print(cor_matrix)

n <- nrow(cor_matrix) 
p_values <- outer(1:ncol(cor_matrix), 1:ncol(cor_matrix), Vectorize(function(i, j) {
        if (i == j) return(NA)  
        cor.test(data_diff[, i], data_diff[, j])$p.value
}))
cor_data <- melt(cor_matrix)
p_data <- melt(p_values)

p_data$significance <- cut(
        p_data$value,
        breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
        labels = c("***", "**", "*", ""),
        include.lowest = TRUE
)

cor_data$significance <- p_data$significance

cor_data$label <- ifelse(
        is.na(cor_data$significance),
        round(cor_data$value, 2),  
        paste0(round(cor_data$value, 2), cor_data$significance)  
)

ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +  
        geom_text(
                aes(label = label),  
                color = "black", size = 4
        ) + 
        scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) + 
        labs(
                title = "Heatmap of Correlations",
                x = "Variables",
                y = "Variables",
                fill = "Correlation"
        ) +
        theme_minimal() +
        theme(
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)  
        )


###  Cryptocurrency return loadings to network factors 

library(ggplot2)
library(dplyr)
library(corrplot)
library(prcomp)
library(zoo)
library(tidyverse)
library(ggplot2)
library(psych) 
library(lmtest)

network_factors_BTC <- read.csv("network_factors_Bitcoin.csv")
str(network_factors_BTC)

network_factors_BTC$Date <- as.Date(network_factors_BTC$Date)


library(zoo)
fill_na_with_moving_avg <- function(x, window_size = 7) {
        x <- na.approx(x, rule = 2, na.rm = FALSE) 
        rollmean_filled <- rollapply(x, width = window_size, FUN = mean, na.rm = TRUE, fill = NA, align = "center")
        x[is.na(x)] <- rollmean_filled[is.na(x)]
        x <- na.locf(x, na.rm = FALSE) 
        x <- na.locf(x, fromLast = TRUE) 
        
        return(x)
}

network_factors_BTC$UniqueAddresses <- fill_na_with_moving_avg(network_factors_BTC$UniqueAddresses)
network_factors_BTC$Transactions <- fill_na_with_moving_avg(network_factors_BTC$Transactions)
network_factors_BTC$Payments <- fill_na_with_moving_avg(network_factors_BTC$Payments)
network_factors_BTC$TransactionVolumeUSD <- fill_na_with_moving_avg(network_factors_BTC$TransactionVolumeUSD)

sum(is.na(network_factors_BTC)) 
str(network_factors_BTC)
head(network_factors_BTC)

network_factors_BTC <- network_factors_BTC %>%
        mutate(
                Delta_Users = c(NA, diff(UniqueAddresses)),
                Delta_Transactions = c(NA, diff(Transactions)),
                Delta_Payments = c(NA, diff(Payments)),
                Delta_TransactionVolume = c(NA, diff(TransactionVolumeUSD))
        )

network_factors_BTC <- na.omit(network_factors_BTC)


pca_model <- principal(network_factors_BTC[, c("Delta_Users", "Delta_Transactions", "Delta_Payments", "Delta_TransactionVolume")],
                       nfactors = 1, rotate = "none", scores = TRUE)
network_factors_BTC$PC_network <- pca_model$scores[, 1]


cor_matrix <- cor(network_factors_BTC[, c("Delta_Users", "Delta_Transactions", "Delta_Payments", "Delta_TransactionVolume", "PC_network")])
print("Cor matrix:")
print(cor_matrix)
norm_cor_matrix <- format(cor_matrix, scientific = FALSE, digits = 4)  
print(norm_cor_matrix)


str(network_factors_BTC)


library(dplyr)

network_factors_weekly <- network_factors_BTC %>%
        mutate(week = as.Date(cut(Date, breaks = "week", start.on.monday = TRUE))) %>%
        group_by(week) %>%
        summarize(
                UniqueAddresses = last(UniqueAddresses),
                Transactions = sum(Delta_Transactions, na.rm = TRUE),
                Payments = sum(Delta_Payments, na.rm = TRUE),
                TransactionVolumeUSD = sum(Delta_TransactionVolume, na.rm = TRUE),
                PC_network = mean(PC_network, na.rm = TRUE)
        ) %>%
        ungroup()
print(head(network_factors_weekly))

crypto_weekly_filtered$week <- as.Date(cut(crypto_weekly_filtered$week, breaks = "week", start.on.monday = TRUE))
network_factors_weekly$week <- as.Date(cut(network_factors_weekly$week, breaks = "week", start.on.monday = TRUE))

full_data_networks_crypto <- merge(crypto_weekly_filtered, network_factors_weekly, by = "week")
print(head(full_data_networks_crypto))

start_date <- as.Date("2021-01-01")
end_date <- as.Date("2024-12-31")

crypto_weekly_Cryptocurrency2 <- crypto_weekly_Cryptocurrency %>%
        filter(week >= start_date & week <= end_date)

full_data_networks_crypto <- full_data_networks_crypto %>%
        filter(week >= start_date & week <= end_date)

crypto_weekly_Cryptocurrency2 <- crypto_weekly_Cryptocurrency2 %>%
        mutate(week = week + 1)  

full_data_networks_crypto <- full_data_networks_crypto %>%
        left_join(
                crypto_weekly_Cryptocurrency2 %>% dplyr::select(week, weekly_return_meme, weekly_return_conventional),
                by = "week"
        )

full_data_networks_crypto <- full_data_networks_crypto %>%
        mutate(
                weekly_return_meme = ifelse(is.na(weekly_return_meme), median(weekly_return_meme, na.rm = TRUE), weekly_return_meme),
                weekly_return_conventional = ifelse(is.na(weekly_return_conventional), median(weekly_return_conventional, na.rm = TRUE), weekly_return_conventional)
        )

full_data_networks_crypto <- full_data_networks_crypto %>%
        mutate(
                Delta_Users = c(NA, diff(UniqueAddresses)),
                Delta_Transactions = c(NA, diff(Transactions)),
                Delta_Payments = c(NA, diff(Payments)),
                Delta_TransactionVolume = c(NA, diff(TransactionVolumeUSD))
        )

full_data_networks_crypto <- na.omit(full_data_networks_crypto)

str(weekly_return_market)

full_data_networks_crypto <- full_data_networks_crypto[is.finite(full_data_networks_crypto$weekly_return_BTC) & is.finite(full_data_networks_crypto$weekly_market_return), ]
str(full_data_networks_crypto)

factors <- c("UniqueAddresses","Transactions", "Payments", "TransactionVolumeUSD", "PC_network")

reg_models <- list()

for (factor in factors) {
        formula <- as.formula(paste("weekly_return_meme ~", factor))
        model <- lm(formula, data = full_data_networks_crypto)
        reg_models[[factor]] <- summary(model)
}

for (factor in factors) {
        cat("\nRegression results for:", factor, "\n")
        print(reg_models[[factor]])
}


for (factor in factors) {
        formula <- as.formula(paste("weekly_return_meme ~", factor))
        model <- lm(formula, data = full_data_networks_crypto)
        reg_models[[factor]] <- summary(model)
}

for (factor in factors) {
        cat("\nRegression results for:", factor, "\n")
        print(reg_models[[factor]])
}


for (factor in factors) {
        formula <- as.formula(paste("weekly_return_conventional ~", factor))
        model <- lm(formula, data = full_data_networks_crypto)
        reg_models[[factor]] <- summary(model)
}

for (factor in factors) {
        cat("\nRegression results for:", factor, "\n")
        print(reg_models[[factor]])
}


full_data_networks_crypto <- full_data_networks_crypto %>%
        mutate(
                Transactions_lag1 = lag(Transactions, 1),
                Payments_lag1 = lag(Payments, 1),
                TransactionVolumeUSD_lag1 = lag(TransactionVolumeUSD, 1),
                PC_network_lag1 = lag(PC_network, 1)
        )


library(lmtest)
grangertest(weekly_return_meme ~ PC_network, order = 2, data = full_data_networks_crypto)


str(full_data_networks_crypto)

full_data_networks_crypto <- full_data_networks_crypto %>%
        mutate(
                log_UniqueAddresses = log(UniqueAddresses + 1),
                log_Transactions = log(abs(Transactions) + 1),  # Берем модуль
                log_Payments = log(abs(Payments) + 1),
                log_TransactionVolumeUSD = log(abs(TransactionVolumeUSD) + 1)
        )

full_data_networks_crypto <- full_data_networks_crypto %>%
        mutate(
                UniqueAddresses_lag1 = lag(UniqueAddresses, 1),
                Transactions_lag1 = lag(Transactions, 1),
                Payments_lag1 = lag(Payments, 1),
                TransactionVolumeUSD_lag1 = lag(TransactionVolumeUSD, 1)
        )

formula <- as.formula("weekly_return_meme ~ log_TransactionVolumeUSD")
model <- lm(formula, data = full_data_networks_crypto)
summary(model)




formula <- as.formula("weekly_return_conventional ~ UniqueAddresses + Transactions + 
                      Payments + TransactionVolumeUSD")
model <- lm(formula, data = full_data_networks_crypto)
summary(model)

library(ggplot2)

summary(full_data_networks_crypto$weekly_return_conventional)

c <- abs(min(full_data_networks_crypto$weekly_return_conventional, na.rm = TRUE)) + 0.001
full_data_networks_crypto <- full_data_networks_crypto %>%
        mutate(weekly_return_conventional_shifted = weekly_return_conventional + c)

exp_model <- glm(weekly_return_conventional_shifted ~ log_UniqueAddresses + log_Transactions +
                         log_Payments + log_TransactionVolumeUSD,
                 family = gaussian(link = "log"), data = full_data_networks_crypto)
summary(exp_model)


gamma_model <- glm(weekly_return_conventional_shifted ~ log_UniqueAddresses + log_Transactions +
                           log_Payments + log_TransactionVolumeUSD, 
                   family = Gamma(link = "log"), data = full_data_networks_crypto)
summary(gamma_model)


ggplot(full_data_networks_crypto, aes(x = log_UniqueAddresses, y = weekly_return_meme)) +
        geom_point() + geom_smooth(method = "lm", se = FALSE) + ggtitle("relationship ?")

ggplot(full_data_networks_crypto, aes(x = log_Transactions, y = weekly_return_meme)) +
        geom_point() + geom_smooth(method = "lm", se = FALSE) + ggtitle("relationship ?")

c_m <- abs(min(full_data_networks_crypto$weekly_return_meme, na.rm = TRUE)) + 0.001
full_data_networks_crypto <- full_data_networks_crypto %>%
        mutate(weekly_return_meme_shifted = weekly_return_meme + c_m)

exp_model <- glm(weekly_return_meme_shifted ~ log_UniqueAddresses + log_Transactions +
                         log_Payments + log_TransactionVolumeUSD,
                 family = gaussian(link = "log"), data = full_data_networks_crypto)
summary(exp_model)


gamma_model <- glm(weekly_return_meme_shifted ~ log_UniqueAddresses + log_Transactions +
                           log_Payments + log_TransactionVolumeUSD, 
                   family = Gamma(link = "log"), data = full_data_networks_crypto)
summary(gamma_model)













