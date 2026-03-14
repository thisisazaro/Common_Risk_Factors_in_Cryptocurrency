




########################### Google Search - Cryptocurrency #####################

google_trends_Cryptocurrency <- read.csv("multiTimelineCryptocurrency.csv", skip = 1, header = TRUE)
str(google_trends_Cryptocurrency)
colnames(google_trends_Cryptocurrency) <- c("Week", "Cryptocurrency")
google_trends_Cryptocurrency$Week <- as.Date(google_trends_Cryptocurrency$Week)
google_trends_Cryptocurrency$Cryptocurrency <- as.numeric(google_trends_Cryptocurrency$Cryptocurrency)

str(google_trends_Cryptocurrency)

library(ggplot2)
ggplot(google_trends_Cryptocurrency, aes(x = Week, y = Cryptocurrency)) +
        geom_line(color = "blue", size = 1) +
        labs(
                title = "Google Trends: Cryptocurrency",
                x = "date",
                y = "search interest"
        ) +
        theme_minimal()

str(crypto_weekly)

library(dplyr)
library(broom)
library(purrr)

google_trends_Cryptocurrency <- google_trends_Cryptocurrency %>%
        rename(week = Week, Google_t = Cryptocurrency) %>%  
        mutate(Google_t = scale(Google_t))  
str(google_trends_Cryptocurrency)


# Объединяем данные о доходности и капитализации
crypto_weekly_Cryptocurrency <- crypto_weekly %>%
        left_join(weekly_market_caps, by = "week") %>%
        left_join(google_trends_Cryptocurrency, by = "week")

# Проверяем результат
summary(crypto_weekly_Cryptocurrency)


crypto_weekly_Cryptocurrency <- crypto_weekly_Cryptocurrency %>%
        mutate(
                weekly_return_SHIB = ifelse(is.infinite(weekly_return_SHIB), NA, weekly_return_SHIB),
                weekly_market_return = ifelse(is.infinite(weekly_market_return), NA, weekly_market_return)
        )

# Проверяем результат
summary(crypto_weekly_Cryptocurrency)



crypto_weekly_Cryptocurrency <- crypto_weekly_Cryptocurrency %>%
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

# Проверяем финальную версию
summary(crypto_weekly_Cryptocurrency)
crypto_weekly_Cryptocurrency <- crypto_weekly_Cryptocurrency %>%
        filter(!is.na(weekly_return_meme) & !is.na(weekly_return_conventional))

str(crypto_weekly_Cryptocurrency)

# Добавляем лаги для доходностей
crypto_weekly_Cryptocurrency_meme <- crypto_weekly_Cryptocurrency %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_meme, 1),
                R_t_2 = lag(weekly_return_meme, 2),
                R_t_3 = lag(weekly_return_meme, 3),
                R_t_4 = lag(weekly_return_meme, 4)
        ) %>%
        na.omit()

crypto_weekly_Cryptocurrency_meme <- crypto_weekly_Cryptocurrency_meme %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_Cryptocurrency_meme)

## Модель для MEME-coins
models_google_crypto_weekly_Cryptocurrency_meme <- list(
        lm(Google_t ~ weekly_return_meme, data = crypto_weekly_Cryptocurrency_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1, data = crypto_weekly_Cryptocurrency_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2, data = crypto_weekly_Cryptocurrency_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_Cryptocurrency_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_Cryptocurrency_meme)
)


# Извлекаем результаты
results_meme <- map_dfr(models_google_crypto_weekly_Cryptocurrency_meme, ~ tidy(.x), .id = "Model")  # Коэффициенты
results_meme
r_squared_meme <- map_dbl(models_google_crypto_weekly_Cryptocurrency_meme, ~ summary(.x)$r.squared)  # R^2
r_squared_meme
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)

## Модель для conventional-coins

# Добавляем лаги для доходностей
crypto_weekly_Cryptocurrency_conventional <- crypto_weekly_Cryptocurrency %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_conventional, 1),
                R_t_2 = lag(weekly_return_conventional, 2),
                R_t_3 = lag(weekly_return_conventional, 3),
                R_t_4 = lag(weekly_return_conventional, 4)
        ) %>%
        na.omit()

crypto_weekly_Cryptocurrency_conventional <- crypto_weekly_Cryptocurrency_conventional %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_Cryptocurrency_conventional)

models_google_crypto_weekly_Cryptocurrency_conventional <- list(
        lm(Google_t ~ weekly_return_conventional, data = crypto_weekly_Cryptocurrency_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1, data = crypto_weekly_Cryptocurrency_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2, data = crypto_weekly_Cryptocurrency_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_Cryptocurrency_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_Cryptocurrency_conventional)
)


# Извлекаем результаты
results_conventional <- map_dfr(models_google_crypto_weekly_Cryptocurrency_conventional, ~ tidy(.x), .id = "Model")  
results_conventional
r_squared_conventional <- map_dbl(models_google_crypto_weekly_Cryptocurrency_conventional, ~ summary(.x)$r.squared)  # R^2
r_squared_conventional
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)

## Модель для weekly_market_return
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


results_conventional <- map_dfr(models_google_crypto_weekly_Cryptocurrency_market, ~ tidy(.x), .id = "Model")  
results_conventional
r_squared_conventional <- map_dbl(models_google_crypto_weekly_Cryptocurrency_market, ~ summary(.x)$r.squared)  # R^2
r_squared_conventional
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)

################################################################################

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


# Объединяем данные о доходности и капитализации
crypto_weekly_doge <- crypto_weekly %>%
        left_join(weekly_market_caps, by = "week") %>%
        left_join(google_trends_doge, by = "week")

# Проверяем результат
summary(crypto_weekly_doge)


crypto_weekly_doge <- crypto_weekly_doge %>%
        mutate(
                weekly_return_SHIB = ifelse(is.infinite(weekly_return_SHIB), NA, weekly_return_SHIB),
                weekly_market_return = ifelse(is.infinite(weekly_market_return), NA, weekly_market_return)
        )

# Проверяем результат
summary(crypto_weekly_doge)



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

# Проверяем финальную версию
summary(crypto_weekly_doge)
crypto_weekly_doge <- crypto_weekly_doge %>%
        filter(!is.na(weekly_return_meme) & !is.na(weekly_return_conventional))


## Модель для MEME-coins
# Добавляем лаги для доходностей
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


# Извлекаем результаты
results_meme <- map_dfr(models_google_crypto_weekly_doge_meme, ~ tidy(.x), .id = "Model")  
results_meme
r_squared_meme <- map_dbl(models_google_crypto_weekly_doge_meme, ~ summary(.x)$r.squared)  # R^2
r_squared_meme
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)


## Модель для conventional-coins

# Добавляем лаги для доходностей
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
summary(crypto_weekly_doge_conventional)

models_google_crypto_weekly_doge_conventional <- list(
        lm(Google_t ~ weekly_return_conventional, data = crypto_weekly_doge_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1, data = crypto_weekly_doge_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2, data = crypto_weekly_doge_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_doge_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_doge_conventional)
)


# Извлекаем результаты
results_conventional <- map_dfr(models_google_crypto_weekly_doge_conventional, ~ tidy(.x), .id = "Model")  
results_conventional
r_squared_conventional <- map_dbl(models_google_crypto_weekly_doge_conventional, ~ summary(.x)$r.squared)  # R^2
r_squared_conventional
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)

## Модель для weekly_market_return
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

################################################################################

########################### Google Search - BitcoinBan ##########################

google_trends_BitcoinBan <- read.csv("multiTimelineBitcoinBan.csv", skip = 1, header = TRUE)
str(google_trends_BitcoinBan)
colnames(google_trends_BitcoinBan) <- c("Week", "BitcoinBan")
google_trends_BitcoinBan$Week <- as.Date(google_trends_BitcoinBan$Week)
google_trends_BitcoinBan$BitcoinBan <- as.numeric(google_trends_BitcoinBan$BitcoinBan)
str(google_trends_BitcoinBan)

library(ggplot2)
ggplot(google_trends_BitcoinBan, aes(x = Week, y = BitcoinBan)) +
        geom_line(color = "blue", linewidth = 1) +
        labs(
                title = "Google Trends: BitcoinBan",
                x = "date",
                y = "search interest"
        ) +
        theme_minimal()


library(dplyr)
library(broom)
library(purrr)

google_trends_BitcoinBan <- google_trends_BitcoinBan %>%
        rename(week = Week, Google_t = BitcoinBan) %>%  
        mutate(Google_t = scale(Google_t))  
str(google_trends_BitcoinBan)

crypto_weekly_BitcoinBan <- crypto_weekly %>%
        left_join(weekly_market_caps, by = "week") %>%
        left_join(google_trends_BitcoinBan, by = "week")
summary(crypto_weekly_BitcoinBan)


crypto_weekly_BitcoinBan <- crypto_weekly_BitcoinBan %>%
        mutate(
                weekly_return_SHIB = ifelse(is.infinite(weekly_return_SHIB), NA, weekly_return_SHIB),
                weekly_market_return = ifelse(is.infinite(weekly_market_return), NA, weekly_market_return)
        )
summary(crypto_weekly_BitcoinBan)

crypto_weekly_BitcoinBan <- crypto_weekly_BitcoinBan %>%
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
summary(crypto_weekly_BitcoinBan)
crypto_weekly_BitcoinBan <- crypto_weekly_BitcoinBan %>%
        filter(!is.na(weekly_return_meme) & !is.na(weekly_return_conventional))


## Модель для MEME-coins
# Добавляем лаги для доходностей
crypto_weekly_BitcoinBan_meme <- crypto_weekly_BitcoinBan %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_meme, 1),
                R_t_2 = lag(weekly_return_meme, 2),
                R_t_3 = lag(weekly_return_meme, 3),
                R_t_4 = lag(weekly_return_meme, 4)
        ) %>%
        na.omit()

crypto_weekly_BitcoinBan_meme <- crypto_weekly_BitcoinBan_meme %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_BitcoinBan_meme)


models_google_crypto_weekly_BitcoinBan_meme <- list(
        lm(Google_t ~ weekly_return_meme, data = crypto_weekly_BitcoinBan_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1, data = crypto_weekly_BitcoinBan_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2, data = crypto_weekly_BitcoinBan_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_BitcoinBan_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_BitcoinBan_meme)
)


results_meme <- map_dfr(models_google_crypto_weekly_BitcoinBan_meme, ~ tidy(.x), .id = "Model")  
results_meme
r_squared_meme <- map_dbl(models_google_crypto_weekly_BitcoinBan_meme, ~ summary(.x)$r.squared)  # R^2
r_squared_meme
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)



## Модель для conventional-coins

# Добавляем лаги для доходностей
crypto_weekly_BitcoinBan_conventional <- crypto_weekly_BitcoinBan %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_conventional, 1),
                R_t_2 = lag(weekly_return_conventional, 2),
                R_t_3 = lag(weekly_return_conventional, 3),
                R_t_4 = lag(weekly_return_conventional, 4)
        ) %>%
        na.omit()

crypto_weekly_BitcoinBan_conventional <- crypto_weekly_BitcoinBan_conventional %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_BitcoinBan_conventional)

models_google_crypto_weekly_BitcoinBan_conventional <- list(
        lm(Google_t ~ weekly_return_conventional, data = crypto_weekly_BitcoinBan_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1, data = crypto_weekly_BitcoinBan_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2, data = crypto_weekly_BitcoinBan_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_BitcoinBan_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_BitcoinBan_conventional)
)


# Извлекаем результаты
results_conventional <- map_dfr(models_google_crypto_weekly_BitcoinBan_conventional, ~ tidy(.x), .id = "Model")  
results_conventional
r_squared_conventional <- map_dbl(models_google_crypto_weekly_BitcoinBan_conventional, ~ summary(.x)$r.squared)  # R^2
r_squared_conventional
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)

## Модель для weekly_market_return
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

################################################################################
############################### Google Search - Hack ###########################

google_trends_Hack <- read.csv("multiTimelineHack.csv", skip = 1, header = TRUE)
str(google_trends_Hack)
colnames(google_trends_Hack) <- c("Week", "Hack")
google_trends_Hack$Week <- as.Date(google_trends_Hack$Week)
google_trends_Hack$Hack <- as.numeric(google_trends_Hack$Hack)

str(google_trends_Hack)

library(ggplot2)
ggplot(google_trends_Hack, aes(x = Week, y = Hack)) +
        geom_line(color = "blue", size = 1) +
        labs(
                title = "Google Trends: Hack",
                x = "date",
                y = "search interest"
        ) +
        theme_minimal()

google_trends_Hack <- google_trends_Hack %>%
        rename(week = Week, Google_t = Hack) %>%  
        mutate(Google_t = scale(Google_t))  
str(google_trends_Hack)

crypto_weekly_Hack <- crypto_weekly %>%
        left_join(weekly_market_caps, by = "week") %>%
        left_join(google_trends_Hack, by = "week")
summary(crypto_weekly_Hack)

crypto_weekly_Hack <- crypto_weekly_Hack %>%
        mutate(
                weekly_return_SHIB = ifelse(is.infinite(weekly_return_SHIB), NA, weekly_return_SHIB),
                weekly_market_return = ifelse(is.infinite(weekly_market_return), NA, weekly_market_return)
        )
summary(crypto_weekly_Hack)

crypto_weekly_Hack <- crypto_weekly_Hack %>%
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
summary(crypto_weekly_Hack)
crypto_weekly_Hack <- crypto_weekly_Hack %>%
        filter(!is.na(weekly_return_meme) & !is.na(weekly_return_conventional))


## Модель для MEME-coins
# Добавляем лаги для доходностей
crypto_weekly_Hack_meme <- crypto_weekly_Hack %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_meme, 1),
                R_t_2 = lag(weekly_return_meme, 2),
                R_t_3 = lag(weekly_return_meme, 3),
                R_t_4 = lag(weekly_return_meme, 4)
        ) %>%
        na.omit()

crypto_weekly_Hack_meme <- crypto_weekly_Hack_meme %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_Hack_meme)


models_google_crypto_weekly_Hack_meme <- list(
        lm(Google_t ~ weekly_return_meme, data = crypto_weekly_Hack_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1, data = crypto_weekly_Hack_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2, data = crypto_weekly_Hack_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_Hack_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_Hack_meme)
)


results_meme <- map_dfr(models_google_crypto_weekly_Hack_meme, ~ tidy(.x), .id = "Model")  
results_meme
r_squared_meme <- map_dbl(models_google_crypto_weekly_Hack_meme, ~ summary(.x)$r.squared)  # R^2
r_squared_meme
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)



## Модель для conventional-coins

# Добавляем лаги для доходностей
crypto_weekly_Hack_conventional <- crypto_weekly_Hack %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_conventional, 1),
                R_t_2 = lag(weekly_return_conventional, 2),
                R_t_3 = lag(weekly_return_conventional, 3),
                R_t_4 = lag(weekly_return_conventional, 4)
        ) %>%
        na.omit()

crypto_weekly_Hack_conventional <- crypto_weekly_Hack_conventional %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_Hack_conventional)

models_google_crypto_weekly_Hack_conventional <- list(
        lm(Google_t ~ weekly_return_conventional, data = crypto_weekly_Hack_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1, data = crypto_weekly_Hack_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2, data = crypto_weekly_Hack_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_Hack_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_Hack_conventional)
)


# Извлекаем результаты
results_conventional <- map_dfr(models_google_crypto_weekly_Hack_conventional, ~ tidy(.x), .id = "Model")  
results_conventional
r_squared_conventional <- map_dbl(models_google_crypto_weekly_Hack_conventional, ~ summary(.x)$r.squared)  # R^2
r_squared_conventional
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)

## Модель для weekly_market_return
crypto_weekly_Hack_market <- crypto_weekly_Hack %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_market_return, 1),
                R_t_2 = lag(weekly_market_return, 2),
                R_t_3 = lag(weekly_market_return, 3),
                R_t_4 = lag(weekly_market_return, 4)
        ) %>%
        na.omit()

crypto_weekly_Hack_market <- crypto_weekly_Hack_market %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_Hack_market)


models_google_crypto_weekly_Hack_market <- list(
        lm(Google_t ~ weekly_market_return, data = crypto_weekly_Hack_market),
        lm(Google_t ~ weekly_market_return + R_t_1, data = crypto_weekly_Hack_market),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2, data = crypto_weekly_Hack_market),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_Hack_market),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_Hack_market)
)


results_market <- map_dfr(models_google_crypto_weekly_Hack_market, ~ tidy(.x), .id = "Model")  
results_market
r_squared_market <- map_dbl(models_google_crypto_weekly_Hack_market, ~ summary(.x)$r.squared)  # R^2
r_squared_market



################################################################################
########################### Google Search - Bitget #############################

google_trends_Bitget <- read.csv("multiTimelineBitget.csv", skip = 1, header = TRUE)
str(google_trends_Bitget)
unique(google_trends_Bitget$Bitget)
colnames(google_trends_Bitget) <- c("Week", "Bitget")
google_trends_Bitget$Week <- as.Date(google_trends_Bitget$Week)
google_trends_Bitget$Bitget <- as.numeric(gsub("<", "", google_trends_Bitget$Bitget))
str(google_trends_Bitget)

library(ggplot2)
ggplot(google_trends_Bitget, aes(x = Week, y = Bitget)) +
        geom_line(color = "blue", linewidth = 1) +
        labs(
                title = "Google Trends: Bitget",
                x = "date",
                y = "search interest"
        ) +
        theme_minimal()


library(dplyr)
library(broom)
library(purrr)

google_trends_Bitget <- google_trends_Bitget %>%
        rename(week = Week, Google_t = Bitget) %>%  
        mutate(Google_t = scale(Google_t))  
str(google_trends_Bitget)

crypto_weekly_Bitget <- crypto_weekly %>%
        left_join(weekly_market_caps, by = "week") %>%
        left_join(google_trends_Bitget, by = "week")

# Проверяем результат
summary(crypto_weekly_Bitget)


crypto_weekly_Bitget <- crypto_weekly_Bitget %>%
        mutate(
                weekly_return_SHIB = ifelse(is.infinite(weekly_return_SHIB), NA, weekly_return_SHIB),
                weekly_market_return = ifelse(is.infinite(weekly_market_return), NA, weekly_market_return)
        )

# Проверяем результат
summary(crypto_weekly_Bitget)



crypto_weekly_Bitget <- crypto_weekly_Bitget %>%
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

# Проверяем финальную версию
summary(crypto_weekly_Bitget)
crypto_weekly_Bitget <- crypto_weekly_Bitget %>%
        filter(!is.na(weekly_return_meme) & !is.na(weekly_return_conventional))


## Модель для MEME-coins
# Добавляем лаги для доходностей
crypto_weekly_Bitget_meme <- crypto_weekly_Bitget %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_meme, 1),
                R_t_2 = lag(weekly_return_meme, 2),
                R_t_3 = lag(weekly_return_meme, 3),
                R_t_4 = lag(weekly_return_meme, 4)
        ) %>%
        na.omit()

crypto_weekly_Bitget_meme <- crypto_weekly_Bitget_meme %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_Bitget_meme)


models_google_crypto_weekly_Bitget_meme <- list(
        lm(Google_t ~ weekly_return_meme, data = crypto_weekly_Bitget_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1, data = crypto_weekly_Bitget_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2, data = crypto_weekly_Bitget_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_Bitget_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_Bitget_meme)
)


# Извлекаем результаты
results_meme <- map_dfr(models_google_crypto_weekly_Bitget_meme, ~ tidy(.x), .id = "Model")  
results_meme
r_squared_meme <- map_dbl(models_google_crypto_weekly_Bitget_meme, ~ summary(.x)$r.squared)  # R^2
r_squared_meme
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)

## Модель для conventional-coins

# Добавляем лаги для доходностей
crypto_weekly_Bitget_conventional <- crypto_weekly_Bitget %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_conventional, 1),
                R_t_2 = lag(weekly_return_conventional, 2),
                R_t_3 = lag(weekly_return_conventional, 3),
                R_t_4 = lag(weekly_return_conventional, 4)
        ) %>%
        na.omit()

crypto_weekly_Bitget_conventional <- crypto_weekly_Bitget_conventional %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_Bitget_conventional)

models_google_crypto_weekly_Bitget_conventional <- list(
        lm(Google_t ~ weekly_return_conventional, data = crypto_weekly_Bitget_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1, data = crypto_weekly_Bitget_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2, data = crypto_weekly_Bitget_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_Bitget_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_Bitget_conventional)
)


# Извлекаем результаты
results_conventional <- map_dfr(models_google_crypto_weekly_Bitget_conventional, ~ tidy(.x), .id = "Model")  
results_conventional
r_squared_conventional <- map_dbl(models_google_crypto_weekly_Bitget_conventional, ~ summary(.x)$r.squared)  # R^2
r_squared_conventional
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)


################################################################################

########################### Google Search - Cryptoban ##########################

google_trends_cryptoban <- read.csv("multiTimelinecryptoban.csv", skip = 1, header = TRUE)
str(google_trends_cryptoban)
colnames(google_trends_cryptoban) <- c("Week", "Cryptoban")
google_trends_cryptoban$Week <- as.Date(google_trends_cryptoban$Week)
google_trends_cryptoban$Cryptoban <- as.numeric(google_trends_cryptoban$Cryptoban)
str(google_trends_cryptoban)

library(ggplot2)
ggplot(google_trends_cryptoban, aes(x = Week, y = Cryptoban)) +
        geom_line(color = "blue", linewidth = 1) +
        labs(
                title = "Google Trends: Cryptoban",
                x = "date",
                y = "search interest"
        ) +
        theme_minimal()


library(dplyr)
library(broom)
library(purrr)

google_trends_cryptoban <- google_trends_cryptoban %>%
        rename(week = Week, Google_t = Cryptoban) %>%  
        mutate(Google_t = scale(Google_t))  
str(google_trends_cryptoban)


# Объединяем данные о доходности и капитализации
crypto_weekly_Cryptoban <- crypto_weekly %>%
        left_join(weekly_market_caps, by = "week") %>%
        left_join(google_trends_cryptoban, by = "week")
summary(crypto_weekly_Cryptoban)


crypto_weekly_Cryptoban <- crypto_weekly_Cryptoban %>%
        mutate(
                weekly_return_SHIB = ifelse(is.infinite(weekly_return_SHIB), NA, weekly_return_SHIB),
                weekly_market_return = ifelse(is.infinite(weekly_market_return), NA, weekly_market_return)
        )
summary(crypto_weekly_Cryptoban)



crypto_weekly_Cryptoban <- crypto_weekly_Cryptoban %>%
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

summary(crypto_weekly_Cryptoban)
crypto_weekly_Cryptoban <- crypto_weekly_Cryptoban %>%
        filter(!is.na(weekly_return_meme) & !is.na(weekly_return_conventional))


## Модель для MEME-coins
# Добавляем лаги для доходностей
crypto_weekly_Cryptoban_meme <- crypto_weekly_Cryptoban %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_meme, 1),
                R_t_2 = lag(weekly_return_meme, 2),
                R_t_3 = lag(weekly_return_meme, 3),
                R_t_4 = lag(weekly_return_meme, 4)
        ) %>%
        na.omit()

crypto_weekly_Cryptoban_meme <- crypto_weekly_Cryptoban_meme %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_Cryptoban_meme)


models_google_crypto_weekly_Cryptoban_meme <- list(
        lm(Google_t ~ weekly_return_meme, data = crypto_weekly_Cryptoban_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1, data = crypto_weekly_Cryptoban_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2, data = crypto_weekly_Cryptoban_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_Cryptoban_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_Cryptoban_meme)
)


# Извлекаем результаты
results_meme <- map_dfr(models_google_crypto_weekly_Cryptoban_meme, ~ tidy(.x), .id = "Model")  
results_meme
r_squared_meme <- map_dbl(models_google_crypto_weekly_Cryptoban_meme, ~ summary(.x)$r.squared)  # R^2
r_squared_meme
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)


## Модель для conventional-coins

# Добавляем лаги для доходностей
crypto_weekly_Cryptoban_conventional <- crypto_weekly_Cryptoban %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_conventional, 1),
                R_t_2 = lag(weekly_return_conventional, 2),
                R_t_3 = lag(weekly_return_conventional, 3),
                R_t_4 = lag(weekly_return_conventional, 4)
        ) %>%
        na.omit()

crypto_weekly_Cryptoban_conventional <- crypto_weekly_Cryptoban_conventional %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_Cryptoban_conventional)

models_google_crypto_weekly_Cryptoban_conventional <- list(
        lm(Google_t ~ weekly_return_conventional, data = crypto_weekly_Cryptoban_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1, data = crypto_weekly_Cryptoban_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2, data = crypto_weekly_Cryptoban_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_Cryptoban_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_Cryptoban_conventional)
)


# Извлекаем результаты
results_conventional <- map_dfr(models_google_crypto_weekly_Cryptoban_conventional, ~ tidy(.x), .id = "Model")  
results_conventional
r_squared_conventional <- map_dbl(models_google_crypto_weekly_Cryptoban_conventional, ~ summary(.x)$r.squared)  # R^2
r_squared_conventional
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)

################################################################################
########################### Google Search - SHIB ###############################
library(readxl)
google_trends_SHIB <- read_excel("multiTimeline_shib.xlsx")
str(google_trends_SHIB)
colnames(google_trends_SHIB) <- c("Week", "SHIB")
google_trends_SHIB$Week <- as.Date(google_trends_SHIB$Week)
google_trends_SHIB$SHIB <- as.numeric(google_trends_SHIB$SHIB)
str(google_trends_SHIB)

library(ggplot2)
ggplot(google_trends_SHIB, aes(x = Week, y = SHIB)) +
        geom_line(color = "blue", linewidth = 1) +
        labs(
                title = "Google Trends: SHIB",
                x = "date",
                y = "search interest"
        ) +
        theme_minimal()


library(dplyr)
library(broom)
library(purrr)

google_trends_SHIB <- google_trends_SHIB %>%
        rename(week = Week, Google_t = SHIB) %>%  
        mutate(Google_t = scale(Google_t))  
str(google_trends_SHIB)


# Объединяем данные о доходности и капитализации
crypto_weekly_SHIB <- crypto_weekly %>%
        left_join(weekly_market_caps, by = "week") %>%
        left_join(google_trends_SHIB, by = "week")
summary(crypto_weekly_SHIB)


crypto_weekly_SHIB <- crypto_weekly_SHIB %>%
        mutate(
                weekly_return_SHIB = ifelse(is.infinite(weekly_return_SHIB), NA, weekly_return_SHIB),
                weekly_market_return = ifelse(is.infinite(weekly_market_return), NA, weekly_market_return)
        )
summary(crypto_weekly_SHIB)



crypto_weekly_SHIB <- crypto_weekly_SHIB %>%
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

summary(crypto_weekly_SHIB)
crypto_weekly_SHIB <- crypto_weekly_SHIB %>%
        filter(!is.na(weekly_return_meme) & !is.na(weekly_return_conventional))


## Модель для MEME-coins
# Добавляем лаги для доходностей
crypto_weekly_SHIB_meme <- crypto_weekly_SHIB %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_meme, 1),
                R_t_2 = lag(weekly_return_meme, 2),
                R_t_3 = lag(weekly_return_meme, 3),
                R_t_4 = lag(weekly_return_meme, 4)
        ) %>%
        na.omit()

crypto_weekly_SHIB_meme <- crypto_weekly_SHIB_meme %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_SHIB_meme)


models_google_crypto_weekly_SHIB_meme <- list(
        lm(Google_t ~ weekly_return_meme, data = crypto_weekly_SHIB_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1, data = crypto_weekly_SHIB_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2, data = crypto_weekly_SHIB_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_SHIB_meme),
        lm(Google_t ~ weekly_return_meme + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_SHIB_meme)
)


# Извлекаем результаты
results_meme <- map_dfr(models_google_crypto_weekly_SHIB_meme, ~ tidy(.x), .id = "Model")  
results_meme
r_squared_meme <- map_dbl(models_google_crypto_weekly_SHIB_meme, ~ summary(.x)$r.squared)  # R^2
r_squared_meme
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)

## Модель для conventional-coins

# Добавляем лаги для доходностей
crypto_weekly_SHIB_conventional <- crypto_weekly_SHIB %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_return_conventional, 1),
                R_t_2 = lag(weekly_return_conventional, 2),
                R_t_3 = lag(weekly_return_conventional, 3),
                R_t_4 = lag(weekly_return_conventional, 4)
        ) %>%
        na.omit()

crypto_weekly_SHIB_conventional <- crypto_weekly_SHIB_conventional %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_SHIB_conventional)

models_google_crypto_weekly_SHIB_conventional <- list(
        lm(Google_t ~ weekly_return_conventional, data = crypto_weekly_SHIB_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1, data = crypto_weekly_SHIB_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2, data = crypto_weekly_SHIB_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_SHIB_conventional),
        lm(Google_t ~ weekly_return_conventional + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_SHIB_conventional)
)


# Извлекаем результаты
results_conventional <- map_dfr(models_google_crypto_weekly_SHIB_conventional, ~ tidy(.x), .id = "Model")  
results_conventional
r_squared_conventional <- map_dbl(models_google_crypto_weekly_SHIB_conventional, ~ summary(.x)$r.squared)  # R^2
r_squared_conventional
# Добавляем R^2 в таблицу
r_squared_df_meme <- tibble(
        term = "R^2",
        estimate = r_squared_meme,
        statistic = NA,
        Model = as.character(1:5)
)

## Модель для weekly_market_return

# Добавляем лаги для доходностей
crypto_weekly_SHIB_market <- crypto_weekly_SHIB %>%
        arrange(week) %>%
        mutate(
                R_t_1 = lag(weekly_market_return, 1),
                R_t_2 = lag(weekly_market_return, 2),
                R_t_3 = lag(weekly_market_return, 3),
                R_t_4 = lag(weekly_market_return, 4)
        ) %>%
        na.omit()

crypto_weekly_SHIB_market <- crypto_weekly_SHIB_market %>%
        filter(if_all(everything(), ~ !is.infinite(.)))
summary(crypto_weekly_SHIB_market)

models_google_crypto_weekly_SHIB_market <- list(
        lm(Google_t ~ weekly_market_return, data = crypto_weekly_SHIB_market),
        lm(Google_t ~ weekly_market_return + R_t_1, data = crypto_weekly_SHIB_market),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2, data = crypto_weekly_SHIB_market),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3, data = crypto_weekly_SHIB_market),
        lm(Google_t ~ weekly_market_return + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = crypto_weekly_SHIB_market)
)


# Извлекаем результаты
results_market <- map_dfr(models_google_crypto_weekly_SHIB_market, ~ tidy(.x), .id = "Model")  
results_market
r_squared_market <- map_dbl(models_google_crypto_weekly_SHIB_market, ~ summary(.x)$r.squared)  # R^2
r_squared_market



################################################################################
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


# Объединяем данные о доходности и капитализации
crypto_weekly_google_bitcoin <- crypto_weekly %>%
        left_join(weekly_market_caps, by = "week") %>%
        left_join(google_trends_bitcoin, by = "week")
summary(crypto_weekly_google_bitcoin)


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


## Модель для MEME-coins
# Добавляем лаги для доходностей
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


# Извлекаем результаты
results_meme <- map_dfr(models_google_crypto_weekly_bitcoin_meme, ~ tidy(.x), .id = "Model")  
results_meme
r_squared_meme <- map_dbl(models_google_crypto_weekly_bitcoin_meme, ~ summary(.x)$r.squared)  # R^2
r_squared_meme


## Модель для conventional-coins

# Добавляем лаги для доходностей
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


# Извлекаем результаты
results_conventional <- map_dfr(models_google_crypto_weekly_bitcoin_conventional, ~ tidy(.x), .id = "Model")  
results_conventional
r_squared_conventional <- map_dbl(models_google_crypto_weekly_bitcoin_conventional, ~ summary(.x)$r.squared)  # R^2
r_squared_conventional



## Модель для weekly_market_return

# Добавляем лаги для доходностей
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

# Определяем модели
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
library(e1071) # Для skewness и kurtosis

# Выбираем нужные переменные
selected_vars <- c("weekly_market_return", "weekly_return_meme", "weekly_return_conventional",
                   "weekly_return_BTC", "weekly_return_ETH", "weekly_return_XRP", 
                   "weekly_return_DOGE", "weekly_return_SHIB")

# Рассчитываем сводные статистики
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

# Преобразуем таблицу в удобный формат
summary_stats_t <- summary_stats %>%
        pivot_longer(cols = everything(), names_to = "Variable_Statistic", values_to = "Value") %>%
        separate(Variable_Statistic, into = c("Variable", "Statistic"), sep = "_(?=[^_]+$)") %>%
        pivot_wider(names_from = Statistic, values_from = Value)
summary_stats_t





