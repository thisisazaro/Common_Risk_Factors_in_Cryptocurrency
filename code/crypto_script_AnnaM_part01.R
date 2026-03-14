
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(moments)
library(broom)
library(zoo)
library(readxl)
library(quantmod)
library(e1071) 
library(kableExtra)

############################# PART I ###########################################

crypto_data <- read_excel("crypto_data_full.xlsx")

crypto_data <- crypto_data[order(crypto_data$Currency, crypto_data$Date), ]

crypto_data <- crypto_data %>%
        mutate(Date = as.Date(Date))

colSums(is.na(crypto_data))

crypto_data <- crypto_data %>%
        mutate(
                Close = na.approx(Close, na.rm = FALSE),
                Volume = na.approx(Volume, na.rm = FALSE),
                Adjusted = na.approx(Adjusted, na.rm = FALSE)
        )


## RiskFreeRate
getSymbols("DGS1MO", src = "FRED")
risk_free_data <- data.frame(Date = index(DGS1MO), Rate = coredata(DGS1MO))
risk_free_data <- risk_free_data %>%
        rename(Rate = DGS1MO)
risk_free_data <- risk_free_data %>%
        filter(Date >= as.Date("2018-01-01") & Date <= as.Date("2024-12-31")) %>%
        mutate(
                Rate = ifelse(is.na(Rate), mean(Rate, na.rm = TRUE), Rate),
        )
risk_free_data <- risk_free_data %>%
        mutate(Rate = Rate / 100)
crypto_data <- crypto_data %>%
        left_join(risk_free_data %>% select(Date, Rate), by = "Date")
crypto_data <- crypto_data %>%
        fill(Rate, .direction = "down")
crypto_data <- crypto_data %>%
        group_by(Currency) %>%
        mutate(
                Return = (Close - lag(Close)) / lag(Close)
        ) %>%
        ungroup()
crypto_data <- crypto_data %>%
        group_by(Currency) %>%
        fill(Return, .direction = "up") %>% 
        ungroup()


## LOG-RETURN

crypto_data <- crypto_data %>%
        mutate(
                Close = log(Close),
                Return = log(Return + 1),
                ExcessReturn = Return - Rate
        )

crypto_data <- crypto_data %>%
        filter(!is.na(Return) & is.finite(Return))
crypto_data <- crypto_data %>%
        filter(!is.na(ExcessReturn) & is.finite(ExcessReturn))

weekly_data <- crypto_data %>%
        mutate(Week = floor_date(Date, unit = "week")) %>%
        group_by(Currency, Week) %>%
        summarize(
                Close = last(Close),               
                Volume = sum(Volume, na.rm = TRUE),
                Return = sum(Return, na.rm = TRUE)
        )

weekly_data <- weekly_data %>%
        mutate(MarketCap = Close * Volume) %>%
        filter(MarketCap >= 1e6)

market_index <- weekly_data %>%
        group_by(Week) %>%
        summarize(
                MarketCap = sum(MarketCap, na.rm = TRUE),
                MarketReturn = sum(Return * (MarketCap / sum(MarketCap, na.rm = TRUE)), na.rm = TRUE)
        )


## Graph

weekly_data <- crypto_data %>%
        mutate(Week = floor_date(Date, "week")) %>%
        group_by(Currency, Week) %>%
        reframe(Return = (Close - lag(Close)) / lag(Close))

monthly_data <- crypto_data %>%
        mutate(Month = floor_date(Date, "month")) %>%
        group_by(Currency, Month) %>%
        reframe(Return = (Close - lag(Close)) / lag(Close))

crypto_data <- crypto_data %>%
        filter(!is.na(Return) & is.finite(Return))

weekly_data <- weekly_data %>%
        filter(!is.na(Return) & is.finite(Return))

monthly_data <- monthly_data %>%
        filter(!is.na(Return) & is.finite(Return))

plot_distribution <- function(data, title) {
        ggplot(data, aes(x = Return)) +
                geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "blue", alpha = 0.5) +
                geom_density(color = "red", linewidth = 1) +
                labs(title = title, x = "Return", y = "Density") +
                theme_minimal()
}

p1 <- plot_distribution(crypto_data, "Daily Returns")
p2 <- plot_distribution(weekly_data, "Weekly Returns")
p3 <- plot_distribution(monthly_data, "Monthly Returns")
grid.arrange(p1, p2, p3, ncol = 1)

market_index <- crypto_data %>%
        group_by(Date) %>%
        summarize(MarketIndex = sum(Close * Volume, na.rm = TRUE) / sum(Volume, na.rm = TRUE)) %>%
        mutate(InvestmentValue = MarketIndex / first(MarketIndex) * 1)
crypto_investment <- crypto_data %>%
        group_by(Currency) %>%
        mutate(InvestmentValue = Close / first(Close) * 1) %>%
        ungroup()
selected_currencies <- c("BTC", "ETH", "XRP")
crypto_plot_data <- crypto_investment %>%
        filter(Currency %in% selected_currencies)
plot_data <- crypto_plot_data %>%
        left_join(market_index, by = "Date", suffix = c("", "_Market"))
p1 <- ggplot(plot_data %>% filter(Currency == "BTC"), aes(x = Date)) +
        geom_line(aes(y = InvestmentValue_Market, color = "Coin Market")) +
        geom_line(aes(y = InvestmentValue, color = "Bitcoin")) +
        scale_color_manual(values = c("Coin Market" = "skyblue", "Bitcoin" = "red")) +
        labs(title = "Bitcoin vs Coin Market", y = "Value of Investment", color = "Legend") +
        theme_minimal()

p2 <- ggplot(plot_data %>% filter(Currency == "ETH"), aes(x = Date)) +
        geom_line(aes(y = InvestmentValue_Market, color = "Coin Market")) +
        geom_line(aes(y = InvestmentValue, color = "Ethereum")) +
        scale_color_manual(values = c("Coin Market" = "skyblue", "Ethereum" = "red")) +
        labs(title = "Ethereum vs Coin Market", y = "Value of Investment", color = "Legend") +
        theme_minimal()

p3 <- ggplot(plot_data %>% filter(Currency == "XRP"), aes(x = Date)) +
        geom_line(aes(y = InvestmentValue_Market, color = "Coin Market")) +
        geom_line(aes(y = InvestmentValue, color = "Ripple")) +
        scale_color_manual(values = c("Coin Market" = "skyblue", "Ripple" = "red")) +
        labs(title = "Ripple vs Coin Market", y = "Value of Investment", color = "Legend") +
        theme_minimal()

grid.arrange(p1, p2, p3, ncol = 1)



# S&P 500 (Yahoo Finance)
getSymbols("^GSPC", src = "yahoo", from = "2018-01-01", to = Sys.Date())
sp500_data <- data.frame(Date = index(GSPC), Close = as.numeric(Cl(GSPC)))
sp500_data <- sp500_data %>%
        mutate(InvestmentValue = Close / first(Close))
plot_data <- plot_data %>%
        left_join(sp500_data, by = "Date", suffix = c("", "_SP500"))
plot_data <- plot_data %>%
        filter(!is.na(InvestmentValue_SP500) & !is.na(InvestmentValue) & !is.na(InvestmentValue_Market))

p1 <- ggplot(plot_data %>% filter(Currency == "BTC"), aes(x = Date)) +
        geom_line(aes(y = InvestmentValue_Market, color = "Coin Market")) +
        geom_line(aes(y = InvestmentValue, color = "Bitcoin")) +
        geom_line(aes(y = InvestmentValue_SP500, color = "S&P 500")) +
        scale_color_manual(values = c(
                "Coin Market" = "skyblue",
                "Bitcoin" = "red",
                "S&P 500" = "darkgreen"
        )) +
        labs(title = "Bitcoin vs Coin Market and S&P 500", y = "$ Value of Investment", color = "Legend") +
        theme_minimal()

p2 <- ggplot(plot_data %>% filter(Currency == "ETH"), aes(x = Date)) +
        geom_line(aes(y = InvestmentValue_Market, color = "Coin Market")) +
        geom_line(aes(y = InvestmentValue, color = "Ethereum")) +
        geom_line(aes(y = InvestmentValue_SP500, color = "S&P 500")) +
        scale_color_manual(values = c(
                "Coin Market" = "skyblue",
                "Ethereum" = "red",
                "S&P 500" = "darkgreen"
        )) +
        labs(title = "Ethereum vs Coin Market and S&P 500", y = "$ Value of Investment", color = "Legend") +
        theme_minimal()

p3 <- ggplot(plot_data %>% filter(Currency == "XRP"), aes(x = Date)) +
        geom_line(aes(y = InvestmentValue_Market, color = "Coin Market")) +
        geom_line(aes(y = InvestmentValue, color = "Ripple")) +
        geom_line(aes(y = InvestmentValue_SP500, color = "S&P 500")) +
        scale_color_manual(values = c(
                "Coin Market" = "skyblue",
                "Ripple" = "red",
                "S&P 500" = "darkgreen"
        )) +
        labs(title = "Ripple vs Coin Market and S&P 500", y = "$ Value of Investment", color = "Legend") +
        theme_minimal()

grid.arrange(p1, p2, p3, ncol = 1)


## Statistic

calculate_t_stat <- function(mean, sd, n) {
        return(mean / (sd / sqrt(n)))
}

calculate_summary_stats <- function(data, freq) {
        data %>%
                group_by(Currency) %>%
                summarize(
                        Frequency = freq,
                        Mean = mean(Return, na.rm = TRUE) * 100, 
                        SD = sd(Return, na.rm = TRUE) * 100, 
                        t_Stat = calculate_t_stat(mean(Return, na.rm = TRUE), 
                                                  sd(Return, na.rm = TRUE), 
                                                  n()),                     
                        Sharpe = Mean / SD,                                  
                        Skewness = skewness(Return, na.rm = TRUE),        
                        Kurtosis = kurtosis(Return, na.rm = TRUE),        
                        Positive = mean(Return > 0, na.rm = TRUE) * 100  
                )
}

daily_stats <- calculate_summary_stats(crypto_data, "Daily")
weekly_stats <- calculate_summary_stats(weekly_data, "Weekly")
monthly_stats <- calculate_summary_stats(monthly_data, "Monthly")
summary_stats <- bind_rows(daily_stats, weekly_stats, monthly_stats)
extreme_events <- crypto_data %>%
        filter(!is.na(Return)) %>%
        summarize(
                LessThanMinus5 = mean(Return < -0.05) * 100,
                LessThanMinus10 = mean(Return < -0.10) * 100,
                LessThanMinus20 = mean(Return < -0.20) * 100,
                LessThanMinus30 = mean(Return < -0.30) * 100,
                MoreThan5 = mean(Return > 0.05) * 100,
                MoreThan10 = mean(Return > 0.10) * 100,
                MoreThan20 = mean(Return > 0.20) * 100,
                MoreThan30 = mean(Return > 0.30) * 100
        )

summary_stats

selected_currencies <- c("BTC", "ETH", "XRP")

summary_stats <- crypto_data %>%
        filter(Currency %in% selected_currencies) %>%
        group_by(Currency) %>%
        summarise(
                Mean = mean(ExcessReturn, na.rm = TRUE),
                Median = median(ExcessReturn, na.rm = TRUE),
                SD = sd(ExcessReturn, na.rm = TRUE),
                Skewness = skewness(ExcessReturn, na.rm = TRUE),
                Kurtosis = kurtosis(ExcessReturn, na.rm = TRUE)
        )

market_return <- crypto_data %>%
        summarise(
                Mean = mean(ExcessReturn, na.rm = TRUE),
                Median = median(ExcessReturn, na.rm = TRUE),
                SD = sd(ExcessReturn, na.rm = TRUE),
                Skewness = skewness(ExcessReturn, na.rm = TRUE),
                Kurtosis = kurtosis(ExcessReturn, na.rm = TRUE)
        ) %>%
        mutate(Currency = "Coin Market Return")
summary_stats <- bind_rows(summary_stats, market_return)
print(summary_stats)

colnames(summary_stats) <- c("Currency", "Frequency", "Mean (%)", "SD (%)", "t-Stat", "Sharpe", "Skewness", "Kurtosis", "% Positive")
kable(summary_stats, caption = "Summary statistics of cryptocurrency returns") %>%
        kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))


panel_a <- crypto_data %>%
        mutate(Year = as.character(lubridate::year(Date))) %>%
        group_by(Year) %>%
        summarise(
                Number = n_distinct(Currency),
                Mean_Market_Cap = mean(Close, na.rm = TRUE),
                Median_Market_Cap = median(Close, na.rm = TRUE),
                Mean_Volume = mean(Volume, na.rm = TRUE),
                Median_Volume = median(Volume, na.rm = TRUE)
        ) %>%
        bind_rows(
                crypto_data %>%
                        summarise(
                                Year = "Full",
                                Number = n_distinct(Currency),
                                Mean_Market_Cap = mean(Close, na.rm = TRUE),
                                Median_Market_Cap = median(Close, na.rm = TRUE),
                                Mean_Volume = mean(Volume, na.rm = TRUE),
                                Median_Volume = median(Volume, na.rm = TRUE)
                        )
        )


panel_b <- crypto_data %>%
        group_by(Currency) %>%
        summarise(
                Mean = mean(Return, na.rm = TRUE),
                Median = median(Return, na.rm = TRUE),
                SD = sd(Return, na.rm = TRUE),
                Skewness = e1071::skewness(Return, na.rm = TRUE),
                Kurtosis = e1071::kurtosis(Return, na.rm = TRUE)
        ) %>%
        rename(
                Coin = Currency
        )

print("Panel A: Characteristics by Year")
print(panel_a)

print("Panel B: Return Characteristics")
print(panel_b)


############ II. Cross-Sectional Return Predictors #############################

calculate_quintile_portfolios <- function(data, characteristic, return_col) {
        data <- data %>%
                group_by(Week = floor_date(Date, "week")) %>%
                mutate(
                        Quintile = ntile(!!sym(characteristic), 5)  
                ) %>%
                ungroup()
        quintile_returns <- data %>%
                group_by(Week, Quintile) %>%
                summarise(
                        Mean_Return = mean(!!sym(return_col), na.rm = TRUE),
                        .groups = "drop"
                )
        
        long_short_strategy <- quintile_returns %>%
                filter(Quintile == 5 | Quintile == 1) %>%
                pivot_wider(names_from = Quintile, values_from = Mean_Return) %>%
                mutate(
                        Long_Short = `5` - `1`
                )
        
        list(quintile_returns = quintile_returns, long_short_strategy = long_short_strategy)
}

result <- calculate_quintile_portfolios(
        data = crypto_data,
        characteristic = "Close",
        return_col = "ExcessReturn"
)


result$long_short_strategy <- result$long_short_strategy %>%
        filter(!is.infinite(Long_Short))


print(result$quintile_returns)
print(result$long_short_strategy)

ggplot(result$quintile_returns, aes(x = Week, y = Mean_Return, color = factor(Quintile))) +
        geom_line() +
        labs(title = "quintile returns", x = "Week", y = "Return (mean)", color = "Квинтиль") +
        theme_minimal()


ggplot(result$long_short_strategy, aes(x = Week, y = Long_Short)) +
        geom_line(color = "blue") +
        labs(title = "Long-Short return", x = "Weel", y = "Return") +
        theme_minimal()

t.test(result$long_short_strategy$Long_Short)


# A. Size Characteristics
# SIZE STRATEGY RETURNS
calculate_quintiles <- function(data, variable) {
        data %>%
                group_by(Currency) %>%
                mutate(Quintile = ntile(!!sym(variable), 5)) %>%
                group_by(Quintile) %>%
                summarize(
                        Mean = mean(ExcessReturn, na.rm = TRUE),
                        tMean = Mean / (sd(ExcessReturn, na.rm = TRUE) / sqrt(n())),
                        .groups = "drop"
                )
}

crypto_data <- crypto_data %>%
        mutate(ExcessReturn = ifelse(is.infinite(ExcessReturn), NA, ExcessReturn)) %>%
        drop_na(ExcessReturn)


mcap_quintiles <- calculate_quintiles(crypto_data, "Volume")
prc_quintiles <- calculate_quintiles(crypto_data, "Close")
maxdprc_quintiles <- calculate_quintiles(crypto_data, "Adjusted")

mcap_quintiles
prc_quintiles
maxdprc_quintiles

mcap_quintiles_clean <- mcap_quintiles %>%
        filter(!is.na(Quintile)) %>%
        mutate(Measure = "MCAP")

prc_quintiles_clean <- prc_quintiles %>%
        filter(!is.na(Quintile)) %>%
        mutate(Measure = "PRC")

maxdprc_quintiles_clean <- maxdprc_quintiles %>%
        filter(!is.na(Quintile)) %>%
        mutate(Measure = "MAXDPRC")

combined_quintiles <- bind_rows(
        mcap_quintiles_clean,
        prc_quintiles_clean,
        maxdprc_quintiles_clean
)
combined_quintiles

kable(combined_quintiles, caption = "Size Strategy Returns by Quintiles")

add_significance_stars <- function(t_value) {
        p_value <- 2 * (1 - pt(abs(t_value), df = Inf))
        case_when(
                p_value < 0.01 ~ "***",
                p_value < 0.05 ~ "**",
                p_value < 0.1  ~ "*",
                TRUE           ~ ""
        )
}

combined_quintiles <- combined_quintiles %>%
        mutate(Significance = add_significance_stars(tMean),
               Mean_with_stars = paste0(round(Mean, 6), Significance))

kable(combined_quintiles %>% select(Quintile, Mean_with_stars, tMean, Measure),
      caption = "Size Strategy Returns by Quintiles",
      col.names = c("Quintile", "Mean", "tMean", "Measure"))


# 5-1 , 3-1
calculate_difference <- function(df, q_high, q_low, label) {
        quintile_low <- df %>% filter(Quintile == q_low)
        quintile_high <- df %>% filter(Quintile == q_high)
        if (nrow(quintile_low) == 0 | nrow(quintile_high) == 0) {
                return(tibble(Quintile = character(), Mean = numeric(), tMean = numeric()))
        }
        diff_mean <- quintile_high$Mean - quintile_low$Mean
        diff_se <- sqrt((quintile_low$Mean^2 / quintile_low$tMean^2) + 
                                (quintile_high$Mean^2 / quintile_high$tMean^2))
        diff_tMean <- diff_mean / diff_se
        tibble(
                Quintile = label,
                Mean = diff_mean,
                tMean = diff_tMean
        )
}

mcap_diff_5_1 <- calculate_difference(mcap_quintiles_clean, 5, 1, "5-1") %>% mutate(Measure = "MCAP")
prc_diff_5_1 <- calculate_difference(prc_quintiles_clean, 5, 1, "5-1") %>% mutate(Measure = "PRC")
maxdprc_diff_5_1 <- calculate_difference(maxdprc_quintiles_clean, 5, 1, "5-1") %>% mutate(Measure = "MAXDPRC")
mcap_diff_3_1 <- calculate_difference(mcap_quintiles_clean, 3, 1, "3-1") %>% mutate(Measure = "MCAP")
prc_diff_3_1 <- calculate_difference(prc_quintiles_clean, 3, 1, "3-1") %>% mutate(Measure = "PRC")
maxdprc_diff_3_1 <- calculate_difference(maxdprc_quintiles_clean, 3, 1, "3-1") %>% mutate(Measure = "MAXDPRC")

combined_quintiles <- combined_quintiles %>%
        mutate(Quintile = as.character(Quintile))

combined_quintiles_with_diff <- bind_rows(
        combined_quintiles,
        mcap_diff_5_1,
        prc_diff_5_1,
        maxdprc_diff_5_1,
        mcap_diff_3_1,
        prc_diff_3_1,
        maxdprc_diff_3_1
)

combined_quintiles_with_diff <- combined_quintiles_with_diff %>%
        mutate(Significance = ifelse(is.na(tMean), "", add_significance_stars(tMean)),
               Mean_with_stars = ifelse(is.na(Mean), "", paste0(round(Mean, 6), Significance)))

kable(combined_quintiles_with_diff %>% select(Quintile, Mean_with_stars, tMean, Measure),
      caption = "Size Strategy Returns by Quintiles",
      col.names = c("Quintile", "Mean", "tMean", "Measure"))


# B. Momentum Characteristics
calculate_momentum_summary <- function(data, lookback_periods) {
        momentum_results <- lapply(lookback_periods, function(period) {
                data %>%
                        group_by(Currency) %>%
                        arrange(Date) %>%
                        mutate(
                                momentum_return = lag(ExcessReturn, period),
                                quintile = ifelse(!is.na(momentum_return), ntile(momentum_return, 5), NA)
                        ) %>%
                        filter(!is.na(momentum_return)) %>%
                        group_by(Date, quintile) %>%
                        summarise(
                                mean_return = mean(ExcessReturn, na.rm = TRUE),
                                .groups = "drop"
                        ) %>%
                        pivot_wider(
                                names_from = quintile,
                                values_from = mean_return,
                                names_prefix = "Q"
                        ) %>%
                        mutate(
                                `5-1` = Q5 - Q1
                        )
        })
        
        names(momentum_results) <- paste0("r_", seq_along(lookback_periods))
        combined_results <- bind_rows(momentum_results, .id = "Lookback_Period")
        summary_table <- combined_results %>%
                group_by(Lookback_Period) %>%
                summarise(
                        Mean_Q1 = mean(Q1, na.rm = TRUE),
                        t_Q1 = t.test(Q1, mu = 0, na.rm = TRUE)$statistic,
                        Mean_Q2 = mean(Q2, na.rm = TRUE),
                        t_Q2 = t.test(Q2, mu = 0, na.rm = TRUE)$statistic,
                        Mean_Q3 = mean(Q3, na.rm = TRUE),
                        t_Q3 = t.test(Q3, mu = 0, na.rm = TRUE)$statistic,
                        Mean_Q4 = mean(Q4, na.rm = TRUE),
                        t_Q4 = t.test(Q4, mu = 0, na.rm = TRUE)$statistic,
                        Mean_Q5 = mean(Q5, na.rm = TRUE),
                        t_Q5 = t.test(Q5, mu = 0, na.rm = TRUE)$statistic,
                        Mean_5_1 = mean(`5-1`, na.rm = TRUE),
                        t_5_1 = t.test(`5-1`, mu = 0, na.rm = TRUE)$statistic,
                        .groups = "drop"
                )
        
        return(summary_table)
}

lookback_periods <- c(7, 14, 21, 28)
momentum_summary <- calculate_momentum_summary(crypto_data, lookback_periods)
momentum_summary

add_significance <- Vectorize(function(value, t_value) {
        if (abs(t_value) > qt(0.99, df = Inf)) {
                paste0(format(round(value, 3), nsmall = 3), "***")
        } else if (abs(t_value) > qt(0.975, df = Inf)) {
                paste0(format(round(value, 3), nsmall = 3), "**")
        } else if (abs(t_value) > qt(0.95, df = Inf)) {
                paste0(format(round(value, 3), nsmall = 3), "*")
        } else {
                format(round(value, 3), nsmall = 3)
        }
})

formatted_summary <- momentum_summary %>%
        mutate(
                Q1 = add_significance(Mean_Q1, t_Q1),
                Q2 = add_significance(Mean_Q2, t_Q2),
                Q3 = add_significance(Mean_Q3, t_Q3),
                Q4 = add_significance(Mean_Q4, t_Q4),
                Q5 = add_significance(Mean_Q5, t_Q5),
                `5-1` = add_significance(Mean_5_1, t_5_1)
        ) %>%
        dplyr::select(Lookback_Period, Q1, Q2, Q3, Q4, Q5, `5-1`)
print(formatted_summary, n = Inf)

formatted_summary %>%
        kable(
                caption = "Momentum Strategy Returns",
                col.names = c("Lookback Period", "Q1", "Q2", "Q3", "Q4", "Q5", "5-1"),
                align = c("l", "c", "c", "c", "c", "c", "c")
        )


calculate_p_value <- function(t_value) {
        2 * (1 - pt(abs(t_value), df = Inf)) # Двусторонний тест
}

momentum_pvalues <- momentum_summary %>%
        mutate(
                p_Q1 = calculate_p_value(t_Q1),
                p_Q2 = calculate_p_value(t_Q2),
                p_Q3 = calculate_p_value(t_Q3),
                p_Q4 = calculate_p_value(t_Q4),
                p_Q5 = calculate_p_value(t_Q5),
                p_5_1 = calculate_p_value(t_5_1)
        ) %>%
        dplyr::select(Lookback_Period, p_Q1, p_Q2, p_Q3, p_Q4, p_Q5, p_5_1)




# C. Volume Characteristics
calculate_volume_strategy <- function(data, volume_column) {
        data %>%
                group_by(Date) %>%
                mutate(
                        quintile = ntile(!!sym(volume_column), 5) 
                ) %>%
                group_by(Date, quintile) %>%
                summarise(
                        mean_return = mean(ExcessReturn, na.rm = TRUE), 
                        .groups = "drop"
                ) %>%
                pivot_wider(
                        names_from = quintile,
                        values_from = mean_return,
                        names_prefix = "Q"
                ) %>%
                mutate(
                        `5-1` = Q1 - Q5 
                )
}

volume_results <- calculate_volume_strategy(crypto_data, "Volume")

volume_summary <- volume_results %>%
        summarise(
                Mean_Q1 = mean(Q1, na.rm = TRUE),
                t_Q1 = t.test(Q1, mu = 0, na.rm = TRUE)$statistic,
                Mean_Q2 = mean(Q2, na.rm = TRUE),
                t_Q2 = t.test(Q2, mu = 0, na.rm = TRUE)$statistic,
                Mean_Q3 = mean(Q3, na.rm = TRUE),
                t_Q3 = t.test(Q3, mu = 0, na.rm = TRUE)$statistic,
                Mean_Q4 = mean(Q4, na.rm = TRUE),
                t_Q4 = t.test(Q4, mu = 0, na.rm = TRUE)$statistic,
                Mean_Q5 = mean(Q5, na.rm = TRUE),
                t_Q5 = t.test(Q5, mu = 0, na.rm = TRUE)$statistic,
                Mean_5_1 = mean(`5-1`, na.rm = TRUE),
                t_5_1 = t.test(`5-1`, mu = 0, na.rm = TRUE)$statistic
        )
volume_summary

volume_formatted <- volume_summary %>%
        mutate(
                Q1 = paste0(
                        format(round(Mean_Q1, 3), nsmall = 3), 
                        ifelse(abs(t_Q1) > qt(0.99, Inf), "***", 
                               ifelse(abs(t_Q1) > qt(0.975, Inf), "**", 
                                      ifelse(abs(t_Q1) > qt(0.95, Inf), "*", "")))
                ),
                Q2 = paste0(
                        format(round(Mean_Q2, 3), nsmall = 3), 
                        ifelse(abs(t_Q2) > qt(0.99, Inf), "***", 
                               ifelse(abs(t_Q2) > qt(0.975, Inf), "**", 
                                      ifelse(abs(t_Q2) > qt(0.95, Inf), "*", "")))
                ),
                Q3 = paste0(
                        format(round(Mean_Q3, 3), nsmall = 3), 
                        ifelse(abs(t_Q3) > qt(0.99, Inf), "***", 
                               ifelse(abs(t_Q3) > qt(0.975, Inf), "**", 
                                      ifelse(abs(t_Q3) > qt(0.95, Inf), "*", "")))
                ),
                Q4 = paste0(
                        format(round(Mean_Q4, 3), nsmall = 3), 
                        ifelse(abs(t_Q4) > qt(0.99, Inf), "***", 
                               ifelse(abs(t_Q4) > qt(0.975, Inf), "**", 
                                      ifelse(abs(t_Q4) > qt(0.95, Inf), "*", "")))
                ),
                Q5 = paste0(
                        format(round(Mean_Q5, 3), nsmall = 3), 
                        ifelse(abs(t_Q5) > qt(0.99, Inf), "***", 
                               ifelse(abs(t_Q5) > qt(0.975, Inf), "**", 
                                      ifelse(abs(t_Q5) > qt(0.95, Inf), "*", "")))
                ),
                `5-1` = paste0(
                        format(round(Mean_5_1, 3), nsmall = 3), 
                        ifelse(abs(t_5_1) > qt(0.99, Inf), "***", 
                               ifelse(abs(t_5_1) > qt(0.975, Inf), "**", 
                                      ifelse(abs(t_5_1) > qt(0.95, Inf), "*", "")))
                )
        ) %>%
        dplyr::select(Q1, Q2, Q3, Q4, Q5, `5-1`)

volume_formatted %>%
        kable(
                caption = "Volume Strategy Returns",
                col.names = c("Q1", "Q2", "Q3", "Q4", "Q5", "5-1"),
                align = c("c", "c", "c", "c", "c", "c")
        )



# D. Volatility Characteristics
calculate_volatility_strategy <- function(data, window = 7) {
        data %>%
                group_by(Currency) %>%
                arrange(Date) %>%
                mutate(
                        std_volume = rollapply(Volume, width = window, FUN = sd, fill = NA, align = "right") # Стандартное отклонение объёма
                ) %>%
                group_by(Date) %>%
                mutate(
                        quintile = ntile(std_volume, 5) 
                ) %>%
                group_by(Date, quintile) %>%
                summarise(
                        mean_return = mean(Return, na.rm = TRUE),
                        .groups = "drop"
                ) %>%
                pivot_wider(
                        names_from = quintile,
                        values_from = mean_return,
                        names_prefix = "Q"
                ) %>%
                mutate(
                        `5-1` = Q1 - Q5 
                )
}

volatility_results <- calculate_volatility_strategy(crypto_data, window = 7)
volatility_summary <- volatility_results %>%
        summarise(
                Mean_Q1 = mean(Q1, na.rm = TRUE),
                t_Q1 = t.test(Q1, mu = 0, na.rm = TRUE)$statistic,
                Mean_Q2 = mean(Q2, na.rm = TRUE),
                t_Q2 = t.test(Q2, mu = 0, na.rm = TRUE)$statistic,
                Mean_Q3 = mean(Q3, na.rm = TRUE),
                t_Q3 = t.test(Q3, mu = 0, na.rm = TRUE)$statistic,
                Mean_Q4 = mean(Q4, na.rm = TRUE),
                t_Q4 = t.test(Q4, mu = 0, na.rm = TRUE)$statistic,
                Mean_Q5 = mean(Q5, na.rm = TRUE),
                t_Q5 = t.test(Q5, mu = 0, na.rm = TRUE)$statistic,
                Mean_5_1 = mean(`5-1`, na.rm = TRUE),
                t_5_1 = t.test(`5-1`, mu = 0, na.rm = TRUE)$statistic
        )
volatility_summary

volatility_formatted <- volatility_summary %>%
        mutate(
                Q1 = paste0(
                        format(round(Mean_Q1, 3), nsmall = 3), 
                        ifelse(abs(t_Q1) > qt(0.99, Inf), "***", 
                               ifelse(abs(t_Q1) > qt(0.975, Inf), "**", 
                                      ifelse(abs(t_Q1) > qt(0.95, Inf), "*", "")))
                ),
                Q2 = paste0(
                        format(round(Mean_Q2, 3), nsmall = 3), 
                        ifelse(abs(t_Q2) > qt(0.99, Inf), "***", 
                               ifelse(abs(t_Q2) > qt(0.975, Inf), "**", 
                                      ifelse(abs(t_Q2) > qt(0.95, Inf), "*", "")))
                ),
                Q3 = paste0(
                        format(round(Mean_Q3, 3), nsmall = 3), 
                        ifelse(abs(t_Q3) > qt(0.99, Inf), "***", 
                               ifelse(abs(t_Q3) > qt(0.975, Inf), "**", 
                                      ifelse(abs(t_Q3) > qt(0.95, Inf), "*", "")))
                ),
                Q4 = paste0(
                        format(round(Mean_Q4, 3), nsmall = 3), 
                        ifelse(abs(t_Q4) > qt(0.99, Inf), "***", 
                               ifelse(abs(t_Q4) > qt(0.975, Inf), "**", 
                                      ifelse(abs(t_Q4) > qt(0.95, Inf), "*", "")))
                ),
                Q5 = paste0(
                        format(round(Mean_Q5, 3), nsmall = 3), 
                        ifelse(abs(t_Q5) > qt(0.99, Inf), "***", 
                               ifelse(abs(t_Q5) > qt(0.975, Inf), "**", 
                                      ifelse(abs(t_Q5) > qt(0.95, Inf), "*", "")))
                ),
                `5-1` = paste0(
                        format(round(Mean_5_1, 3), nsmall = 3), 
                        ifelse(abs(t_5_1) > qt(0.99, Inf), "***", 
                               ifelse(abs(t_5_1) > qt(0.975, Inf), "**", 
                                      ifelse(abs(t_5_1) > qt(0.95, Inf), "*", "")))
                )
        ) %>%
        dplyr::select(Q1, Q2, Q3, Q4, Q5, `5-1`)


volatility_formatted %>%
        kable(
                caption = "Volatility Strategy Returns",
                col.names = c("Q1", "Q2", "Q3", "Q4", "Q5", "5-1"),
                align = c("c", "c", "c", "c", "c", "c")
        )

############ III. Cryptocurrency Factors #######################################

# A. Cryptocurrency Factor Model


crypto_data2 <- crypto_data %>%
        group_by(Date) %>%
        mutate(
                Total_MCAP = sum(Close * Volume, na.rm = TRUE), 
                Weight = (Close * Volume) / Total_MCAP,        
                CMKT = sum(Weight * ExcessReturn, na.rm = TRUE)      
        ) %>%
        ungroup()
str(crypto_data2)

crypto_data2 <- crypto_data2 %>%
        group_by(Date) %>%
        mutate(
                Size_Rank = ntile(Total_MCAP, 3),
                Small = mean(ExcessReturn[Size_Rank == 1], na.rm = TRUE),
                Big = mean(ExcessReturn[Size_Rank == 3], na.rm = TRUE),
                CSMB = Small - Big
        ) %>%
        ungroup() %>%
        mutate(CSMB = ifelse(is.na(CSMB), mean(CSMB, na.rm = TRUE), CSMB))

crypto_data2 <- crypto_data2 %>%
        group_by(Date) %>%
        mutate(
                Momentum_Group = ntile(Return, 3),
                High = mean(ExcessReturn[Momentum_Group == 3], na.rm = TRUE),
                Low = mean(ExcessReturn[Momentum_Group == 1], na.rm = TRUE),
                CMOM = High - Low
        ) %>%
        ungroup() %>%
        mutate(CMOM = ifelse(is.na(CMOM), mean(CMOM, na.rm = TRUE), CMOM))

strategies_list <- list(
        "MCAP" = combined_quintiles %>% filter(Measure == "MCAP") %>% select(Quintile, Mean),
        "PRC" = combined_quintiles %>% filter(Measure == "PRC") %>% select(Quintile, Mean),
        "MAXDPRC" = combined_quintiles %>% filter(Measure == "MAXDPRC") %>% select(Quintile, Mean),
        "r_1,0" = momentum_summary %>% select(Lookback_Period, Mean_5_1) %>% rename(Mean = Mean_5_1),
        "r_2,0" = momentum_summary %>% select(Lookback_Period, Mean_5_1) %>% rename(Mean = Mean_5_1),
        "r_3,0" = momentum_summary %>% select(Lookback_Period, Mean_5_1) %>% rename(Mean = Mean_5_1),
        "r_4,0" = momentum_summary %>% select(Lookback_Period, Mean_5_1) %>% rename(Mean = Mean_5_1),
        "PRCVOL" = volume_summary %>% select(Mean_5_1) %>% rename(Mean = Mean_5_1),
        "STDPRCVOL" = volatility_summary %>% select(Mean_5_1) %>% rename(Mean = Mean_5_1)
)

run_regression <- function(strategy_data, factor_data) {
        if (nrow(strategy_data) == 0) return(NULL)
        if ("Quintile" %in% colnames(strategy_data)) {
                factor_data <- factor_data %>%
                        mutate(Quintile = cut(Total_MCAP, breaks = 5, labels = c(1, 2, 3, 4, 5))) %>%
                        left_join(strategy_data, by = "Quintile") %>%
                        filter(!is.na(Mean))
        } else {
                factor_data$Mean <- rep(strategy_data$Mean, length.out = nrow(factor_data))
        }
        
        model1 <- lm(Mean ~ CMKT, data = factor_data)
        model2 <- lm(Mean ~ CMKT + CSMB, data = factor_data)
        model3 <- lm(Mean ~ CMKT + CSMB + CMOM, data = factor_data)
        
        extract_results <- function(model) {
                summary_model <- summary(model)
                
                all_vars <- c("(Intercept)", "CMKT", "CSMB", "CMOM")
                coefs <- summary_model$coefficients[, 1]
                t_stats <- summary_model$coefficients[, 3]
                
                coefs_full <- setNames(rep(0, length(all_vars)), all_vars)
                t_stats_full <- setNames(rep(0, length(all_vars)), all_vars)
                
                coefs_full[names(coefs)] <- coefs
                t_stats_full[names(t_stats)] <- t_stats
                
                r_squared <- summary_model$r.squared
                m.a.e <- mean(abs(model$residuals))
                
                return(c(coefs_full, t_stats_full, "R_squared" = r_squared, "MAE" = m.a.e))
        }
        
        results <- lapply(list(model1, model2, model3), extract_results) %>%
                do.call(rbind, .) %>%
                as.data.frame()
        
        return(results)
}

results_list <- list()
for (strategy_name in names(strategies_list)) {
        results_list[[strategy_name]] <- run_regression(strategies_list[[strategy_name]], crypto_data2)
}

results_list <- results_list[!sapply(results_list, is.null)]

final_results <- bind_rows(results_list, .id = "Strategy")
colnames(final_results) <- make.names(colnames(final_results), unique = TRUE)
print(colnames(final_results))
final_results <- final_results %>%
        rename(
                `(Intercept)` = `X.Intercept....1`,
                `CMKT` = `CMKT...2`,
                `CSMB` = `CSMB...3`,
                `CMOM` = `CMOM...4`,
                `(Intercept)_t` = `X.Intercept....5`,
                `CMKT_t` = `CMKT...6`,
                `CSMB_t` = `CSMB...7`,
                `CMOM_t` = `CMOM...8`
        )

format_t_stat <- Vectorize(function(t_value) {
        if (is.na(t_value)) return("")
        return(paste0("(", round(t_value, 2), ")"))
})

final_results <- final_results %>%
        mutate(
                `(Intercept)_t` = format_t_stat(`(Intercept)_t`),
                CMKT_t = format_t_stat(CMKT_t),
                CSMB_t = format_t_stat(CSMB_t),
                CMOM_t = format_t_stat(CMOM_t)
        )

add_significance <- function(coef, t_value) {
        if (is.na(coef) | is.na(t_value)) return(as.character(coef))
        if (abs(as.numeric(t_value)) > 2.58) return(paste0(round(coef, 4), "***"))  
        if (abs(as.numeric(t_value)) > 1.96) return(paste0(round(coef, 4), "**"))   
        if (abs(as.numeric(t_value)) > 1.645) return(paste0(round(coef, 4), "*"))   
        return(as.character(round(coef, 4)))
}

clean_t_stat <- function(t_value) {
        t_value <- gsub("[()]", "", t_value)
        t_value <- suppressWarnings(as.numeric(t_value))  
        return(ifelse(is.na(t_value), NA, t_value)) 
}



final_results <- final_results %>%
        mutate(
                `(Intercept)_t` = sapply(`(Intercept)_t`, clean_t_stat),
                CMKT_t = sapply(CMKT_t, clean_t_stat),
                CSMB_t = sapply(CSMB_t, clean_t_stat),
                CMOM_t = sapply(CMOM_t, clean_t_stat)
        )

add_significance <- function(coef, t_value) {
        if (is.na(coef) | is.na(t_value)) return(as.character(coef))
        if (abs(t_value) > 2.58) return(paste0(round(coef, 4), "***")) 
        if (abs(t_value) > 1.96) return(paste0(round(coef, 4), "**"))   
        if (abs(t_value) > 1.645) return(paste0(round(coef, 4), "*"))   
        return(as.character(round(coef, 4)))
}
final_results <- final_results %>%
        mutate(
                `(Intercept)` = mapply(add_significance, `(Intercept)`, `(Intercept)_t`),
                CMKT = mapply(add_significance, CMKT, CMKT_t),
                CSMB = mapply(add_significance, CSMB, CSMB_t),
                CMOM = mapply(add_significance, CMOM, CMOM_t)
        )

final_results <- final_results %>%
        select(Strategy, `(Intercept)`, `(Intercept)_t`, CMKT, CMKT_t, CSMB, CSMB_t, CMOM, CMOM_t, R_squared, MAE)


final_results %>%
        kable(digits = 8, format = "html", escape = FALSE) %>%
        kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


