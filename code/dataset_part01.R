

library(quantmod)
library(dplyr)

# Список символов (замените на первые 200 символов из вашего списка)
crypto_symbols <- c(
        "BTC-USD", "XRP-USD", "ETH-USD", "BCH-USD", "ADA-USD", 
        "XEM-USD", "LTC-USD", "TRX-USD", "XLM-USD", "MIOTA-USD",
        "DASH-USD", "EOS-USD", "XMR-USD", "NEO-USD", "QTUM-USD",
        "BTG-USD", "ETC-USD", "LSK-USD", "ICX-USD", "NANO-USD",
        "SC-USD", "BCN-USD", "ZEC-USD", "XVG-USD", "OMG-USD",
        "BCC-USD", "BTS-USD", "PPT-USD", "DOGE-USD", "DCN-USD",
        "BNB-USD", "SNT-USD", "ARDR-USD", "KCS-USD", "STRAT-USD",
        "STEEM-USD", "USDT-USD", "WAVES-USD", "VEN-USD", "DGB-USD",
        "KMD-USD", "DRGN-USD", "HC-USD", "KIN-USD", "ETN-USD",
        "GNT-USD", "REP-USD", "VGX-USD", "VERI-USD", "ARK-USD",
        "XP-USD", "BAT-USD", "RDD-USD", "DCR-USD", "QASH-USD",
        "DENT-USD", "LRC-USD", "PAC-USD", "SALT-USD", "FUN-USD",
        "NXS-USD", "KNC-USD", "PIVX-USD", "ZRX-USD", "POWR-USD",
        "FCT-USD", "AION-USD", "AE-USD", "REQ-USD", "ELF-USD",
        "SUB-USD", "XDN-USD", "BTM-USD", "WAXP-USD", "NXT-USD",
        "RHOC-USD", "MAID-USD", "GBYTE-USD", "NAS-USD", "MONA-USD",
        "ICN-USD", "GAS-USD", "BTCD-USD", "SYS-USD", "SAN-USD",
        "QSP-USD", "LINK-USD", "ENG-USD", "XZC-USD", "TNB-USD",
        "POE-USD", "PAY-USD", "ZCL-USD", "WTC-USD", "GNO-USD",
        "CVC-USD", "DGD-USD", "VEE-USD", "RDN-USD", "LEND-USD",
        "GXC-USD", "ACT-USD", "DBC-USD", "STORM-USD", "STORJ-USD",
        "ATM-USD", "ENJ-USD", "GAME-USD", "NEBL-USD", "SMART-USD",
        "VTC-USD", "INK-USD", "SKY-USD", "NULS-USD", "BTX-USD",
        "XBY-USD", "UKG-USD", "BAY-USD", "CND-USD", "CNX-USD",
        "UTK-USD", "PHX-USD", "MED-USD", "PLR-USD", "NAV-USD",
        "UBQ-USD", "MCO-USD", "BLOCK-USD", "R-USD", "ANT-USD",
        "CTR-USD", "BNT-USD", "SNM-USD", "CMT-USD", "MANA-USD",
        "COB-USD", "AST-USD", "DATA-USD", "ITC-USD", "EDG-USD",
        "RCN-USD", "SNGLS-USD", "TRIG-USD", "DNT-USD", "RLC-USD",
        "AMB-USD", "BRD-USD", "EMC2-USD", "PART-USD", "ADX-USD",
        "BURST-USD", "ETP-USD", "MOON-USD", "WABI-USD", "WINGS-USD"
)


# Период загрузки данных
start_date <- "2018-01-01"
end_date <- "2024-12-31"

# Пустой список для хранения данных
crypto_data <- list()

# Цикл для загрузки данных по каждой криптовалюте
for (symbol in crypto_symbols[1:200]) { # Ограничиваем 200 валютами
        tryCatch({
                # Загрузка данных
                data <- getSymbols(Symbols = symbol, src = "yahoo", 
                                   from = start_date, to = end_date, auto.assign = FALSE)
                # Преобразование в датафрейм
                data_df <- data.frame(Date = index(data), coredata(data))
                # Добавление символа
                data_df <- data_df %>% mutate(Symbol = symbol)
                # Сохранение в список
                crypto_data[[symbol]] <- data_df
                message(paste("Данные для", symbol, "загружены успешно."))
        }, error = function(e) {
                message(paste("Ошибка при загрузке данных для", symbol, ":", e$message))
        })
}

str(crypto_data)

# Устанавливаем библиотеки
library(dplyr)
library(rlang) 

result_df <- bind_rows(lapply(names(crypto_data), function(symbol) {
        df <- crypto_data[[symbol]]
        
        if (!is.null(df)) {
                currency_name <- strsplit(symbol, "-")[[1]][1]
                
                # Определяем названия столбцов
                close_col <- paste0(currency_name, ".USD.Close")
                volume_col <- paste0(currency_name, ".USD.Volume")
                adjusted_col <- paste0(currency_name, ".USD.Adjusted")
                
                # Проверяем, существуют ли все нужные столбцы
                if (all(c("Date", close_col, volume_col, adjusted_col) %in% names(df))) {
                        
                        # Переименовываем столбцы
                        df <- df %>%
                                rename_at(vars(close_col, volume_col, adjusted_col), ~ c("Close", "Volume", "Adjusted")) %>%
                                select(Date, Close, Volume, Adjusted) %>%
                                mutate(Currency = currency_name)
                        
                        return(df)
                }
        }
        
        # Возвращаем пустой датафрейм, если данные некорректны
        return(data.frame())
}))

# Просмотр первых строк
head(result_df)





unique(result_df$Currency)
unique(result_df$Date)

# Сохранение данных в CSV
#write.csv(result_df, "crypto_data_2018_2024.csv", row.names = FALSE)
message("Все данные успешно сохранены в файл crypto_data_2018_2024.csv")



crypto_data <- read.csv("crypto_data_2018_2024.csv", stringsAsFactors = FALSE)
str(crypto_data)
library(quantmod)
library(dplyr)

# Список дополнительных криптовалют
# Список дополнительных криптовалют
crypto_symbols_02 <- c(
        "Etherparty-USD", "FirstBlood-USD", "OST-USD", "HempCoin-USD", "PayPie-USD", 
        "Tierion-USD", "SIRINLABS-USD", "Peercoin-USD", "Counterparty-USD", "MobileGo-USD", 
        "SHIELD-USD", "DECENT-USD", "Horizen-USD", "LBRYCredits-USD", "Asch-USD", 
        "Modum-USD", "Melon-USD", "Eidoo-USD", "AgorasTokens-USD", "Rise-USD",
        "Blox-USD", "Metal-USD", "LATOKEN-USD", "QuantumResistantLedger-USD", "Wagerr-USD",
        "GenesisVision-USD", "CloakCoin-USD", "DecisionToken-USD", "Gifto-USD", "NewYorkCoin-USD",
        "Oyster-USD", "SpankChain-USD", "Viacoin-USD", "Groestlcoin-USD", "BlockmasonCreditProtocol-USD",
        "RevolutionVR-USD", "YOYOW-USD", "Delphy-USD", "Gulden-USD", "Cofound.it-USD",
        "adToken-USD", "Aeon-USD", "Pura-USD", "Shift-USD", "Monetha-USD",
        "Spectrecoin-USD", "COS-USD", "Presearch-USD", "Feathercoin-USD", "Viberate-USD",
        "WeTrust-USD", "Lunyr-USD", "Grid+-USD", "SaluS-USD", "Matchpool-USD",
        "Snovian.Space-USD", "Everex-USD", "SuperNET-USD", "Agrello-USD", "Jinn-USD",
        "Mercury-USD", "VIBE-USD", "NoLimitCoin-USD", "Rivetz-USD", "PotCoin-USD",
        "Blocktix-USD", "Zeusshield-USD", "PepeCash-USD", "Namecoin-USD", "Monolith-USD",
        "Pascal-USD", "Humaniq-USD", "Peerplays-USD", "bitCNY-USD", "Datum-USD",
        "Dimecoin-USD", "BlackCoin-USD", "HyperSpace-USD", "Diamond-USD", "Worldcore-USD",
        "BeanCash-USD", "Bounty0x-USD", "Propy-USD", "MoedaLoyaltyPoints-USD", "ColossusXT-USD",
        "TaaS-USD", "I/O Coin-USD", "NimiqExchangeToken-USD", "LoMoCoin-USD", "MinexCoin-USD",
        "SIBCoin-USD", "IXT-USD", "Stox-USD", "Paypex-USD", "Crown-USD",
        "XEL-USD", "MintCoin-USD", "GridCoin-USD", "SolarCoin-USD", "Neumark-USD",
        "Elixir-USD", "AirToken-USD", "Expanse-USD", "Myriad-USD", "Omni-USD",
        "HEAT-USD", "FairCoin-USD", "ION-USD", "Mothership-USD", "DomRaider-USD",
        "Flixxo-USD", "Phore-USD", "Golos-USD", "MonetaryUnit-USD", "WhiteCoin-USD",
        "Aeron-USD", "EarthCoin-USD", "Bodhi-USD", "Dovu-USD", "HiveterminalToken-USD",
        "Musicoin-USD", "Publica-USD", "OAX-USD", "ATBCoin-USD", "DeepOnion-USD",
        "Numeraire-USD", "KickToken-USD", "Rialto-USD", "Sprouts-USD", "Playkey-USD",
        "FoldingCoin-USD", "Donu-USD", "Lykke-USD", "OKCash-USD", "Voise-USD",
        "Radium-USD", "Maecenas-USD", "VeriCoin-USD", "DiviExchangeToken-USD", "TargetCoin-USD",
        "ClearPoll-USD", "Nexium-USD", "Primas-USD", "Credo-USD", "FedoraCoin-USD",
        "NuShares-USD", "ALIS-USD", "CircuitsOfValue-USD", "Zoin-USD", "Rubycoin-USD",
        "SteemDollars-USD", "LockTrip-USD", "Polybius-USD", "MetrixCoin-USD", "SunContract-USD",
        "SwarmCity-USD", "Change-USD", "Patientory-USD", "Pinkcoin-USD", "GlobalCurrencyReserve-USD",
        "Gambit-USD", "BCAP-USD", "DecentBet-USD", "Aventus-USD", "Solaris-USD",
        "ECC-USD", "Bismuth-USD", "Incent-USD", "Paragon-USD", "Mysterium-USD"
)


# API-ключ CoinMarketCap
api_key <- "b8e45962-1c22-486c-9869-8a97d1ecfda2"

# Определяем диапазон дат
start_date <- as.Date("2018-01-01")
end_date <- as.Date("2024-12-31")

# Создаем список всех дат (по дням)
date_list <- seq(start_date, end_date, by = "days")

# Функция для получения исторических данных за конкретную дату
get_crypto_data_cmc <- function(date) {
    # Форматируем дату
    date_str <- format(date, "%Y-%m-%d")
    
    # Формируем URL запроса
    url <- paste0("https://pro-api.coinmarketcap.com/v1/cryptocurrency/listings/historical?date=", date_str)
    
    # Отправляем запрос с API-ключом
    response <- GET(url, add_headers(`X-CMC_PRO_API_KEY` = api_key))
    
    # Проверяем статус ответа
    if (http_status(response)$category != "Success") {
        message(paste("Ошибка при загрузке данных за", date_str, "-", http_status(response)$message))
        return(NULL)
    }
    
    # Парсим JSON-ответ
    data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))

    # Проверяем, есть ли данные
    if (is.null(data$data)) {
        message(paste("Нет данных для", date_str))
        return(NULL)
    }

    # Преобразуем данные в DataFrame
    df <- data.frame(
        Date = date_str,
        Symbol = sapply(data$data, function(x) x$symbol),
        Close = sapply(data$data, function(x) x$quote$USD$price),
        Volume = sapply(data$data, function(x) x$quote$USD$volume_24h),
        Market_Cap = sapply(data$data, function(x) x$quote$USD$market_cap)
    )

    return(df)
}

# Итоговый DataFrame для всех дат
crypto_data_all <- bind_rows(lapply(date_list, get_crypto_data_cmc))

# Просматриваем первые строки
head(crypto_data_all)



# Сохранение данных в CSV
write.csv(result_df_new, "crypto_data_extended_2018_2024.csv", row.names = FALSE)
message("Обновленный набор данных успешно сохранен в файл crypto_data_extended_2018_2024.csv")

















library(tidyverse)
library(httr)
library(jsonlite)
library(writexl)


library(httr)
library(jsonlite)



crypto_list <- c(
        "potcoin", "blocktix", "zeusshield", "pepecash", "namecoin", "monolith", 
        "pascal", "humaniq", "peerplays", "bitcny", "datum", "dimecoin", "blackcoin", 
        "hyperspace", "diamond", "worldcore", "bean-cash", "bounty0x", "propy", 
        "moeda-loyalty-points", "colossusxt", "taas", "i-o-coin", "nimiq-exchange-token", 
        "lomocoin", "minexcoin", "sibcoin", "ixt", "stox", "paypex", "crown", "xel", 
        "mintcoin", "gridcoin", "solarcoin", "neumark", "elixir", "airtoken", "expanse", 
        "myriad", "omni", "heat", "faircoin", "ion", "mothership", "domraider", "flixxo", 
        "phore", "golos", "monetaryunit", "whitecoin", "aeron", "earthcoin", "bodhi", 
        "dovu", "hiveterminal-token", "musicoin", "publica", "oax", "atbcoin", "deeponion", 
        "numeraire", "kicktoken", "rialto", "sprouts", "playkey", "foldingcoin", "donu", 
        "lykke", "okcash", "voise", "radium", "maecenas", "vericoin", "divi-exchange-token", 
        "target-coin", "clearpoll", "nexium", "primas", "credo", "fedoracoin", "nushares", 
        "alis", "circuits-of-value", "zoin", "rubycoin", "steem-dollars", "locktrip", 
        "polybius", "metrix-coin", "suncontract", "swarm-city", "change", "patientory", 
        "pinkcoin", "global-currency-reserve", "gambit", "bcap", "decentbet", "aventus", 
        "solaris", "ecc", "bismuth", "incent", "paragon", "mysterium", "flo", "hush", 
        "quantum", "databits", "posw-coin", "e-coin", "stealth", "xenon", "ecobit", 
        "pandacoin", "blackmoon", "chrono-tech", "internet-of-people", "qwark", "ormeus-coin", 
        "energycoin", "lampix", "blue-protocol", "wild-crypto", "mybit", "clams", 
        "open-trading-network", "geocoin", "investfeed", "oraclechain", "straks", 
        "waves-community-token", "soarcoin", "autonio", "sportyco", "hedge", "obsidian", 
        "espers", "russian-miner-coin", "curecoin", "obits", "encrypgen", "verify", 
        "artbyte", "bitusd", "blockcat", "icos", "firstcoin", "bitsend", "vcash", 
        "boolberry", "cannabiscoin", "mercury-protocol", "spreadcoin", "primecoin"
)
get_crypto_data <- function(crypto_id, start_date, end_date) {
        url <- paste0("https://api.coingecko.com/api/v3/coins/", crypto_id, 
                      "/market_chart?vs_currency=usd&days=max&interval=daily")
        
        response <- GET(url)
        
        if (status_code(response) == 200) {
                data <- content(response, as = "parsed", type = "application/json")
                
                df <- tibble(
                        date = as.POSIXct(unlist(data$prices)[seq(1, length(data$prices), by = 2)] / 1000, origin = "1970-01-01"),
                        price = unlist(data$prices)[seq(2, length(data$prices), by = 2)]
                )
                
                df <- df %>%
                        filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>%
                        mutate(crypto = crypto_id)
                
                return(df)
        } else {
                return(NULL)
        }
}
start_date <- "2018-01-01"
end_date <- "2025-01-31"

crypto_data <- map_df(crypto_list, ~get_crypto_data(.x, start_date, end_date))
str(crypto_data)



response <- GET("https://api.coingecko.com/api/v3/coins/list")
crypto_list_full <- fromJSON(content(response, as = "text"))
head(crypto_list_full)
crypto_list_full$id <- tolower(crypto_list_full$id)
crypto_list_full$name <- tolower(crypto_list_full$name)
crypto_list_lower <- tolower(crypto_list)
valid_crypto_list <- crypto_list_full$id[crypto_list_full$id %in% crypto_list_lower]
length(valid_crypto_list)
head(valid_crypto_list)


library(readr)
selected_cryptos <- read_csv("selected_1000.csv")
selected_cryptos <- read_csv("selected_1000.csv")$id

str(selected_cryptos)

crypto_list_yahoo <- paste0(selected_cryptos, "-USD")
print(crypto_list_yahoo)  # Проверяем список тикеров



library(quantmod)
# Функция для проверки доступности криптовалют
check_crypto_availability <- function(ticker) {
        tryCatch({
                getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
                return(TRUE)
        }, error = function(e) {
                return(FALSE)
        })
}
# Проверяем список
available_cryptos <- crypto_list_yahoo[sapply(crypto_list_yahoo, check_crypto_availability)]
print(available_cryptos)







get_crypto_yahoo <- function(ticker, start_date, end_date) {
        tryCatch({
                data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
                df <- data.frame(Date = index(data), coredata(data))
                df$crypto <- ticker
                return(df)
        }, error = function(e) {
                warning(paste("Ошибка загрузки данных для", ticker))
                return(NULL)
        })
}


start_date <- "2018-01-01"
end_date <- "2025-01-31"

crypto_data_yahoo <- map_df(crypto_list_yahoo, ~get_crypto_yahoo(.x, start_date, end_date))
colnames(crypto_data_yahoo)
# Проверяем результат
str(crypto_data_yahoo)
head(crypto_data_yahoo)
write.csv(crypto_data_yahoo, "crypto_data_2018_2024_full.csv", row.names = FALSE)

crypto_data_yahoo_2 <- crypto_data_yahoo
# Заменяем NA на 0 во всех числовых столбцах
crypto_data_yahoo_2[is.na(crypto_data_yahoo_2)] <- 0
sum(is.na(crypto_data_yahoo_2))  # Должно вывести 0
str(crypto_data_yahoo_2)
colnames(crypto_data_yahoo_2)

library(dplyr)
library(tidyr)

# Определяем полный диапазон дат
full_dates <- tibble(Date = seq(as.Date("2018-01-01"), as.Date("2025-01-31"), by = "day"))


if ("crypto" %in% names(crypto_data_yahoo_2)) {
        crypto_data_yahoo_2 <- crypto_data_yahoo_2[, !(names(crypto_data_yahoo_2) %in% "crypto")]
}

# Преобразуем данные в длинный формат
crypto_data_long <- crypto_data_yahoo_2 %>%
        pivot_longer(
                cols = -Date,  
                names_to = c("Currency", "Metric"),
                names_pattern = "(.+)\\.USD\\.(.+)"
        )

# Проверяем дубликаты перед pivot_wider()
duplicates <- crypto_data_long %>%
        count(Date, Currency, Metric) %>%
        filter(n > 1)

if (nrow(duplicates) > 0) {
        print("Обнаружены дубликаты, они будут агрегированы по среднему значению.")
        
        crypto_data_long <- crypto_data_long %>%
                group_by(Date, Currency, Metric) %>%
                summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
}

# Разворачиваем в широкий формат
crypto_data_long <- crypto_data_long %>%
        pivot_wider(
                names_from = Metric, 
                values_from = value,
                values_fn = mean  # Агрегируем дубликаты по среднему значению
        ) %>%
        filter(!is.na(Close)) %>%
        mutate(Currency = toupper(Currency)) 

str(crypto_data_long)


library(dplyr)
library(tidyr)
print(colnames(crypto_data_long))

library(dplyr)
library(tidyr)

# Определяем полный диапазон дат
full_dates <- tibble(Date = seq(as.Date("2018-01-01"), as.Date("2024-12-31"), by = "day"))

# Проверяем структуру данных перед преобразованием
print(colnames(crypto_data_long))

library(dplyr)  # Загружаем dplyr

crypto_data_long <- crypto_data_long %>%
        dplyr::select(-High, -Low, -Open)  # Явное указание dplyr::select()

# Проверяем результат
print(colnames(crypto_data_long))
library(dplyr)

# Обрезаем данные с 1 января 2018 по 31 декабря 2024
crypto_data_long <- crypto_data_long %>%
        filter(Date >= as.Date("2018-01-01") & Date <= as.Date("2024-12-31"))

# Проверяем диапазон дат после фильтрации
range(crypto_data_long$Date)

library(writexl)
write_xlsx(crypto_data_long, "crypto_data_full.xlsx")  # Сохраняем в файл

str(crypto_data_long)

# Преобразуем Date в crypto_data, если он хранится как текст
crypto_data <- crypto_data %>%
        mutate(Date = as.Date(Date))

# Объединяем данные
crypto_data <- bind_rows(crypto_data, crypto_data_long)

# Проверяем диапазон дат после объединения
range(crypto_data$Date)
str(crypto_data)
unique(crypto_data$Currency)

write_xlsx(crypto_data, "crypto_data_full.xlsx") 
summary(crypto_data)
