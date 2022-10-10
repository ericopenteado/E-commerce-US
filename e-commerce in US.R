setwd("~/Gávea")

library(tidyverse)
library(fredr)
library(magrittr)
library(zoo)
library(lubridate)
library(plotly)
library(seasonal)
library(tidyquant)
library(ggpubr)
library(patchwork)
library(png)   


fredr_set_key("86941f06099be97ec73d19596daec554")

dados <- fredr(
  series_id = "ECOMPCTNSA",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2022-01-01")
)

dados %<>% dplyr::select(date, value)

colnames(dados) = c("date", "US E-Commerce as % of Retail")

observation_start = as.Date("1999-10-01")
observation_end = as.Date("2022-01-01")

monthly = seq.Date(observation_start, observation_end, by = "m") %>% as.data.frame()

colnames(monthly) = "date"

a <- full_join(dados, monthly, by = "date")

a %<>% arrange(date)

a %<>%
  mutate(`US E-Commerce as % of Retail` = na.approx(`US E-Commerce as % of Retail`))

shade <-
  dados %>% dplyr::filter(date >= "2020-01-01")

ggplot()+
  geom_line(data = a, aes(x = date, y = `US E-Commerce as % of Retail`),col = "darkblue") +
  geom_rect(data = shade, aes(xmin = date %>% first, xmax = date %>% last, ymin = -Inf, ymax = Inf), alpha = 0.08) +
  labs(title ='US E-Commerce as % of Retail', x = NULL, y = "Value")

ind_max = which(a$`US E-Commerce as % of Retail` == a$`US E-Commerce as % of Retail` %>% max)

max_atingido = a[ind_max,]$date
max_values =  a[ind_max,]$`US E-Commerce as % of Retail`



ggplot()+
  geom_line(data = a, aes(x = date, y = `US E-Commerce as % of Retail`),lty = 1.2, col = "darkblue") +
  geom_rect(data = shade, aes(xmin = date %>% first, xmax = date %>% last, ymin = -Inf, ymax = Inf), alpha = 0.08) +
  labs(title ='US E-Commerce as % of Retail', x = NULL, y = "Value")+
  geom_text()+
  annotate("text", x = max_atingido + 2, y =  max_values + 1, size = 3,
           label = paste0("Máximo, ",max_atingido %>% format("%b de %Y")))



a %<>% mutate(mom = `US E-Commerce as % of Retail`/`US E-Commerce as % of Retail`%>%lag(1)-1)

ggplot()+
  geom_line(data = a, aes(x = date, y = `US E-Commerce as % of Retail`),lty = 1.2, col = "darkblue") +
  geom_rect(data = shade, aes(xmin = date %>% first, xmax = date %>% last, ymin = -Inf, ymax = Inf), alpha = 0.08) +
  labs(title ='US E-Commerce as % of Retail', x = NULL, y = "Value")+
  geom_text()+
  annotate("text", x = max_atingido + 2, y =  max_values + 1, size = 3,
           label = paste0("Máximo, ",max_atingido %>% format("%b de %Y")))


timeseries = ts(a %>% dplyr::select(-mom))

fit <- forecast::auto.arima(timeseries, seasonal = T)

forec = forecast::forecast(fit, h=3)

tickers = c('EBAY', 'AMZN')

prices_acoes = tq_get(tickers,
                      from = "2010-01-01",
                      to = Sys.Date(),
                      get = "stock.prices") %>% dplyr::select(date, symbol, close)


colnames(prices_acoes) = c("date", "Ticker", "Close Value")

ggplot()+
  geom_line(data = prices_acoes, aes(x = date, y =`Close Value`, col = Ticker))+
geom_rect(data = shade, aes(xmin = date %>% first, xmax = date %>% last, ymin = -Inf, ymax = Inf), alpha = 0.08)+
scale_color_brewer(palette = "Set1")+
labs(title ='Stock Prices - Amazon & Ebay', subtitle = '2 most influential E-commerce companies in US',x = NULL,
     caption = 'EconData Analytics')

prices_acoes %<>% mutate(ano = lubridate::year(date)) %>%
  group_by(ano, Ticker) %>%
  mutate(var_ano = mean(`Close Value`, na.rm  = T)) %>%
  dplyr::select(ano, var_ano)

prices_acoes %<>% distinct()

prices_acoes$ano %<>% make_date()

shade_2 <-
  prices_acoes %>% group_by(Ticker) %>% dplyr::filter(ano >= "2020-01-01") %>% unique

colnames(shade_2) = c("Ticker", "ano")

shade_2$ano %<>% as.Date()


ggplot()+
  geom_line(data = prices_acoes, aes(x = ano, y = var_ano, col = Ticker))+
  geom_rect(data = shade_2, aes(xmin = ano %>% first, xmax = ano %>% last, ymin = -Inf, ymax = Inf), alpha = 0.08)+
  scale_color_brewer(palette = "Set1")+
  labs(title ='Annual Mean - Amazon & Ebay', subtitle = '2 most influential E-commerce companies in US',x = NULL,
       caption = 'EconData Analytics')

