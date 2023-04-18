library(readr)
library(fUnitRoots)
library(tseries)
library(forecast)
library(ggplot2)
library(readr)
library(readxl)
library(xts)

setwd("~/Lutero")

na_var <- "Consumo de Energia Elétrica (MWh)"

dados <- read_excel("consumo.xlsx", col_types = c("text", 
                                                  "numeric", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric"))

dados$data <- as.Date(paste(dados$Ano, 
                            dados$Mes, 
                            1, 
                            sep = "-"))

# Cria objeto Time Series
ST <- as.xts(ts(dados[, na_var], 
              start=c(2003, 1), 
              f=12))
colnames(ST) <- c('CEE')
plot(ST)

# Testes da estacionariedade da série
# KPSS
urkpssTest(ST, 
           type = c("tau"), 
           lags = c("short"),
           use.lag = NULL, 
           doplot = TRUE)

# Dickey-Fuller
adf.test(ST, alternative="stationary")

# ACF e ACF parcial
acf(ST)
pacf(ST)

# Busca dos parâmetros do SARIMA com base na otimização do AIC

# Transformação de BOX-COX
l <- BoxCox.lambda(ST)
ajuste <- auto.arima(ST, 
                     lambda = l, 
                     allowdrift = TRUE, 
                     allowmean = TRUE, 
                     seasonal = TRUE,
                     stepwise = TRUE,
                     approximation = FALSE,
                     trace = TRUE
) 
ajuste

previsao <- forecast(ajuste, h = 12)
plot(previsao)


autoplot(previsao, xlab = 'Data decimal', ylab = 'Consumo mensal')


# Plote o gráfico com a série original e estimativas
previsoes <- as.xts(as.data.frame(previsao$mean), 
                    order.by = seq(from = as.Date("2023-01-01"), 
                                   to = as.Date("2023-12-01"), 
                                   by = "month"))
colnames(previsoes) <- c(na_var)

valor_estimado <- as.xts(as.data.frame(ajuste$fitted), 
                           order.by = as.Date(dados$data))
colnames(valor_estimado) <- c("na_var")

valor_est_obs <- cbind(ST, 
                       valor_estimado, 
                       previsoes)

valor_est_obs<- consumo_est_obs["2005/"]

plot(valor_est_obs$CEE, 
     main = "Série original, estimativas e previsões", 
     xlab = "Data", 
     ylab = na_var, 
     col = "blue", 
     lwd = 1, 
     type = "l", 
     xaxt = "n")
lines(valor_est_obs$CEE.Estimado, 
      col = "red", 
      lwd = 1, 
      lty = 5)
lines(valor_est_obs$CEE.Estimado.1, 
      col = "green", 
      lwd = 1, 
      lty = 2)
