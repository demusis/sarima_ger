library(readr)
library(fUnitRoots)
library(tseries)
library(forecast)

dados <- Geracao_FV_EMT_2014_2022

# Cria objeto Time Series
ST <- ts(dados$Pot_Ins_kWp, 
              start=c(2015, 8), 
              f=12)
autoplot(ST)

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

# Se rejeitarmos a hipótese da estacionariedade, busca-se o valor de d.
# Calculando as correlações para uma diferença de ordem 1
acf(diff(ST, 
         differences=1))
pacf(diff(ST, 
          differences=1))

# Busca dos parâmetros do SARIMA com base na otimização do AIC

# Transformação de BOX-COX
# l <- BoxCox.lambda(ST)

ajuste <- auto.arima(ST, 
#                     lambda = l, 
                     allowdrift = TRUE, 
                     allowmean = TRUE, 
                     seasonal.test = "seas",
                     stepwise = FALSE,
                     approximation = FALSE,
                     trace = TRUE
) 
ajuste

previsao <- forecast(ajuste, h = 5)
autoplot(previsao, xlab = 'Data decimal', ylab = 'Consumo mensal')


