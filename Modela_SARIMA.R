library(readr)
library(fUnitRoots)
library(tseries)
library(forecast)
library(ggplot2)
library(readr)
library(readxl)
library(xts)

# setwd("~/Lutero")

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

valor_est_obs <- cbind(ST, 
                       valor_estimado, 
                       previsoes)
colnames(valor_est_obs) <- c("CEE observado", 
                              "CEE estimado", 
                              "CEE previsto")
valor_est_obs<- valor_est_obs["2005/"]

plot(valor_est_obs$`CEE observado`, 
     main = "Série original, estimativas e previsões", 
     xlab = "Data", 
     ylab = na_var, 
     col = "blue", 
     lwd = 1, 
     type = "l", 
     xaxt = "n")
lines(valor_est_obs$`CEE estimado`, 
      col = "red", 
      lwd = 1, 
      lty = 5)
lines(valor_est_obs$`CEE previsto`, 
      col = "green", 
      lwd = 1, 
      lty = 2)

# -------------------------------------------------------------

library(ggplot2)

# Criar um data frame com as colunas necessárias
df <- data.frame(Data = index(valor_est_obs),
                 CEE_observado = as.vector(valor_est_obs$`CEE observado`),
                 CEE_estimado = as.vector(valor_est_obs$`CEE estimado`),
                 CEE_previsto = as.vector(valor_est_obs$`CEE previsto`))

# Usar ggplot2 para criar o gráfico e adicionar a legenda
g <- ggplot(df, aes(x = Data)) +
  geom_line(aes(y = CEE_observado, color = "CEE observado")) +
  geom_line(aes(y = CEE_estimado, color = "CEE estimado"), linetype = "dashed") +
  geom_line(aes(y = CEE_previsto, color = "CEE previsto"), linetype = "dotted") +
  labs(title = "Série original, estimativas e previsões",
       x = "Data",
       y = na_var) +
  scale_color_manual("", 
                     breaks = c("CEE observado", "CEE estimado", "CEE previsto"),
                     values = c("blue", "red", "green")) +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  theme(legend.position = "bottomright")

# Exibir gráfico
print(g)

