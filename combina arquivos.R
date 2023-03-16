# Ler Arquivos xlsx Consumo EMT

library(forecast)
library(xlsx)

obtemDado <- function(arquivo) {
  wb <- loadWorkbook(arquivo)
  
  sheets <- getSheets(wb)
  sheet <- sheets[[1]]
  
  df1 <-
    readRows(
      sheet,
      startRow = 5,
      endRow = length(getRows(sheet)),
      startColumn = 1,
      endColumn = 2
    )
  df1 <- as.data.frame(df1)

  df2 <-
    as.data.frame(readRows(
      sheet,
      startRow = 1,
      endRow = 2,
      startColumn = 2,
      endColumn = 2
    ))
  df1$ano <- as.numeric(df2[[1, 1]])
  df1$mes <- as.numeric(df2[[2, 1]])
  
  colnames(df1) <- c('agente', 'consumo', 'ano', 'mes')
  df1[df1$agente == 'EMT - ENERGISA MATO GROSSO - DISTRIBUIDORA DE ENERGIA S.A.', ]
}

dados <- data.frame(agente=as.Date(character()),
                    consumo=character(), 
                    ano=numeric(),
                    mes=numeric(),
                    stringsAsFactors=FALSE) 

# Subdiretório de trabalho
setwd("C:/Users/GPCMT/OneDrive/Orientações/Lutero/R")
la <- list.files('.', pattern ="*.xlsx", full.names = FALSE)
for (aux in la) {
  dados <- rbind(dados, obtemDado(aux))
}

write.csv(dados, 'dados.csv')

dados$data <- as.Date(paste(dados$ano, dados$mes, '1', sep = '-'))


ts_aux <-
  ts(as.numeric(dados[order(dados$data), 'consumo']),
     start = c(as.numeric(format(min(dados$data), '%Y')),
               as.numeric(format(min(dados$data), '%m'))),
     frequency = 12)

l <- BoxCox.lambda(ts_aux)

ajuste <- auto.arima(ts_aux, 
                     lambda = l, 
                     allowdrift = TRUE, 
                     allowmean = TRUE, 
                     seasonal.test = "seas",
                     stepwise=FALSE, # Afeta desempenho
                     approximation=FALSE
) 


previsao <- forecast(ajuste, h = 12)
autoplot(previsao, xlab = 'Data decimal', ylab = 'Consumo mensal')

autoplot(ts_aux)


# Anual 

dados_anual <- aggregate(as.numeric(consumo) ~ ano, 
                         dados,
                         FUN = sum,
                         na.action = na.pass)
colnames(dados_anual)[2] <- 'consumo'


ts_anual <-
  ts(as.numeric(dados_anual$consumo),
     start = c(min(dados_anual$ano)))

l <- BoxCox.lambda(ts_anual)

ajuste <- auto.arima(ts_anual, 
                     lambda = l, 
                     allowdrift = TRUE, 
                     allowmean = TRUE, 
                     seasonal.test = "seas",
                     stepwise=FALSE, # Afeta desempenho
                     approximation=FALSE
) 


previsao <- forecast(ajuste, h = 1)
autoplot(previsao, xlab = 'Data decimal', ylab = 'Consumo anual')

autoplot(ts_anual)
