library(forecast)
  
serie = read.csv('SPX.csv', sep = ';')
serie$Date.Time = as.Date(serie$Date.Time, format="%d/%m/%Y")
  
  
plot(serie$Date.Time, serie$Open, type = "l", xlab = "Tempo", ylab = "Valor de Abertura", 
       col = "indianred4", xaxt = "n") 
  
title(main= list("Série Temporal do Valor de Abertura do SPX",
                   col= "royalblue4",
                   font= 3,
                   cex=1))
  
years = format(serie$Date.Time, "%Y")
unique_years = unique(years)
axis(1, at = as.Date(paste(unique_years, "-01-01", sep = "")), labels = unique_years)
  
# Objeto de série temporal
  
start_year = as.numeric(format(min(serie$Date.Time), "%Y"))
start_month = as.numeric(format(min(serie$Date.Time), "%m"))
start_day = as.numeric(format(min(serie$Date.Time), "%d"))
  
tsOpen = ts(serie$Open, start = c(start_month, start_day), frequency = 365)

# Decomposição

decomp_SPX = decompose(tsOpen)
  
# Tendência
  
plot(serie$Date.Time ,decomp_SPX$trend, type = "l",col="indianred4", ylab="Valor de Abertura", xlab = "Tempo", xaxt="n")
  
title(main= list("Tendência do Valor de Abertura do SPX",
                   col= "royalblue4",
                   font= 3,
                   cex=1))
  
  
axis(1, at=as.Date(paste(unique_years, "-01-01", sep = "")), labels=unique_years)
  


# Sazonalidade

plot(serie$Date.Time, decomp_SPX$seasonal, type="l", col = "indianred4", ylab = "sazonalidade", xlab = "Tempo", xaxt = "n")

title(main= list("Sazonalidade do Valor de Abertura do SPX",
                 col= "royalblue4",
                 font= 3,
                 cex=1))

axis(1, at=as.Date(paste(unique_years, "-01-01", sep = "")), labels=unique_years)

# Ruído

plot(serie$Date.Time, decomp_SPX$random, type="l", col = "indianred4", ylab = "Ruído", xlab = "Tempo", xaxt = "n")

title(main= list("Ruído do Valor de Abertura do SPX",
                 col= "royalblue4",
                 font= 3,
                 cex=1))

axis(1, at=as.Date(paste(unique_years, "-01-01", sep = "")), labels=unique_years)

# Estimação

## Crise de 2008

### ARIMA

# dados de treinamento: até dia 15/09/2008 (início da crise)
antesCrise = window(tsOpen, start = c(1, 2), end = c(6, 1688 - sum(365, 365, 366, 365, 365 - 1)))

auto_arimaCrise = auto.arima(antesCrise)

forecast_crise = forecast(auto_arimaCrise, h=365)

plot(forecast_crise)

lines(window(tsOpen, end = end(tsOpen), start = end(antesCrise)), col = "red")

legend("topleft", legend=c("Previsão", "Dados Reais"), col=c("blue", "red"), lty=1)


### Suavização exponencial

# Ajustar o modelo de suavização exponencial
hw_model = HoltWinters(antesCrise)

forecast_hw = forecast(hw_model, h=365)

plot(forecast_hw, xlab="Tempo", ylab="Valor de Abertura", main="Previsão Suavização Exponencial e Dados Reais")

lines(window(tsOpen, end = end(tsOpen), start = end(antesCrise)), col = "red")

legend("topleft", legend=c("Previsão Suavização Exponencial", "Dados Reais"), col=c("blue", "red"), lty=1)


## Pandemia

### ARIMA

# dados de treinamento: até dia 11/03/2020 (início da pandemia)
antesPandemia = window(tsOpen, start = c(1, 2), end = c(6, 4581 - sum(365, 365, 366, 365, 365 - 1)))

auto_arimaPandemia = auto.arima(antesPandemia)

forecast_pandemia = forecast(auto_arimaPandemia, h=365)

plot(forecast_pandemia)

lines(window(tsOpen, end = end(tsOpen), start = end(antesPandemia)), col = "red")

legend("topleft", legend=c("Previsão", "Dados Reais"), col=c("blue", "red"), lty=1)

### Suavização exponencial

hw_pandemia = HoltWinters(antesPandemia)

forecast_expPandemia = forecast(hw_pandemia, h=365)

plot(forecast_expPandemia, xlab="Tempo", ylab="Valor de Abertura", main="Previsão Suavização Exponencial e Dados Reais")

lines(window(tsOpen, end = end(tsOpen), start = end(antesPandemia)), col = "red")

legend("topleft", legend=c("Previsão Suavização Exponencial", "Dados Reais"), col=c("blue", "red"), lty=1)







