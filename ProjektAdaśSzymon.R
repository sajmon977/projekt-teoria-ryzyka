library(tseries)

#zadanie 1
dane_all <- read.csv2(file="data.csv")
dane_all[,1] <- as.Date(dane_all[,1],"%d.%m.%Y")   #wczytanie pierwszej kolumny jako daty
dane_all[,-1] <- lapply(dane_all[,-1], function(x) as.numeric(as.character(x))) #wczytanie danych jako liczb
datess = as.Date(c("2017-01-01","2017-02-01","2017-03-01","2017-04-01","2017-05-01","2017-06-01","2017-07-01","2017-08-01","2017-09-01","2017-10-01","2017-11-01","2017-12-01",
          "2018-01-01","2018-02-01","2018-03-01","2018-04-01","2018-05-01","2018-06-01","2018-07-01","2018-08-01","2018-09-01","2018-10-01","2018-11-01","2018-12-01",
          "2019-01-01","2019-02-01","2019-03-01","2019-04-01","2019-05-01","2019-06-01","2019-07-01","2019-08-01","2019-09-01","2019-10-01","2019-11-01","2019-12-01","2020-01-01"))
dates = sapply(datess, function(x) sum(dane_all[,1] < x) + 1) # numery wierszy z pierwszym dniem kazdego miesiaca
dates[37]<- 754 # poniewaz nie mamy obserwacji ze stycznia 2020, bierzemy cene zamkniecia z ostatniego dnia grudnia

return_all <- dane_all[dates[-1],-1]/dane_all[dates[-length(dates)],-1] -1 # miesieczne proste stopy zwrotu dla poszczegolnch firm
mu_all <- apply(return_all[,-1],2,mean)  # srednie miesieczne stopy zwrotu
cov_all <- cov(return_all)     # macierz kowariancji z calego okresu dla miesiecznych stop zwrotu

#zadanie 2
return <- c()
for(i in 1:(dim(return_all)[1]-11)){
  return[i] <- mean(apply(return_all[i:(i+11),],2,mean))
}    # wektor srednich miesiecznych stop zwrotu z 1 roku wstecz dla wszystkich spolek
portfelX<- 10000   # kapital poczatkowy
wagi_all <- matrix(0,ncol=8,nrow=24,dimnames = list(NULL,colnames(return_all)))
for(i in 1:(length(dates)-13)){
  wagi_all[i,] <- portfolio.optim(as.matrix(return_all[i:(i+11),]),return[i],shorts = TRUE)$pw
  portfelX[i+1]<- portfelX[i]*sum(wagi_all[i,]*(1+return_all[12+i,])) 
} # wyliczamy wagi i zysk naszego portfela dla kolejnych okresow

#rownowaznie mozna liczyc wagi bez portfolio.optim w ten sposob
markowitz <- function(return){
  m = mean(rowMeans(return))
  ones = matrix(rep(1,dim(return)[2]))
  mu = colMeans(return)
  sigma = solve(cov(return))
  
  A = (t(mu) %*% sigma %*% ones)[1,1]
  B = (t(mu) %*% sigma %*% mu)[1,1]
  C = (t(ones) %*% sigma %*% ones)[1,1]
  Dev = sqrt((C*return^2-2*A*return+B)/(B*C-A^2))
  gamma=(C*m-A)/(B*C-A^2)
  delta=(B-A*m)/(B*C-A^2)
  return((gamma * mu  + delta * t(ones)) %*% sigma)
}

weights = c()
for(i in 1:24){
  weights <- c(weights,markowitz(return_all[i:(i+11),]))
}
weights = matrix(weights, ncol = dim(return_all)[2], dimnames = list(as.character(dates[13:36]),colnames(return_all)), byrow = T)

#Zad3
portfelY <- 10000   # poczatkowa wartosc portfelaY
wagiY <- rep(1/8,8)  # wagi portfela Y
for(i in 1:(length(dates)-13)){
  portfelY[i+1]<- portfelY[i]*sum(wagiY*(1+return_all[12+i,])) 
}   # wyliczamy zysk portfela 1/8
portfel_ret_all <- matrix(c(portfelX[-1]/portfelX[-length(portfelY)]-1,portfelY[-1]/portfelY[-length(portfelY)]-1),ncol=2,dimnames=list(NULL,c("portfelX","portfelY")))
# powyzej mamy stopy zwrotu z portfela X i Y
portfel_mu_all <- apply(portfel_ret_all,2,mean)    # srednie stopy zwrotu z obydwu portfeli
portfel_var_all <- apply(portfel_ret_all,2,var)    # wariancje z miesiecznych stóp zwrotu z obydwu portfeli

#zad4
USbond <- read.csv2(file="USbond.csv",sep = ";")
USbond <- USbond[,1:2]
USbond[,1] <- as.Date(USbond[,1],"%d.%m.%Y")
colnames(USbond) <- c("data","zamkniecie")
USbond <- USbond[nrow(USbond):1,]    # notowana jest rentownosc obligacji. nie ich cena. Szczególy w pdf.
databond <- sapply(datess, function(x) sum(USbond[,1] < x) + 1)  # powtarzamy podzielenie na miesiace wedlug daty
databond[37] <- 919  # problem taki sam jak z notowaniami
stopa_all <- (USbond[databond,2]*(1/12))/100 # rentownosc obligacji 1 dnia kazdego miesiaca, w skali miesiecznej, dzielimy przez 100 poniewaz jest wynik procentowy

sp <- read.csv2(file="S&P500.csv", sep = ",")
sp <- sp[,c(1,5)]
sp[,1] <- as.Date(sp[,1])
sp[,2] <- as.numeric(as.character(sp[,2]))
spdsz <- sp[-1,2]/sp[-dim(sp)[1],2] -1 
gielda_all <- sp[dates[-1],2]/sp[dates[-length(dates)],2] -1  #miesieczne stopy zwrotu z indeksu S&P 500
stopyzwrotu <- matrix(c(portfel_ret_all,gielda_all[13:length(gielda_all)],stopa_all[13:(length(stopa_all)-1)]),ncol=4,dimnames = list(NULL,c("portfelX","portfelY","gielda_all","stopa_all")))
#dostajemy macierz z miesiecznymi stopami zwrotu z porfela x,Y,S&P500 i inwestujac w obligacje dzisiecioletnie w USA, tutaj bierzemy poczatek kazdej miesiecznej inwestycji
odchs <- c()
for(i in 1:24){
  odchs[i] <-sd(gielda_all[i:(11+i)])  
}   #policzylismy odchylenia z miesiecznych stóp zwrotu dla poszczególnych okresów, w których inwestujemy.

sharpe_all <- matrix(ncol=3,nrow = 24,dimnames = list(NULL,c("portfelX","portfelY","S&P500")))
for(i in 1:3){
  sharpe_all[,i] <- (stopyzwrotu[,i]-stopyzwrotu[,4])/odchs  
} # policzylismy sharpe_ratio dla portfeli X,Y i S&P500
beta_allX <- c()
beta_allY <- c()
alfa_allX <- c()
alfa_allY <- c()
for(i in 1:12){
  CAPM <- lm((stopyzwrotu[i:(11+i),1]-stopyzwrotu[i:(11+i),4])~(stopyzwrotu[i:(11+i),3]-stopyzwrotu[i:(11+i),4]))
  CAPM2 <- lm((stopyzwrotu[i:(11+i),2]-stopyzwrotu[i:(11+i),4])~(stopyzwrotu[i:(11+i),3]-stopyzwrotu[i:(11+i),4]))
  alfa_allX[i] <- CAPM$coefficients[1]
  beta_allX[i] <- CAPM$coefficients[2]
  alfa_allY[i] <- CAPM2$coefficients[1]
  beta_allY[i] <- CAPM2$coefficients[2]
}
alfa_all <- matrix(c(alfa_allX,alfa_allY),ncol=2,dimnames = list(NULL,c("alfa_allX","alfa_allY")))
beta_all <- matrix(c(beta_allX,beta_allY),ncol=2,dimnames = list(NULL,c("beta_allX","beta_allY")))

# to jest na razie zle we wtorek sie dowiem jak to poprawic

 
#z



# dokladnie sharpe_ratio po prostu stopa zwrotu miesieczna?
# model CAPM mamy 24 stopy zwrotu czyli mozemy tylko dla ostatniego roku sprawdzac jak dziala CAPM?









#RYNKOWA STOPA ZWROTU S&P500?
#MIESIECZNA STOPA ZWROTU Z OBLIGACJI, PRZEMNOZENIE RAZY LICZBA DNI?
#ODCHYLENIE MIESIAC OBSERWACJI?

# portfel brzegowy wszystko w jedna spólke?
# co z miesiecznymi stopami zwrotu (ostatnia)
# skroty na gieldzie 2-3-4 literowe
# portfolio.optim ok, czy liczyc??? na piechote?
# srednia z 12 miesiecy z 8 spólek?
# dlugosc opisu?
# xtable tabela w latexie
# wstep, opis spólek, macierz korelacji, jak dziala nasza strategia, porównanie portfeli, wag
# drowdawn, krzywa efektywnosci, legendy, analiza CAPM, podsumowanie, analiza ryzyka, linie (SML, CML)?
# tabelki z wagami, zwrotami, ryzyko itp. Dlaczego ostatnia stopa zwrotu jest z 31 grudnia? Wytlumaczyc stopy zwrotu
# dobranie calego portfela z akcji moze byc trudne (duze koszty transakcji), ale mozemy "kupic" kontrakt
# sharpe ratio z wspólczynnikiem wolnym od ryzyka.Beta w regresji liniowej. Duze zmiany bety- model jest niestabilny.