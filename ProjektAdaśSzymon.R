library(tseries) #biblioteka potrzebna do uzycia funkcji portfolio.optim (wagi w portfelu markowitza)
library(xtable) #biblioteka potrzebna do uzycia funkcji xtable (generowanie tabel do latexa)

#zadanie 1
dane_all <- read.csv2(file="data.csv") #wczytanie danych z pliku
dane_all[,1] <- as.Date(dane_all[,1],"%d.%m.%Y") #zmiana formatu pierwszej kolumny jako daty
dane_all[,-1] = lapply(dane_all[,-1], function(x) as.numeric(as.character(x))) #zmiana formatu pozostalych kolumn jako liczb
dates = as.Date(c("2017-01-01","2017-02-01","2017-03-01","2017-04-01","2017-05-01","2017-06-01","2017-07-01","2017-08-01","2017-09-01","2017-10-01","2017-11-01","2017-12-01",
                  "2018-01-01","2018-02-01","2018-03-01","2018-04-01","2018-05-01","2018-06-01","2018-07-01","2018-08-01","2018-09-01","2018-10-01","2018-11-01","2018-12-01",
                  "2019-01-01","2019-02-01","2019-03-01","2019-04-01","2019-05-01","2019-06-01","2019-07-01","2019-08-01","2019-09-01","2019-10-01","2019-11-01","2019-12-01","2020-01-01"))
dates = c(sapply(dates[-length(dates)], function(x) sum(dane_all[,1] < x) + 1),754)
#numery wierszy z pierwszym dniem kazdego miesiaca z wyjatkiem ostatniej obesrwacji gdzie numer wiersza jest z ostatniego dnia grudnia, poniewaz nie mamy cene zamkniecia ze stycznia 2020

return_all = dane_all[dates[-1],-1]/dane_all[dates[-length(dates)],-1] -1 #miesieczne proste stopy zwrotu dla poszczegolnch firm
mu_all = apply(return_all,2,mean) #srednie miesieczne stopy zwrotu
cov_all = cov(return_all) #macierz kowariancji z calego okresu dla miesiecznych stop zwrotu

mu_yearly = (dane_all[754,2:9]/dane_all[1,2:9])^(365/as.numeric(dane_all[754,1]-dane_all[1,1]))-1 #tworzymy tabele do latexa
xtable(as.data.frame(100*rbind(t(mu_all),mu_yearly),row.names = c("miesiÄ™czne", "roczne")), type = "latex")
xtable(as.data.frame(100*cov_all), type = "latex")
xtable(as.data.frame(100*cor(return_all)), type = "latex")



#zadanie 2
return = sapply(1:(dim(return_all)[1]-11), function(x) mean(apply(return_all[x:(x+11),],2,mean)))
#wektor srednich miesiecznych stop zwrotu z jednego roku wstecz dla wszystkich spolek
wagi_all <- matrix(0,ncol=8,nrow=25,dimnames = list(as.character(dane_all[dates[13:37],1]),colnames(return_all)))
for(i in 1:length(return)){
  wagi_all[i,] <- portfolio.optim(as.matrix(return_all[i:(i+11),]),return[i],shorts = TRUE)$pw
} #wyliczamy wagi

#rownowaznie wagi mozna policzyc bez portfolio.optim w ten sposob
markowitz <- function(return){ #funkcja wyliczajaca wagi dla zadanej macierzy stop zwrotu return
  m = mean(rowMeans(return))
  ones = matrix(rep(1,dim(return)[2]))
  mu = colMeans(return)
  sigma = solve(cov(return))
  
  A = (t(mu) %*% sigma %*% ones)[1,1]
  B = (t(mu) %*% sigma %*% mu)[1,1]
  C = (t(ones) %*% sigma %*% ones)[1,1]
  gamma=(C*m-A)/(B*C-A^2)
  delta=(B-A*m)/(B*C-A^2)
  return((gamma * mu  + delta * t(ones)) %*% sigma)
}
#a nastepnie tworzymy macierz wag dla naszych danych
weights = matrix(t(sapply(1:(dim(return_all)[1]-11), function(x) markowitz(return_all[x:(x+11),]))), ncol = 8, dimnames = list(as.character(dane_all[dates[13:37],1]),colnames(return_all))) 


portfelX <- 10000 #wyliczamy zysk z inwestycji z naszego portfela
for(i in 1:(length(dates)-13)){
  portfelX[i+1]<- portfelX[i]*sum(wagi_all[i,]*(1+return_all[12+i,]))
}

plot(dane_all[dates[13:37],1],portfelX, type='l', col='black', xlab = 'daty', ylab = 'wartosc portfela', lwd=2)



#Zad3
portfelY = 10000 #wartosc portfelaY
portfeleBrzegowe = matrix(rep(10000, 8), ncol = 8, dimnames = list(1,colnames(return_all)))
#wartosci portfeli brzegowych czyli inwestycji tylko w jedna spolke

for(i in 1:(length(dates)-13)){
  portfelY[i+1]<- portfelY[i]* rep(1/8, 8) %*% t(1 + return_all[12 + i,])
  portfeleBrzegowe = rbind(portfeleBrzegowe, matrix(portfeleBrzegowe[dim(portfeleBrzegowe)[1],]*(1+return_all[12+i,]), ncol = 8, dimnames = list(as.character(i+1),colnames(return_all))))
} #wyliczamy zysk portfela 1/8 oraz portfele brzegowe

plot(dane_all[dates[13:37],1],portfelX, type='l', col=1,ylim = c(7700,20000), xlab = 'czas', ylab = 'wartosc portfela', lwd=2)
lines(dane_all[dates[13:37],1],portfelY, col=2, lwd=2)
for(i in 1:dim(portfeleBrzegowe)[2]){
  lines(dane_all[dates[13:37],1],portfeleBrzegowe[,i], col=(2+i))
}
legend(dane_all[dates[12]+1,1],20500,legend=c("porfelX", "porfelY",colnames(dane_all)[2:9]),
       col=c(1:10), lty=1, cex=0.46)
a








portfel_ret_all <- matrix(c(portfelX[-1]/portfelX[-length(portfelY)]-1,portfelY[-1]/portfelY[-length(portfelY)]-1),ncol=2,dimnames=list(NULL,c("portfelX","portfelY")))
# powyzej mamy stopy zwrotu z portfela X i Y
portfel_mu_all <- apply(portfel_ret_all,2,mean)    # srednie stopy zwrotu z obydwu portfeli
portfel_var_all <- apply(portfel_ret_all,2,var)    # wariancje z miesiecznych st?p zwrotu z obydwu portfeli



#zad4
USbond <- read.csv2(file="USbond.csv",sep = ";")
USbond <- USbond[,1:2]
USbond[,1] <- as.Date(USbond[,1],"%d.%m.%Y")
colnames(USbond) <- c("data","zamkniecie")
USbond <- USbond[nrow(USbond):1,]    # notowana jest rentownosc obligacji. nie ich cena. Szczególy w pdf.
databond <- sapply(datess, function(x) sum(USbond[,1] < x) + 1)  # powtarzamy podzielenie na miesiace wedlug daty
databond[37] <- 919  # problem taki sam jak z notowaniami
stopa_all <- ((1 +(USbond[databond,2])/100)^(1/12) -1) # rentownosc obligacji 1 dnia kazdego miesiaca, w skali miesiecznej, dzielimy przez 100 poniewaz jest wynik procentowy
#stopa_all <- (USbond[databond,2]*(1/12))/100

sp <- read.csv2(file="S&P500.csv", sep = ",")
sp <- sp[,c(1,5)]
sp[,1] <- as.Date(sp[,1])
sp[,2] <- as.numeric(as.character(sp[,2]))
spdsz <- sp[-1,2]/sp[-dim(sp)[1],2] -1 
gielda_all <- sp[dates[-1],2]/sp[dates[-length(dates)],2] -1  #miesieczne stopy zwrotu z indeksu S&P 500
stopyzwrotu <- matrix(c(portfel_ret_all,gielda_all[13:length(gielda_all)],stopa_all[13:(length(stopa_all)-1)]),ncol=4,dimnames = list(NULL,c("portfelX","portfelY","gielda_all","stopa_all")))
#dostajemy macierz z miesiecznymi stopami zwrotu z porfela x,Y,S&P500 i inwestujac w obligacje dzisiecioletnie w USA, tutaj bierzemy poczatek kazdej miesiecznej inwestycji
odchs <-sd(gielda_all[13:36])  
#policzylismy odchylenia z miesiecznych rynkowych stóp zwrotu dla okresu, w których inwestujemy.
sharpe_all <- matrix(ncol=3,nrow = 1,dimnames = list(NULL,c("portfelX","portfelY","S&P500")))
for(i in 1:3){
  sharpe_all[,i] <- (mean(stopyzwrotu[,i])-mean(stopyzwrotu[,4]))/odchs  
} # policzylismy sharpe_ratio dla portfeli X,Y i S&P500
beta_allX <- c()
beta_allY <- c()
alfa_allX <- c()
alfa_allY <- c()
for(i in 1:24){
  CAPM <- lm((rowSums(return_all[i:(11+i),]*wagi_all[i,])-stopa_all[i:(11+i)])~(gielda_all[i:(11+i)]-stopa_all[i:(11+i)]))
  CAPM2 <- lm((rowSums(return_all[i:(11+i),]*(1/8))-stopa_all[i:(11+i)])~(gielda_all[i:(11+i)]-stopa_all[i:(11+i)]))
  alfa_allX[i] <- CAPM$coefficients[1]
  beta_allX[i] <- CAPM$coefficients[2]
  alfa_allY[i] <- CAPM2$coefficients[1]
  beta_allY[i] <- CAPM2$coefficients[2]
} # dla modelu CAPM liczymy bety i alfy
alfa_all <- matrix(c(alfa_allX,alfa_allY),ncol=2,dimnames = list(NULL,c("alfa_allX","alfa_allY")))
beta_all <- matrix(c(beta_allX,beta_allY),ncol=2,dimnames = list(NULL,c("beta_allX","beta_allY")))
result_CAPM <- matrix(ncol=2,nrow = 24)
result_CAPM <- alfa_all + beta_all*(stopyzwrotu[,3]-stopyzwrotu[,4])  # przewidywany wynik wedlug modelu CAPM
plot(result_CAPM[,1],type = "l")
lines(stopyzwrotu[,1], col="blue")
plot(result_CAPM[,2],type = "l")
lines(stopyzwrotu[,2], col="blue")
# czy moze lepiej to zilustrowac w formie tabelki/ albo w ogóle np. bledów?
# dodatkowe wykresy i wskazniki
# policzymy drawdown
portfelSP500 <- 10000
portfelbonds <- 10000
for(i in 2:25){
  portfelSP500[i] <-  portfelSP500[(i-1)]*(1+stopyzwrotu[(i-1),3])
  portfelbonds[i] <-  portfelbonds[(i-1)]*(1+stopyzwrotu[(i-1),4])
}
DrowdawnsX <- c()
DrowdawnsY <- c()
DrowdawnsSP500 <- c()
Drowdawnsbonds <- c()
for(i in 1:24){
  DrowdawnsX[i] <- max(portfelX[i]-portfelX[(i+1):25])
  DrowdawnsY[i] <- max(portfelY[i]-portfelY[(i+1):25])
  DrowdawnsSP500[i] <- max(portfelSP500[i]-portfelSP500[(i+1):25])
  Drowdawnsbonds[i] <- max(portfelbonds[i]-portfelbonds[(i+1):25])
}
Drowdawn <- apply(matrix(c(DrowdawnsX,DrowdawnsY,DrowdawnsSP500,Drowdawnsbonds),ncol=4),2,max)
# wykres zysków portfela X i Y, S&P500 i obligacji
plot(dane_all[dates[13:37],1],portfelX,type="l",col=1,xlab = "czas",ylab = "wartosc portfela")
lines(dane_all[dates[13:37],1],portfelY,col=2)
lines(dane_all[dates[13:37],1],portfelSP500,col=3)
lines(dane_all[dates[13:37],1],portfelbonds,col=4)
legend(dane_all[dates[12]+1,1],15940,legend=c("porfelX", "porfelY","portfelSP500","portfelbonds"),
       col=c(1,2,3,4), lty=1, cex=0.7)
a







#z







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