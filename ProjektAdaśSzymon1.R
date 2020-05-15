library(tseries) #biblioteka potrzebna do uzycia funkcji portfolio.optim (wagi w portfelu markowitza)
library(xtable) #biblioteka potrzebna do uzycia funkcji xtable (generowanie tabel do latexa)
library(quantmod) #biblioteka potrzebna do uzycia funkcji rollapply



#zadanie 1
dane_all <- read.csv2(file="data.csv") #wczytanie danych z pliku
dane_all[,1] <- as.Date(dane_all[,1],"%d.%m.%Y") #zmiana formatu pierwszej kolumny jako daty
dane_all[,-1] = lapply(dane_all[,-1], function(x) as.numeric(as.character(x))) #zmiana formatu pozostalych kolumn jako liczb
datess = as.Date(c("2017-01-01","2017-02-01","2017-03-01","2017-04-01","2017-05-01","2017-06-01","2017-07-01","2017-08-01","2017-09-01","2017-10-01","2017-11-01","2017-12-01",
                  "2018-01-01","2018-02-01","2018-03-01","2018-04-01","2018-05-01","2018-06-01","2018-07-01","2018-08-01","2018-09-01","2018-10-01","2018-11-01","2018-12-01",
                  "2019-01-01","2019-02-01","2019-03-01","2019-04-01","2019-05-01","2019-06-01","2019-07-01","2019-08-01","2019-09-01","2019-10-01","2019-11-01","2019-12-01","2020-01-01"))
dates = c(sapply(datess[-length(datess)], function(x) sum(dane_all[,1] < x) + 1),754)
#numery wierszy z pierwszym dniem kazdego miesiaca z wyjatkiem ostatniej obesrwacji gdzie numer wiersza jest z ostatniego dnia grudnia, poniewaz nie mamy ceny zamkniecia ze stycznia 2020

return_all = as.data.frame(dane_all[dates[-1],-1]/dane_all[dates[-length(dates)],-1] -1, row.names = as.character(dane_all[dates[-1],1]))
#miesieczne proste stopy zwrotu dla poszczegolnch spolek
mu_all = apply(return_all,2,mean) #srednie miesieczne stopy zwrotu
cov_all = cov(return_all) #macierz kowariancji z calego okresu dla miesiecznych stop zwrotu

mu_yearly = (dane_all[754,2:9]/dane_all[1,2:9])^(365/as.numeric(dane_all[754,1]-dane_all[1,1]))-1 #roczna stopa z inwestycji w kazda ze spolek liczona tak jak dla obligacji, a nie jako sredni zwrot
xtable(as.data.frame(100*rbind(t(mu_all),mu_yearly),row.names = c("miesieczne", "roczne")), type = "latex") #srednie stopy zwrotu
xtable(as.data.frame(100*cov_all), type = "latex") #macierz kowariancji
xtable(as.data.frame(100*cor(return_all)), type = "latex") #macierz korelacji



#zadanie 2
return = sapply(1:(dim(return_all)[1]-11), function(x) mean(apply(return_all[x:(x+11),],2,mean))) #wektor srednich miesiecznych stop zwrotu z jednego roku wstecz dla wszystkich spolek
wagi_all <- matrix(0,ncol=8,nrow=25,dimnames = list(as.character(dane_all[dates[13:37],1]),colnames(return_all)))
for(i in 1:length(return)){ #wyliczamy wagi dla portfelu markowitza
  wagi_all[i,] <- portfolio.optim(as.matrix(return_all[i:(i+11),]),return[i],shorts = TRUE)$pw
}

#rownowaznie wagi mozna policzyc bez portfolio.optim w ten sposob
markowitz <- function(return, return_mABC){ #funkcja wyliczajaca wagi dla zadanej macierzy stop zwrotu return, w zaleznosci od return_mABC zwroci albo wagi albo m,A,B,C
  m = mean(rowMeans(return))
  ones = matrix(rep(1,dim(return)[2]))
  mu = colMeans(return)
  sigma = solve(cov(return))
  
  A = (t(mu) %*% sigma %*% ones)[1,1]
  B = (t(mu) %*% sigma %*% mu)[1,1]
  C = (t(ones) %*% sigma %*% ones)[1,1]
  gamma=(C*m-A)/(B*C-A^2)
  delta=(B-A*m)/(B*C-A^2)
  
  if(return_mABC){
    return(c(m, A, B, C))
  }else{
    return((gamma * mu  + delta * t(ones)) %*% sigma)
  }
}

#a nastepnie tworzymy macierz wag dla naszych danych
wagi_all = matrix(t(sapply(1:(dim(return_all)[1]-11), function(x) markowitz(return_all[x:(x+11),],FALSE))), ncol = 8, dimnames = list(as.character(dane_all[dates[13:37],1]),colnames(return_all))) 

xtable(as.data.frame(t(wagi_all[1,]),row.names = c("waga")), type = "latex") #tabela wag z pierwszego okresu
xtable(as.data.frame(t(apply(wagi_all,2,mean)), row.names = c("waga")), type = "latex") #tabela srednich wartosci wag
xtable(as.data.frame(t(apply(abs(wagi_all[-1,]-wagi_all[-dim(wagi_all)[1],]),2,mean)), row.names = c("zmiana wag")), type = "latex") #tabela srednich zmian wag

portfelX <- 10000 #wyliczamy wartosc naszego portfela w czasie
for(i in 1:(dim(wagi_all)[1]-1)){
  portfelX[i+1]<- portfelX[i]*sum(wagi_all[i,]*(1+return_all[12+i,]))
}



#zadanie 3
portfelY = 10000 #wartosc portfelaY (portfel 1/8)
portfeleBrzegowe = matrix(rep(10000, 8), ncol = 8, dimnames = list(1,colnames(return_all))) #wartosci portfeli brzegowych czyli inwestycji tylko w jedna spolke

for(i in 1:(length(dates)-13)){ #wyliczamy wartosc portfela 1/8 oraz portfeli brzegowych
  portfelY[i+1]<- portfelY[i]* rep(1/8, 8) %*% t(1 + return_all[12 + i,])
  portfeleBrzegowe = rbind(portfeleBrzegowe, portfeleBrzegowe[dim(portfeleBrzegowe)[1],]*(1+return_all[12+i,]),make.row.names=FALSE)
}
rownames(portfeleBrzegowe) = as.character(dane_all[dates[13:37],1]) #zmiana nazw wierszy na daty

plot(dane_all[dates[13:37],1],portfelX, type='l', col=rainbow(10)[1], ylim = c(7000,20000), xlim = c(dane_all[dates[13],1],dane_all[dim(dane_all)[1],1] + 320), xlab = 'czas', ylab = 'wartosc portfela', cex.lab = 1.5, lwd=3)
#stworzenie wykresu i naniesienie portfelX
lines(dane_all[dates[13:37],1],portfelY, col=rainbow(10)[2], lwd=3) #dodanie portfelY
for(i in 1:dim(portfeleBrzegowe)[2]){ #dodanie portfeli brzegowych do wykresu
  lines(dane_all[dates[13:37],1],portfeleBrzegowe[,i], col=rainbow(10)[2+i])
}
legend(dane_all[dim(dane_all)[1],1]-50,22000,legend=c("porfelX", "porfelY",colnames(dane_all)[2:9]),col=rainbow(10), lwd = c(3,3,rep(1,8)), lty=1, y.intersp = 0.3, x.intersp = 0.3, cex = 2, bty = 'n')

portfel_ret_all = matrix(c(portfelX[-1]/portfelX[-length(portfelY)]-1,portfelY[-1]/portfelY[-length(portfelY)]-1),ncol=2,dimnames=list(as.character(dane_all[dates[14:37],1]),c("portfelX","portfelY")))
portfeleBrzegowe_ret_all = portfeleBrzegowe[-1,]/portfeleBrzegowe[-dim(portfeleBrzegowe)[1],]-1 #obliczamy stopy zwrotu z portfela X, Y oraz portfeli brzegowych
portfel_mu_all = apply(portfel_ret_all,2,mean)
portfeleBrzegowe_mu_all = apply(portfeleBrzegowe_ret_all,2,mean) #srednie stopy zwrotu z portfeli
portfel_var_all = apply(portfel_ret_all,2,var)
portfeleBrzegowe_var_all = apply(portfeleBrzegowe_ret_all,2,var) #wariancje ze stop zwrotu z portfeli

#wykres wariancji vs sredni zwrot dla wszystkich portfeli
plot(c(portfel_var_all,portfeleBrzegowe_var_all), c(portfel_mu_all, portfeleBrzegowe_mu_all), xlim=c(0.001,0.021), pch=19, col = rainbow(10), xlab = 'wariancja', ylab = 'sredni zwrot', cex.lab = 1.5, lwd=c(14,14,rep(8,8)))
legend(0.017, 0.028, legend=c("porfelX", "porfelY", colnames(dane_all)[2:9]), col=rainbow(10), pt.lwd = c(6,6,rep(0.8,8)), pch=19, y.intersp = 0.2, x.intersp = 0.3, cex = 2, bty = 'n')

returns = as.matrix(return_all[1:12,]) %*% matrix(c(wagi_all[1,],rep(1/8,8),diag(8)), dimnames = list(NULL,c("portfelX", "portfelY", colnames(return_all))) ,ncol=10)
#zwrot z portfeli w 2017
param = markowitz(return_all[1:12,],TRUE) #m A B C tak jak w portfelu markowitza (funkcja markowitz) wyliczony dla pierwszego roku
Mean = seq(0,1.5*max(colMeans(return_all[1:12,])),by=1.5*max(colMeans(return_all[1:12,]))/256) #rowno rozlozone punkty
Dev = (param[4]*Mean^2-2*param[2]*Mean+param[3])/(param[3]*param[4]-param[2]^2) #wariancja do krzywej efektywnosci z Mean

plot(Dev,Mean, type="l", xlab = 'wariancja', ylab = 'sredni zwrot', xlim = c(0.0004, 0.006), lwd = 4) #rysuje krzywa efektywnosci
points(apply(returns,2,var), apply(returns,2,mean), pch=19, col = rainbow(10), lwd = c(14,14,rep(8,8))) #nanosi wszystkie portfele
legend(0.0037, 0.06, legend=c("krzywa efektywnosci","porfelX", "porfelY",colnames(dane_all)[2:9]), col=c(1,rainbow(10)), lwd = 4, seg.len = 0.5, pt.lwd = c(0,6,6,rep(0.8,8)), lty = c(1,rep(NA,10)), pch=c(NA,rep(19,10)), y.intersp = 0.2, x.intersp = 0.3, cex = 2, bty = 'n')



#zadanie 4
USbond <- read.csv2(file="USbond.csv",sep = ";") #wczytanie danych rentownosci obligacji
USbond <- USbond[,1:2] # pozostawienie tylko dat i zamkniecia
USbond[,1] <- as.Date(USbond[,1],"%d.%m.%Y") # wczytanie 1-szej kolumny jako data
colnames(USbond) <- c("data","zamkniecie") # dodanie nazw kolumn
USbond <- USbond[nrow(USbond):1,]    # notowana jest rentownosc obligacji. Byly w odwrotnej kolejnosci wiec je zamieniamy
databond <- sapply(datess, function(x) sum(USbond[,1] < x) + 1)  # wektor gdzie kolejne wiersze sa poczatkiem kolejnej inwestycji, czyli poczatek kazdego miesiaca
databond[37] <- 919  # problem taki sam jak z notowaniami
stopa_all <- ((1 +(USbond[databond,2])/100)^(1/12) -1) # rentownosc obligacji 1 dnia kazdego miesiaca, po przeliczeniu na skale miesieczna, dzielimy przez 100 poniewaz jest wynik procentowy

sp <- read.csv2(file="S&P500.csv", sep = ",") # wczytanie danych indeksu S&P500
sp <- sp[,c(1,5)]   # zostawienie tylko daty i wartosci zamkniecia
sp[,1] <- as.Date(sp[,1]) # wczytanie 1-szej kolumny jako data 
sp[,2] <- as.numeric(as.character(sp[,2])) # wczytanie 2-giej kolumny jako liczba
spdsz <- sp[-1,2]/sp[-dim(sp)[1],2] -1 # policzenie dziennych stóp zwrotu z S&P500
gielda_all <- sp[dates[-1],2]/sp[dates[-length(dates)],2] -1  #miesieczne stopy zwrotu z indeksu S&P 500
stopyzwrotu <- matrix(c(portfel_ret_all,gielda_all[13:length(gielda_all)],stopa_all[13:(length(stopa_all)-1)]),ncol=4,dimnames = list(NULL,c("portfelX","portfelY","gielda_all","stopa_all")))
#dostajemy macierz z miesiecznymi stopami zwrotu z porfela x,Y,S&P500 i inwestujac w obligacje dzisiecioletnie w USA, tutaj bierzemy poczatek kazdej miesiecznej inwestycji

plot(dane_all[dates[13:37],1],portfelX,type="l",col=1,xlab = "czas",ylab = "wartosc portfela") # wykres zyskow portfela X 
lines(dane_all[dates[13:37],1],portfelY,col=2) # dorysowanie zyskow portfela Y
lines(dane_all[dates[13:37],1],portfelSP500,col=3)# dorysowanie zyskow portfela  S&P500 
lines(dane_all[dates[13:37],1],portfelbonds,col=4)# dorysowanie zyskow portfela obligacji
legend(dane_all[dates[12]+1,1],15940,legend=c("porfelX", "porfelY","portfelSP500","portfelbonds"),
       col=c(1,2,3,4), lty=1, cex=0.7) # dodanie legendy

odchs <-sd(gielda_all[13:36]) #policzylismy odchylenia z miesiecznych rynkowych stop zwrotu dla okresu, w ktorych inwestujemy.
sharpe_all <- matrix(ncol=3,nrow = 1,dimnames = list(NULL,c("portfelX","portfelY","S&P500"))) # pusta macierz do której bedziemy wpisywali wartosci sharpe ratio z naszych portfeli
Sharpe_brzeg <- matrix(ncol=8,nrow = 1,dimnames = list(NULL,colnames(portfeleBrzegowe_ret_all)))  # pusta macierz do której bedziemy wpisywali wartosci sharpe ratio z naszych portfeli brzegowych
for(i in 1:3){
  sharpe_all[,i] <- (mean(stopyzwrotu[,i])-mean(stopyzwrotu[,4]))/odchs  
} # policzylismy sharpe_ratio dla portfeli X,Y i S&P500
for(i in 1:8){
  Sharpe_brzeg[,i] <- (mean(portfeleBrzegowe_ret_all[,i])-mean(stopyzwrotu[,4]))/odchs
} # Sharpe_ratio dla portfeli brzegowych
sharpe<- matrix(nrow=2,ncol=11) # pusta macierz na wszystkie sharpe ratio
sharpe[2,] <- c(sharpe_all,Sharpe_brzeg) # wpisanie wartosci do powyzszych macierzy
sharpe[1,] <- c(colnames(sharpe_all),colnames(Sharpe_brzeg)) # dodanie nazw do maciery sharpe ratio
xtable(sharpe) # tabelka do latexa 

#CAPM, zle, liczone stopy zwrotu z portfela na podstawie praktycznych stop zwrotu z rynku, szczególy w pdf-ie.
beta_all1X <- c() # pusty wektor na CAPM
beta_all1Y <- c() # pusty wektor na CAPM
alfa_all1X <- c() # pusty wektor na CAPM
alfa_all1Y <- c() # pusty wektor na CAPM
for(i in 1:24){
  CAPM1 <- lm((rowSums(return_all[i:(11+i),]*wagi_all[i,])-stopa_all[i:(11+i)])~(gielda_all[i:(11+i)]-stopa_all[i:(11+i)]))
  CAPM21 <- lm((rowSums(return_all[i:(11+i),]*(1/8))-stopa_all[i:(11+i)])~(gielda_all[i:(11+i)]-stopa_all[i:(11+i)]))
  alfa_all1X[i] <- CAPM1$coefficients[1]
  beta_all1X[i] <- CAPM1$coefficients[2]
  alfa_all1Y[i] <- CAPM21$coefficients[1]
  beta_all1Y[i] <- CAPM21$coefficients[2]
} # dla modelu CAPM liczymy bety i alfy
alfa_all1 <- matrix(c(alfa_all1X,alfa_all1Y),ncol=2,dimnames = list(NULL,c("alfa_allX","alfa_allY"))) # zapisanie wartosci alfa i beta CAPM-u dla portfelaX
beta_all1 <- matrix(c(beta_all1X,beta_all1Y),ncol=2,dimnames = list(NULL,c("beta_allX","beta_allY"))) # zapisanie wartosci alfa i beta CAPM-u dla portfelaY
result_CAPM1 <- matrix(ncol=2,nrow = 24) # pusta macierz do miejsc na wartosci modelu CAPM
result_CAPM1 <- stopyzwrotu[,4] + beta_all1*(stopyzwrotu[,3]-stopyzwrotu[,4])  # przewidywany wynik wedlug modelu CAPM
plot(dane_all[dates[13:36],1],result_CAPM1[,1],type = "l",ylim=c(-0.15,0.15),xlab = "czas", ylab = "stopa zwrotu") # wykres wymodelowanych stóp zwrotu przez CAPM
lines(dane_all[dates[13:36],1],stopyzwrotu[,1], col="blue") # dodanie prawdziwych stóp zwrotu
plot(dane_all[dates[13:36],1],result_CAPM1[,2],type = "l",ylim=c(-0.15,0.15),xlab = "czas", ylab = "stopa zwrotu") # wykres wymodelowanych stóp zwrotu przez CAPM
lines(dane_all[dates[13:36],1],stopyzwrotu[,2], col="blue") # dodanie prawdziwych stóp zwrotu

#CAPM2 wlasciwie policzony
beta_allX <- c() # pusty wektor na CAPM
beta_allY <- c() # pusty wektor na CAPM
alfa_allX <- c() # pusty wektor na CAPM
alfa_allY <- c() # pusty wektor na CAPM
for(i in 1:24){
  CAPM <- lm((rowSums(return_all[i:(11+i),]*wagi_all[i,])-stopa_all[i:(11+i)])~(gielda_all[i:(11+i)]-stopa_all[i:(11+i)]))
  CAPM2 <- lm((rowSums(return_all[i:(11+i),]*(1/8))-stopa_all[i:(11+i)])~(gielda_all[i:(11+i)]-stopa_all[i:(11+i)]))
  alfa_allX[i] <- CAPM$coefficients[1]
  beta_allX[i] <- CAPM$coefficients[2]
  alfa_allY[i] <- CAPM2$coefficients[1]
  beta_allY[i] <- CAPM2$coefficients[2]
} # dla modelu CAPM liczymy bety i alfy
alfa_all <- matrix(c(alfa_allX,alfa_allY),ncol=2,dimnames = list(NULL,c("alfa_allX","alfa_allY"))) # zapisanie wartosci alfa i beta CAPM-u dla portfelaX
beta_all <- matrix(c(beta_allX,beta_allY),ncol=2,dimnames = list(NULL,c("beta_allX","beta_allY"))) # zapisanie wartosci alfa i beta CAPM-u dla portfelaY
result_CAPM <- matrix(ncol=2,nrow = 24) # pusta macierz do miejsc na wartosci modelu CAPM
eststzwrotugielda <- rollapply(gielda_all[-length(gielda_all)],12,by=1,mean) # wyestymowana rynkowa stopa zwrotu
result_CAPM <- alfa_all + beta_all*(eststzwrotugielda-stopyzwrotu[,4])  # przewidywany wynik wedlug modelu CAPM
plot(dane_all[dates[13:36],1],result_CAPM[,1],ylim=c(-0.15,0.15),type = "l",xlab = "czas", ylab = "stopa zwrotu") # wykres wymodelowanych stóp zwrotu przez CAPM
lines(dane_all[dates[13:36],1],stopyzwrotu[,1], col="blue") # dodanie prawdziwych stóp zwrotu
legend(dane_all[dates[22],1],-0.08,legend=c("CAPM", "faktyczna stopa ","zwrotu z portfelaX"),
       col=c(1,"blue","white"),pt.lwd = 10, y.intersp = 0.2, x.intersp = 0.3, lty=1, cex=2, bty = 'n') # dodanie legendy
plot(dane_all[dates[13:36],1],result_CAPM[,2],ylim=c(-0.15,0.15),type = "l",xlab = "czas", ylab = "stopa zwrotu") # wykres wymodelowanych stóp zwrotu przez CAPM
lines(dane_all[dates[13:36],1],stopyzwrotu[,2], col="blue") # dodanie prawdziwych stóp zwrotu
legend(dane_all[dates[22],1],-0.08,legend=c("CAPM", "faktyczna stopa ","zwrotu z portfelaY"),
       col=c(1,"blue","white"),pt.lwd = 10,y.intersp = 0.2, x.intersp = 0.3, lty=1, cex=2, bty = 'n')  # dodanie legendy
# Krzywa SML
bet <- c(0:18)*0.1 # wektor bet
Rm <- stopyzwrotu[1,4] + bet*(eststzwrotugielda[1]-stopyzwrotu[1,4]) # wartosci krzywej SML
plot(bet,Rm, type = "l",xlim= c(0,2.2),ylim= c(0,0.045),xlab = "beta", ylab = "oczekiwana stopa zwrotu") # narysowanie krzywej SML
points(beta_all[1,1],mean(rowSums(return_all[1:12,]*wagi_all[1,])),col= rainbow(10)[1],pch=20) # naniesienie punktu portfela X
points(beta_all[1,2],mean(rowSums(return_all[1:12,]*(1/8))),col= rainbow(10)[2],pch=20) # naniesienie punktu portfela X
for(i in 1:8){ 
  punktpB <- lm((return_all[1:12,i]-stopa_all[1:12])~(gielda_all[1:12]-stopa_all[1:12]))  
  points(punktpB$coefficients[2],mean(return_all[1:12,i]),col=rainbow(10)[(i+2)],pch=20)
} # dodanie punktów portfeli brzegowych
legend(1.77,0.045,lty = c(1,rep(NA,10)), y.intersp = 0.8, legend = c("SML","portfelX","portfelY",colnames(return_all)),col=c(1,rainbow(10)),bty='n',pch=c(NA,rep(20,10))) # dodanie leendy

# policzymy drawdown
portfelSP500 <- 10000 # wartosc poczatkowa inwestycji
portfelbonds <- 10000 # wartosc poczatkowa inwestycji
for(i in 2:25){
  portfelSP500[i] <-  portfelSP500[(i-1)]*(1+stopyzwrotu[(i-1),3])
  portfelbonds[i] <-  portfelbonds[(i-1)]*(1+stopyzwrotu[(i-1),4])
} # policzylismy wartosc portfela inwestujac 10 000$ w S&P500 i obligacje
DrawdownsX <- c() # pusty wektor
DrawdownsY <- c() # pusty wektor
DrawdownsSP500 <- c() # pusty wektor
Drawdownsbonds <- c() # pusty wektor
Drawdownbrzeg <- matrix(ncol = 8,nrow = 24) # pusta macierz gdzie bedziemy wpisywali wartosci drawdown
portfelebrzegowe1 <- matrix(rep(10000, 8), ncol = 8,nrow=25, dimnames = list(NULL,colnames(return_all))) # pusta macierz portfeli brzegowych
for(j in 1:8){
  for(i in 2:25){
    portfelebrzegowe1[i,j] <- portfelebrzegowe1[(i-1),j]*(1+return_all[(11+i),j])
} 
} # policzylismy wartosci portfeli brzegowych
for(j in 1:8){
  for(i in 1:24){
    Drawdownbrzeg[i,j]<- max(portfelebrzegowe1[i,j]-portfelebrzegowe1[(i+1):25,j])
  }
} # policzony drawdown portfeli brzegowych
for(i in 1:24){
  DrawdownsX[i] <- max(portfelX[i]-portfelX[(i+1):25])
  DrawdownsY[i] <- max(portfelY[i]-portfelY[(i+1):25])
  DrawdownsSP500[i] <- max(portfelSP500[i]-portfelSP500[(i+1):25])
  Drawdownsbonds[i] <- max(portfelbonds[i]-portfelbonds[(i+1):25])
  } # liczymy Drawdown dla PortfelaX,Y, S&P500 i obligacji
Drawdown <- matrix(ncol=12,nrow = 2) # pusta macierz na drawdown
Drawdown[2,] <- apply(matrix(c(DrawdownsX,DrawdownsY,DrawdownsSP500,Drawdownsbonds,Drawdownbrzeg),ncol=12),2,max) # uzupelnienie wartosci drawdown 
Drawdown[1,] <- c("DrawdownsX","DrawdownsY","DrawdownsSP500","Drawdownsbonds",colnames(return_all)) # uzupelnienie nazw do powyzszej macierzy
xtable(Drawdown) # tabela do latexa

