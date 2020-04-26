library(tseries)

dane_all <- read.csv2(file="data.csv")
dane_all[,1] <- as.Date(dane_all[,1],"%d.%m.%Y")   #wczytanie pierwszej kolumny jako daty
dane_all[,-1] <- lapply(dane_all[,-1], function(x) as.numeric(as.character(x))) #wczytanie danych jako liczb
dates = as.Date(c("2017-01-01","2017-02-01","2017-03-01","2017-04-01","2017-05-01","2017-06-01","2017-07-01","2017-08-01","2017-09-01","2017-10-01","2017-11-01","2017-12-01",
          "2018-01-01","2018-02-01","2018-03-01","2018-04-01","2018-05-01","2018-06-01","2018-07-01","2018-08-01","2018-09-01","2018-10-01","2018-11-01","2018-12-01",
          "2019-01-01","2019-02-01","2019-03-01","2019-04-01","2019-05-01","2019-06-01","2019-07-01","2019-08-01","2019-09-01","2019-10-01","2019-11-01","2019-12-01","2020-01-01"))
dates = sapply(dates, function(x) sum(dane_all[,1] < x) + 1) # numery wierszy z pierwszym dniem kazdego miesiaca
dates[37]<- 754 # poniewaz nie mamy obserwacji ze stycznia 2020, bierzemy cene zamkniecia z ostatniego dnia grudnia

return_all <- dane_all[dates[-1],-1]/dane_all[dates[-length(dates)],-1] -1 # miesieczne proste stopy zwrotu dla poszczeg�lnch firm
mu_all <- apply(return_all[,-1],2,mean)  # srednie miesieczne stopy zwrotu
cov_all <- cov(return_all)     # macierz kowariancji z calego okresu dla miesiecznych st�p zwrotu
for(i in 1:(dim(return_all)[1]-11)){
  return[i] <- mean(apply(return_all[i:(i+11),],2,mean))
}    # wektor srednich miesiecznych st�p zwrotu z 1 roku wstecz dla wszystkich sp�lek
portfelX<- c()
portfelX[1] <- 10000   # kapital poczatkowy
for(i in 1:(length(dates)-12)){
  a <- portfolio.optim(as.matrix(return_all[i:(i+11),]),return[i],shorts = TRUE)
  portfelX[i+1]<- portfelX[i]*sum(a$pw*(1+return_all[12+i,])) 
}   # dlaczego ostatnia obserwacja jest NA? jeszcze nw 
portfelX
dim(dane_all[mies[-length(mies)],-1])


return_all <- a             #miesieczne stopy zwrotu ze wszystkich akcji




# co z miesiecznymi stopami zwrotu (ostatnia)
# skr?ty na gieldzie 2-3-4 literowe
# portfolio.optim ok, czy liczyc??? na piechote?
