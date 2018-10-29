##### Gold Mine VAluation 
library(readxl)
Gold_Prices <-  read_excel("gold-60.xlsx")

head(Gold_Prices)
str(Gold_Prices)
class(Gold_Prices$Price)
class(Gold_Prices$Month)

Gold_Prices$Month<- as.Date(Gold_Prices$Month)
Gold_Prices<- Gold_Prices[,-3]

Gold_Prices$Return <- rep(0, length(Gold_Prices$Month))


for( i in 2: length(Gold_Prices$Month)){
  Gold_Prices$Return[i]<- (Gold_Prices$Price[i]-Gold_Prices$Price[i-1])/ Gold_Prices$Price[i-1]
}
##### Characterize Distribution of Price
hist(Gold_Prices$Price)
qqnorm(Gold_Prices$Price)
boxplot(Gold_Prices$Price)
shapiro.test(Gold_Prices$Price)
####Returns are normal
shapiro.test(Gold_Prices$Return)
#### What is the average monthly return
mean(Gold_Prices$Return)
#### What is the the sigma of monthly return
sd(Gold_Prices$Return)



#### Gold Price Simulation
mu<- mean(Gold_Prices$Return)
sigma <- sd(Gold_Prices$Return)

periods <- 0:(12*20)
sO <- 1331.30

asset.paths <- function(s0, mu, sigma, 
                        nsims = 10000, 
                        periods = c(0, 1)   # time periods at which to simulate prices
) 
{
  s0 = as.vector(s0)
  nsteps = length(periods)
  dt = c(periods[1], diff(periods))
  
  if( length(s0) == 1 ) {
    drift = mu - 0.5 * sigma^2
    if( nsteps == 1 ) {
      s0 * exp(drift * dt + sigma * sqrt(dt) * rnorm(nsims))
    } else {
      temp = matrix(exp(drift * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims)
      for(i in 2:nsteps) temp[i,] = temp[i,] * temp[(i-1),]
      s0 * temp
    }
  } else {
    require(MASS)
    drift = mu - 0.5 * diag(sigma)
    n = length(mu)
    
    if( nsteps == 1 ) {
      s0 * exp(drift * dt + sqrt(dt) * t(mvrnorm(nsims, rep(0, n), sigma)))
    } else {
      temp = array(exp(as.vector(drift %*% t(dt)) + t(sqrt(dt) * mvrnorm(nsteps * nsims, rep(0, n), sigma))), c(n, nsteps, nsims))
      for(i in 2:nsteps) temp[,i,] = temp[,i,] * temp[,(i-1),]
      s0 * temp
    }
  }
}


pricesofgold <- asset.paths(sO, mu, sigma, nsims = 10000, periods = periods)

matplot(pricesofgold[,1:1000], type = 'l', xlab = 'Months', ylab = 'Prices')
####Prices of Gold at the End of the Mine
pricesofgold[241,]
mean(pricesofgold[241,])
hist(pricesofgold[241,])

                 
####DCF Method of Valuation
monthlyaverage <- rep(0, 241)
for( i in 1:241){
 monthlyaverage[i] <-mean(pricesofgold[i,])
}

###This is a plot of the Average Expected Price of Gold
plot(1:241, monthlyaverage,type = 'l', main = 'Average Price of Gold in Each Month', ylab = 'Price of Gold', xlab = 'Month')
costs <- rep(250, 241)
ounces_a_month <- 50000/12

### Costs Grow at 5% annually
for(i in 1:length(costs)){
  costs[i]<- 250* 1.05^floor(length(costs[1:i])/12)
}
costs 

cashflows <- (monthlyaverage*ounces_a_month)-(costs*ounces_a_month) 
cashflows 
#### now to discount, but at what rate?
#### lets assume Rs = Rf+ Beta(Rm-Rf)
#### Beta of A similar Gold Mining Compnay 
### We will use Goldcorp.inc use their beta and assume we are all equity
#####Calculate Beta## Upload Monthly Returns of SP500
library(BatchGetSymbols)
tickers= 'SPY'
first.date =Sys.Date()-365*5
last.date= Sys.Date()

l.out <- BatchGetSymbols(tickers = tickers, first.date = first.date,last.date = last.date)
SP500 <-  l.out$df.tickers
tickers= 'GG'
l.out <- BatchGetSymbols(tickers = tickers, first.date = first.date,last.date = last.date)
GoldCorp <- l.out$df.tickers
SP500$return <- rep(0, length(SP500$ref.date))
GoldCorp$return<- rep(0, length(GoldCorp$ref.date))
for (i in 2 : length(SP500$ref.date)){
  SP500$return[i]<- (SP500$price.close[i]-SP500$price.close[i-1])/SP500$price.close[i-1]
}
for ( i in 2: length(GoldCorp$ref.date)){
  GoldCorp$return[i]<- (GoldCorp$price.close[i]-GoldCorp$price.close[i-1])/GoldCorp$price.close[i-1]
}


BetaGold <- lm(GoldCorp$return~ SP500$return)

summary(BetaGold)
BetaGoldCorp <- BetaGold$coefficients[2]
BetaGoldCorp
### Current 10 year Treasury 
Rf <- 2.86/100
#### Average Return of Market from 2013-2018
Marke_Return <- c(32.15,
                  13.52,
                  1.38,
                  11.77,
                  21.64
)
Rmarket <- mean(Marke_Return)/100

Rs<- Rf+BetaGoldCorp*(Rmarket-Rf)
Rs
 
###Value Cash Flows

 present_value <- rep(0,length(cashflows))
for(i in 1:length(cashflows)){
  
  present_value[i]<-cashflows[i]/((1+Rs/12)^i) 
  
}

###NPV = Sum of PV + Start Costs
NPV <- sum(present_value)- 100000000
sum(present_value)
NPV
### The Value of the Mine using DCF is $259,785,566
### note the beta can fluctuate and this code recalculates the beta 


### Option To Abadnon at Year 10#### European PUT
x <- costs[120]


future.payoff10 = pmax(0, x - pricesofgold[120,] )
future.payoff10
discounted.payoff = future.payoff10 * exp(-Rs/12 * 10)
### the value of the option to abadon 
mean(discounted.payoff)
valueabandon10 <- mean(discounted.payoff)*ounces_a_month
valueabandon10
Valueofproject10 <- NPV + valueabandon10
Valueofproject10
### The Value of the option to abandon at Year 10 is $259,788,757
### Option to Abandon at any time i.e. an American Option
future.payoffany = pmax(0, costs - pricesofgold )
discounted.payoffany = future.payoffany*exp(-Rs/12*20)
future.payoffany

mean(discounted.payoffany)*ounces_a_month
value_abandon_any <- mean(discounted.payoffany)* ounces_a_month
value_abandon_any
valueofprojectany <- value_abandon_any + NPV
valueofprojectany
### the value of value of the option to abandon the project at any time is  $259,854,415



