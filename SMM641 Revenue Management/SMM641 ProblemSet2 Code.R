library(stargazer)
library(nloptr)

data = read.csv("CongestionPricing.csv",header=T)

# Row count
N=nrow(data) 
head(data)

population <- 192000

#### Question 1A (single price across both Peak Periods and Non Peak Periods) ####-----------------

for (i in 1:N){
  data$maxWTP[i]=max(data[i,2:3]) #calc max willingness to pay over peak & non peak periods
}

data[1:10,]

#Find Max price that a customer is willing to pay of all maxWTPs
maxprice=max(data$maxWTP)

# Defining empty array variables we will be introducing
demand=rep(NA,maxprice)
revenue=rep(NA,maxprice)

#Finding how many people buy at each price according to their WTP
for (p in 1:maxprice){
  demand[p]=sum(data$maxWTP>=p) 
  revenue[p]=p*demand[p] 
}

# Identifying the Best Price
revenueBest=max(revenue) 
priceBest=which(revenue == revenueBest)
revenueBestPopulation = revenueBest/N * population

print(paste("If a single price is to be charged across all time slots, the optimal price to maximise revenue is: £",priceBest, 
            "with an associated revenue of: £", round(revenueBestPopulation,0)))


# Finding Peak Demand and Non-Peak Demand
surplusNonPeak<-rep(0,N)
surplusPeak<-rep(0,N)

#Calculates the ma xurplus for Non-peak and peak times
for (i in 1:N){
  surplusNonPeak[i]=data[i,3]-priceBest

  # New columns to store this info 
  data$surplusNonPeak[i]=data[i,3]-priceBest
}

#Viewing the data
data[1:10,]

#Creates matrix of Surplus during peak times
#Surplus will help determine what customers will pick (Non-Peak vs Peak)
surplusPeak<-matrix(0,N,maxprice)
for (p in 1:maxprice){
  for (i in 1:N){
    surplusPeak[i,p]=data[i,2]-p
  }
}

#Calculating the demand for Peak & Non Peak times and Revenue
demandNonPeak<-matrix(0,maxprice)
demandPeak<-matrix(0,maxprice)
revenue<-matrix(0,maxprice)

for (p in 1:maxprice){
  demandNonPeak[p]=sum((surplusNonPeak>surplusPeak[,p])*(surplusNonPeak>=0)) #Counts customers with maximum non-peak surplus > current surplus 
  demandPeak[p]=sum((surplusPeak[,p]>=surplusNonPeak)*(surplusPeak[,p]>=0)) #Counts customers with surplus at price p during peak time > non-peak surplus.
  revenue[p]=p*demandNonPeak[p]+p*demandPeak[p]
}

# Finds the associated demand at the Best Price
PeakdemandBest = demandPeak[priceBest]/N*population
NonPeakdemandBest = demandNonPeak[priceBest]/N*population
totaldemandBest = PeakdemandBest  + NonPeakdemandBest
print(paste("The Non Peak demand is:", round(NonPeakdemandBest,0), "cars"))
print(paste("The Peak demand is:", round(PeakdemandBest,0), "cars"))
print(paste("The total demand at the optimal price of £", priceBest, "is: ", round(totaldemandBest,0), "cars"))

# Calculate Average Speed for Peak and Non Peak times
peak_avspeed = 30 - (0.0625*(PeakdemandBest/1000)) 
nonpeak_avspeed = 30 - (0.0625*(NonPeakdemandBest/1000)) 

emissionspeakperCar <- ifelse(
  peak_avspeed >= 25,
  235.0 - (1.4 * peak_avspeed),
  617.5 - (16.7 * peak_avspeed)
)
emissionsnpeakperCar <- ifelse(
  nonpeak_avspeed >= 25,
  235.0 - (1.4 * nonpeak_avspeed),
  617.5 - (16.7 * nonpeak_avspeed)
)

# Calculating associated Emissions
peak_emissions = emissionspeakperCar * PeakdemandBest
nonpeak_emissions = emissionsnpeakperCar * NonPeakdemandBest
print(paste("The average speed of cars travelling in Peak time is:", round(peak_avspeed,2), "km/h, with average emissions per car:", round(emissionspeakperCar,2), "g/km, amounting to emissions of:", round(peak_emissions,0), "g/km"))
print(paste("The average speed of cars travelling in Non Peak time is:", round(nonpeak_avspeed,2), "km/h, with average emissions per car:", round(emissionsnpeakperCar,2), "g/km, amounting to emissions of:", round(nonpeak_emissions,0), "g/km"))

print(paste("Total emissions when a single price is enforced during both the Peak and Non Peak period:", round(sum(peak_emissions+nonpeak_emissions),0), "g/km"))

# Plotting Revenue against Price for Single Price of Non Peak and Peak Times
xaxis=1:maxprice
plot(xaxis,revenue/N*population/1000,pch = 16, type="s",col="blue",las=1, xaxt="n",
     xlab="Price",ylab="Revenue (in £1K)", main = "Expected Revenue with a Single Price")
xticks <- seq(0, maxprice, by=50)
axis(side = 1, at = xticks)
axis(side = 1, at = priceBest)
lines(c(priceBest,priceBest),c(0, revenueBestPopulation/1000),lty=2)
axis(side = 2, at = round(revenueBestPopulation/1000,0),line=NA,las=1,pos=18, tick=F)
lines(c(20,priceBest),c(revenueBestPopulation/1000, revenueBestPopulation/1000),lty=2)

#### Question 1B (introducing Non-Peak price of 7) #### -------------------------------------------------------------------------------------------

maxprice=max(data[2:3])
basePrice=7
demandNonPeak<-rep(0,maxprice)
demandPeak<-rep(0,maxprice)
revenue<-rep(0,maxprice)

surplusNonPeak<-rep(0,N)

for (i in 1:N){
  surplusNonPeak[i]=data[i,3]-basePrice
  # New columns to store this info 
  data$surplusNonPeak[i]=data[i,3]-basePrice
}
data[1:10,]

#create an empty matrix with N number of rows (345) and different price points
surplusPeak<-matrix(0,N,maxprice)

#calculates to understand the surplus of each customer at each price point
for (p in 1:maxprice){
  for (i in 1:N){
    surplusPeak[i,p]=data[i,2]-p
  }
}

#Rename columns headers
colnames(surplusPeak)=paste0("p=",1:maxprice)

head(surplusPeak)

#Identify at which price which consumers will buy Non Peak and Peak times 
#Surplus Non Peak > Surplus Peak they will favour non peak. 
#Returns TRUE/FALSE, add all them together
for (p in 1:maxprice){
  demandNonPeak[p]=sum((surplusNonPeak>surplusPeak[,p])*(surplusNonPeak>=0))
  demandPeak[p]=sum((surplusPeak[,p]>=surplusNonPeak)*(surplusPeak[,p]>=0))
  revenue[p]=basePrice*demandNonPeak[p]+p*demandPeak[p]
}

# Identifying the Best Price
revenueBest =max(revenue[basePrice:maxprice])
priceBest=which(revenue == revenueBest)
revenueBestPopulation = revenueBest/N * population
print(paste("When Non Peak period is priced at £ 7, the optimal price for the Peak periods to maximinise revenue is: £",priceBest))
print(paste("The expected revenue from this is: £",round(revenueBestPopulation,2)))

# Finds the associated demand at the Best Price
demandPeakBest =demandPeak[priceBest]/N*population
demandNonPeakBest = demandNonPeak[priceBest]/N*population
totaldemandBest = demandPeakBest + demandNonPeakBest
print(paste("The Non Peak demand is:", round(demandNonPeakBest,0), "cars"))
print(paste("The Peak demand is:", round(demandPeakBest,0), "cars"))
print(paste("The total demand at the optimal price of £", priceBest, "is: ", round(totaldemandBest,0), "cars"))

# Calculate Average Speed for Peak and Non Peak times
peak_avspeed = 30 - (0.0625*(demandPeakBest/1000)) 
nonpeak_avspeed = 30 - (0.0625*(demandNonPeakBest/1000)) 

emissionspeakperCar <- ifelse(
  peak_avspeed >= 25,
  235.0 - (1.4 * peak_avspeed),
  617.5 - (16.7 * peak_avspeed)
)
emissionsnpeakperCar <- ifelse(
  nonpeak_avspeed >= 25,
  235.0 - (1.4 * nonpeak_avspeed),
  617.5 - (16.7 * nonpeak_avspeed)
)

# Calculating Emissions
peak_emissions = emissionspeakperCar * demandPeakBest
nonpeak_emissions = emissionsnpeakperCar * demandNonPeakBest
print(paste("The average speed of cars travelling in Peak time is:", round(peak_avspeed,2), "km/h, with average emissions per car:", round(emissionspeakperCar,2), "g/km, amounting to emissions of:", round(peak_emissions,0), "g/km"))
print(paste("The average speed of cars travelling in Non Peak time is:", round(nonpeak_avspeed,2), "km/h, with average emissions per car:", round(emissionsnpeakperCar,2), "g/km, amounting to emissions of:", round(nonpeak_emissions,0), "g/km"))

print(paste("Total emissions when a higher price is enforced during the Peak period:", round(sum(peak_emissions+nonpeak_emissions),0), "g/km"))

#### Question 1C (introducing NonPeak price of 7, minimising emissions) #### --------------------------------------
#### Minimising Total Emissions

baseprice = 7

#Introducting new variables that are populated with zeros 17 times
demandNonPeak<-rep(0,maxprice)
demandPeak<-rep(0,maxprice)
revenue<-rep(0,maxprice)
speedNonPeak <- rep(0,maxprice)
speedPeak <- rep(0,maxprice)
totalemissions<-rep(0,maxprice)
emissionsNonPeak<-rep(0,maxprice)
emissionsPeak<-rep(0,maxprice)

for (p in 1:maxprice){
  demandNonPeak[p]=(sum((surplusNonPeak>surplusPeak[,p])*(surplusNonPeak>=0))/N)*population
  demandPeak[p]=(sum((surplusPeak[,p]>=surplusNonPeak)*(surplusPeak[,p]>=0))/N)*population
  speedNonPeak[p]= 30 - ((0.0625 * ((demandNonPeak[p])/1000)))
  speedPeak[p]= 30 - ((0.0625 * ((demandPeak[p])/1000)))
  revenue[p]=basePrice*demandNonPeak[p]+p*demandPeak[p]
  emissionsNonPeak[p] = if (demandNonPeak[p] > 0) {
    emissionsNonPeak[p] = if (speedNonPeak[p] >= 25) {
      (235 - (1.4 * speedNonPeak[p])) * demandNonPeak[p]
    } else {
      (617.5 - (16.7 * speedNonPeak[p])) * demandNonPeak[p]
    }
  } else {
    emissionsNonPeak[p] = 0
  }
  emissionsPeak[p] = if (speedPeak[p] >= 25) {
    (235 - (1.4 * speedPeak[p])) * demandPeak[p]
  }else {
    (617.5 - (16.7 * speedPeak[p])) * demandPeak[p]
  }
  totalemissions[p]=emissionsPeak[p]+emissionsNonPeak[p]
}

#Create a new dataframe storing information for every possible Peak time price
data2<-data.frame(matrix(nrow=17, ncol=9))
colnames(data2)=c("Peak price","demandNonPeak","demandPeak","speedNonPeak", "speedPeak","revenue","emissionsNonPeak","emissionsPeak","totalemissions")
peakPrice=(1:17)
index = (1:17)
data2[index,1]=peakPrice
data2[index,2]=demandNonPeak
data2[index,3]=demandPeak
data2[index,4]=speedNonPeak
data2[index,5]=speedPeak
data2[index,6]=revenue
data2[index,7]=emissionsNonPeak
data2[index,8]=emissionsPeak
data2[index,9]=totalemissions

#Define a subset that meets the constraints
leastemissions <- data2[data2$revenue >= 1100000, ] |> 
  subset(totalemissions == min(totalemissions))

#Results
print(paste("When Non Peak period is priced at £ 7, the optimal price to minimise emissions whilst remaining profitable is: £",leastemissions$`Peak price`))
print(paste("The expected revenue from this is: £",round(leastemissions$revenue,2)))
print(paste("The average speed of cars travelling in Peak time is:", round(leastemissions$speedPeak,2), "km/h, amounting to emissions of:", round(leastemissions$emissionsPeak,0), "g/km"))
print(paste("The average speed of cars travelling in Non Peak time is:", round(leastemissions$speedNonPeak,2), "km/h, amounting to emissions of:", round(leastemissions$emissionsNonPeak,0), "g/km"))
print(paste("Total emissions when a price of £", round(leastemissions$`Peak price`,0), "is enforced during the Peak period:", round(leastemissions$totalemissions,0), "g/km"))

#Visualise Total Emissions v Peak Price 
par(mfrow=c(1,1))
xaxis=1:maxprice
plot(xaxis,totalemissions/100000,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Peak Period",ylab="Total Emissions (in 100,000 g/km)")
xticks <- seq(0, maxprice, by=1)
axis(side = 1, at = xticks)
axis(side = 1, at = leastemissions$`Peak price`)
lines(c(leastemissions$`Peak price`,leastemissions$`Peak price`),c(0, leastemissions$totalemissions/100000),lty=2)
axis(side = 2, at = round(leastemissions$totalemissions/100000,3),las=1)
lines(c(0,leastemissions$`Peak price`),c(leastemissions$totalemissions/100000, leastemissions$totalemissions/100000),lty=2)


## Alternative method -------------------------------------------------------------------------------------
#### Question 1C - Using a Linear model 
## Using a Linear Regressions model to predict, gives slightly different answers 

PeakPrice=1:maxprice

# Linear Model for Peak Demand
fitPeak <-lm(demandPeak ~ PeakPrice)
InterceptPeak=coef(fitPeak)[1]
CoefPricePeak=coef(fitPeak)[2]

# Linear Model for Non Peak Demand
fitNonPeak <-lm(demandNonPeak ~ PeakPrice)
InterceptNonPeak=coef(fitNonPeak)[1]
CoefPriceNonPeak=coef(fitNonPeak)[2]

library(stargazer)
stargazer(fitNonPeak,fitPeak, type="text")

# Define the evaluation function
eval_f <- function(x) {
  basePrice <- 7
  peakPrice <- x[1]  
  demandNonPeakLinear <- InterceptNonPeak + CoefPriceNonPeak * basePrice
  demandPeakLinear <- InterceptPeak + CoefPricePeak * peakPrice
  speedNonPeakLinear <- 30 - (0.0625 * (demandNonPeakLinear / 1000))
  speedPeakLinear <- 30 - (0.0625 * (demandPeakLinear / 1000))
  emissionsNonPeakLinear <- if (speedNonPeakLinear >= 25) {
    (235 - (1.4 * speedNonPeakLinear)) * demandNonPeakLinear
  } else {
    (617.5 - (16.7 * speedNonPeakLinear)) * demandNonPeakLinear
  }
  emissionsPeakLinear <- if (speedPeakLinear >= 25) {
    (235 - (1.4 * speedPeakLinear)) * demandPeakLinear
  } else {
    (617.5 - (16.7 * speedPeakLinear)) * demandPeakLinear
  }
  totalEmissionsLinear <- emissionsNonPeakLinear + emissionsPeakLinear
  return(totalEmissionsLinear)
}

# Define inequality constraints
eval_g_ineq <- function(x) {
  basePrice <- 7
  peakPrice <- x[1]
  demandNonPeakLinear <- InterceptNonPeak + CoefPriceNonPeak * basePrice
  demandPeakLinear <- InterceptPeak + CoefPricePeak * peakPrice
  revenue <- basePrice * demandNonPeakLinear + peakPrice * demandPeakLinear
  # Constraints
  constraint <- c(
    -demandNonPeakLinear,        
    -demandPeakLinear,           
    1100000 - revenue      
  )
  
  return(constraint)
}

# Set up optimisation
x0 <- c(7)  
lb <- c(1)  # Lower bound
ub <- c(17) # Upper bound
opts <- list("algorithm" = "NLOPT_LN_COBYLA",
             "xtol_rel" = 1.0e-9,
             "maxeval" = 1000)

# Run optimisation
resultLinear <- nloptr(
  x0 = x0,
  eval_f = eval_f,
  lb = lb,
  ub = ub,
  eval_g_ineq = eval_g_ineq,
  opts = opts
)

# Results
priceOpt <- resultLinear$solution
emissionsOpt <- resultLinear$objective

print(paste("When Non Peak period is priced at £ 7, the optimal price to minimise emissions whilst remaining profitable is: £",round(priceOpt,1)))
print(paste("Total emissions when a price of £", round(priceOpt,1), "is enforced during the Peak period:", round(emissionsOpt,0), "g/km"))

#Visualising the Linear Model for Peak Period Demand
par(mfrow=c(1,2))
plot(xaxis,demandPeak,pch = 16, type="s",col="blue", las=1, xaxt="n",
     xlab="Price for Peak Period",ylab="Peak Period Demand", yaxt = "n" )
abline(fitPeak, lwd=2, col="darkred")

#Visualising the Linear Model for Non Peak Period Demand
plot(xaxis,demandNonPeak,pch = 16, type="s",col="blue", las=1, xaxt="n", yaxt = "n",
     xlab="Price for Peak Period",ylab="Non-Peak Period Demand")
abline(fitNonPeak, lwd=2, col="darkred")
