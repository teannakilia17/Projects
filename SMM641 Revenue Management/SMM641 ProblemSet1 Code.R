#Question 1
#Revenue based on FCFS basis
mLd1= 32 # Mean Demand for Discounted-Fare, Poisson
mHd1= 18 # Mean Demand for High-Fare, Poisson
pL1= (1-0.2)*195 # Price for Discounted-Fare
pH1= 195 # Price for High-Fare
capacity1= 23 # Capacity

ExpRevenue1 = rep(0,capacity1+1) ##index starts from 1 in R; #creates a variable with 23 values of 0 
for (i in 1:1){  #FSFC so no capacity constraints, that's why the parameters for i is only 1
  protect1 = i-1 #essentially protecting 0 rooms
  availforLowFare1 = capacity1-protect1; #rooms available at the 
  ExpRevenue1[i]= 0;#creates a variable with 23 values of 0 
#Demand for rooms at the discounted rate 
  for(dL1 in 0:100){ #based on Poison distribution probability of achieving 50 is negligible 
    soldLowFare1 = min(availforLowFare1,dL1) #sells the minimum value of either number of rooms made available (not protected) or demand for rooms at discounted rate
    remainforHighFare1 = capacity1-soldLowFare1
#Demand for rooms at full price
    for(dH1 in 0:100){ 
      soldHighFare1 = min(remainforHighFare1,dH1) #sells the minimum value of either number of rooms remaining or demand for rooms at full price
      RevenueThisIter1 = (pL1*soldLowFare1)+ (pH1*soldHighFare1) #calculates the revenue by multiplying cost of the rooms and the number rooms sold at both price points
      ExpRevenue1[i] = ExpRevenue1[i]+ #revenue from previous demand realisation set up + the revenue from this demand realisation set up multiplied by the probability of obtaining the demand for both prices
        (RevenueThisIter1*dpois(dL1,mLd1)*dpois(dH1,mHd1))
    }
  }
}
      
RevenueFCFS = ExpRevenue1[1]
print(paste("Daily Hotel Revenue (FCFS):£", round(RevenueFCFS,2)))

#Identifying protection level to maximise revenue
mLd2= 32 
mHd2= 18 
pL2= (1-0.2)*195 
pH2= 195 
capacity2= 23 

#creates a variable with 23 values of 0 
ExpRevenue2 = rep(0,capacity2+1) #index starts from 1 in R

#replaces 0 with the Exp revenue associated with protecting j amount of rooms
for (j in 1:(capacity2+1)){ #iterate all possible demand realisation combination
  protect2 = j-1 #converts index into protection level
  availforLowFare2 = capacity2-protect2; #calculates the number of seats available at Discounted price
  ExpRevenue2[j]= 0;
#Demand for rooms at the discounted rate 
  for(dL2 in 0:100){ #based on Poison distribution probability of achieving 50 is negligible 
    soldLowFare2 = min(availforLowFare2,dL2) #sells the minimum value of either number of rooms made available (not protected) or demand for rooms at discounted rate
    remainforHighFare2 = capacity2-soldLowFare2 
#Demand for rooms at full price
    for(dH2 in 0:100){ 
      soldHighFare2 = min(remainforHighFare2,dH2) #sells the minimum value of either number of rooms remaining or demand for rooms at full price
      RevenueThisIter2 = (pL2*soldLowFare2)+ (pH2*soldHighFare2) #calculates the revenue by multiplying cost of the rooms and the number rooms sold at both price points
      ExpRevenue2[j] = ExpRevenue2[j]+ #revenue from previous demand realisation set up + the revenue from this demand realisation set up multiplied by the probability of obtaining the demand for both prices
        (RevenueThisIter2*dpois(dL2,mLd2)*dpois(dH2,mHd2))
    } 
  }
}
Protectindexbest = which(ExpRevenue2 == max(ExpRevenue2)) #identifies the number in the ExpRevenue2 variable that returns the maximum revenue
ProtectBest = Protectindexbest-1 #to mitigate the indexing of 1 on R 
print(paste("The Protection Level to maximise daily revenue:", ProtectBest))

OptimalExpRevenue = max(ExpRevenue2) #identifies the maximum revenue that can be make from identifying the optimal protection level
print(paste("With this protection level, the hotel's expected daily revenue:£", round(OptimalExpRevenue,2))) #rounding the value to 2 decimal points to reflect money accurately 

#Percent % improvement compared to FCFS
PercentChange = ((OptimalExpRevenue - RevenueFCFS)/RevenueFCFS) * 100 
print(paste("The percentage increase on revenue by implementing a protection level: ",round(PercentChange,2),"%"))

