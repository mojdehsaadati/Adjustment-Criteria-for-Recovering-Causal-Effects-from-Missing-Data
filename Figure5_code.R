install.packages("mice")
# This is the code for generating data set for a graph figure 5.
# To show how the adjustment work.

###################### Graph 1 ####################
# Creating random numbers for each variables. 
#We consider the variables have binary values. 
n = 5000
Z2 <- rbinom(n, size= 1, prob = 0.6)
Z1 <- rbinom(n, size= 1, prob = 0.9)
R1 <- integer(n);
R3 <- integer(n);  
Z3 <- integer(n);
Z4 <- integer(n);

Y <- integer(n);
X <- integer(n);

######## Table 1 P(Z3 | Z1) and Table 2 P(Z4 | Z1) ############ 
for ( i in 1:n)   
{ 
  if(Z1[i] == 0)
  {   
    Z3[i] = rbinom(1 , size =1,prob = 0.65) 
    Z4[i] = rbinom(1, size =1,prob = 0.5) 
  }   
  else if(Z1[i] == 1)
  {   
    Z3[i] = rbinom(1, size =1,prob = 0.25) 
    Z4[i] = rbinom(1, size =1,prob = 0.8) 
  }   
}

######## Table 3 P(X | Z3,Z2)  ############ 
for ( i in 1:n)   
{  if(Z3[i] == 0 & Z2[i] == 0)
  X[i] = rbinom(1,size =1,prob = 0.4) 
if(Z3[i] == 0 & Z2[i] == 1)
  X[i] = rbinom(1,size =1,prob = 0.75) 
if(Z3[i] == 1 & Z2[i] == 0)
  X[i] = rbinom(1,size =1,prob = 0.5) 
if(Z3[i] == 1 & Z2[i] == 1)
  X[i] = rbinom(1,size =1,prob = 0.9) 
}


######## Table 4 P(Y | Z4,X)  ############ 
for ( i in 1:n)   
{  if(Z4[i] == 0 & X[i] == 0)
  Y[i] = rbinom(1, size =1, prob = 0.15) 
if(Z4[i] == 0 & X[i] == 1)
  Y[i] = rbinom(1, size=1, prob = 0.3) 
if(Z4[i] == 1 & X[i] == 0)
  Y[i] = rbinom(1, size=1, prob = 0.6) 
if(Z4[i] == 1 & X[i] == 1)
  Y[i] = rbinom(1, size=1, prob = 0.2) 
}

######## Table 5 P(R3 | Z2)  ############ 
for ( i in 1:n)   
{ 
  if(Z2[i] == 0)
    R3[i] = rbinom(1, size =1,prob = 0.7) 
  else if(Z2[i] == 1)
    R3[i] = rbinom(1, size =1,prob = 0.5) 
}
######## Table 6 P(R1 | X)  ############ 
for ( i in 1:n)   
{ 
  if(X[i] == 0)
    R1[i] = rbinom(1, size =1,prob = 0.8) 
  else if(X[i] == 1)
    R1[i] = rbinom(1, size =1,prob = 0.1) 
}


fullDataSet <- data.frame(Z2,Z1,R1,R3,Z3,Z4,Y,X) 

##### Computing Adjustment Set P(y | do(x)) ######
##### We need to compute this for different sets of values ####

#### Y = 0 , X = 0 ##########
P_YXZ1 <- nrow(subset(fullDataSet, Y ==0 & X == 0 & Z1 == 0))
P_XZ1 <- nrow(subset(fullDataSet, (X == 0 & Z1 == 0)))
P_Z1 <- nrow(subset(fullDataSet, Z1==0))

part_1 = P_YXZ1/P_XZ1*P_Z1/n

P_YXZ1_2 <- nrow(subset(fullDataSet, Y ==0 & X == 0 & Z1 == 1))
P_XZ1_2 <- nrow(subset(fullDataSet, (X == 0 & Z1 == 1)))
P_Z1_2 <- nrow(subset(fullDataSet, Z1==1))
part_2 = P_YXZ1_2/P_XZ1_2*P_Z1_2/n  

CE_00 <- part_1 + part_2 
#### Y = 1 , X = 0 ##########
P_YXZ1 <- nrow(subset(fullDataSet, Y ==1 & X == 0 & Z1 == 0))
P_XZ1 <- nrow(subset(fullDataSet, (X == 0 & Z1 == 0)))
P_Z1 <- nrow(subset(fullDataSet, Z1==0))

part_1 = P_YXZ1/P_XZ1*P_Z1/n

P_YXZ1_2 <- nrow(subset(fullDataSet, Y ==1 & X == 0 & Z1 == 1))
P_XZ1_2 <- nrow(subset(fullDataSet, (X == 0 & Z1 == 1)))
P_Z1_2 <- nrow(subset(fullDataSet, Z1==1))
part_2 = P_YXZ1_2/P_XZ1_2*P_Z1_2/n  

CE_10 <- part_1 + part_2 

#### Y = 0 , X = 1 ##########
P_YXZ1 <- nrow(subset(fullDataSet, Y ==0 & X == 1 & Z1 == 0))
P_XZ1 <- nrow(subset(fullDataSet, (X == 1 & Z1 == 0)))
P_Z1 <- nrow(subset(fullDataSet, Z1==0))

part_1 = P_YXZ1/P_XZ1*P_Z1/n

P_YXZ1_2 <- nrow(subset(fullDataSet, Y ==0 & X == 1 & Z1 == 1))
P_XZ1_2 <- nrow(subset(fullDataSet, (X == 1 & Z1 == 1)))
P_Z1_2 <- nrow(subset(fullDataSet, Z1==1))
part_2 = P_YXZ1_2/P_XZ1_2*P_Z1_2/n  

CE_01 <- part_1 + part_2


#### Y = 1 , X = 1 ##########
P_YXZ1 <- nrow(subset(fullDataSet, Y ==1 & X == 1 & Z1 == 0))
P_XZ1 <- nrow(subset(fullDataSet, (X == 1 & Z1 == 0)))
P_Z1 <- nrow(subset(fullDataSet, Z1==0))

part_1 = P_YXZ1/P_XZ1*P_Z1/n

P_YXZ1_2 <- nrow(subset(fullDataSet, Y ==1 & X == 1 & Z1 == 1))
P_XZ1_2 <- nrow(subset(fullDataSet, (X == 1 & Z1 == 1)))
P_Z1_2 <- nrow(subset(fullDataSet, Z1==1))
part_2 = P_YXZ1_2/P_XZ1_2*P_Z1_2/n  

CE_11 <- part_1 + part_2
################# missing data ###########################################
missingData <- fullDataSet
missingData$Z3[missingData$R3 == 0] <- NA
missingData$Z1[missingData$R1 == 0] <- NA
######################## m-adjustment ###################################  

##### Computing M-Adjustment Set P(y | do(x)) ######
##### We need to compute this for different sets of values ####

#### Y = 0 , X = 0 ##########
MP_YXZ3 <- nrow(subset(missingData, Y ==0 & X == 0 & Z3 == 0 & R3 ==1))
MP_XZ3 <- nrow(subset(missingData, (X == 0 & Z3 == 0 & R3 == 1)))
MP_Z3 <- nrow(subset(missingData, Z3==0 & R3 == 1))
NM <-  nrow(subset(missingData, R3 == 1))
Mpart_1 = MP_YXZ3/MP_XZ3*MP_Z3/NM

MP_YXZ3_2 <- nrow(subset(missingData, Y ==0 & X == 0 & Z3 == 1 & R3 == 1))
MP_XZ3_2 <- nrow(subset(missingData, (X == 0 & Z3 == 1 & R3 == 1)))
MP_Z3_2 <- nrow(subset(missingData, Z3==1  & R3 == 1))
Mpart_2 = MP_YXZ3_2/MP_XZ3_2*MP_Z3_2/NM  


CEM_00 <- Mpart_1 + Mpart_2 

#### Y = 1 , X = 0 ##########
MP_YXZ3 <- nrow(subset(missingData, Y ==1 & X == 0 & Z3 == 0  & R3 == 1))
MP_XZ3 <- nrow(subset(missingData, (X == 0 & Z3 == 0 &   R3 == 1)))
MP_Z3 <- nrow(subset(missingData, Z3==0 &  R3 == 1))

Mpart_1 = MP_YXZ3/MP_XZ3*MP_Z3/NM

MP_YXZ3_2 <- nrow(subset(missingData, Y ==1 & X == 0 & Z3 == 1  & R3 == 1))
MP_XZ3_2 <- nrow(subset(missingData, (X == 0 & Z3 == 1  & R3 == 1)))
MP_Z3_2 <- nrow(subset(missingData, Z3==1 & R3 == 1))
Mpart_2 = MP_YXZ3_2/MP_XZ3_2*MP_Z3_2/NM  

CEM_10 <- Mpart_1 + Mpart_2 

#### Y = 0 , X = 1 ##########
MP_YXZ3 <- nrow(subset(missingData, Y ==0 & X == 1 & Z3 == 0 & R3 ==1))
MP_XZ3 <- nrow(subset(missingData, (X == 1 & Z3 == 0 & R3 ==1)))
MP_Z3 <- nrow(subset(missingData, Z3==0 & R3 ==1))

Mpart_1 = MP_YXZ3/MP_XZ3*MP_Z3/NM

MP_YXZ3_2 <- nrow(subset(missingData, Y ==0 & X == 1 & Z3 == 1 & R3 ==1))
MP_XZ3_2 <- nrow(subset(missingData, (X == 1 & Z3 == 1 & R3 ==1)))
MP_Z3_2 <- nrow(subset(missingData, Z3==1 & R3 ==1))
Mpart_2 = MP_YXZ3_2/MP_XZ3_2*MP_Z3_2/NM  

CEM_01 <- Mpart_1 + Mpart_2

#### Y = 1 , X = 1 ##########
MP_YXZ3 <- nrow(subset(missingData, Y ==1 & X == 1 & Z3 ==0  & R3 == 1))
MP_XZ3 <- nrow(subset(missingData, (X == 1 & Z3 == 0  & R3 == 1)))
MP_Z3 <- nrow(subset(missingData, Z3==0  & R3 == 1))

Mpart_1 = MP_YXZ3/MP_XZ3*MP_Z3/NM

MP_YXZ3_2 <- nrow(subset(missingData, Y ==1 & X == 1 & Z3 == 1 & R3 == 1))
MP_XZ3_2 <- nrow(subset(missingData, (X == 1 & Z3 == 1  & R3 == 1)))
MP_Z3_2 <- nrow(subset(missingData, Z3==1  & R3 == 1))
Mpart_2 = MP_YXZ3_2/MP_XZ3_2*MP_Z3_2/NM  

CEM_11 <- Mpart_1 + Mpart_2

#################  adjustment with imputation #########################
md.pattern(missingData)
md.pairs(missingData)

library(mice)
imputed_Data <- mice(missingData, m=5, maxit = 15, method = 'pmm', seed = 500)
complete_1 <- complete(imputed_Data,1)
complete_2 <- complete(imputed_Data,2)
complete_3 <- complete(imputed_Data,3)
complete_4 <- complete(imputed_Data,4)
complete_5 <- complete(imputed_Data,5)

Z1_new <- round( rowMeans(data.frame(complete_1$Z1,complete_2$Z1,complete_3$Z1, complete_4$Z1,complete_5$Z1)))
Z3_new <- round( rowMeans(data.frame(complete_1$Z3,complete_2$Z3,complete_3$Z3, complete_4$Z3,complete_5$Z3)))

complete_ave<- data.frame(complete_3$R3,complete_3$Z2,complete_3$X, complete_3$R1,complete_3$Y,complete_3$Z4, Z1_new,Z3_new)  
####################################################################
##### Computing Adjustment Set for imputed data P(y | do(x)) ######
##### We need to compute this for different sets of values ####

#### Y = 0 , X = 0 ##########
IP_YXZ1 <- nrow(subset(complete_ave, Y ==0 & X == 0 & Z1_new == 0))
IP_XZ1 <- nrow(subset(complete_ave, (X == 0 & Z1_new == 0)))
IP_Z1 <- nrow(subset(complete_ave, Z1_new==0))

Ipart_1 = IP_YXZ1/IP_XZ1*IP_Z1/n

IP_YXZ1_2 <- nrow(subset(complete_ave, Y ==0 & X == 0 & Z1_new == 1))
IP_XZ1_2 <- nrow(subset(complete_ave, (X == 0 & Z1_new == 1)))
IP_Z1_2 <- nrow(subset(complete_ave, Z1_new==1))
Ipart_2 = IP_YXZ1_2/IP_XZ1_2*IP_Z1_2/n  

ICE_00 <- Ipart_1 + Ipart_2 
#### Y = 1 , X = 0 ##########
IP_YXZ1 <- nrow(subset(complete_ave, Y ==1 & X == 0 & Z1_new == 0))
IP_XZ1 <- nrow(subset(complete_ave, (X == 0 & Z1_new == 0)))
IP_Z1 <- nrow(subset(complete_ave, Z1_new==0))

Ipart_1 = IP_YXZ1/IP_XZ1*IP_Z1/n

IP_YXZ1_2 <- nrow(subset(complete_ave, Y ==1 & X == 0 & Z1_new == 1))
IP_XZ1_2 <- nrow(subset(complete_ave, (X == 0 & Z1_new == 1)))
IP_Z1_2 <- nrow(subset(complete_ave, Z1_new==1))
Ipart_2 = IP_YXZ1_2/IP_XZ1_2*IP_Z1_2/n  

ICE_10 <- Ipart_1 + Ipart_2 

#### Y = 0 , X = 1 ##########
IP_YXZ1 <- nrow(subset(complete_ave, Y==0 & X == 1 & Z1_new == 0))
IP_XZ1 <- nrow(subset(complete_ave, (X == 1 & Z1_new == 0)))
IP_Z1 <- nrow(subset(complete_ave, Z1_new==0))

Ipart_1 = IP_YXZ1/IP_XZ1*IP_Z1/n

IP_YXZ1_2 <- nrow(subset(complete_ave, Y ==0 & X == 1 & Z1_new == 1))
IP_XZ1_2 <- nrow(subset(complete_ave, (X == 1 & Z1_new == 1)))
IP_Z1_2 <- nrow(subset(complete_ave, Z1_new ==1))
Ipart_2 = IP_YXZ1_2/IP_XZ1_2*IP_Z1_2/n  

ICE_01 <- Ipart_1 + Ipart_2


#### Y = 1 , X = 1 ##########
IP_YXZ1 <- nrow(subset(complete_ave, Y ==1 & X == 1 & Z1_new == 0))
IP_XZ1 <- nrow(subset(complete_ave, (X == 1 & Z1_new == 0)))
IP_Z1 <- nrow(subset(complete_ave, Z1_new==0))

Ipart_1 = IP_YXZ1/IP_XZ1*IP_Z1/n

IP_YXZ1_2 <- nrow(subset(complete_ave, Y ==1 & X == 1 & Z1_new == 1))
IP_XZ1_2 <- nrow(subset(complete_ave, (X == 1 & Z1_new == 1)))
IP_Z1_2 <- nrow(subset(complete_ave, Z1_new==1))
Ipart_2 = IP_YXZ1_2/IP_XZ1_2*IP_Z1_2/n  

ICE_11 <- Ipart_1 + Ipart_2
