install.packages("mice")
install.packages("missForest")
# This is the code for generating data set for a graph figure 5.
# To show how the adjustment work.

###################### Graph 1 ####################
# Creating random numbers for each variables. 
#We consider the variables have binary values. 
n = 5000
R5 <- rbinom(n, size= 1, prob = 0.85)
Z3 <- rbinom(n, size= 1, prob = 0.5)
R3 <- integer(n);
X1 <- integer(n);  
Z5 <- integer(n);
C <- integer(n);
X2 <- integer(n);
Y <- integer(n);

######## Table 1 P(Z3 | Z5) and Table 2 P(X2 | Z3) and Table 3 P(X1 | Z3) ######## 
for ( i in 1:n)   
{ 
  if(Z3[i] == 0)
  {   
    X1[i] = rbinom(1 , size =1, prob = 0.5) 
    Z5[i] = rbinom(1, size =1, prob = 0.3) 
    X2[i] = rbinom(1, size =1, prob = 0.5) 
    }   
  else if(Z3[i] == 1)
  {   
    X1[i] = rbinom(1, size =1,prob = 0.6) 
    Z5[i] = rbinom(1, size =1,prob = 0.9) 
    X2[i] = rbinom(1, size =1,prob = 0.8) 
  }   
}

######## Table P(Y| X1, Z5, X2)  ############ 
for ( i in 1:n)   
{ 
  if(X1[i] == 0 & Z5[i] == 0 & X2[i] == 0)
    Y[i] = rbinom(1,size =1,prob = 0.9) 
  if(X1[i] == 0 & Z5[i] == 0 & X2[i] == 1)
    Y[i] = rbinom(1,size =1,prob = 0.88) 
  if(X1[i] == 0 & Z5[i] == 1 & X2[i] == 0)
    Y[i] = rbinom(1,size =1,prob = 0.2) 
  if(X1[i] == 0 & Z5[i] == 1 & X2[i] == 1)
    Y[i] = rbinom(1,size =1,prob = 0.35) 

  if(X1[i] == 1 & Z5[i] == 0 & X2[i] == 0)
    Y[i] = rbinom(1,size =1,prob = 0.9) 
  if(X1[i] == 1 & Z5[i] == 0 & X2[i] == 1)
    Y[i] = rbinom(1,size =1,prob = 0.3) 
  if(X1[i] == 1 & Z5[i] == 1 & X2[i] == 0)
    Y[i] = rbinom(1,size =1,prob = 0.75) 
  if(X1[i] == 1 & Z5[i] == 1 & X2[i] == 1)
    Y[i] = rbinom(1,size =1,prob = 0.15) 

}

######## Table  P(C | Y, R5)  ############ 
for ( i in 1:n)   
{ if(Y[i] == 0 & R5[i] == 0)
    C[i] = rbinom(1, size =1, prob = 0.7) 
  if(Y[i] == 0 & R5[i] == 1)
    C[i] = rbinom(1, size=1, prob = 0.32) 
  if(Y[i] == 1 & R5[i] == 0)
    C[i] = rbinom(1, size=1, prob = 0.1) 
  if(Y[i] == 1 & R5[i] == 1)
    C[i] = rbinom(1, size=1, prob = 0.3) 
}

######## Table  P(R3 | C)  ############ 
for ( i in 1:n)   
{ 
  if(C[i] == 0)
    R3[i] = rbinom(1, size =1,prob = 0.8) 
  else if(C[i] == 1)
    R3[i] = rbinom(1, size =1,prob = 0.7) 
}

fullDataSet <- data.frame(Z3,X1,X2,Z5,Y,C,R5,R3) 

##### Computing Adjustment Set P(y | do(x)) ######
##### We need to compute this for different sets of values ####
start_time <- Sys.time()
#### Y = 0 , X1 = 0 , X2 = 0 ##########
P_YXZ35 <- nrow(subset(fullDataSet, Y == 0 & X1 == 0 &  X2 == 0 & Z3 == 0   ))
P_XZ35 <- nrow(subset(fullDataSet, (X1 == 0 & X2 == 0 & Z3 == 0   )))
P_Z35 <- nrow(subset(fullDataSet,  Z3 == 0   ))

part_1 = P_YXZ35/P_XZ35*P_Z35/n

P_YXZ35_2 <- nrow(subset(fullDataSet, Y ==0 & X1 == 0 & X2 == 0 & Z3 == 1   ))
P_XZ35_2 <- nrow(subset(fullDataSet, (X1 == 0 & X2 == 0 & Z3 == 1   )))
P_Z35_2 <- nrow(subset(fullDataSet,  Z3 == 1   ))
part_2 = P_YXZ35_2/P_XZ35_2*P_Z35_2/n  

CE_000 <- part_1 + part_2 
#### Y = 1 , X1 = 0 , X2 = 0 ##########

P_YXZ35 <- nrow(subset(fullDataSet, Y ==1 & X1 == 0 & X2 == 0 & Z3 == 0   ))
P_XZ35 <- nrow(subset(fullDataSet, (X1 == 0 & X2 == 0 & Z3 == 0   )))
P_Z35 <- nrow(subset(fullDataSet,  Z3 == 0   ))

part_1 = P_YXZ35/P_XZ35*P_Z35/n

P_YXZ35_2 <- nrow(subset(fullDataSet, Y ==1 & X1 == 0 & X2 == 0 & Z3 == 1   ))
P_XZ35_2 <- nrow(subset(fullDataSet, (X1 == 0 & X2 == 0 &  Z3 == 1   )))
P_Z35_2 <- nrow(subset(fullDataSet,  Z3 == 1   ))
part_2 = P_YXZ35_2/P_XZ35_2*P_Z35_2/n  

CE_100 <- part_1 + part_2 

#### Y = 0 , X1 = 1 , X2 = 0 ##########
P_YXZ35 <- nrow(subset(fullDataSet, Y ==0 & X1 == 1 & X2 == 0 &  Z3 == 0   ))
P_XZ35 <- nrow(subset(fullDataSet, (X1 == 1 & X2 == 0 &  Z3 == 0   )))
P_Z35 <- nrow(subset(fullDataSet,  Z3 == 0   ))

part_1 = P_YXZ35/P_XZ35*P_Z35/n

P_YXZ35_2 <- nrow(subset(fullDataSet, Y ==0 & X1 == 1 & X2 == 0 &  Z3 == 1   ))
P_XZ35_2 <- nrow(subset(fullDataSet, (X1 == 1 & X2 == 0 &  Z3 == 1   )))
P_Z35_2 <- nrow(subset(fullDataSet, Z3 == 1   ))
part_2 = P_YXZ35_2/P_XZ35_2*P_Z35_2/n  

CE_010 <- part_1 + part_2 


#### Y = 1 , X1 = 1 , X2 = 0 ##########
P_YXZ35 <- nrow(subset(fullDataSet, Y ==1 & X1 == 1 & X2 == 0 & Z3 == 0   ))
P_XZ35 <- nrow(subset(fullDataSet, (X1 == 1 & X2 == 0 & Z3 == 0   )))
P_Z35 <- nrow(subset(fullDataSet,  Z3 == 0   ))

part_1 = P_YXZ35/P_XZ35*P_Z35/n

P_YXZ35_2 <- nrow(subset(fullDataSet, Y ==1 & X1 == 1 & X2 == 0  &  Z3 == 1   ))
P_XZ35_2 <- nrow(subset(fullDataSet, (X1 == 1 & X2 == 0 & Z3 == 1   )))
P_Z35_2 <- nrow(subset(fullDataSet, Z3 == 1   ))
part_2 = P_YXZ35_2/P_XZ35_2*P_Z35_2/n  

CE_110 <- part_1 + part_2 
#### Y = 0 , X1 = 0 , X2 = 1 ##########
P_YXZ35 <- nrow(subset(fullDataSet, Y == 0 & X1 == 0 &  X2 == 1 & Z3 == 0   ))
P_XZ35 <- nrow(subset(fullDataSet, (X1 == 0 & X2 == 1 & Z3 == 0   )))
P_Z35 <- nrow(subset(fullDataSet,  Z3 == 0   ))

part_1 = P_YXZ35/P_XZ35*P_Z35/n

P_YXZ35_2 <- nrow(subset(fullDataSet, Y ==0 & X1 == 0 & X2 == 1 & Z3 == 1   ))
P_XZ35_2 <- nrow(subset(fullDataSet, (X1 == 0 & X2 == 1 & Z3 == 1   )))
P_Z35_2 <- nrow(subset(fullDataSet,  Z3 == 1   ))
part_2 = P_YXZ35_2/P_XZ35_2*P_Z35_2/n  

CE_001 <- part_1 + part_2 
#### Y = 1 , X1 = 0 , X2 = 0 ##########

P_YXZ35 <- nrow(subset(fullDataSet, Y ==1 & X1 == 0 & X2 == 1 & Z3 == 0   ))
P_XZ35 <- nrow(subset(fullDataSet, (X1 == 0 & X2 == 1 & Z3 == 0   )))
P_Z35 <- nrow(subset(fullDataSet,  Z3 == 0   ))

part_1 = P_YXZ35/P_XZ35*P_Z35/n

P_YXZ35_2 <- nrow(subset(fullDataSet, Y ==1 & X1 == 0 & X2 == 1 & Z3 == 1   ))
P_XZ35_2 <- nrow(subset(fullDataSet, (X1 == 0 & X2 == 1 &  Z3 == 1   )))
P_Z35_2 <- nrow(subset(fullDataSet,  Z3 == 1   ))
part_2 = P_YXZ35_2/P_XZ35_2*P_Z35_2/n  

CE_101 <- part_1 + part_2  

#### Y = 0 , X1 = 1 , X2 = 0 ##########
P_YXZ35 <- nrow(subset(fullDataSet, Y ==0 & X1 == 1 & X2 == 1 &  Z3 == 0   ))
P_XZ35 <- nrow(subset(fullDataSet, (X1 == 1 & X2 == 1 &  Z3 == 0   )))
P_Z35 <- nrow(subset(fullDataSet,  Z3 == 0   ))

part_1 = P_YXZ35/P_XZ35*P_Z35/n

P_YXZ35_2 <- nrow(subset(fullDataSet, Y ==0 & X1 == 1 & X2 == 1 &  Z3 == 1   ))
P_XZ35_2 <- nrow(subset(fullDataSet, (X1 == 1 & X2 == 1 &  Z3 == 1   )))
P_Z35_2 <- nrow(subset(fullDataSet, Z3 == 1   ))
part_2 = P_YXZ35_2/P_XZ35_2*P_Z35_2/n  

CE_011 <- part_1 + part_2

#### Y = 1 , X1 = 1 , X2 = 0 ##########
P_YXZ35 <- nrow(subset(fullDataSet, Y ==1 & X1 == 1 & X2 == 1 & Z3 == 0   ))
P_XZ35 <- nrow(subset(fullDataSet, (X1 == 1 & X2 == 1 & Z3 == 0   )))
P_Z35 <- nrow(subset(fullDataSet,  Z3 == 0   ))

part_1 = P_YXZ35/P_XZ35*P_Z35/n

P_YXZ35_2 <- nrow(subset(fullDataSet, Y ==1 & X1 == 1 & X2 == 1  &  Z3 == 1   ))
P_XZ35_2 <- nrow(subset(fullDataSet, (X1 == 1 & X2 == 1 & Z3 == 1   )))
P_Z35_2 <- nrow(subset(fullDataSet, Z3 == 1   ))
part_2 = P_YXZ35_2/P_XZ35_2*P_Z35_2/n  

CE_111 <- part_1 + part_2 
print("Causal effect computation in original dataset")
end_time <- Sys.time()
print(end_time-start.time)
################# missing data ###########################################
missingData <- fullDataSet
missingData$Z3[missingData$R3 == 0] <- NA
missingData$Z5[missingData$R5 == 0] <- NA
######################## m-adjustment ###################################  

##### Computing M-Adjustment Set P(y | do(x)) ######
##### We need to compute this for different sets of values ####
start_timeM <- Sys.time()
#### Y = 0 , X1 = 0 , X2 = 0 ##########
MP_YXZ5 <- nrow(subset(missingData, Y ==0 & X1 == 0 & X2 == 0 & Z5 == 0  & R5 ==1))
MP_XZ5 <- nrow(subset(missingData, (X1 == 0 & X2 == 0 & Z5 == 0 & R5 == 1)))
MP_Z5 <- nrow(subset(missingData, Z5==0 & R5 == 1))
NM <-  nrow(subset(missingData, R5 == 1))
Mpart_1 = MP_YXZ5/MP_XZ5*MP_Z5/NM

MP_YXZ5_2 <- nrow(subset(missingData, Y ==0 & X1 == 0  & X2 == 0 & Z5 == 1 & R5 == 1))
MP_XZ5_2 <- nrow(subset(missingData, (X1 == 0 & X2 == 0 & Z5 == 1 & R5 == 1)))
MP_Z5_2 <- nrow(subset(missingData, Z5==1  & R5 == 1))
Mpart_2 = MP_YXZ5_2/MP_XZ5_2*MP_Z5_2/NM  


CEM_000 <- Mpart_1 + Mpart_2 

#### Y = 1 , X1 = 0 , X2 = 0 ##########
MP_YXZ5 <- nrow(subset(missingData, Y ==1 & X1 == 0 & X2 == 0 & Z5 == 0  & R5 == 1))
MP_XZ5 <- nrow(subset(missingData, (X1 == 0 & X2 == 0 & Z5 == 0 & R5 == 1)))
MP_Z5 <- nrow(subset(missingData, Z5==0 & R5 == 1))

Mpart_1 = MP_YXZ5/MP_XZ5*MP_Z5/NM

MP_YXZ5_2 <- nrow(subset(missingData, Y ==1 & X1 == 0 & X2 == 0 & Z5 == 1  & R5 == 1))
MP_XZ5_2 <- nrow(subset(missingData, (X1 == 0 & X2 == 0 & Z5 == 1  & R5 == 1)))
MP_Z5_2 <- nrow(subset(missingData, Z5==1 & R5 == 1))
Mpart_2 = MP_YXZ5_2/MP_XZ5_2*MP_Z5_2/NM  

CEM_100 <- Mpart_1 + Mpart_2 

#### Y = 0 , X = 1 ##########
MP_YXZ5 <- nrow(subset(missingData, Y ==0 & X1 == 1  & X2 == 0  & Z5 == 0 & R5 ==1))
MP_XZ5 <- nrow(subset(missingData, (X1 == 1 & X2 == 0 & Z5 == 0 & R5 ==1)))
MP_Z5 <- nrow(subset(missingData, Z5==0 & R5 ==1))

Mpart_1 = MP_YXZ5/MP_XZ5*MP_Z5/NM

MP_YXZ5_2 <- nrow(subset(missingData, Y ==0 & X1 == 1 & X2 == 0 & Z5 == 1 & R5 ==1))
MP_XZ5_2 <- nrow(subset(missingData, (X1 == 1 & X2 == 0 & Z5 == 1 & R5 ==1)))
MP_Z5_2 <- nrow(subset(missingData, Z5==1 & R5 ==1))
Mpart_2 = MP_YXZ5_2/MP_XZ5_2*MP_Z5_2/NM  

CEM_010 <- Mpart_1 + Mpart_2

#### Y = 1 , X1 = 1 , X2 = 0 ##########
MP_YXZ5 <- nrow(subset(missingData, Y ==1 & X1 == 1 & X2 == 0 & Z5 ==0  & R5 == 1))
MP_XZ5 <- nrow(subset(missingData, (X1 == 1 & X2 == 0 & Z5 == 0  & R5 == 1)))
MP_Z5 <- nrow(subset(missingData, Z5==0  & R5 == 1))

Mpart_1 = MP_YXZ5/MP_XZ5*MP_Z5/NM

MP_YXZ5_2 <- nrow(subset(missingData, Y ==1 & X1 == 1 & X2 == 0 & Z5 == 1 & R5 == 1))
MP_XZ5_2 <- nrow(subset(missingData, (X1 == 1 & X2 == 0 & Z5 == 1  & R5 == 1)))
MP_Z5_2 <- nrow(subset(missingData, Z5 ==1  & R5 == 1))
Mpart_2 = MP_YXZ5_2/MP_XZ5_2*MP_Z5_2/NM  

CEM_110 <- Mpart_1 + Mpart_2

#### Y = 0 , X1 = 0 , X2 = 1 ##########
MP_YXZ5 <- nrow(subset(missingData, Y ==0 & X1 == 0 & X2 == 1 & Z5 == 0 & R5 ==1))
MP_XZ5 <- nrow(subset(missingData, (X1 == 0 & X2 == 1 & Z5 == 0 & R5 == 1)))
MP_Z5 <- nrow(subset(missingData, Z5==0 & R5 == 1))
NM <-  nrow(subset(missingData, R5 == 1))
Mpart_1 = MP_YXZ5/MP_XZ5*MP_Z5/NM

MP_YXZ5_2 <- nrow(subset(missingData, Y ==0 & X1 == 0  & X2 == 1 & Z5 == 1 & R5 == 1))
MP_XZ5_2 <- nrow(subset(missingData, (X1 == 0 & X2 == 1 & Z5 == 1 & R5 == 1)))
MP_Z5_2 <- nrow(subset(missingData, Z5==1  & R5 == 1))
Mpart_2 = MP_YXZ5_2/MP_XZ5_2*MP_Z5_2/NM  


CEM_001 <- Mpart_1 + Mpart_2 

#### Y = 1 , X1 = 0 , X2 = 0 ##########
MP_YXZ5 <- nrow(subset(missingData, Y ==1 & X1 == 0 & X2 == 1 & Z5 == 0  & R5 == 1))
MP_XZ5 <- nrow(subset(missingData, (X1 == 0 & X2 == 1 & Z5 == 0 & R5 == 1)))
MP_Z5 <- nrow(subset(missingData, Z5==0 & R5 == 1))

Mpart_1 = MP_YXZ5/MP_XZ5*MP_Z5/NM

MP_YXZ5_2 <- nrow(subset(missingData, Y ==1 & X1 == 0 & X2 == 1 & Z5 == 1  & R5 == 1))
MP_XZ5_2 <- nrow(subset(missingData, (X1 == 0 & X2 == 1 & Z5 == 1  & R5 == 1)))
MP_Z5_2 <- nrow(subset(missingData, Z5==1 & R5 == 1))
Mpart_2 = MP_YXZ5_2/MP_XZ5_2*MP_Z5_2/NM  

CEM_101 <- Mpart_1 + Mpart_2 

#### Y = 0 , X1 = 1 , X2 = 1 ##########
MP_YXZ5 <- nrow(subset(missingData, Y ==0 & X1 == 1  & X2 == 1  & Z5 == 0 & R5 ==1))
MP_XZ5 <- nrow(subset(missingData, (X1 == 1 & X2 == 1 & Z5 == 0 & R5 ==1)))
MP_Z5 <- nrow(subset(missingData, Z5==0 & R5 ==1))

Mpart_1 = MP_YXZ5/MP_XZ5*MP_Z5/NM

MP_YXZ5_2 <- nrow(subset(missingData, Y ==0 & X1 == 1 & X2 == 1 & Z5 == 1 & R5 ==1))
MP_XZ5_2 <- nrow(subset(missingData, (X1 == 1 & X2 == 1 & Z5 == 1 & R5 ==1)))
MP_Z5_2 <- nrow(subset(missingData, Z5==1 & R5 ==1))
Mpart_2 = MP_YXZ5_2/MP_XZ5_2*MP_Z5_2/NM  

CEM_011 <- Mpart_1 + Mpart_2

#### Y = 1 , X1 = 1 , X2 = 1 ##########
MP_YXZ5 <- nrow(subset(missingData, Y ==1 & X1 == 1 & X2 == 1 & Z5 ==0  & R5 == 1))
MP_XZ5 <- nrow(subset(missingData, (X1 == 1 & X2 == 1 & Z5 == 0  & R5 == 1)))
MP_Z5 <- nrow(subset(missingData, Z5==0  & R5 == 1))

Mpart_1 = MP_YXZ5/MP_XZ5*MP_Z5/NM

MP_YXZ5_2 <- nrow(subset(missingData, Y ==1 & X1 == 1 & X2 == 1 & Z5 == 1 & R5 == 1))
MP_XZ5_2 <- nrow(subset(missingData, (X1 == 1 & X2 == 1 & Z5 == 1  & R5 == 1)))
MP_Z5_2 <- nrow(subset(missingData, Z5 ==1  & R5 == 1))
Mpart_2 = MP_YXZ5_2/MP_XZ5_2*MP_Z5_2/NM  

CEM_111 <- Mpart_1 + Mpart_2

print("adjustment technique")
end_timeM <- Sys.time()
print(end_timeM-start_timeM)
#################  adjustment with imputation #########################
md.pattern(missingData)
md.pairs(missingData)

library(mice)
start_timeI <- Sys.time()
imputed_Data <- mice(missingData, m=5, maxit = 15, method = 'pmm', seed = 500)
complete_1 <- complete(imputed_Data,1)
complete_2 <- complete(imputed_Data,2)
complete_3 <- complete(imputed_Data,3)
complete_4 <- complete(imputed_Data,4)
complete_5 <- complete(imputed_Data,5)

Z5_new <- round( rowMeans(data.frame(complete_1$Z5,complete_2$Z5,complete_3$Z5, complete_4$Z5,complete_5$Z5)))
Z3_new <- round( rowMeans(data.frame(complete_1$Z3,complete_2$Z3,complete_3$Z3, complete_4$Z3,complete_5$Z3)))

complete_ave<- data.frame(complete_3$R3,complete_3$X1,complete_3$R5, complete_3$X2,complete_3$Y,complete_3$C, Z5_new,Z3_new)  

####################################################################
##### Computing Adjustment Set for imputed data P(y | do(x)) ######
##### We need to compute this for different sets of values ####

#### Y = 0 , X1 = 0 , X2 = 0 ##########
IP_YXZ35 <- nrow(subset(complete_ave, Y ==0 & X1 == 0 &  X2 == 0  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_ave, (X1 == 0 &  X2 == 0  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_ave, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_ave, Y ==0 & X1 == 0 & X2 == 0  & Z3_new == 1 ))
IP_XZ35_2 <- nrow(subset(complete_ave, (X1 == 0 & X2 == 0  & Z3_new == 1)))
IP_Z35_2 <- nrow(subset(complete_ave, Z3_new == 1 ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

ICE_000 <- Ipart_1 + Ipart_2 
#### Y = 1 , X1 = 0, , X2 = 0  ##########

IP_YXZ35 <- nrow(subset(complete_ave, Y ==1 & X1 == 0 & X2 == 0  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_ave, (X1 == 0 & X2 == 0  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_ave, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_ave, Y ==1 & X1 == 0 & X2 == 0  & Z3_new == 1  ))
IP_XZ35_2 <- nrow(subset(complete_ave, (X1 == 0 &  X2 == 0  & Z3_new == 1  )))
IP_Z35_2 <- nrow(subset(complete_ave, Z3_new == 1  ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

ICE_100 <- Ipart_1 + Ipart_2

#### Y = 0 , X1 = 1 , X2 = 0 ##########
IP_YXZ35 <- nrow(subset(complete_ave, Y==0 & X1 == 1 &  X2 == 0  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_ave, (X1 == 1 &  X2 == 0  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_ave, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_ave, Y ==0 & X1 == 1 &  X2 == 0  & Z3_new == 1 ))
IP_XZ35_2 <- nrow(subset(complete_ave, (X1 == 1 &  X2 == 0  & Z3_new == 1 )))
IP_Z35_2 <- nrow(subset(complete_ave, Z3_new == 1 ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

ICE_010 <- Ipart_1 + Ipart_2 

#### Y = 1 , X1 = 1 , X2 = 0 ##########
IP_YXZ35 <- nrow(subset(complete_ave, Y ==1 & X1 == 1 &  X2 == 0  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_ave, (X1 == 1 &  X2 == 0  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_ave, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_ave, Y ==1 & X1 == 1 & X2 == 0  & Z3_new == 1  ))
IP_XZ35_2 <- nrow(subset(complete_ave, (X1 == 1 &  X2 == 0  & Z3_new == 1  )))
IP_Z35_2 <- nrow(subset(complete_ave, Z3_new == 1  ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

ICE_110 <- Ipart_1 + Ipart_2 

#### Y = 0 , X1 = 0 , X2 = 1 ##########
IP_YXZ35 <- nrow(subset(complete_ave, Y ==0 & X1 == 0 &  X2 == 1  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_ave, (X1 == 0 &  X2 == 1  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_ave, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_ave, Y ==0 & X1 == 0 & X2 == 1  & Z3_new == 1 ))
IP_XZ35_2 <- nrow(subset(complete_ave, (X1 == 0 & X2 == 1  & Z3_new == 1 )))
IP_Z35_2 <- nrow(subset(complete_ave, Z3_new == 1 ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

ICE_001 <- Ipart_1 + Ipart_2 
#### Y = 1 , X1 = 0, , X2 = 0  ##########

IP_YXZ35 <- nrow(subset(complete_ave, Y ==1 & X1 == 0 & X2 == 1  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_ave, (X1 == 0 & X2 == 1  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_ave, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_ave, Y ==1 & X1 == 0 & X2 == 1  & Z3_new == 1  ))
IP_XZ35_2 <- nrow(subset(complete_ave, (X1 == 0 &  X2 == 1  & Z3_new == 1  )))
IP_Z35_2 <- nrow(subset(complete_ave, Z3_new == 1  ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

ICE_101 <- Ipart_1 + Ipart_2 

#### Y = 0 , X1 = 1 , X2 = 0 ##########
IP_YXZ35 <- nrow(subset(complete_ave, Y==0 & X1 == 1 &  X2 == 1  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_ave, (X1 == 1 &  X2 == 1  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_ave, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_ave, Y ==0 & X1 == 1 &  X2 == 1  & Z3_new == 1))
IP_XZ35_2 <- nrow(subset(complete_ave, (X1 == 1 &  X2 == 1  & Z3_new == 1)))
IP_Z35_2 <- nrow(subset(complete_ave, Z3_new == 1))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

ICE_011 <- Ipart_1 + Ipart_2 
#### Y = 1 , X1 = 1 , X2 = 0 ##########
IP_YXZ35 <- nrow(subset(complete_ave, Y ==1 & X1 == 1 &  X2 == 1  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_ave, (X1 == 1 &  X2 == 1  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_ave, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_ave, Y ==1 & X1 == 1 & X2 == 1  & Z3_new == 1))
IP_XZ35_2 <- nrow(subset(complete_ave, (X1 == 1 &  X2 == 1 & Z3_new == 1  )))
IP_Z35_2 <- nrow(subset(complete_ave, Z3_new == 1 ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  


ICE_111 <- Ipart_1 + Ipart_2  
end_timeI <- Sys.time()
print("Mice Time")


#################  adjustment with imputation #########################
md.pattern(missingData)
md.pairs(missingData)

library(missForest)
start_timeF <- Sys.time()
imputed_DataF <-missForest(missingData)
complete_1 <- imputed_DataF$ximp

Z5_new <- round( complete_1$Z5)
Z3_new <- round(complete_1$Z3)

complete_aveF<- data.frame(complete_1$R3,complete_1$X1,complete_1$R5, complete_1$X2,complete_1$Y,complete_1$C, Z5_new,Z3_new)  



####################################################################
##### Computing Adjustment Set for imputed data P(y | do(x)) ######
##### We need to compute this for different sets of values ####

#### Y = 0 , X1 = 0 , X2 = 0 ##########
IP_YXZ35 <- nrow(subset(complete_aveF, Y ==0 & X1 == 0 &  X2 == 0  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_aveF, (X1 == 0 &  X2 == 0  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_aveF, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_aveF, Y ==0 & X1 == 0 & X2 == 0  & Z3_new == 1 ))
IP_XZ35_2 <- nrow(subset(complete_aveF, (X1 == 0 & X2 == 0  & Z3_new == 1)))
IP_Z35_2 <- nrow(subset(complete_aveF, Z3_new == 1 ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

FCE_000 <- Ipart_1 + Ipart_2 
#### Y = 1 , X1 = 0, , X2 = 0  ##########

IP_YXZ35 <- nrow(subset(complete_aveF, Y ==1 & X1 == 0 & X2 == 0  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_aveF, (X1 == 0 & X2 == 0  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_aveF, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_aveF, Y ==1 & X1 == 0 & X2 == 0  & Z3_new == 1  ))
IP_XZ35_2 <- nrow(subset(complete_aveF, (X1 == 0 &  X2 == 0  & Z3_new == 1  )))
IP_Z35_2 <- nrow(subset(complete_aveF, Z3_new == 1  ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

FCE_100 <- Ipart_1 + Ipart_2

#### Y = 0 , X1 = 1 , X2 = 0 ##########
IP_YXZ35 <- nrow(subset(complete_aveF, Y==0 & X1 == 1 &  X2 == 0  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_aveF, (X1 == 1 &  X2 == 0  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_aveF, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_aveF, Y ==0 & X1 == 1 &  X2 == 0  & Z3_new == 1 ))
IP_XZ35_2 <- nrow(subset(complete_aveF, (X1 == 1 &  X2 == 0  & Z3_new == 1 )))
IP_Z35_2 <- nrow(subset(complete_aveF, Z3_new == 1 ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

FCE_010 <- Ipart_1 + Ipart_2 

#### Y = 1 , X1 = 1 , X2 = 0 ##########
IP_YXZ35 <- nrow(subset(complete_aveF, Y ==1 & X1 == 1 &  X2 == 0  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_aveF, (X1 == 1 &  X2 == 0  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_aveF, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_aveF, Y ==1 & X1 == 1 & X2 == 0  & Z3_new == 1  ))
IP_XZ35_2 <- nrow(subset(complete_aveF, (X1 == 1 &  X2 == 0  & Z3_new == 1  )))
IP_Z35_2 <- nrow(subset(complete_aveF, Z3_new == 1  ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

FCE_110 <- Ipart_1 + Ipart_2 

#### Y = 0 , X1 = 0 , X2 = 1 ##########
IP_YXZ35 <- nrow(subset(complete_aveF, Y ==0 & X1 == 0 &  X2 == 1  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_aveF, (X1 == 0 &  X2 == 1  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_aveF, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_aveF, Y ==0 & X1 == 0 & X2 == 1  & Z3_new == 1 ))
IP_XZ35_2 <- nrow(subset(complete_aveF, (X1 == 0 & X2 == 1  & Z3_new == 1 )))
IP_Z35_2 <- nrow(subset(complete_aveF, Z3_new == 1 ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

FCE_001 <- Ipart_1 + Ipart_2 
#### Y = 1 , X1 = 0, , X2 = 0  ##########

IP_YXZ35 <- nrow(subset(complete_aveF, Y ==1 & X1 == 0 & X2 == 1  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_aveF, (X1 == 0 & X2 == 1  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_aveF, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_aveF, Y ==1 & X1 == 0 & X2 == 1  & Z3_new == 1  ))
IP_XZ35_2 <- nrow(subset(complete_aveF, (X1 == 0 &  X2 == 1  & Z3_new == 1  )))
IP_Z35_2 <- nrow(subset(complete_aveF, Z3_new == 1  ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

FCE_101 <- Ipart_1 + Ipart_2 

#### Y = 0 , X1 = 1 , X2 = 0 ##########
IP_YXZ35 <- nrow(subset(complete_aveF, Y==0 & X1 == 1 &  X2 == 1  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_aveF, (X1 == 1 &  X2 == 1  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_aveF, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_aveF, Y ==0 & X1 == 1 &  X2 == 1  & Z3_new == 1))
IP_XZ35_2 <- nrow(subset(complete_aveF, (X1 == 1 &  X2 == 1  & Z3_new == 1)))
IP_Z35_2 <- nrow(subset(complete_aveF, Z3_new == 1))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  

FCE_011 <- Ipart_1 + Ipart_2 
#### Y = 1 , X1 = 1 , X2 = 0 ##########
IP_YXZ35 <- nrow(subset(complete_aveF, Y ==1 & X1 == 1 &  X2 == 1  & Z3_new == 0  ))
IP_XZ35 <- nrow(subset(complete_aveF, (X1 == 1 &  X2 == 1  & Z3_new == 0  )))
IP_Z35 <- nrow(subset(complete_aveF, Z3_new == 0  ))

Ipart_1 = IP_YXZ35/IP_XZ35*IP_Z35/n

IP_YXZ35_2 <- nrow(subset(complete_aveF, Y ==1 & X1 == 1 & X2 == 1  & Z3_new == 1))
IP_XZ35_2 <- nrow(subset(complete_aveF, (X1 == 1 &  X2 == 1 & Z3_new == 1  )))
IP_Z35_2 <- nrow(subset(complete_aveF, Z3_new == 1 ))
Ipart_2 = IP_YXZ35_2/IP_XZ35_2*IP_Z35_2/n  


FCE_111 <- Ipart_1 + Ipart_2  
end_timeF <- Sys.time()
print("Random Forest")
dif_madj <- print(abs(CEM_000 - CE_000) + abs(CEM_001 - CE_001) + abs(CEM_010 - CE_010)
                  + abs(CEM_011 - CE_011) + abs(CEM_100 - CE_100) +  abs(CEM_101 - CE_101)
                  +  abs(CEM_110 - CE_110) +  abs(CEM_111 - CE_111) )
timedifM <- end_timeM-start_timeM
dif_mice <- print(abs(ICE_000 - CE_000) + abs(ICE_001 - CE_001) + abs(ICE_010 - CE_010)
                  + abs(ICE_011 - CE_011) + abs(ICE_100 - CE_100) +  abs(ICE_101 - CE_101)
                  +  abs(ICE_110 - CE_110) +  abs(ICE_111 - CE_111) )

timedifI <- end_timeI-start_timeI
dif_missF <- print(abs(FCE_000 - CE_000) + abs(FCE_001 - CE_001) + abs(FCE_010 - CE_010)
                  + abs(FCE_011 - CE_011) + abs(FCE_100 - CE_100) +  abs(FCE_101 - CE_101)
                  +  abs(FCE_110 - CE_110) +  abs(FCE_111 - CE_111) )
timedifF <- end_timeF-start_timeF

  print(paste0(' timedifF = ', timedifF , ' timedifI = ', timedifI ,' timedifM = ', timedifM , 
          ' dif_missF', dif_missF , ' dif_mice' , dif_mice , ' dif_madj' , dif_madj))
