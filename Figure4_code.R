install.packages("mice")
# This is the code for generating data set for a graph figure 4.
# To show how the adjustment work.

# Creating random numbers for each variables. 
#We consider the variables have binary values. 
  ###################### Graph 1 ####################
    n = 5000
    Z_3 <- rbinom(n, size= 1, prob=0.625)
    Z_2 <- rbinom(n, size= 1, prob= 0.42)
    Z_1 <- integer(n);
    R_Y <- integer(n);  
    X <- integer(n);
    Y <- integer(n);
   
    
    ######## Table 1 P(X | Z_3) ############ 
    for ( i in 1:n)   
    {  if(Z_3[i] == 0)
      X[i] = rbinom(1,size =1,prob = 0.25) 
      else
      X[i] = rbinom(1,size =1,prob = 0.125) 
    }
  
    ######## Table 2 P(Z_1 | Z_3, X) ############     
    for ( i in 1:n)   
    {  if(Z_3[i] == 0 & X[i] == 0)
        Z_1[i] = rbinom(1,size =1,prob = 0.75) 
      else if(Z_3[i] == 0 & X[i] == 1)
        Z_1[i] = rbinom(1,size =1,prob = 0.6) 
      else if(Z_3[i] == 1 & X[i] == 0)
        Z_1[i] = rbinom(1,size =1,prob = 0.42) 
      else
        Z_1[i] = rbinom(1,size =1,prob = 0.5) 
    }
    ######## Table 3 P(Y | Z_1, Z_2) ############     
    for ( i in 1:n)   
    {  if(Z_1[i] == 0 & Z_2[i] == 0)
        Y[i] = rbinom(1,size =1,prob = 0.75) 
    else if(Z_1[i] == 0 & Z_2[i] == 1)
        Y[i] = rbinom(1,size =1,prob = 0.6) 
    else if(Z_1[i] == 1 & Z_2[i] == 0)
        Y[i] = rbinom(1,size =1,prob = 0.42) 
    else
        Y[i] = rbinom(1,size =1,prob = 0.5) 
    }
    
    ######## Table 4 P(R_Y | Z_2, Z_3) ############     
    for ( i in 1:n)   
    {
    if(Z_2[i] == 0 & Z_3[i] == 0)
        R_Y[i] = rbinom(1,size =1,prob = 0.75) 
    else if(Z_2[i] == 0 & Z_3[i] == 1)
        R_Y[i] = rbinom(1,size =1,prob = 0.6) 
    else if(Z_2[i] == 1 & Z_3[i] == 0)
        R_Y[i] = rbinom(1,size =1,prob = 0.42) 
    else
        R_Y[i] = rbinom(1,size =1,prob = 0.5) 
    }
    fullDataSet <- data.frame(X,Y,Z_1,Z_2,Z_3,R_Y) 
    
    ##### Computing Adjustment Set P(y | do(x)) ######
    ##### We need to compute this for different sets of values ####
    
    #### Y = 0 , X = 0 ##########
    P_YXZ3 <- nrow(subset(fullDataSet, Y ==0 & X == 0 & Z_3 == 0))
    P_XZ3 <- nrow(subset(fullDataSet, (X == 0 & Z_3 == 0)))
    P_Z3 <- nrow(subset(fullDataSet, Z_3==0))
      
    part_1 = P_YXZ3/P_XZ3*P_Z3/n
    
    P_YXZ3_2 <- nrow(subset(fullDataSet, Y ==0 & X == 0 & Z_3 == 1))
    P_XZ3_2 <- nrow(subset(fullDataSet, (X == 0 & Z_3 == 1)))
    P_Z3_2 <- nrow(subset(fullDataSet, Z_3==1))
    part_2 = P_YXZ3_2/P_XZ3_2*P_Z3_2/n  
    
    CE_00 <- part_1 + part_2 
    #### Y = 1 , X = 0 ##########
    P_YXZ3 <- nrow(subset(fullDataSet, Y ==1 & X == 0 & Z_3 == 0))
    P_XZ3 <- nrow(subset(fullDataSet, (X == 0 & Z_3 == 0)))
    P_Z3 <- nrow(subset(fullDataSet, Z_3==0))
    
    part_1 = P_YXZ3/P_XZ3*P_Z3/n
    
    P_YXZ3_2 <- nrow(subset(fullDataSet, Y ==1 & X == 0 & Z_3 == 1))
    P_XZ3_2 <- nrow(subset(fullDataSet, (X == 0 & Z_3 == 1)))
    P_Z3_2 <- nrow(subset(fullDataSet, Z_3==1))
    part_2 = P_YXZ3_2/P_XZ3_2*P_Z3_2/n  
    
    CE_10 <- part_1 + part_2 
  
    #### Y = 0 , X = 1 ##########
    P_YXZ3 <- nrow(subset(fullDataSet, Y ==0 & X == 1 & Z_3 == 0))
    P_XZ3 <- nrow(subset(fullDataSet, (X == 1 & Z_3 == 0)))
    P_Z3 <- nrow(subset(fullDataSet, Z_3==0))
    
    part_1 = P_YXZ3/P_XZ3*P_Z3/n
    
    P_YXZ3_2 <- nrow(subset(fullDataSet, Y ==0 & X == 1 & Z_3 == 1))
    P_XZ3_2 <- nrow(subset(fullDataSet, (X == 1 & Z_3 == 1)))
    P_Z3_2 <- nrow(subset(fullDataSet, Z_3==1))
    part_2 = P_YXZ3_2/P_XZ3_2*P_Z3_2/n  
    
    CE_01 <- part_1 + part_2
      
    
    #### Y = 1 , X = 1 ##########
    P_YXZ3 <- nrow(subset(fullDataSet, Y ==1 & X == 1 & Z_3 == 0))
    P_XZ3 <- nrow(subset(fullDataSet, (X == 1 & Z_3 == 0)))
    P_Z3 <- nrow(subset(fullDataSet, Z_3==0))
    
    part_1 = P_YXZ3/P_XZ3*P_Z3/n
    
    P_YXZ3_2 <- nrow(subset(fullDataSet, Y ==1 & X == 1 & Z_3 == 1))
    P_XZ3_2 <- nrow(subset(fullDataSet, (X == 1 & Z_3 == 1)))
    P_Z3_2 <- nrow(subset(fullDataSet, Z_3==1))
    part_2 = P_YXZ3_2/P_XZ3_2*P_Z3_2/n  
    
    CE_11 <- part_1 + part_2
  ################# missing data ###########################################
    missingData <- fullDataSet
    missingData$Y[missingData$R_Y == 0] <- NA 
  ######################## m-adjustment ###################################  
    
    ##### Computing M-Adjustment Set P(y | do(x)) ######
    ##### We need to compute this for different sets of values ####
    
    #### Y = 0 , X = 0 ##########
    MP_YXZ3 <- nrow(subset(missingData, Y ==0 & X == 0 & Z_3 == 0 & Z_2 == 0 & R_Y ==1))
    MP_XZ3 <- nrow(subset(missingData, (X == 0 & Z_3 == 0 & Z_2 == 0 & R_Y == 1)))
    MP_Z3 <- nrow(subset(missingData, Z_3==0 & Z_2 == 0 & R_Y == 1))
    NM <-  nrow(subset(missingData, R_Y == 1))
    Mpart_1 = MP_YXZ3/MP_XZ3*MP_Z3/NM
    
    MP_YXZ3_2 <- nrow(subset(missingData, Y ==0 & X == 0 & Z_3 == 1& Z_2 == 0 & R_Y ==1))
    MP_XZ3_2 <- nrow(subset(missingData, (X == 0 & Z_3 == 1& Z_2 == 0 & R_Y ==1)))
    MP_Z3_2 <- nrow(subset(missingData, Z_3==1 & Z_2 == 0 & R_Y ==1))
    Mpart_2 = MP_YXZ3_2/MP_XZ3_2*MP_Z3_2/NM  
  
    MP_YXZ3_3 <- nrow(subset(missingData, Y ==0 & X == 0 & Z_3 == 0 & Z_2 == 1 & R_Y ==1))
    MP_XZ3_3 <- nrow(subset(missingData, (X == 0 & Z_3 == 0 & Z_2 == 1 & R_Y == 1)))
    MP_Z3_3 <- nrow(subset(missingData, Z_3==0 & Z_2 == 1 & R_Y == 1))
    Mpart_3 = MP_YXZ3_3/MP_XZ3_3*MP_Z3_3/NM
    
    MP_YXZ3_4 <- nrow(subset(missingData, Y ==0 & X == 0 & Z_3 == 1& Z_2 == 1 & R_Y ==1))
    MP_XZ3_4 <- nrow(subset(missingData, (X == 0 & Z_3 == 1& Z_2 == 1 & R_Y ==1)))
    MP_Z3_4 <- nrow(subset(missingData, Z_3==1 & Z_2 == 1 & R_Y ==1))
    Mpart_4 = MP_YXZ3_4/MP_XZ3_4*MP_Z3_4/NM  
    
    CEM_00 <- Mpart_1 + Mpart_2 + Mpart_3 + Mpart_4
    
    #### Y = 1 , X = 0 ##########
    MP_YXZ3 <- nrow(subset(missingData, Y ==1 & X == 0 & Z_3 == 0 & Z_2 == 0 & R_Y ==1))
    MP_XZ3 <- nrow(subset(missingData, (X == 0 & Z_3 == 0 & Z_2 == 0 & R_Y ==1)))
    MP_Z3 <- nrow(subset(missingData, Z_3==0 & Z_2 == 0 & R_Y ==1))
    
    Mpart_1 = MP_YXZ3/MP_XZ3*MP_Z3/NM
    
    MP_YXZ3_2 <- nrow(subset(missingData, Y ==1 & X == 0 & Z_3 == 1& Z_2== 0 & R_Y ==1))
    MP_XZ3_2 <- nrow(subset(missingData, (X == 0 & Z_3 == 1& Z_2 == 0 & R_Y ==1)))
    MP_Z3_2 <- nrow(subset(missingData, Z_3==1& Z_2 == 0 & R_Y ==1))
    Mpart_2 = MP_YXZ3_2/MP_XZ3_2*MP_Z3_2/NM  
    
    MP_YXZ3_3 <- nrow(subset(missingData, Y ==1 & X == 0 & Z_3 == 0 & Z_2 == 1 & R_Y ==1))
    MP_XZ3_3 <- nrow(subset(missingData, (X == 0 & Z_3 == 0 & Z_2 == 1 & R_Y ==1)))
    MP_Z3_3 <- nrow(subset(missingData, Z_3==0 & Z_2 == 1 & R_Y ==1))
    
    Mpart_3 = MP_YXZ3_3/MP_XZ3_3*MP_Z3_3/NM
    
    MP_YXZ3_4 <- nrow(subset(missingData, Y ==1 & X == 0 & Z_3 == 1 & Z_2 == 1 & R_Y ==1))
    MP_XZ3_4 <- nrow(subset(missingData, (X == 0 & Z_3 == 1& Z_2 == 1 & R_Y ==1)))
    MP_Z3_4 <- nrow(subset(missingData, Z_3==1& Z_2 == 1 & R_Y ==1))
    Mpart_4 = MP_YXZ3_4/MP_XZ3_4*MP_Z3_4/NM  
    
    CEM_10 <- Mpart_1 + Mpart_2 + Mpart_3 + Mpart_4
    
    #### Y = 0 , X = 1 ##########
    MP_YXZ3 <- nrow(subset(missingData, Y ==0 & X == 1 & Z_3 == 0& Z_2 == 0 & R_Y ==1))
    MP_XZ3 <- nrow(subset(missingData, (X == 1 & Z_3 == 0& Z_2 == 0 & R_Y ==1)))
    MP_Z3 <- nrow(subset(missingData, Z_3==0& Z_2 == 0 & R_Y ==1))
    
    Mpart_1 = MP_YXZ3/MP_XZ3*MP_Z3/NM
    
    MP_YXZ3_2 <- nrow(subset(missingData, Y ==0 & X == 1 & Z_3 == 1 & Z_2 == 0 & R_Y ==1))
    MP_XZ3_2 <- nrow(subset(missingData, (X == 1 & Z_3 == 1 & Z_2 == 0 & R_Y ==1)))
    MP_Z3_2 <- nrow(subset(missingData, Z_3==1 & Z_2 == 0 & R_Y ==1))
    Mpart_2 = MP_YXZ3_2/MP_XZ3_2*MP_Z3_2/NM  
  
    MP_YXZ3_3 <- nrow(subset(missingData, Y ==0 & X == 1 & Z_3 == 0 & Z_2 == 1 & R_Y ==1))
    MP_XZ3_3 <- nrow(subset(missingData, (X == 1 & Z_3 == 0& Z_2 == 1 & R_Y ==1)))
    MP_Z3_3 <- nrow(subset(missingData, Z_3==0 & Z_2 == 1 & R_Y ==1))
    
    part_3 = MP_YXZ3_3/MP_XZ3_3*MP_Z3_3/NM
    
    MP_YXZ3_4 <- nrow(subset(missingData, Y ==0 & X == 1 & Z_3 == 1 & Z_2 == 1 & R_Y ==1))
    MP_XZ3_4 <- nrow(subset(missingData, (X == 1 & Z_3 == 1 & Z_2 == 1 & R_Y ==1)))
    MP_Z3_4 <- nrow(subset(missingData, Z_3==1 & Z_2 == 1 & R_Y ==1))
    Mpart_4 = MP_YXZ3_4/MP_XZ3_4*MP_Z3_4/NM  
    
    CEM_01 <- Mpart_1 + Mpart_2 + Mpart_3 + Mpart_4
    
    #### Y = 1 , X = 1 ##########
    MP_YXZ3 <- nrow(subset(missingData, Y ==1 & X == 1 & Z_3 ==0 & Z_2 == 0 & R_Y ==1))
    MP_XZ3 <- nrow(subset(missingData, (X == 1 & Z_3 == 0 & Z_2 == 0 & R_Y ==1)))
    MP_Z3 <- nrow(subset(missingData, Z_3==0 & Z_2 == 0 & R_Y ==1))
    
    Mpart_1 = MP_YXZ3/MP_XZ3*MP_Z3/NM
    
    MP_YXZ3_2 <- nrow(subset(missingData, Y ==1 & X == 1 & Z_3 == 1 & Z_2 == 0 & R_Y ==1))
    MP_XZ3_2 <- nrow(subset(missingData, (X == 1 & Z_3 == 1 & Z_2 == 0 & R_Y ==1)))
    MP_Z3_2 <- nrow(subset(missingData, Z_3==1 & Z_2 == 0 & R_Y ==1))
    Mpart_2 = MP_YXZ3_2/MP_XZ3_2*MP_Z3_2/NM  
    
    
    MP_YXZ3_3 <- nrow(subset(missingData, Y ==1 & X == 1 & Z_3 ==0 & Z_2 == 1 & R_Y ==1))
    MP_XZ3_3 <- nrow(subset(missingData, (X == 1 & Z_3 == 0 & Z_2 == 1 & R_Y ==1)))
    MP_Z3_3 <- nrow(subset(missingData, Z_3==0 & Z_2 == 1 & R_Y ==1))
    
    Mpart_3 = MP_YXZ3_3/MP_XZ3_3*MP_Z3_3/NM
    
    MP_YXZ3_4 <- nrow(subset(missingData, Y ==1 & X == 1 & Z_3 == 1 & Z_2 == 1 & R_Y ==1))
    MP_XZ3_4 <- nrow(subset(missingData, (X == 1 & Z_3 == 1 & Z_2 == 1 & R_Y ==1)))
    MP_Z3_4 <- nrow(subset(missingData, Z_3==1 & Z_2 == 1 & R_Y ==1))
    Mpart_4 = MP_YXZ3_4/MP_XZ3_4*MP_Z3_4/NM  
    
    
    CEM_11 <- Mpart_1 + Mpart_2 + Mpart_3 + Mpart_4
    
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

      Y_new <- round( rowMeans(data.frame(complete_1$Y,complete_2$Y,complete_3$Y, complete_4$Y,complete_5$Y)))
      complete_ave<- data.frame(complete_3$X,complete_3$Z_1,complete_3$Z_2, complete_3$Z_3,complete_3$R_Y, Y_new)  
      ####################################################################
      ##### Computing Adjustment Set for imputed data P(y | do(x)) ######
      ##### We need to compute this for different sets of values ####
      
      #### Y = 0 , X = 0 ##########
      IP_YXZ3 <- nrow(subset(complete_ave, Y_new ==0 & X == 0 & Z_3 == 0))
      IP_XZ3 <- nrow(subset(complete_ave, (X == 0 & Z_3 == 0)))
      IP_Z3 <- nrow(subset(complete_ave, Z_3==0))
      
      Ipart_1 = IP_YXZ3/IP_XZ3*IP_Z3/n
      
      IP_YXZ3_2 <- nrow(subset(complete_ave, Y_new ==0 & X == 0 & Z_3 == 1))
      IP_XZ3_2 <- nrow(subset(complete_ave, (X == 0 & Z_3 == 1)))
      IP_Z3_2 <- nrow(subset(complete_ave, Z_3==1))
      Ipart_2 = IP_YXZ3_2/IP_XZ3_2*IP_Z3_2/n  
      
      ICE_00 <- Ipart_1 + Ipart_2 
      #### Y = 1 , X = 0 ##########
      IP_YXZ3 <- nrow(subset(complete_ave, Y_new ==1 & X == 0 & Z_3 == 0))
      IP_XZ3 <- nrow(subset(complete_ave, (X == 0 & Z_3 == 0)))
      IP_Z3 <- nrow(subset(complete_ave, Z_3==0))
      
      Ipart_1 = IP_YXZ3/IP_XZ3*IP_Z3/n
      
      IP_YXZ3_2 <- nrow(subset(complete_ave, Y_new ==1 & X == 0 & Z_3 == 1))
      IP_XZ3_2 <- nrow(subset(complete_ave, (X == 0 & Z_3 == 1)))
      IP_Z3_2 <- nrow(subset(complete_ave, Z_3==1))
      Ipart_2 = IP_YXZ3_2/IP_XZ3_2*IP_Z3_2/n  
      
      ICE_10 <- Ipart_1 + Ipart_2 
      
      #### Y = 0 , X = 1 ##########
      IP_YXZ3 <- nrow(subset(complete_ave, Y_new ==0 & X == 1 & Z_3 == 0))
      IP_XZ3 <- nrow(subset(complete_ave, (X == 1 & Z_3 == 0)))
      IP_Z3 <- nrow(subset(complete_ave, Z_3==0))
      
      Ipart_1 = IP_YXZ3/IP_XZ3*IP_Z3/n
      
      IP_YXZ3_2 <- nrow(subset(complete_ave, Y_new ==0 & X == 1 & Z_3 == 1))
      IP_XZ3_2 <- nrow(subset(complete_ave, (X == 1 & Z_3 == 1)))
      IP_Z3_2 <- nrow(subset(complete_ave, Z_3==1))
      Ipart_2 = IP_YXZ3_2/IP_XZ3_2*IP_Z3_2/n  
      
      ICE_01 <- Ipart_1 + Ipart_2
  
      
      #### Y = 1 , X = 1 ##########
      IP_YXZ3 <- nrow(subset(complete_ave, Y_new ==1 & X == 1 & Z_3 == 0))
      IP_XZ3 <- nrow(subset(complete_ave, (X == 1 & Z_3 == 0)))
      IP_Z3 <- nrow(subset(complete_ave, Z_3==0))
      
      Ipart_1 = IP_YXZ3/IP_XZ3*IP_Z3/n
      
      IP_YXZ3_2 <- nrow(subset(complete_ave, Y_new == 1 & X == 1 & Z_3 == 1))
      IP_XZ3_2 <- nrow(subset(complete_ave, (X == 1 & Z_3 == 1)))
      IP_Z3_2 <- nrow(subset(complete_ave, Z_3==1))
      Ipart_2 = IP_YXZ3_2/IP_XZ3_2*IP_Z3_2/n  
      
      ICE_11 <- Ipart_1 + Ipart_2
      
      
      
    ################################################################################################  
      
      library(missForest)
      start_timeF <- Sys.time()
      imputed_DataF <-missForest(missingData)
      complete_1 <- imputed_DataF$ximp
      
      Y_new <- round( complete_1$Y)
      complete_aveF<- data.frame(complete_1$X,complete_1$Z_1,complete_1$Z_2, complete_1$Z_3,complete_1$R_Y, Y_new)  
      
      
      
      ####################################################################
      ##### Computing Adjustment Set for imputed data P(y | do(x)) ######
      ##### We need to compute this for different sets of values ####
      
      #### Y = 0 , X1 = 0 , X2 = 0 ##########
      #### Y = 0 , X = 0 ##########
      IP_YXZ3 <- nrow(subset(complete_aveF, Y_new ==0 & X == 0 & Z_3 == 0))
      IP_XZ3 <- nrow(subset(complete_aveF, (X == 0 & Z_3 == 0)))
      IP_Z3 <- nrow(subset(complete_aveF, Z_3==0))
      
      Ipart_1 = IP_YXZ3/IP_XZ3*IP_Z3/n
      
      IP_YXZ3_2 <- nrow(subset(complete_aveF, Y_new ==0 & X == 0 & Z_3 == 1))
      IP_XZ3_2 <- nrow(subset(complete_aveF, (X == 0 & Z_3 == 1)))
      IP_Z3_2 <- nrow(subset(complete_aveF, Z_3==1))
      Ipart_2 = IP_YXZ3_2/IP_XZ3_2*IP_Z3_2/n  
      
      FCE_00 <- Ipart_1 + Ipart_2 
      #### Y = 1 , X = 0 ##########
      IP_YXZ3 <- nrow(subset(complete_aveF, Y_new ==1 & X == 0 & Z_3 == 0))
      IP_XZ3 <- nrow(subset(complete_aveF, (X == 0 & Z_3 == 0)))
      IP_Z3 <- nrow(subset(complete_aveF, Z_3==0))
      
      Ipart_1 = IP_YXZ3/IP_XZ3*IP_Z3/n
      
      IP_YXZ3_2 <- nrow(subset(complete_aveF, Y_new ==1 & X == 0 & Z_3 == 1))
      IP_XZ3_2 <- nrow(subset(complete_aveF, (X == 0 & Z_3 == 1)))
      IP_Z3_2 <- nrow(subset(complete_aveF, Z_3==1))
      Ipart_2 = IP_YXZ3_2/IP_XZ3_2*IP_Z3_2/n  
      
      FCE_10 <- Ipart_1 + Ipart_2 
      
      #### Y = 0 , X = 1 ##########
      IP_YXZ3 <- nrow(subset(complete_aveF, Y_new ==0 & X == 1 & Z_3 == 0))
      IP_XZ3 <- nrow(subset(complete_aveF, (X == 1 & Z_3 == 0)))
      IP_Z3 <- nrow(subset(complete_aveF, Z_3==0))
      
      Ipart_1 = IP_YXZ3/IP_XZ3*IP_Z3/n
      
      IP_YXZ3_2 <- nrow(subset(complete_aveF, Y_new ==0 & X == 1 & Z_3 == 1))
      IP_XZ3_2 <- nrow(subset(complete_aveF, (X == 1 & Z_3 == 1)))
      IP_Z3_2 <- nrow(subset(complete_aveF, Z_3==1))
      Ipart_2 = IP_YXZ3_2/IP_XZ3_2*IP_Z3_2/n  
      
      FCE_01 <- Ipart_1 + Ipart_2
      
      
      #### Y = 1 , X = 1 ##########
      IP_YXZ3 <- nrow(subset(complete_aveF, Y_new ==1 & X == 1 & Z_3 == 0))
      IP_XZ3 <- nrow(subset(complete_aveF, (X == 1 & Z_3 == 0)))
      IP_Z3 <- nrow(subset(complete_aveF, Z_3==0))
      
      Ipart_1 = IP_YXZ3/IP_XZ3*IP_Z3/n
      
      IP_YXZ3_2 <- nrow(subset(complete_aveF, Y_new ==1 & X == 1 & Z_3 == 1))
      IP_XZ3_2 <- nrow(subset(complete_aveF, (X == 1 & Z_3 == 1)))
      IP_Z3_2 <- nrow(subset(complete_aveF, Z_3==1))
      Ipart_2 = IP_YXZ3_2/IP_XZ3_2*IP_Z3_2/n  
      
      FCE_11 <- Ipart_1 + Ipart_2
      
      
      
      end_timeF <- Sys.time()
      print("Random Forest")
      dif_madj <- print(abs(CEM_00 - CE_00) + abs(CEM_01 - CE_01) + abs(CEM_10 - CE_10)
                        + abs(CEM_11 - CE_11))
      timedifM <- end_timeM-start_timeM
      dif_mice <- print(abs(ICE_00 - CE_00) + abs(ICE_01 - CE_01) + abs(ICE_10 - CE_10)
                        + abs(ICE_11 - CE_11))
      
      timedifI <- end_timeI-start_timeI
      dif_missF <- print(abs(FCE_00 - CE_00) + abs(FCE_01 - CE_01) + abs(FCE_10 - CE_10)
                         + abs(FCE_11 - CE_11))
      timedifF <- end_timeF-start_timeF
      
      print(paste0(' timedifF = ', timedifF , ' timedifI = ', timedifI ,' timedifM = ', timedifM , 
                   ' dif_missF', dif_missF , ' dif_mice' , dif_mice , ' dif_madj' , dif_madj))
      
      
