## This r script is code for basic principal components analysis.
## translated from the psuedo code.

## variants here is integer indicating which variant are using.
# 1. grandmean center; 2 column center  3 row center 4 column standardized 5 row standardized
# 6. AMMI  7. Correspondence anlaysis

PCA<-function(C,variants,name){
  # NEED CALL FROM PREVIOUS FUNCTION INSTEAD OF RE-EXTRACT FROM MATRICE C.
  NR = dim(C)[1]
  NC = dim(C)[2]
  #STEST = 10^(-9)/sqrt(NR)
  STEST = 1e-8
  #initialized matrix PR, PC, vector EV.
  # determine the limit for the number of components.
  NXL = min(NR-1, NC-1,7)
  #NXL = 1
  PR = matrix(0, nrow = NR, ncol = NXL)
  PC = matrix(0, nrow =NXL, ncol = NC)
  EV = rep(0, 7)
  #store temporary information NX ITER STEST SFT
  temp = matrix(0, ncol = 4, nrow = NXL)
  colnames(temp) = c("NX","ITER","STEST","SFT")


  if(variants == 7){
    CA.P = ca(C)
    PR = CA.P$rowcoord[,1:7]
    PC = t(CA.P$colcoord[,1:7])
    EV = CA.P$sv[1:7]
  }
  else{
    for(NX in 1:NXL){
      SFT = 1
      ITER = 0
      # initialized the VR with uniform random function
      VR = runif(NR, min = -0.5, max = 0.5)
      SR = VR
      while(ITER<100 & SFT>STEST){
        ITER = ITER + 1
        #compute VC from VR
        VC = t(VR)%*%C
        EVP = sum(VC^2)
        SVP = sqrt(EVP)
        VC = VC/SVP
        # recalculate VR from VC by reverse sum
        VR = C%*%t(VC)
        EVP = sum(VR^2)
        SVP = sqrt(EVP)
        VR = VR/SVP
        SFT = max(abs(VR-SR))
        # retain VR as SR
        SR = VR
      }
      print(ITER)
      #Store Eigenvalue first
      EV[NX] = EVP

      SQVP = sqrt(SVP)
      VR = VR*SQVP
      VC = VC*SQVP

      PR[,NX] = VR
      PC[NX,] = VC

      # write NX, ITER, STEST, SFT into a matrix
      temp[NX,] = c(NX, ITER, STEST, SFT)
      # remove NX from data copy
      C = C - VR%*%VC
    }
  }
  return(list(PC = PC,PR = PR, EV = EV))
}

