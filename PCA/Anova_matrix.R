#Function to compute anova values
Anova_matrix<-function(D, variants,name){
  # assertion: stop if D is not a matrix
  stopifnot(is.matrix(D))
  #store number of columns, number of rows
  NC = dim(D)[2]
  NR = dim(D)[1]
  NRC = NC*NR
  AR = rep(0, NR)
  AC = rep(0, NC)

  SDR = rep(0,NR)
  SDC = rep(0,NC)

  ## For grand_mean
  GM = 0
  ## For loop for row average
  for(i in 1:NR){
    AR[i]=sum(D[i,])/NC
    SDR[i] = sd(D[i,])## // change it in iterating way.
    GM = GM+ sum(D[i,])
  }



  GM = GM/NRC

  ## for loop for col average
  for(i in 1:NC){
    AC[i]=sum(D[,i])/NR
    SDC[i] = sd(D[,i])
  }

  ## copy the matrix
  C = D
  ## Grand Mean Centered
  if(variants == 1){
    C = D - GM
  }

  # Cols Centered
  if(variants == 2){
    for(j in 1:NC){
      C[,j] = D[,j] - AC[j]
    }
  }

  #Rows Centered
  if(variants == 3){
    for(i in 1:NR){
      C[i,] = D[i,] - AR[i]
    }
  }
  #Double Centered(AMMI)
  if(variants == 4){
    TMAT = NRC*GM
    for(i in 1:NR){
      ARi = AR[i]
      for(j in 1:NC){
        C[i,j] = C[i,j]- AR[i]-AC[j]+GM
      }
    }
  }

  #Cols Standardized
  if(variants == 5){
    for(j in 1:NC){
      C[,j] = (D[,j] - AC[j])/SDC[j]
    }
  }
  #Row Standardized
  if(variants == 6){
    for(i in 1:NR){
      C[i,] = (D[i,] - AR[i])/SDR[i]
    }
  }
  #Correspondece Analysis
  if(variants == 7){
    C = D
  }


  ## ANOVA
  ## compute sum of squrare row and sum of square column:

  DSSR = NC*sum((AR - GM)^2)

  DSSC = NR*sum((AC - GM)^2)

  DSST = sum((D-GM)^2)

  ## sum of squares for RXC interactions

  DSSRXC = DSST-DSSC-DSSR

  ## degree of freedom
  DDFT = NRC - 1
  DDFR = NR - 1
  DDFC = NC - 1
  DDFRXC = (NR-1)*(NC-1)

  # write table into an output file
  SECOND=read_file$attr[[2]]
  FOURTH=read_file$attr[[4]]
  FIFTH=read_file$attr[[5]]
  INTERACTION=paste(substr(SECOND,1,1),"*",substr(FOURTH,1,1),sep=" ")
  Source<- c("Total",paste("  ",SECOND),paste("  ",FOURTH),paste("   ",INTERACTION))
  df<-c(DDFR,DDFC,DDFRXC,DDFT)
  SS<-c(DSSR,DSSC,DSSRXC,DSST)
  #Percent<-c(DSSR,DSSC,DSSRXC,DSST)/DSST
  table=format(data.frame(df,SS,row.names = Source),justify="left")
  table$df=round(as.numeric(table$df),3)
  table$SS=round(as.numeric(table$SS),3)
  colnames(table)<-NULL
  name4=paste("ANOVA for",FIFTH,"analysis 4: Double centered (AMMI)")
  #sink(name, append = TRUE)
  if(variants == 1){
    cat("Anova Table 1: Grand Mean Centered",file= 'Output1',append = TRUE,sep='\n')

    cat(capture.output(table), file = 'Output1',append= TRUE, sep = '\n')
  }
  if(variants == 2){
    cat("Anova Table 2: Cols Mean", append = TRUE, sep = " ")
  }
  if(variants == 3){
    cat("Anova Table 3: Rows Mean", append = TRUE, sep = " ")
  }
  if(variants == 4){
    cat(name4,file= 'Output4',append = TRUE,sep='\n')
    cat("-----------------------------------------------",file= 'Output4',append = TRUE,sep='\n')
    cat("           df  SS",file= 'Output4',append = TRUE,sep='\n')
    cat("-----------------------------------------------",file= 'Output4',append = TRUE,sep='\n')
    cat(capture.output(table), file = 'Output4',append= TRUE, sep = '\n')
    cat("-----------------------------------------------",file= 'Output4',append = TRUE,sep='\n')
  }
  if(variants == 5){
    cat("Anova Table 5: Cols Standardized", append = TRUE, sep = " ")
  }
  if(variants == 6){
    cat("Anova Table 6: Row Standardized", append = TRUE, sep = " ")
  }
  if(variants == 7){
    cat("Anova Table 7: Correspondence Analysis", append = TRUE, sep = " ")
  }
  #sink()
  return(list(table = table, C = C))
}
