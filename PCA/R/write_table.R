#write the output into tables
write_table=function(D,variants,name){
  NC = dim(D)[2]
  NR = dim(D)[1]
  SECOND=the_file$attr[[2]]
  FOURTH=the_file$attr[[4]]
  FIFTH=the_file$attr[[5]]
  #grand mean centered
  if(variants == 1){
    data1 = Anova_matrix(the_file[[2]],1,paste('Output',as.character(1)))$C
    result1= PCA(data1,1,paste('Output',as.character(1)))
    SSTOTAL1=rep(0,7)
    ROWVEC1=rep(0,NR)
    COLVEC1=rep(0,NC)
    ROWSUM1=rep(0,7)
    COLSUM1=rep(0,7)
    INTERSUM1=rep(0,7)
    df1=rep(0,7)
    for(i in 1:7){
      C=as.matrix(result1[[1]][i,])%*%result1[[2]][,i]
      SSTOTAL1[i]=sum(C^2)
      df1[i]=NR+NC-2*i+1
      for(j in 1:NC){
        ROWVEC1[j]=sum((C[j, ])^2)
        ROWSUM1[i]=sum(ROWVEC1[j])*NC
      }
      for(k in 1:NR){
        COLVEC1[k]=sum((C[, k])^2)
        COLSUM1[i]=sum(COLVEC1[k])*NR
      }
      INTERSUM1[i]=SSTOTAL1[i]-ROWSUM1[i]-COLSUM1[i]
    }
    df1_res= Anova_matrix(the_file[[2]],1,paste('Output',as.character(1)))[[6]]-sum(df1)
    SSTOTAL1_RES=Anova_matrix(the_file[[2]],1,paste('Output',as.character(1)))[[3]]-sum(SSTOTAL1)
    Source<- c("Total",paste("  ","IPC1"),paste("  ","IPC2"),paste("  ","IPC3"),paste("  ","IPC4"),paste("  ","IPC5"),
               paste("  ","IPC6"),paste("  ","IPC7"),paste("  ","Residual"))
    df1<-c(Anova_matrix(the_file[[2]],1,paste('Output',as.character(1)))[[6]],df1[1],df1[2],df1[3],df1[4],df1[5],df1[6],df1[7],df1_res)
    SS1<-c(Anova_matrix(the_file[[2]],1,paste('Output',as.character(1)))[[3]],SSTOTAL1[1],SSTOTAL1[2],SSTOTAL1[3],SSTOTAL1[4],SSTOTAL1[5],SSTOTAL1[6],SSTOTAL1[7],SSTOTAL1_RES)
    COLS1<-c(0,COLSUM1[1],COLSUM1[2],COLSUM1[3],COLSUM1[4],COLSUM1[5],COLSUM1[6],COLSUM1[7],0)
    ROWS1<-c(0,ROWSUM1[1],ROWSUM1[2],ROWSUM1[3],ROWSUM1[4],ROWSUM1[5],ROWSUM1[6],ROWSUM1[7],0)
    CxR1<-c(0,INTERSUM1[1],INTERSUM1[2],INTERSUM1[3],INTERSUM1[4],INTERSUM1[5],INTERSUM1[6],INTERSUM1[7],0)
    table1=format(data.frame(df1,SS1,COLS1,ROWS1,CxR1,row.names = Source),justify="left",digits=4)
    name1=paste("ANOVA for",FIFTH,"analysis 1: Grand Mean Centered")
    cat(name1,file= paste(FIFTH,"PCA1"),append = TRUE,sep='\n')
    cat("-----------------------------------------------",file= paste(FIFTH,"PCA1"),append = TRUE,sep='\n')
    cat(capture.output(table1), file = paste(FIFTH,"PCA1"),append= TRUE, sep = '\n')
    cat("-----------------------------------------------",file= paste(FIFTH,"PCA1"),append = TRUE,sep='\n')
  }
  #column centered
  if(variants == 2){
    data2 = Anova_matrix(the_file[[2]],2,paste('Output',as.character(2)))$C
    result2= PCA(data2,2,paste('Output',as.character(2)))
    SSTOTAL2=rep(0,7)
    ROWVEC2=rep(0,NR)
    ROWSUM2=rep(0,7)
    INTERSUM2=rep(0,7)
    df2=rep(0,7)
    for(i in 1:7){
      C=as.matrix(result2[[1]][i,])%*%result2[[2]][,i]
      SSTOTAL2[i]=sum(C^2)
      df2[i]=NR+NC-2*i
      for(j in 1:NC){
        ROWVEC2[j]=sum((C[j, ])^2)
        ROWSUM2[i]=sum(ROWVEC2[j])*NC
      }
      INTERSUM2[i]=SSTOTAL2[i]-ROWSUM2[i]
    }
    df2_res= Anova_matrix(the_file[[2]],2,paste('Output',as.character(2)))[[6]]-sum(df2)
    SSTOTAL2_RES=Anova_matrix(the_file[[2]],2,paste('Output',as.character(2)))[[3]]-sum(SSTOTAL2)
    Source<- c("Total",paste("  ","IPC1"),paste("  ","IPC2"),paste("  ","IPC3"),paste("  ","IPC4"),paste("  ","IPC5"),
               paste("  ","IPC6"),paste("  ","IPC7"),paste("  ","Residual"))
    df2<-c(Anova_matrix(the_file[[2]],2,paste('Output',as.character(2)))[[6]],df2[1],df2[2],df2[3],df2[4],df2[5],df2[6],df2[7],df2_res)
    SS2<-c(Anova_matrix(the_file[[2]],2,paste('Output',as.character(2)))[[3]],SSTOTAL2[1],SSTOTAL2[2],SSTOTAL2[3],SSTOTAL2[4],SSTOTAL2[5],SSTOTAL2[6],SSTOTAL2[7],SSTOTAL2_RES)
    ROWS2<-c(0,ROWSUM2[1],ROWSUM2[2],ROWSUM2[3],ROWSUM2[4],ROWSUM2[5],ROWSUM2[6],ROWSUM2[7],0)
    CxR2<-c(0,INTERSUM2[1],INTERSUM2[2],INTERSUM2[3],INTERSUM2[4],INTERSUM2[5],INTERSUM2[6],INTERSUM2[7],0)
    table2=format(data.frame(df2,SS2,ROWS2,CxR2,row.names = Source),justify="left",digits=4)
    name2=paste("ANOVA for",FIFTH,"analysis 2: Column Mean Centered")
    cat(name2,file= paste(FIFTH,"PCA2"),append = FALSE,sep='\n')
    cat("-----------------------------------------------",file= paste(FIFTH,"PCA2"),append = TRUE,sep='\n')
    cat(capture.output(table2), file = paste(FIFTH,"PCA2"),append= TRUE, sep = '\n')
    cat("-----------------------------------------------",file= paste(FIFTH,"PCA2"),append = TRUE,sep='\n')
  }
  if(variants == 3){
    data3 = Anova_matrix(the_file[[2]],3,paste('Output',as.character(3)))$C
    result3= PCA(data3,3,paste('Output',as.character(3)))
    SSTOTAL3=rep(0,7)
    COLVEC3=rep(0,NC)
    COLSUM3=rep(0,7)
    INTERSUM3=rep(0,7)
    df3=rep(0,7)
    for(i in 1:7){
      C=as.matrix(result3[[1]][i,])%*%result3[[2]][,i]
      SSTOTAL3[i]=sum(C^2)
      df3[i]=NR+NC-2*i
      for(k in 1:NR){
        COLVEC3[k]=sum((C[, k])^2)
        COLSUM3[i]=sum(COLVEC3[k])*NR
      }
      INTERSUM3[i]=SSTOTAL3[i]-COLSUM3[i]
    }
    df3_res= Anova_matrix(the_file[[2]],3,paste('Output',as.character(3)))[[6]]-sum(df3)
    SSTOTAL3_RES=Anova_matrix(the_file[[2]],3,paste('Output',as.character(3)))[[3]]-sum(SSTOTAL3)
    Source<- c("Total",paste("  ","IPC1"),paste("  ","IPC2"),paste("  ","IPC3"),paste("  ","IPC4"),paste("  ","IPC5"),
               paste("  ","IPC6"),paste("  ","IPC7"),paste("  ","Residual"))
    df3<-c(Anova_matrix(the_file[[2]],3,paste('Output',as.character(3)))[[6]],df3[1],df3[2],df3[3],df3[4],df3[5],df3[6],df3[7],df3_res)
    SS3<-c(Anova_matrix(the_file[[2]],3,paste('Output',as.character(3)))[[3]],SSTOTAL3[1],SSTOTAL3[2],SSTOTAL3[3],SSTOTAL3[4],SSTOTAL3[5],SSTOTAL3[6],SSTOTAL3[7],SSTOTAL3_RES)
    COLS3<-c(0,COLSUM3[1],COLSUM3[2],COLSUM3[3],COLSUM3[4],COLSUM3[5],COLSUM3[6],COLSUM3[7],0)
    CxR3<-c(0,INTERSUM3[1],INTERSUM3[2],INTERSUM3[3],INTERSUM3[4],INTERSUM3[5],INTERSUM3[6],INTERSUM3[7],0)
    table3=format(data.frame(df3,SS3,COLS3,CxR3,row.names = Source),justify="left",digits=4)
    name3=paste("ANOVA for",FIFTH,"analysis 3: Row Centered")
    cat(name3,file= paste(FIFTH,"PCA3"),append = FALSE,sep='\n')
    cat("-----------------------------------------------",file= paste(FIFTH,"PCA3"),append = TRUE,sep='\n')
    cat(capture.output(table3), file = paste(FIFTH,"PCA3"),append= TRUE, sep = '\n')
    cat("-----------------------------------------------",file= paste(FIFTH,"PCA3"),append = TRUE,sep='\n')
  }
  #double centered
  if(variants == 4){
    data4 = Anova_matrix(the_file[[2]],4,paste('Output',as.character(4)))$C
    result4= PCA(data4,4,paste('Output',as.character(4)))[[3]]
    EV4=rep(0,7)
    df4=rep(0,7)
    for (i in 1:7){
      EV4[i]=result4[i]
      df4[i]=NC+NR-2*i-1
    }
    EV_RES=Anova_matrix(the_file[[2]],4,paste('Output',as.character(4)))[[9]]-sum(EV4)
    df4_res=Anova_matrix(the_file[[2]],4,paste('Output',as.character(4)))[[10]]-sum(df4)
    INTERACTION=paste(substr(SECOND,1,1),"x",substr(FOURTH,1,1),sep="")
    Source<- c("Total",paste(" ",SECOND),paste(" ",FOURTH),paste(" ",INTERACTION),paste("   ","IPC1"),
               paste("   ","IPC2"),paste("   ","IPC3"),paste("   ","IPC4"),paste("   ","IPC5"),
               paste("   ","IPC6"),paste("   ","IPC7"),paste("   ","Residual"))
    df<-c(Anova_matrix(the_file[[2]],4,paste('Output',as.character(4)))[[6]],
          Anova_matrix(the_file[[2]],4,paste('Output',as.character(4)))[[8]],
          Anova_matrix(the_file[[2]],4,paste('Output',as.character(4)))[[7]],
          Anova_matrix(the_file[[2]],4,paste('Output',as.character(4)))[[10]],df4[1],df4[2],df4[3],df4[4],df4[5],df4[6],df4[7],df4_res)
    SS<-c(Anova_matrix(the_file[[2]],4,paste('Output',as.character(4)))[[3]],
          Anova_matrix(the_file[[2]],4,paste('Output',as.character(4)))[[5]],
          Anova_matrix(the_file[[2]],4,paste('Output',as.character(4)))[[4]],
          Anova_matrix(the_file[[2]],4,paste('Output',as.character(4)))[[9]],EV4[1],EV4[2],EV4[3],EV4[4],EV4[5],EV4[6],EV4[7],EV_RES)
    #Percent<-c(DSSR,DSSC,DSSRXC,DSST)/DSST
    table4=format(data.frame(df,SS,row.names = Source),justify="left")
    table4$df=round(as.numeric(table4$df),3)
    table4$SS=round(as.numeric(table4$SS),3)
    name4=paste("ANOVA for",FIFTH,"analysis 4: Double centered (AMMI)")
    cat(name4,file=paste(FIFTH,"PCA4"),append = FALSE,sep='\n')
    cat("-----------------------------------------------",file= paste(FIFTH,"PCA4"),append = TRUE,sep='\n')
    cat(capture.output(table4), file = paste(FIFTH,"PCA4"),append= TRUE, sep = '\n')
    cat("-----------------------------------------------",file= paste(FIFTH,"PCA4"),append = TRUE,sep='\n')
  }
  if(variants == 5){
    data5 = Anova_matrix(the_file[[2]],5,paste('Output',as.character(5)))$C
    result5= PCA(data5,5,paste('Output',as.character(5)))
    SSTOTAL5=rep(0,7)
    ROWVEC5=rep(0,NR)
    ROWSUM5=rep(0,7)
    INTERSUM5=rep(0,7)
    df5=rep(0,7)
    for(i in 1:7){
      C=as.matrix(result5[[1]][i,])%*%result5[[2]][,i]
      SSTOTAL5[i]=sum(C^2)
      df5[i]=NR+NC-2*i
      for(j in 1:NC){
        ROWVEC5[j]=sum((C[j, ])^2)
        ROWSUM5[i]=sum(ROWVEC5[j])*NR
      }
      INTERSUM5[i]=SSTOTAL5[i]-ROWSUM5[i]
    }
    df5_res= Anova_matrix(the_file[[2]],5,paste('Output',as.character(5)))[[6]]-sum(df5)
    SSTOTAL5_RES=Anova_matrix(the_file[[2]],5,paste('Output',as.character(5)))[[3]]-sum(SSTOTAL5)
    Source<- c("Total",paste("  ","IPC1"),paste("  ","IPC2"),paste("  ","IPC3"),paste("  ","IPC4"),paste("  ","IPC5"),
               paste("  ","IPC6"),paste("  ","IPC7"),paste("  ","Residual"))
    df5<-c(Anova_matrix(the_file[[2]],5,paste('Output',as.character(5)))[[6]],df5[1],df5[2],df5[3],df5[4],df5[5],df5[6],df5[7],df5_res)
    SS5<-c(Anova_matrix(the_file[[2]],5,paste('Output',as.character(5)))[[3]],SSTOTAL5[1],SSTOTAL5[2],SSTOTAL5[3],SSTOTAL5[4],SSTOTAL5[5],SSTOTAL5[6],SSTOTAL5[7],SSTOTAL5_RES)
    ROWS5<-c(0,ROWSUM5[1],ROWSUM5[2],ROWSUM5[3],ROWSUM5[4],ROWSUM5[5],ROWSUM5[6],ROWSUM5[7],0)
    CxR5<-c(0,INTERSUM5[1],INTERSUM5[2],INTERSUM5[3],INTERSUM5[4],INTERSUM5[5],INTERSUM5[6],INTERSUM5[7],0)
    table5=format(data.frame(df5,SS5,ROWS5,CxR5,row.names = Source),justify="left",digits=4)
    name5=paste("ANOVA for",FIFTH,"analysis 5: Column Mean Standarized")
    cat(name5,file= paste(FIFTH,"PCA5"),append = FALSE,sep='\n')
    cat("-----------------------------------------------",file= paste(FIFTH,"PCA5"),append = TRUE,sep='\n')
    cat(capture.output(table5), file = paste(FIFTH,"PCA5"),append= TRUE, sep = '\n')
    cat("-----------------------------------------------",file= paste(FIFTH,"PCA5"),append = TRUE,sep='\n')
  }
  if(variants == 6){
    data6 = Anova_matrix(the_file[[2]],6,paste('Output',as.character(6)))$C
    result6= PCA(data6,6,paste('Output',as.character(6)))
    SSTOTAL6=rep(0,7)
    COLVEC6=rep(0,NC)
    COLSUM6=rep(0,7)
    INTERSUM6=rep(0,7)
    df6=rep(0,7)
    for(i in 1:7){
      C=as.matrix(result6[[1]][i,])%*%result6[[2]][,i]
      SSTOTAL6[i]=sum(C^2)
      df6[i]=NR+NC-2*i
      for(k in 1:NR){
        COLVEC6[k]=sum((C[, k])^2)
        COLSUM6[i]=sum(COLVEC6[k])*NC
      }
      INTERSUM6[i]=SSTOTAL6[i]-COLSUM6[i]
    }
    df6_res= Anova_matrix(the_file[[2]],6,paste('Output',as.character(6)))[[6]]-sum(df6)
    SSTOTAL6_RES=Anova_matrix(the_file[[2]],6,paste('Output',as.character(6)))[[3]]-sum(SSTOTAL6)
    Source<- c("Total",paste("  ","IPC1"),paste("  ","IPC2"),paste("  ","IPC3"),paste("  ","IPC4"),paste("  ","IPC5"),
               paste("  ","IPC6"),paste("  ","IPC7"),paste("  ","Residual"))
    df6<-c(Anova_matrix(the_file[[2]],6,paste('Output',as.character(6)))[[6]],df6[1],df6[2],df6[3],df6[4],df6[5],df6[6],df6[7],df6_res)
    SS6<-c(Anova_matrix(the_file[[2]],6,paste('Output',as.character(6)))[[3]],SSTOTAL6[1],SSTOTAL6[2],SSTOTAL6[3],SSTOTAL6[4],SSTOTAL6[5],SSTOTAL6[6],SSTOTAL6[7],SSTOTAL6_RES)
    COLS6<-c(0,COLSUM6[1],COLSUM6[2],COLSUM6[3],COLSUM6[4],COLSUM6[5],COLSUM6[6],COLSUM6[7],0)
    CxR6<-c(0,INTERSUM6[1],INTERSUM6[2],INTERSUM6[3],INTERSUM6[4],INTERSUM6[5],INTERSUM6[6],INTERSUM6[7],0)
    table6=format(data.frame(df6,SS6,COLS6,CxR6,row.names = Source),justify="left",digits=4)
    name6=paste("ANOVA for",FIFTH,"analysis 6: Row Standarized")
    cat(name6,file= paste(FIFTH,"PCA6"),append = FALSE,sep='\n')
    cat("-----------------------------------------------",file= paste(FIFTH,"PCA6"),append = TRUE,sep='\n')
    cat(capture.output(table6), file = paste(FIFTH,"PCA6"),append= TRUE, sep = '\n')
    cat("-----------------------------------------------",file= paste(FIFTH,"PCA6"),append = TRUE,sep='\n')
  }
  if(variants == 7){
    cat("Anova Table 7: Correspondence Analysis", append = TRUE, sep = " ")
  }
}

