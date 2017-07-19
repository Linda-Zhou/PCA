#'This function perform PCA analysis
#'Please type "run_this()"in the console below
#'@param  select your files from your documents
#'@return PCA results of 7 variants
#'@return Anova tables of 7 variants
#'@return PCA graph
#'@export
#'
run_this<-function(){
  source('PCA_v5.R')
  source('Anova_matrix_v3.R')
  source('read_v3.R')
  source('write_table.R')
  library(ca)
  print("Choose the file in your browser")
  the_file = read(file.choose())
  PR = matrix(NA,nrow=dim(the_file[[2]])[1],ncol = 7*7)
  PC = matrix(NA,nrow=dim(the_file[[2]])[2],ncol = 7*7)
  for(i in 1:7){
    data = Anova_matrix(the_file[[2]],i,paste('Output',as.character(i)))$C
    result = PCA(data,i,paste('Output',as.character(i)))
    pr = result$PR
    pc = result$PC
    low = (7*i-6)
    upper = (7*i)
    PR[,low:upper]= pr[,1:7]
    PC[,low:upper]= t(pc[1:7,])
    write_table(data,i,paste('Output',as.character(i)))
    print(paste("Finished analysis",i,"of 7"))
  }
  print("Finished all 7 analysis")
  print("Tables are ready in the same directory of your file")
  makeplot<-function(){
    choice = readline(prompt = "Enter the variant you want: ")
    PC_choice1 = readline(prompt = "Enter the PC you want for x-axis: ")
    PC_choice2 = readline(prompt = "Enter the PC you want for y-axis: ")
    print("Graphing...")
    choice=as.integer(choice)
    PC_choice1=as.integer(PC_choice1)
    PC_choice2=as.integer(PC_choice2)
    target.PR = PR[,(7*choice-6):(7*choice)]
    target.PC = PC[,(7*choice-6):(7*choice)]
    PR1 = target.PR[,PC_choice1]
    PR2 = target.PR[,PC_choice2]
    PC1 = target.PC[,PC_choice1]
    PC2 = target.PC[,PC_choice2]
    x_min = min(c(min(PR1),min(PC1)))
    x_max = max(c(max(PR1),max(PC1)))
    y_min = min(c(min(PR2),min(PC2)))
    y_max = max(c(max(PR2),max(PC2)))
    plot(PC1,PC2,col = 4, xlim = c(x_min,x_max),ylim = c(y_min,y_max),pch=17,xlab = paste('IPC',as.character(PC_choice1),sep = ''),ylab = paste('IPC',as.character(PC_choice2),sep = ''),main = paste('variant',as.character(choice),sep = ''),cex=0.5,asp=1)
    lines(PR1,PR2,col = 2,type = 'p',pch=16,cex=0.5)
    #text(PC1,PC2,pos=2,cex=0.8,col=3)
    #text(PR1,PR2,pos=4,cex=0.8,col=1)
    abline(h = 0, v = 0, lty=2)
    legend("topright", legend = c("column score","row score"),pch=c(17,16),col=c(4,2),pt.cex = 1,cex = 0.5)
  }

  want_graph=readline(prompt="want graphs? Type y/n:")
  if (want_graph =="y"){
    makeplot()
    print("Graph is ready. Thanks for using PCA software!")
  }else if (want_graph == "n"){
    print("Thanks for using PCA software!")
  }else{
    want_graph2=readline(prompt="Invalid command. Please type y/n:")
    if (want_graph2=="y"){
      makeplot()
    }else{
      print("Thanks for using PCA software!")
    }
  }
}
