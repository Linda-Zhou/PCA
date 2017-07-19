# This r script is for part that reads file names from the key board.
# first, run the whole script and make sure function read_file is in the global environment.
# second, type file = read_file() in the console.
# It is expected to see "enter the file name please: " in the console
# type the file name without quotation marks after the colon, in the console.
# if the file name is correct, then now file is a variable that content the data set that you
# want to import.  Just type file in the console and you can see your whole data set printed
# in the console. ( Not recommended for large data set, your console will explode ---- No, it
# won't, I am just kidding, R studio is strong.)
# if the file name is not correct, you are supposed to be able to input the name again.
# if you want to quit the function, just press esc key at the left top corner.

read_file<-function(){
  ## asign the text input into variable name, in other words, name is a variables that contains
  ## a string which represents a file name.
  name = readline(prompt = "Enter the input filename: ")

  ## use a boolean to tell if the file name exist or not.
  ## function file.exists(argument =name of the file) is a function to test if the file exists
  ## in the current directory
  if(file.exists(name)){
    return(read(name))
  }

  ## construct a while loop to allow user try again and again if their first try failed.
  while(!file.exists(name)){

    print("Seems like the file is not in the current directory, please try again!")
    print("If you want to quit the function, press key esc")

    # similar procedure as above.
    name1 = readline(prompt = "Enter the file name again please: ")
    if(file.exists(name1)){
      return(read(name1))
    }
  }
}


# HELPER FUNCTION READ(NAME)
read<-function(name){
  file = file(name , "r")
  firstline = readLines(file, n = 1)
  # split the string with space as an separation
  firstline= strsplit(firstline, split = " ")[[1]]
  firstline = firstline[which(firstline!="")]
  # assertion: check if 5 items; check if intergers hold
  if(length(firstline)!=5){
    stop("Error: first line fails to have exactly five items.")
  }
  # extract the number of the column
  NC = as.numeric(firstline[1])
  # extract the name of the column
  IC = firstline[2]
  # extract the number of the row
  NR = as.numeric(firstline[3])
  if(NC%%1!=0| NR%%1!=0){
    stop("First line fails to contain integer value for number of rows or columns.")
  }
  # extract the name of the row
  IR = firstline[4]
  # extract the title of the dataset
  ID = firstline[5]
  # read the data into a vector
  data_vector = scan(file = name, skip = 1)
  #construct a matrix using the vector
  D = matrix(data_vector, nrow = NR, ncol = NC, byrow = TRUE)
  attr = list(NC = NC, IC = IC, NR = NR, IR=IR, ID = ID)
  close(file)
  line = paste(ID,"pca7.txt", sep = "")
  write(line, file = line)
  return(list(attr=attr, D=D, name = line))
}
