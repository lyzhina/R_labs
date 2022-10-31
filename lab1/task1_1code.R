# task 1
fix_column <- function(column){
  fixed_col <- gsub(" ", "", column)
  fixed_col <- as.numeric(fixed_col)
  
  if (any(is.na(fixed_col))){
    return(column)
  }
  return(fixed_col)
}

fix_data <- function(dataframe){
  fix_dataframe <- lapply(dataframe, fix_column)
  fix_dataframe <- data.frame(fix_dataframe)
  return(fix_dataframe)
}

getwd()
data <- read.csv('lab1_e1.csv')
data<-fix_data(data)
print(data)
