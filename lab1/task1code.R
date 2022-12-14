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

#task 2
merge_data <- function(data){
  merged_data <- Reduce(function(x, y) merge(x, y, by='id'), data)
  return(merged_data)
}

get_id <- function(data){
  merged_data <- merge_data(data)
  merged_data <- na.omit(merged_data)
  data_without_id <- merged_data[, -c(1)]
  means <- rowMeans(data_without_id)
  mean_temp <- data.frame(id=merged_data$id, mean_temp=means)
  return(mean_temp)
}

getwd()
load('lab1_e2.RData')
new_data<-get_id(all_data)
print(new_data)