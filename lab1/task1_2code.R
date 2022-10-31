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


