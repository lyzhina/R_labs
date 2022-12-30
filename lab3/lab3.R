#task 1
library(ggplot2)
library(dplyr)

merge_trades <- function(trades) {
  c = trades[[1]]
  for (i in 2:length(trades)) {
    c = mapply(c, c, trades[[i]], SIMPLIFY=FALSE)
  }
  return(subset(as.data.frame(c), select = -geo ))
}

plot_graphic_all_export_volume <- function(export_data){
  gg <-  ggplot(export_data, aes(x = partner, y = values, fill = sitc06)) +
    geom_col(position = "dodge") +
    xlab('Партнеры') + 
    ylab('Объем экспорта') +
    ggtitle('Общий объем экспорта в Европе в 2008-2019 гг') + 
    scale_fill_discrete(name = 'Товары на экспорт') + 
    coord_flip() +
    theme_bw() + 
    theme(plot.background = element_rect(fill = "lavenderblush", colour = NA), legend.position=c(.7, .5),                                          legend.background = element_rect(fill="lavenderblush", size=0.5, linetype="solid"))
  plot(gg)
}


plot_graphic_leaders <- function(leaders){
  gg <-  ggplot(leaders, aes(x = partner, y = values, fill = sitc06)) +
    geom_col(position = "dodge") +
    xlab('Партнеры') + 
    ylab('Объем экспорта') +
    ggtitle('Ведущие страны по экспорту') + 
    scale_fill_discrete(name = 'Товары на экспорт') + 
    coord_flip() + 
    theme_bw() + 
    theme(plot.background = element_rect(fill = "lavenderblush", colour = NA), legend.position=c(.7, .5),                                          legend.background = element_rect(fill="lavenderblush", size=0.5, linetype="solid"))
  plot(gg)
}

plot_graphic <- function(export_data, str_){
  gg <- ggplot(export_data, aes(x = partner, y = values, group = sitc06)) + 
    geom_col(aes(fill = sitc06)) +
    xlab('Партнеры') +
    ylab('Объем экспорта') +
    ggtitle(paste0('Ведущие страны по экспорту в Европе в ', str_, ' г.')) +
    scale_fill_discrete(name = 'Товары на экспорт') + coord_flip() +
    geom_text(aes(label = share), position = position_stack(vjust = 0.5)) +
    theme_bw() + 
    theme(plot.background = element_rect(fill = "lavenderblush", colour = NA), legend.position=c(.7, .5),                                          legend.background = element_rect(fill="lavenderblush", size=0.5, linetype="solid"))
}


path = 'C:/Users/maria/gitprojects/R_labs/lab3/trades.RData'
load(path)
merged_trades <- merge_trades(trades)
import_data <- merged_trades[merged_trades$indic_et == 'Imports in million of ECU/EURO', ]
export_data <- merged_trades[merged_trades$indic_et == 'Exports in million of ECU/EURO', ]
union_data <- bind_rows(export_data, import_data)
leaders <- subset(export_data, export_data$value >= 10000) 

plot_graphic_all_export_volume(export_data)
plot_graphic_leaders(leaders)

dub_export_data <- merged_trades[merged_trades$indic_et == 'Share of exports by partner (%)', ]
dub_data <- cbind(export_data, share = dub_export_data$values)
dates <- unique(dub_data$time)
lapply(dates, 
       function(date) {
         export_data = subset(dub_data, dub_data$time == date & dub_data$value >= 10000)
         plot_graphic(export_data, substring(date, 1, 4))
       })


 #task 2

library(tidyr)
library(stringr)

plot_graphic_reg <- function(data, graph_type){
  data <- data[complete.cases(data),]
  for (i in 2:length(names(data))) {
    data[[i]] <- gsub("-", 0, data[[i]])
    data[[i]] <- as.numeric(data[[i]])
  }
  
  sim_filter <- str_detect(data$Регион, 'федеральный округ')
  rdf <- fill(mutate(data, Округ = if_else(sim_filter, Регион, NULL)), Округ)
  har_filter <- !str_detect(rdf$Регион, 'Федерация|федеральный округ')
  rdf <- filter(rdf, har_filter)
  
  sel_import <- select_at(rdf, vars(matches("Импорт")))
  sel_export <- select_at(rdf, vars(matches("Экспорт")))
  
  sel_import$Сумма <- rowSums(sel_import, na.rm = TRUE)
  sel_export$Сумма <- rowSums(sel_export, na.rm = TRUE)
  
  rdf$SumImport <- sel_import$Сумма
  rdf$SumExport <- sel_export$Сумма
  
  if (graph_type == 'bilateral'){
    rdf[,"SumImport"] <- -rdf[,"SumImport"]
  }
  rdf <- filter(rdf, Округ == 'Северо-Западный федеральный округ')
  rdf <- rdf[,c("Регион", "SumExport", "SumImport" )]
  rdf <- pivot_longer(rdf, !Регион, names_to = "Экспорт/Импорт", values_to = "млн USD")
  
  overall_region <- rdf %>% group_by(Регион, `Экспорт/Импорт`) 
  overall_region <- overall_region %>% summarise(sum = sum(`млн USD`))
  
  overall_region |>
    ggplot(mapping = aes(x = Регион, y = sum, fill = `Экспорт/Импорт`)) +
    geom_col(color = 'black', size = 0.2, position = 'dodge') + 
    ggtitle('Северо-Западный федеральный округ') + ylab('млн USD') + coord_flip()  + 
    theme_bw() + 
    theme(plot.title = element_text(size = 12), plot.background = element_rect(fill = "lavenderblush", colour = NA), legend.position=c(-.6, .9), legend.background = element_rect(fill=adjustcolor("lavenderblush", alpha = 0), 
                                                                                                                                                                                                                           size=0.5, linetype="solid"))
}

path = 'C:/Users/maria/gitprojects/R_labs/lab3/ExpImp.RData'
load(path)
plot_graphic_reg(ExpImp, 'unilateral')
plot_graphic_reg(ExpImp, 'bilateral')