install.packages("plotrix")     
library(plotrix)


data<-read.table(file.choose(), sep=",", header=T, stringsAsFactors = FALSE)
data$area <- data$area*100

#### LINEPLOTS
#data preparation
soma_por_classe_ano <- aggregate(data$area, 
                                 by = list(data$class_name, data$band), 
                                 FUN = sum)

colnames(soma_por_classe_ano) <- c("classe", "ano", "hectares")
soma_por_classe_ano$ano <- as.character(soma_por_classe_ano$ano)
soma_por_classe_ano$classe <- as.factor(soma_por_classe_ano$classe)
str(soma_por_classe_ano)

#general chart
ggplot(data=soma_por_classe_ano, aes(x=ano, y=hectares, group= classe, 
                                     color=classe)) +
  geom_line(size=1.5) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_color_manual(values=c('#006400', '#935132', '#FFEFC3', '#D5A6BD', '#EA9999', 
                              '#ffd966', '#0000ff', '#D5D5E5', '#c59ff4' , '#af2a2a', 
                              '#687537'), 
                     name="classe de uso do solo") +
  xlab("ano") + ylab("hectares") +  
  scale_y_continuous(limits = c(0, 50000), breaks = seq(0, 50000, 5000)) 
  