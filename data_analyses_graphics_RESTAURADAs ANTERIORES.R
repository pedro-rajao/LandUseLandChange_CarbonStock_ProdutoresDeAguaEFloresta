library(ggplot2)
library(dplyr)
library(gganimate)


# data loading and preparation
data<-read.table(file.choose(), sep=",", header=T, stringsAsFactors = FALSE) #class_area_by_district_by_yearRestauradas (1)
data_mod1 <- cbind(data[1:2], stack(data[3:62]))
data_mod1$ind <- as.character(data_mod1$ind)
data_mod <- na.omit(data_mod1)

data_mod$ind[data_mod$ind=="X3"] <- "Floresta Natural"
data_mod$ind[data_mod$ind=="X9"] <- "Floresta Plantada"
data_mod$ind[data_mod$ind=="X11"] <- "Campo Alagado e Área Pantanosa"
data_mod$ind[data_mod$ind=="X15"] <- "Pastagem"
data_mod$ind[data_mod$ind=="X21"] <- "Mosaico de Usos"
data_mod$ind[data_mod$ind=="X24"] <- "Área Urbanizada"
data_mod$ind[data_mod$ind=="X25"] <- "Outras Áreas não Vegetadas"
data_mod$ind[data_mod$ind=="X29"] <- "Afloramento Rochoso"
data_mod$ind[data_mod$ind=="X33"] <- "Rio"

data_mod$ind <- as.factor(data_mod$ind)
list(levels(data_mod$ind))
View(data_mod)
str(data_mod)

##### GRAPHICS
# barplot 1985 - 2021
data_mod$year <- as.character(data_mod$year)
ggplot(data=data_mod, aes(x=year, y=values, fill=ind)) +
  geom_bar(stat="identity") +
  theme_classic() + 
  coord_flip() + 
  scale_fill_manual(values=c("#006400", "#fff3bf",  "#ffd966"), 
                    name="classe de uso do solo") +
  theme(legend.position="top", axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 8)) +  
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
  xlab("ano") + ylab("hectares")


# barplot 2008 - 2021
data_mod_2008 = filter(data_mod, year >= 2008)
ggplot(data=data_mod_2008, aes(x=year, y=values, fill=ind)) +
  geom_bar(stat="identity") +
  theme_classic() + 
  coord_flip() + 
  scale_fill_manual(values=c("#006400", "#fff3bf",  "#ffd966"), 
                    name="classe de uso do solo") +
  theme(legend.position="top", axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
  xlab("ano") + ylab("hectares")


# function for barplot 2008-2021 of all SITES (local) 
barplot_facet <- function(data, x, y, facet_var, fill, fill_colors, 
                          n_cols = 2) {
  facet_levels <- unique(data[[facet_var]])
  n_levels <- length(facet_levels)
  n_rows <- ceiling(n_levels/n_cols)
  grid_arr <- vector("list", length = n_levels)
  for (i in seq_along(facet_levels)) {
    level <- facet_levels[i]
    plot_data <- data[data[[facet_var]] == level, ]
    p <- ggplot(plot_data, aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = fill_colors, 
                        name="classe de uso do solo") +
      labs(title = paste(level)) +
      theme_classic() +
      coord_flip() +  
      scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
      xlab("ano") + ylab("hectares")
    
    filename <- paste(facet_var, level, ".jpg", sep = "_")
    ggsave(filename, plot = p, device = "jpg")
    print(p)
  }
}

barplot_facet(data = data_mod_2008, x = year, y = values, 
              facet_var = 'propriedade_POL', fill = ind, 
              fill_colors = c("#006400", "#fff3bf",  "#ffd966"), 
              n_cols = 3)



# function for barplot 2008-2021 of all sites and polygons
barplot_facet <- function(data, x, y, facet_var, fill, fill_colors, 
                          n_cols = 2) {
  facet_levels <- unique(data[[facet_var]])
  n_levels <- length(facet_levels)
  n_rows <- ceiling(n_levels/n_cols)
  grid_arr <- vector("list", length = n_levels)
  for (i in seq_along(facet_levels)) {
    level <- facet_levels[i]
    plot_data <- data[data[[facet_var]] == level, ]
    p <- ggplot(plot_data, aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = fill_colors, 
                        name="classe de uso do solo") +
      labs(title = paste(level)) +
      theme_classic() +
      coord_flip() +  
      scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 25)) +
      xlab("ano") + ylab("hectares")
    
    filename <- paste(facet_var, level, ".jpg", sep = "_")
    ggsave(filename, plot = p, device = "jpg")
    print(p)
  }
}

barplot_facet(data = data_mod_2008, x = year, y = values, 
              facet_var = 'propriedade_POL', fill = ind, 
              fill_colors = c("#006400", "#fff3bf",  "#ffd966"), 
              n_cols = 3)


#### LINEPLOTS
#data preparation
soma_por_classe_ano <- aggregate(data_mod$values, 
                                  by = list(data_mod$ind, data_mod$year), 
                                    FUN = sum)

colnames(soma_por_classe_ano) <- c("classe", "ano", "hectares")
soma_por_classe_ano$ano <- as.character(soma_por_classe_ano$ano)
soma_por_classe_ano$classe <- as.factor(soma_por_classe_ano$classe)

View(soma_por_classe_ano)

# Diretório onde o arquivo será salvo
diretorio <- "C:/Users/pedro.rajao/Desktop"
# Nome do arquivo de saída
nome_arquivo <- "soma_por_classe_ano_Final_v2.csv"
# Caminho completo do arquivo
caminho_arquivo <- file.path(diretorio, nome_arquivo)
# Exportar o data frame para o arquivo CSV
write.csv2(soma_por_classe_ano, caminho_arquivo, row.names = FALSE)


####### general chart
# Gerar o gráfico com ggplot
grafico <- ggplot(data = soma_por_classe_ano, aes(x = ano, y = hectares, group = classe, color = classe)) +
  geom_line(size = 2) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_color_manual(values = c('#45C2A5', "#006400", '#935132', "#fff3bf", '#FF99FF', "#ffd966", '#0000ff'), 
                     name = "classe de uso do solo") +
  xlab("ano") + ylab("hectares") +  
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, 10)) +
  guides(color = "none")

# Remover a legenda do gráfico
grafico <- grafico + theme(legend.position = "none")

# Salvar o gráfico em alta resolução
ggsave("C:/Users/pedro.rajao/Desktop/soma_por_classe_ano.png", plot = grafico, dpi = 300)



### function  for all sites_polygons Lineplots charts
#data preparation
soma_por_prop_classe_ano <- aggregate(data_mod$values, 
                                 by = list(data_mod$ind, data_mod$year,
                                           data_mod$propriedade_POL), 
                                 FUN = sum)

colnames(soma_por_prop_classe_ano) <- c("classe", "ano", "prop_POL", "hectares")
soma_por_prop_classe_ano$ano <- as.factor(soma_por_prop_classe_ano$ano)
soma_por_prop_classe_ano$classe <- as.factor(soma_por_prop_classe_ano$classe)
soma_por_prop_classe_ano$prop_POL <- as.factor(soma_por_prop_classe_ano$prop_POL)
View(soma_por_prop_classe_ano)
# Diretório onde o arquivo será salvo
diretorio <- "C:/Users/pedro.rajao/Desktop"
# Nome do arquivo de saída
nome_arquivo <- "soma_por_prop_classe_anoRestauracao_Finalv2.csv"
# Caminho completo do arquivo
caminho_arquivo <- file.path(diretorio, nome_arquivo)
# Exportar o data frame para o arquivo CSV
write.csv2(soma_por_prop_classe_ano, caminho_arquivo, row.names = FALSE)



# function for charts
lineplot_facet <- function(data, x, y, facet_var, group_var, color_var, colors, 
                           x_label, y_label, y_lim, y_breaks, n_cols = 3) {
  facet_levels <- unique(data[[facet_var]])
  n_levels <- length(facet_levels)
  n_rows <- ceiling(n_levels/n_cols)
  
  for (i in 1:n_levels) {
    level <- facet_levels[i]
    subset_data <- data[data[[facet_var]] == level, ]
    
    p <- ggplot(data = subset_data, aes(x = {{x}}, y = {{y}}, group = {{group_var}}, color = {{color_var}})) +
      geom_line(size = 2) +
      theme_classic() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
      scale_color_manual(values = colors) +
      xlab(x_label) + ylab(y_label)  +
      ggtitle(paste(facet_var, level, sep = " = "))
    
    filename <- paste(facet_var, level, ".jpg", sep = "_")
    ggsave(filename, plot = p, device = "jpg")
    print(p)
  }
}

lineplot_facet(data = soma_por_prop_classe_ano, x = ano, y = hectares, 
               facet_var = 'prop_POL', group_var = classe, color_var = classe, 
               colors = c("#006400", "#fff3bf",  "#ffd966"), 
               x_label = "ano", y_label = "hectares")


str(soma_por_classe_ano)





