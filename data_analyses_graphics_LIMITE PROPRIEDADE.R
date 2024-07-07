library(ggplot2)
library(jpeg)
library(gganimate)
library(gridExtra)
library(ggthemes)

# data loading and preparation
data<-read.table(file.choose(), sep=",", header=T, stringsAsFactors = FALSE)
data_mod <- cbind(data[1:2], stack(data[3:50]))
data_mod$ind <- as.character(data_mod$ind)
data_mod$ind[data_mod$ind=="X3"] <- "Floresta Natural"
data_mod$ind[data_mod$ind=="X9"] <- "Floresta Plantada"
data_mod$ind[data_mod$ind=="X11"] <- "Campo Alagado e Área Pantanosa"
data_mod$ind[data_mod$ind=="X15"] <- "Pastagem"
data_mod$ind[data_mod$ind=="X21"] <- "Mosaico de Usos"
data_mod$ind[data_mod$ind=="X24"] <- "Área Urbanizada"
data_mod$ind[data_mod$ind=="X25"] <- "Outras Áreas não Vegetadas"
data_mod$ind[data_mod$ind=="X29"] <- "Afloramento Rochoso"
data_mod$ind[data_mod$ind=="X33"] <- "Rio"
data_mod$ind[data_mod$ind=="X30"] <- "Mineração"
data_mod$ind[data_mod$ind=="X39"] <- "Soja"
data_mod$ind[data_mod$ind=="X41"] <- "Outras Lavouras Temporárias"



data_mod$ind <- as.factor(data_mod$ind)
data_mod <- na.omit(data_mod)

View(data_mod)

##### GRAPHICS
### LINEPLOTS
#data preparation
soma_por_classe_ano <- aggregate(data_mod$values, 
                                 by = list(data_mod$ind, data_mod$year), 
                                 FUN = sum)

colnames(soma_por_classe_ano) <- c("classe", "ano", "hectares")
soma_por_classe_ano$ano <- as.character(soma_por_classe_ano$ano)
soma_por_classe_ano$classe <- as.factor(soma_por_classe_ano$classe)
str(soma_por_classe_ano)



# CRESCIMENTO ILIMITADO
densidades <- c("Floresta Natural" = 80, "Floresta Plantada" = 30, "Mosaico de Usos" = 10, "Pastagem" = 2, "Outras Lavouras Temporárias" = 2)
incrementos <- c("Floresta Natural" = 0.005, "Floresta Plantada" = 0.1, "Mosaico de Usos" = 0.2, "Pastagem" = 0.2, "Outras Lavouras Temporárias" = 1) # Ajuste dos incrementos
df_final <- data.frame(classe = character(), ano = integer(), estoque = numeric(), stringsAsFactors = FALSE)

# Loop para preencher o dataframe com os estoques de carbono
for (classe in unique(soma_por_classe_ano$classe)) {
  anos_classe <- unique(soma_por_classe_ano$ano[soma_por_classe_ano$classe == classe])
  for (ano in anos_classe) {
    estoque <- 0
    if (classe %in% names(densidades)) {
      densidade <- densidades[classe]
      estoque_inicial <- soma_por_classe_ano$hectares[soma_por_classe_ano$classe == classe & soma_por_classe_ano$ano == ano]
      if (as.integer(ano) >= 2022) {
        anos_passados <- as.integer(ano) - 2022
        estoque <- estoque_inicial * densidade + (anos_passados *  estoque_inicial)
      } else {
        estoque <- estoque_inicial * densidade
      }
    }
    # Adicionar linha apenas se o estoque não for zero
    if (estoque != 0) {
      df_final <- rbind(df_final, data.frame(classe = classe, ano = as.integer(ano), estoque = estoque))
    }
  }
}

# Adicionar projeção de 2022 até 2075 usando crescimento logarítmico
for (classe in unique(df_final$classe)) {
  estoque_2022 <- df_final$estoque[df_final$classe == classe & df_final$ano == 2022]
  incremento <- incrementos[classe]
  for (ano in 2023:2075) {
    anos_passados <- ano - 2022
    taxa_crescimento <- incremento / (1 + exp(-0.8 * (anos_passados - 10))) # Ajuste da taxa de crescimento ao longo do tempo com ponto de inflexão em 2040
    estoque_final <- estoque_2022 * (1 + taxa_crescimento * anos_passados) # Calculando o estoque usando crescimento logarítmico
    df_final <- rbind(df_final, data.frame(classe = classe, ano = ano, estoque = estoque_final))
  }
}


# CRESCIMENTO LIMITADO
# Dados de densidade e incremento anual por classe
densidades <- c("Floresta Natural" = 80, "Floresta Plantada" = 30, "Mosaico de Usos" = 10, "Pastagem" = 2, "Outras Lavouras Temporárias" = 2)
incrementos <- c("Floresta Natural" = 0.0025, "Floresta Plantada" = 0.3, "Mosaico de Usos" = 0.15, "Pastagem" = 0.1, "Outras Lavouras Temporárias" = 1) # Ajuste dos incrementos
df_final <- data.frame(classe = character(), ano = integer(), estoque = numeric(), stringsAsFactors = FALSE)

# Loop para preencher o dataframe com os estoques de carbono
for (classe in unique(soma_por_classe_ano$classe)) {
  anos_classe <- unique(soma_por_classe_ano$ano[soma_por_classe_ano$classe == classe])
  for (ano in anos_classe) {
    estoque <- 0
    if (classe %in% names(densidades)) {
      densidade <- densidades[classe]
      estoque_inicial <- soma_por_classe_ano$hectares[soma_por_classe_ano$classe == classe & soma_por_classe_ano$ano == ano]
      if (as.integer(ano) >= 2022) {
        anos_passados <- as.integer(ano) - 2022
        estoque <- estoque_inicial * densidade + (anos_passados *  estoque_inicial)
      } else {
        estoque <- estoque_inicial * densidade
      }
    }
    # Adicionar linha apenas se o estoque não for zero
    if (estoque != 0) {
      df_final <- rbind(df_final, data.frame(classe = classe, ano = as.integer(ano), estoque = estoque))
    }
  }
}

# Adicionar projeção de 2022 até 2075 usando crescimento logarítmico
for (classe in unique(df_final$classe)) {
  estoque_2022 <- df_final$estoque[df_final$classe == classe & df_final$ano == 2022]
  incremento <- incrementos[classe]
  for (ano in 2023:2075) {
    anos_passados <- ano - 2022
    taxa_crescimento <- incremento / (1 + exp(-0.5 * (anos_passados - 12))) # Ajuste da taxa de crescimento ao longo do tempo com ponto de inflexão em 2040
    estoque_final <- estoque_2022 * (1 + taxa_crescimento * anos_passados) # Calculando o estoque usando crescimento logarítmico
    
    # Limitar o crescimento pelo valor da classe "Floresta Natural" se necessário
    if (classe != "Floresta Natural") {
      estoque_floresta_natural <- df_final$estoque[df_final$classe == "Floresta Natural" & df_final$ano == ano]
      if (estoque_final > estoque_floresta_natural) {
        estoque_final <- estoque_floresta_natural
      }
    }
    
    df_final <- rbind(df_final, data.frame(classe = classe, ano = ano, estoque = estoque_final))
  }
}

str()


#general chart LULC
ggplot(data=soma_por_classe_ano, aes(x=ano, y=estoque, group= classe, 
                                     color=classe)) +
  geom_line(size=2) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_color_manual(values=c('#D5D5E5', '#af2a2a',  '#45C2A5', "#006400", '#935132', '#9c0027',
                              "#fff3bf", '#FF99FF', '#f54ca9', "#ffd966", '#0000ff', '#f5b3c8' ), 
                     name="classe de uso do solo") +
  xlab("ano") + ylab("hectares") 

library(dplyr)

# Calcular a soma das classes nos anos especificados
somas <- df_final %>%
  filter(ano %in% c(2035, 2050, 2070)) %>%
  group_by(ano) %>%
  summarise(soma = sum(estoque)/1000)
# Filtrar os dados para os anos desejados
anos_destacados <- c(2025, 2035, 2050, 2070)
df_destacados <- df_final[df_final$ano %in% anos_destacados, ]

# Separar os dados da classe Floresta Natural
df_floresta <- df_final %>% filter(classe == "Floresta Natural")

ggplot(data = df_final, aes(x = ano, y = estoque, group = classe, color = classe)) +
  geom_line(data = df_final %>% anti_join(df_floresta), size = 2) +  # Linhas de outras classes
  geom_line(data = df_floresta, size = 2) +  # Linha da classe Floresta Natural
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 12),  # Tamanho da fonte do eixo y
        legend.position = "none") +
  scale_color_manual(values = c("#006400", '#935132', "#fff3bf", '#f54ca9', "#ffd966"), 
                     name = "classe de uso do solo") +
  xlab("") + ylab("Gt C") +
  xlim(2015, 2075) +
  scale_y_continuous(labels = scales::comma_format(scale = 0.001)) +
  scale_x_continuous(breaks = seq(2015, 2075, by = 5)) +  # Exibir todos os anos
  geom_text(data = somas, aes(label = paste(ano, ":", round(soma, 1), "Gt"), x = ano, y = soma), 
            vjust = -40, hjust = 1.1, size = 3, color = "black") + 
  geom_vline(data = df_destacados, aes(xintercept = ano), linetype = "dashed", color = "black")



#######################################################
#######################################################
##### function  for all sites_polygons Lineplots charts
#data preparation
soma_por_prop_classe_ano <- aggregate(data_mod$values, 
                                      by = list(data_mod$ind, data_mod$year,
                                                data_mod$propriedade_POL), 
                                      FUN = sum)

colnames(soma_por_prop_classe_ano) <- c("classe", "ano", "prop_POL", "hectares")
soma_por_prop_classe_ano$ano <- as.character(soma_por_prop_classe_ano$ano)
soma_por_prop_classe_ano$classe <- as.factor(soma_por_prop_classe_ano$classe)
soma_por_prop_classe_ano$prop_POL <- as.character(soma_por_prop_classe_ano$prop_POL)

str(soma_por_prop_classe_ano)

write.csv2(soma_por_prop_classe_ano, 
          file = "C:/Users/pedro.rajao/Desktop/AGEVAP/10. quantificação C-GEE PAF/1. LANDSAT-mapbiomas/limite_propriedades/soma_por_prop_classe_ano2.csv", row.names = FALSE)

#### function for chart generates and visualize

lineplot_facet <- function(data, x, y, facet_var, group_var, color_var, colors, 
                           x_label, y_label, y_lim, y_breaks, n_cols = 3) {
  facet_levels <- unique(data[[facet_var]])
  n_levels <- length(facet_levels)
  n_rows <- ceiling(n_levels/n_cols)
  
  # Definir cores para cada classe
  class_colors <- c("Floresta Natural" = "#006400", "Floresta Plantada" = '#af2a2a', 
                    "Campo Alagado e Área Pantanosa" = '#45C2A5', "Pastagem" = "#ffd966", 
                    "Mosaico de Usos" = "#fff3bf", "Área Urbanizada" = '#FF99FF', 
                    "Outras Áreas não Vegetadas" =  '#935132', "Rio" = '#0000ff')
  
  for (i in 1:n_levels) {
    level <- facet_levels[i]
    subset_data <- data[data[[facet_var]] == level, ]
    
    p <- ggplot(data = subset_data, aes({{x}}, {{y}}, group = {{group_var}}, color = {{color_var}})) +
      geom_line(size = 2) +
      theme_classic() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
      scale_color_manual(values = class_colors, guide = FALSE) +
      xlab(x_label) + ylab(y_label)  +
      ggtitle(paste(level))
    
    filename <- paste(facet_var, level, ".jpg", sep = "_")
    ggsave(filename, plot = p, device = "jpg")
    print(p)
  }
}

lineplot_facet(data = soma_por_prop_classe_ano, x = ano, y = hectares, 
               facet_var = 'prop_POL', group_var = classe, color_var = classe, 
               colors = class_colors,
               x_label = "ano", y_label = "hectares")


### function for export CHARTS
lineplot_facet <- function(data, x, y, facet_var, group_var, color_var, colors, 
                           x_label, y_label, y_lim, y_breaks, n_cols = 3, output_dir = "") {
  facet_levels <- unique(data[[facet_var]])
  n_levels <- length(facet_levels)
  n_rows <- ceiling(n_levels/n_cols)
  
  # Definir cores para cada classe
  class_colors <- c("Floresta Natural" = "#006400", "Floresta Plantada" = '#af2a2a', 
                    "Campo Alagado e Área Pantanosa" = '#45C2A5', "Pastagem" = "#ffd966", 
                    "Mosaico de Usos" = "#fff3bf", "Área Urbanizada" = '#FF99FF', 
                    "Outras Áreas não Vegetadas" =  '#935132', "Rio" = '#0000ff')
  
  for (i in 1:n_levels) {
    level <- facet_levels[i]
    subset_data <- data[data[[facet_var]] == level, ]
    
    p <- ggplot(data = subset_data, aes({{x}}, {{y}}, group = {{group_var}}, color = {{color_var}})) +
      geom_line(size = 2) +
      theme_classic() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
      scale_color_manual(values = class_colors, guide = FALSE) +
      xlab(x_label) + ylab(y_label)  +
      ggtitle(paste(level))
    
    filename <- paste(output_dir, level, sep = "/")  # Caminho do diretório + nome do arquivo
    ggsave(filename, plot = p, device = "png", dpi = 300)
    print(p)
  }
}

# Exemplo de uso com diretório de saída
output_directory <- "C:/Users/pedro.rajao/Desktop/AGEVAP/10. quantificação C-GEE PAF/1. LANDSAT-mapbiomas/limite_propriedades"
lineplot_facet(data = soma_por_prop_classe_ano, x = ano, y = hectares, 
               facet_var = 'prop_POL', group_var = classe, color_var = classe, 
               colors = class_colors,
               x_label = "ano", y_label = "hectares",
               output_dir = output_directory)







