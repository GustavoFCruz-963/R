# Pacotes a serem instalados e carregados ---------------------------------
pacotes <- c("plotly","tidyverse","ggrepel","reshape2","FactoMineR",
             "knitr","kableExtra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#carregando a base de dados
load("gestao_municipal.Rdata")

#visualizando em formato tabela
gestao_municipal %>%
  kable() %>%
  kable_styling(bootstrap_options = 'striped',
                full_width = T,
                font_size = 12)

#verificando caracteristicas da tabela
summary(gestao_municipal)

#montando tabela de contingencias
tab <- table(gestao_municipal$avaliação, 
             gestao_municipal$ano)
tab

#Fazendo teste Qui Quadrado
qui2 <- chisq.test(tab)
qui2

# p-value é menor que 5% de nível de significancia, portanto podemos seguir com
#ANACOR

#Verificando componentes do qui2
qui2$observed
qui2$expected
#residuos
res <- qui2$observed - qui2$expected
res
#residuos padronizados
qui2$residuals
#residuos padronizados ajustados
qui2$stdres


#Mapa de calor dos residuos padronizados ajustados
data.frame(qui2$stdres) %>%
  rename(avaliação = 1, 
         ano = 2) %>%
  ggplot(aes( x = ano, y = avaliação, fill = Freq, label= round(Freq, 3))) +
  geom_tile()+
  geom_text(size = 3) + 
  scale_fill_gradient2(low = "yellow",
                       mid="white",
                       high = "red", 
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(),
        panel.background = element_rect("white"),
        legend.position = "none")

#Aplicando ANACOR
anacor <- CA(tab)

#Mostrando os autovalores
anacor$eig

####Construção de um mapa perceptual mais elegante

#Capturando as coordenadas
coordenadas <- rbind(anacor$row$coord, anacor$col$coord)
coordenadas  

#Capturando a quantidade de categorias
id_var <- apply(gestao_municipal[ ,1:2],
                MARGIN = 2,
                FUN = function(x) nlevels(as.factor(x)))
id_var

#Unindo as coordenadas com a quantidade de variaveis e seus nomes
coord_final <- data.frame(coordenadas, Variable = rep(names(id_var), id_var))
coord_final

coord_final %>%
  rownames_to_column() %>%
  rename(Category = 1) %>%
  ggplot(aes( x = Dim.1, y = Dim.2, fill = Variable, 
              color = Variable, shape = Variable)) +
  geom_point(size = 3) + 
  geom_label_repel(max.overlaps = 100, 
                   size = 3,
                   color = "white", 
                   label = rownames(coord_final)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") + 
  labs(x = paste("Dimension 1:", paste0(round(anacor$eig[1,2], digits = 2), "%")),
       y = paste("Dimension 2:", paste0(round(anacor$eig[2,2], digits = 2), "%"))) +
  scale_fill_viridis_d(option = "cividis") +
  scale_color_viridis_d(option = "cividis") +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")
