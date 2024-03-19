#Graficos CMA

#lendo pacotes
library(openxlsx)
library(tidyverse)
library(reshape2) 
library(viridis)
library(hrbrthemes)
library(RColorBrewer)

setwd("C:/Users/RAPOSA-DO-CAMPO/Documents/JULIANA_ANGELO")
dir()

dados<-read.xlsx("tabelasecma.xlsx", sep.names = " ", sheet=1)#IMportando a tabela com os dados excel
names(dados)#noemas das colunas

dados_1<-melt(dados, id.vars=c("se"))#Reshape-formado wide para long
dados_filtrados <- dados_1 %>%
  filter(value != 0)
p1<-
  ggplot(dados_filtrados, aes(fill= interaction(value, variable), y=value, x=reorder(se, value, FUN=sum))) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic(base_size = 14) +
  xlab("Categorias de serviços ecossistêmicos")+ylab("Número de menções")+labs(fill = "Legendas")+
  scale_fill_manual(values=c('#ACD39E','#D9F0D3','#456A76','#689FB0','#A1DDEF','#B9E5F3','#D0EEF7','#E0F3F8','#F5F5F5','#8073AC','#B2ABD2','#DBC3D6','#D8DAEB','#F4A582'),
                    labels=c("Hábitat para espécies (11)","Manutenção da diversidade genética (2)","Regulação do clima e qualidade do ar (1)","Sequestro e estoque de carbono (1)", "Controle de eventos extremos (1)", 
                             "Purificação da água (4)","Proteção do solo (4)","Dispersão (1)", "Regulação do fluxo de água (3)", 
                             "Recreação e saúde mental (1)","Beleza cênica (5)", "Inspiração para cultura (1)",
                             "Valor intrínseco (3)","Fornecimento de água (7)"))+
  scale_y_continuous(limits=c(0, 26), breaks = seq(0, 26, by = 10))+
  geom_text(
    aes(label = after_stat(y), group = se),          
    stat = 'summary', fun = sum, vjust = -1) 
p1
view(dados_1)

ggsave("grafico.png", p1)#expotar imagem em png

ggsave("grafico.svg", p1)#expotar imagem em svg

install.packages('svglite')
library(svglite)


