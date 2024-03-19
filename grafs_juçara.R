#Graficos JUÇARA

#lendo pacotes
library(openxlsx)
library(tidyverse)
library(reshape2) 
library(viridis)
library(hrbrthemes)
library(RColorBrewer)


dir()

dados<-read.xlsx("tabelasejuçara.xlsx", sep.names = " ", sheet=1)#IMportando a tabela com os dados excel
names(dados)#noemas das colunas

dados_1<-melt(dados, id.vars=c("se"))#Reshape-formado wide para long
dados_filtrados <- dados_1 %>%
  filter(value != 0)
p1<-
ggplot(dados_filtrados, aes(fill= interaction(value, variable), y=value, x=reorder(se, value, FUN=sum))) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic(base_size = 14) +
  xlab("Categorias de serviços ecossistêmicos")+ylab("Número de menções")+labs(fill = "Legendas")+
  scale_fill_manual(values=c('#ACD39E','#D9F0D3','#456A76','#689FB0', '#A1DDEF','#B9E5F3', '#D0EEF7','#E0F3F8','#F5F5F5',
                            '#8073AC','#B2ABD2','#D8DAEB','#F4A582','#FDDBC7'),
                    labels=c("Hábitat para espécies (17)","Manutenção da diversidade genética (5)",
                             "Regulação do clima e qualidade do ar (1)","Controle de eventos extremos (2)",
                             "Purificação da água (2)","Proteção do solo (2)","Dispersão (8)","Polinização (4)",
                             "Regulação do fluxo de água (3)","Recreação e saúde mental (3)","Beleza cênica (4)",
                             "Valor intrínseco (5)", "Fornecimento de alimentos (2)","Fornecimento de água (6)"))+
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


