# Pacotes necess�rios ..........................................................
# instala��o:
install.packages("factoextra")
install.packages("cluster")
install.packages("magrittr")
#carregar pacotes:
library("cluster")
library("factoextra")
library("magrittr")

# Entrada dos dados ............................................................

# Diret�rios e Arquivos:
getwd()                              # Qual o diret�rio que o script est� apontando
list.files()                         # Quais arquivos est�o contidos no diret�rio
setwd("C:/Users/andre/Downloads")    # selecione o diret�rio que est� o arquivo (lembre de usar a barra invertida "/" ao inv�s da "\")
base <- read.csv2(file = "ads.csv")  # Leitura de uma base formato csv brasileiro (decimal separado por v�rgula)
head(base)                           # exibe as 6 primeiras linhas da base de dados
str(base)                            # comando para analisar como est�o classificadas cada coluna (n�merico, texto, etc.)

# Preparando base de dados:
dados1 <- na.omit(base)      # eliminando dados ausentes            
dados <- scale(dados1[-1])   # padronizando dados

# k-Means Cluster Analysis ....................................................

# Determinar n�mero de clusters:
wss <- (nrow(dados)-1)*sum(apply(dados,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dados, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="N� de Clusters", ylab="Soma dos quadrados dos grupos")

# N�mero �timo de clusters:
fviz_nbclust(dados, kmeans, method = "gap_stat") # Linha vertical tracejada

#*********************************************************************#
#*********************************************************************#
# Coloque o n�mero de cluster desejado e n�o precisa fazer mais nada  #
n = 6
#*********************************************************************#
#*********************************************************************#

# Ajuste do modelo pelo n�mero de clusters determinados pelo analista:
res.hk <-hkmeans(dados, n)
names(res.hk) # elementos poss�veis para extra��o
res.hk$size   # tamanho dos clusters
dados <- data.frame(dados1, res.hk$cluster)
head(dados)

# Dendograma:
fviz_dend(res.hk, cex = 0.6, palette = "jco", rect = TRUE, rect_border = "jco", rect_fill = TRUE)
write.csv2(dados, file = "clusters.csv")

# Refer�ncias ..................................................................
# https://www.statmethods.net/advstats/cluster.html
# http://lincolntneves.weebly.com/blog/anlise-de-cluster-no-r
# https://rstudio-pubs-static.s3.amazonaws.com/33876_1d7794d9a86647ca90c4f182df93f0e8.html
# https://stackoverflow.com/questions/45526629/convert-first-column-in-data-frame-to-row-index-in-r
# https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/
# https://www.rdocumentation.org/packages/factoextra/versions/1.0.7/topics/fviz_dend
# https://www.datanovia.com/en/lessons/hierarchical-k-means-clustering-optimize-clusters/