### Executar os seguintes arquivos
###  1 - importa-filtra-basededados.r
###  2 - preprocessamento-regras-associacao.r
###  3 - phyton script ../python-scripts/prepare-database-association-rule.py
###  4 - para executar a função geraHistogramaItens o script countItensRule.py deve ser executado

library("BBmisc")
if (!require("arules")) {
  install.packages("arules")
  library("arules")
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library("tidyverse")
}
if (!require("tidyr")) {
  install.packages("tidyr")
  library("tidyr")
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
if (!require("arulesViz")) {
  install.packages("arulesViz")
  library(arulesViz)
}

readTransactions <- function(fileName) {
  return (read.transactions(fileName,
                            format = "basket",
                            sep = ",", 
                            cols = NULL))
}
itemFrequencyGGPlot <- function(x, topN) {
  library(tidyverse)
  x %>%
    itemFrequency %>%
    sort %>%
    tail(topN) %>%
    as.data.frame %>%
    tibble::rownames_to_column() %>%
    ggplot(aes(reorder(rowname, `.`),`.`)) +
    geom_col() + 
    coord_flip()
}  


geraHistogramaItens <- function (fileName) {
  library(magrittr)
  
  data <- readLines(fileName)
  # then send the text down this pipeline:
  char_counts <- data %>% 
    paste(collapse="") %>%
    strsplit(split=",") %>% unlist %>% 
    `[`(!. %in% c("", " ", ".", ",")) %>%
    table
  
  counts_df <- data.frame(
    char = names(char_counts), 
    count = as.numeric(char_counts), 
    stringsAsFactors=FALSE) 
  counts_df <- counts_df[order(counts_df$count, decreasing = T),]
  ggplot(counts_df, aes(x=char, y = count)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(y="Quantidade", x="Questões") +
    geom_bar(stat="identity")
}

sp.all.tr <- readTransactions("./output/sp_transactions.csv")
itemLabels(sp.all.tr)

# Escolha minsup
itemFrequencyPlot(sp.all.avgNotaA.tr, 
                  support=0.02,
                  topN=90,
                  type="relative", main="Item Frequency sp.all.avgNotaA.tr",
                  col=brewer.pal(8,'Pastel2'))

### Primeira analise
minSuporte <- 0.02
minConfianca <- 0.1

sp.all.rules.2per <- apriori(data=sp.all.tr, 
                        parameter=list (
                          supp= minSuporte,
                          conf = minConfianca, 
                          minlen=2,
                          maxtime=40
                        ), 
                        control = list (verbose=T))

### Gerando itens frequentes utlizados pelo apriori, durante a execução o algoritmo mostra a quantidade itens que estão 
###  sendo utlizandos ex: $ sorting and recoding items ... [79 item(s)] done [0.11s].

itemFrequencyGGPlot(sp.all.tr, 79)

plot(sp.all.rules, method = "two-key", control=list(main = ""))
sp.all.rules.avgNotaA <- subset(sp.all.rules, rhs %ain% c("AVG_NOTA=A"))
sp.all.rules.avgNotaB <- subset(sp.all.rules, rhs %ain% c("AVG_NOTA=B"))
sp.all.rules.avgNotaC <- subset(sp.all.rules, rhs %ain% c("AVG_NOTA=C"))

plot(sp.all.rules, method = "two-key", control=list(main = ""))

df <- as(subset(sp.all.rules,subset = size(lhs) + size(rhs) == 2), "data.frame")
df[order(df$lift,decreasing = T), ][1:10,]
df <- as(subset(sp.all.rules,subset = size(lhs) + size(rhs) == 3), "data.frame")
df[order(df$lift,decreasing = T), ][1:10,]
df <- as(subset(sp.all.rules,subset = size(lhs) + size(rhs) == 4), "data.frame")
df[order(df$lift,decreasing = T), ][1:10,]
df <- as(subset(sp.all.rules,subset = size(lhs) + size(rhs) == 5), "data.frame")
df[order(df$lift,decreasing = T), ][1:10,]
df <- as(subset(sp.all.rules,subset = size(lhs) + size(rhs) == 6), "data.frame")
df[order(df$lift,decreasing = T), ][1:10,]
df <- as(subset(sp.all.rules,subset = size(lhs) + size(rhs) == 7), "data.frame")
df[order(df$lift,decreasing = T), ][1:10,]
df <- as(subset(sp.all.rules,subset = size(lhs) + size(rhs) == 8), "data.frame")
df[order(df$lift,decreasing = T), ][1:10,]

### Segunda analise
minSuporte <- 0.002
minConfianca <- 0.1
sp.all.rules <- apriori(data=sp.all.tr, 
                        parameter=list (
                          supp= minSuporte,
                          conf = minConfianca, 
                          minlen=2,
                          maxtime=40
                        ), 
                        control = list (verbose=T))

### Gerando itens frequentes utlizados pelo apriori, durante a execução o algoritmo mostra a quantidade itens que estão 
###  sendo utlizandos ex: $ sorting and recoding items ... [135 item(s)] done [0.11s].

itemFrequencyGGPlot(sp.all.tr, 135)

plot(sp.all.rules, method = "two-key", control=list(main = ""))
plot(sp.all.rules,  engine = "plotly" ,measure=c("support", "confidence"), shading = "lift")

sp.all.rules.avgNotaA <- subset(sp.all.rules, rhs %ain% c("AVG_NOTA=A"))
sp.all.rules.avgNotaB <- subset(sp.all.rules, rhs %ain% c("AVG_NOTA=B"))
sp.all.rules.avgNotaC <- subset(sp.all.rules, rhs %ain% c("AVG_NOTA=C"))
plot(sp.all.rules.avgNotaA, method = "two-key", control=list(main = ""))
plot(sp.all.rules.avgNotaB, method = "two-key", control=list(main = ""))
plot(sp.all.rules.avgNotaC, method = "two-key", control=list(main = ""))

plot(sp.all.rules.avgNotaA,  engine = "plotly" ,measure=c("support", "confidence"), shading = "lift")
plot(sp.all.rules.avgNotaB,  engine = "plotly" ,measure=c("support", "confidence"), shading = "lift")
plot(sp.all.rules.avgNotaC,  engine = "plotly" ,measure=c("support", "confidence"), shading = "lift")

dfA <- as(sp.all.rules.avgNotaA, "data.frame")
write.table(dfA[order(dfA$lift,decreasing = T), ][1:1000,],"./output/dfA.txt",sep="\t",row.names=FALSE)
geraHistogramaItens('./output-avg-a.txt')
dfB <- as(sp.all.rules.avgNotaB, "data.frame")  
write.table(dfB[order(dfB$lift,decreasing = T), ][1:1000,],"./output/dfB.txt",sep="\t",row.names=FALSE)
geraHistogramaItens('./output-avg-b.txt')
dfC <- as(sp.all.rules.avgNotaC, "data.frame")  
write.table(dfC[order(dfC$lift,decreasing = T), ][1:1000,],"./output/dfC.txt",sep="\t",row.names=FALSE)
geraHistogramaItens('./output-avg-c.txt')

