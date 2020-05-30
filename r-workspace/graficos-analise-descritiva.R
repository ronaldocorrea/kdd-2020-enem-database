# para executar este script deve-se 
# executar primeiro o script 
# importa-filtra-basededados.R

library(tidyverse)
library(gridExtra)
library(moments)
library(packHV)
# ADD da base de estudantes que fizeram o ENEM em 2012,
# residentes em São Paulo, SP

dataFrame <- spCandidatos
estudantes <- nrow(dataFrame)

# análises univariadas - atributos categóricos - distribuições de frequência

# o atributo color de um plot exige que seja um <fct>

ggplot(data = dataFrame, mapping = aes(x = IDADE)) + geom_bar()
# ggsave('../output/IDADE.png')

dataFrame %>% count(TP_SEXO) %>% mutate(percentual = 100 * n / estudantes) -> genders
plotTpSexo <- ggplot(data = genders, mapping = aes(x = TP_SEXO, y = percentual)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Total", x="Sexo")+
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/TP_SEXO.png",plotTpSexo)


dataFrame %>% count(ST_CONCLUSAO) %>% mutate(percentual = 100 * n / estudantes) -> conclusion
pltotconclusion <- ggplot(data = conclusion, mapping = aes(x = ST_CONCLUSAO, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Total", x="Situação Conclusão")+
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/ST_CONCLUSAO.png",pltotconclusion)


dataFrame %>% count(TP_ESCOLA) %>% mutate(percentual = 100 * n / estudantes) -> schools
plotTpEscola <- ggplot(data = schools, mapping = aes(x = TP_ESCOLA, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Total", x="Tipo de escola ensino médio")+
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/TP_ESCOLA.png",plotTpEscola)


dataFrame %>% count(IN_TP_ENSINO) %>% mutate(percentual = 100 * n / estudantes) -> teach
plotInTpEnsino <- ggplot(data = teach, mapping = aes(x = IN_TP_ENSINO, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Total", x="Tipo de instituição que concluiu ou \nconcluirá o Ensino Médio")+
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/IN_TP_ENSINO.png",plotInTpEnsino)


dataFrame %>% count(TP_COR_RACA) %>% mutate(percentual = 100 * n / estudantes) -> ethnics
plotTpRaca <- ggplot(data = ethnics, mapping = aes(x = TP_COR_RACA, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Total", x="Cor/raça")+
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/TP_COR_RACA.png",plotTpRaca)


dataFrame %>% count(ID_DEPENDENCIA_ADM) %>% mutate(percentual = 100 * n / estudantes) -> dependency
plotIdDependenciaAdm <- ggplot(data = dependency, mapping = aes(x = ID_DEPENDENCIA_ADM, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Total", x="Dependência administrativa (Escola)") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/ID_DEPENDENCIA_ADM.png",plotIdDependenciaAdm)


dataFrame %>% count(ID_LOCALIZACAO) %>% mutate(percentual = 100 * n / estudantes) -> location
plotLocalizacaoEscola <- ggplot(data = location, mapping = aes(x = ID_LOCALIZACAO, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Total", x="Localização (Escola)") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/ID_LOCALIZACAO.png",plotLocalizacaoEscola)


dataFrame %>% count(NO_MUNICIPIO_PROVA) %>% mutate(percentual = 100 * n / estudantes) -> mun
plotNomeMunicipioAplicacaoProva <- ggplot(data = mun, mapping = aes(x = NO_MUNICIPIO_PROVA, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Total", x="Nome do Município da aplicação da prova") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/NO_MUNICIPIO_PROVA.png",plotNomeMunicipioAplicacaoProva)

dataFrame %>% count(IN_PRESENCA_CN) %>% mutate(percentual = 100 * n / estudantes) -> cn
plotInPresencaCN <- ggplot(data = cn, mapping = aes(x = IN_PRESENCA_CN, y = percentual)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Total", x="Presença na prova objetiva de Ciências da Natureza") +
  geom_bar(stat = 'identity', width = 0.1 )
ggsave("./analise-descritiva/IN_PRESENCA_CN.png",plotInPresencaCN)

dataFrame %>% count(IN_PRESENCA_CH) %>% mutate(percentual = 100 * n / estudantes) -> ch
plotInPresencaCH <- ggplot(data = ch, mapping = aes(x = IN_PRESENCA_CH, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Total", x="Presença na prova objetiva de Ciências Humanas") +
  geom_bar(stat = 'identity', width = 0.1 )
ggsave("./analise-descritiva/IN_PRESENCA_CH.png",plotInPresencaCH)

dataFrame %>% count(IN_PRESENCA_LC) %>% mutate(percentual = 100 * n / estudantes) -> lc
plotInPresencaLC <- ggplot(data = lc, mapping = aes(x = IN_PRESENCA_LC, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Total", x="Presença na prova objetiva de Linguagens e Códigos") +
  geom_bar(stat = 'identity', width = 0.1 )
ggsave("./analise-descritiva/IN_PRESENCA_LC.png",plotInPresencaLC)


dataFrame %>% count(IN_PRESENCA_MT) %>% mutate(percentual = 100 * n / estudantes) -> mt
plotInPresencaMT <- ggplot(data = mt, mapping = aes(x = IN_PRESENCA_MT, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Total", x="Presença na prova objetiva de Matemática") +
  geom_bar(stat = 'identity', width = 0.1 )
ggsave("./analise-descritiva/IN_PRESENCA_MT.png",plotInPresencaMT)


dataFrame %>% count(IN_STATUS_REDACAO) %>% mutate(percentual = 100 * n / estudantes) -> red
plotInStatusRedacaao <- ggplot(data = red, mapping = aes(x = IN_STATUS_REDACAO, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Total", x="Presença à redação") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/IN_STATUS_REDACAO.png",plotInStatusRedacaao)


# análises univariadas - atributos contínuos - histogramas

plotNuNtCN <- ggplot(data = dataFrame, mapping = aes(x = NU_NT_CN)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Percentual", x="Nota da prova de Ciências da Natureza") +
  geom_histogram(binwidth = 10)
ggsave("./analise-descritiva/NU_NT_CN.png",plotNuNtCN)

plotNuNtCH <- ggplot(data = dataFrame, mapping = aes(x = NU_NT_CH)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Percentual", x="Nota da prova de Ciências Humanas") +
  geom_histogram(binwidth = 10)
ggsave("./analise-descritiva/NU_NT_CH.png",plotNuNtCH)


plotNuNtLC <- ggplot(data = dataFrame, mapping = aes(x = NU_NT_LC)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Percentual", x="Nota da prova de Linguagens e Códigos") +
  geom_histogram(binwidth = 10)
ggsave("./analise-descritiva/NU_NT_LC.png",plotNuNtLC)


plotNuNtMt <- ggplot(data = dataFrame, mapping = aes(x = NU_NT_MT)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Percentual", x="Nota da prova de Matemática") +
  geom_histogram(binwidth = 10)
ggsave("./analise-descritiva/NU_NT_MT.png",plotNuNtMt)


plotNotaRedacaoComp1 <- ggplot(data = dataFrame, mapping = aes(x = NU_NOTA_COMP1)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Percentual", x="Nota da competência 1") +
  geom_histogram(binwidth = 20)
ggsave("./analise-descritiva/NU_NOTA_COMP1.png",plotNotaRedacaoComp1)


plotNotaRedacaoComp2<- ggplot(data = dataFrame, mapping = aes(x = NU_NOTA_COMP2)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Percentual", x="Nota da competência 2") +
  geom_histogram(binwidth = 20)
ggsave("./analise-descritiva/NU_NOTA_COMP2.png",plotNotaRedacaoComp2)


plotNotaRedacaoComp3 <- ggplot(data = dataFrame, mapping = aes(x = NU_NOTA_COMP3)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Percentual", x="Nota da competência 3") +
  geom_histogram(binwidth = 20)
ggsave("./analise-descritiva/NU_NOTA_COMP3.png",plotNotaRedacaoComp3)


plotNotaRedacaoComp4 <- ggplot(data = dataFrame, mapping = aes(x = NU_NOTA_COMP4)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Percentual", x="Nota da competência 4") +
  geom_histogram(binwidth = 20)
ggsave("./analise-descritiva/NU_NOTA_COMP4.png",plotNotaRedacaoComp4)


plotNotaRedacaoComp5 <- ggplot(data = dataFrame, mapping = aes(x = NU_NOTA_COMP5)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Percentual", x="Nota da competência 5") +
  geom_histogram(binwidth = 20)
ggsave("./analise-descritiva/NU_NOTA_COMP5.png",plotNotaRedacaoComp5)


plotNotaRedacao <- ggplot(data = dataFrame, mapping = aes(x = NU_NOTA_REDACAO)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Percentual", x="Nota da prova de redação") +
  geom_histogram(binwidth = 20)
ggsave("./analise-descritiva/NU_NOTA_REDACAO.png",plotNotaRedacao)


plotMediaProvaObjetiva <- ggplot(data = dataFrame, mapping = aes(x = AVG_NOTA)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Percentual", x="Média notas provas objetivas") +
  geom_histogram(binwidth = 20)
ggsave("./analise-descritiva/AVG_NOTA.png",plotMediaProvaObjetiva)


# análises univariadas - atributos categóricos - questionário socioeconômico

# vale analisar se as perguntas de 40 em diante, do EJA, possuem algum impacto


dataFrame %>% count(Q01) %>% mutate(percentual = 100 * n / estudantes) -> Q01
plotQ01 <- ggplot(data = Q01, mapping = aes(x = Q01, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(breaks=c("A","B","C","D","E","F","G","H","I"), 
                   labels=c("A - Não estudou",
                            "B - Da 1a à 4a série do \n Ensino Fundamental",
                            "C - a 5a à 8a série do \n Ensino Fundamental",
                            "D - Ensino Médio incompleto",
                            "E - Ensino Médio ",
                            "F - Ensino Superior incompleto",
                            "G - Ensino Superior",
                            "H - Pós-graduação",
                            "I - Não sei")) + 
  labs(y="Percentual", x="Q01 - Até quando seu pai estudou?")+
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q01.png",plotQ01)

dataFrame %>% count(Q02) %>% mutate(percentual = 100 * n / estudantes) -> Q02
plotQ02 <- ggplot(data = Q02, mapping = aes(x = Q02, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(breaks=c("A","B","C","D","E","F","G","H","I"), 
                   labels=c("A - Não estudou",
                            "B - Da 1a à 4a série do \n Ensino Fundamental",
                            "C - a 5a à 8a série do \n Ensino Fundamental",
                            "D - Ensino Médio incompleto",
                            "E - Ensino Médio ",
                            "F - Ensino Superior incompleto",
                            "G - Ensino Superior",
                            "H - Pós-graduação",
                            "I - Não sei")) + 
  labs(y="Percentual", x="Q02 - Até quando sua mãe estudou?") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q02.png",plotQ02)

dataFrame %>% count(Q03) %>% mutate(percentual = 100 * n / estudantes) -> Q03
plotQ03 <-ggplot(data = Q03, mapping = aes(x = Q03, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(breaks=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q"), 
                   labels=c("A - Nenhuma renda",
                            "B - até R$622,00",
                            "C - de R$ 622,01 até R$ 933,00",
                            "D - de R$ 933,01 até R$ 1244,00",
                            "E - de R$ 1244,01 até R$ 1555,00",
                            "F - de R$ 1555,01 até R$ 1866,00",
                            "G - de R$ 1866,01 até R$ 2488,00",
                            "H - de R$ 2488,01 até R$ 3110,00",
                            "I - de R$ 3110,01 até R$ 3732,00",
                            "J - de R$ 3732,01 até R$ 4354,00",
                            "K - de R$ 4354,01 até R$ 4976,00",
                            "L - de R$ 4976,01 até R$ 5598,00",
                            "M - de R$ 5598,01 até R$ 6220,00",
                            "N - de R$ 6220,01 até R$ 7464,00",
                            "O - de R$ 7464,01 até R$ 9330,00",
                            "P - de R$ 9330,01 até R$ 12440,00",
                            "Q - mais de R$ 12440,00")) + 
  labs(y="Percentual", x="Q03 - Qual é a renda mensal de sua família? \n(Some a sua renda com a dos seus familiares.)") +  
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q03.png",plotQ03)

dataFrame %>% count(Q04) %>% mutate(percentual = 100 * n / estudantes) -> Q04
plotQ04 <- ggplot(data = Q04, mapping = aes(x = as.numeric(Q04), y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(y="Percentual", x="Q04 - Quantas pessoas moram em sua casa (incluindo você)") +  
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q04.png",grid.arrange(plotQ04))

dataFrame %>% count(Q05) %>% mutate(percentual = 100 * n / estudantes) -> Q05
plotQ05 <- ggplot(data = Q05, mapping = aes(x = Q05, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(breaks=c("A","B","C","D","E"), 
                   labels=c("A -  Própria e quitada",
                            "B - Própria e em pagamento (financiada)",
                            "C - Alugada",
                            "D - Cedida",
                            "E - Outra situação \n(loteamento não regularizado, ocupação, etc)")) + 
  labs(y="Percentual", x="Q05 - A residência de sua família é?") +  
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q05.png",grid.arrange(plotQ05))

dataFrame %>% count(Q06) %>% mutate(percentual = 100 * n / estudantes) -> Q06
plotQ06 <- ggplot(data = Q06, mapping = aes(x = Q06, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(breaks=c("A","B","C","D"), 
                   labels=c("A - Zona rural",
                            "B - Zona urbana",
                            "C - Comunidade indígena",
                            "D - Comunidade quilombola")) + 
  labs(y="Percentual", x="Q06 - A residência de sua família está localizada em?") +  
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q06.png",grid.arrange(plotQ06))

dataFrame %>% count(Q07) %>% mutate(percentual = 100 * n / estudantes) -> Q07
plotQ07 <- ggplot(data = Q07, mapping = aes(x = Q07, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q07 - Você tem em sua casa? TV em cores & Categórica") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q07.png",grid.arrange(plotQ07))

dataFrame %>% count(Q08) %>% mutate(percentual = 100 * n / estudantes) -> Q08
plotQ08 <- ggplot(data = Q08, mapping = aes(x = Q08, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q08 - Você tem em sua casa? Videocassete e/ou DVD & Categórica ") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q08.png",grid.arrange(plotQ08))

dataFrame %>% count(Q09) %>% mutate(percentual = 100 * n / estudantes) -> Q09
plotQ09 <- ggplot(data = Q09, mapping = aes(x = Q09, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q09 - Você tem em sua casa? Rádio ") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q09.png",grid.arrange(plotQ09))

dataFrame %>% count(Q10) %>% mutate(percentual = 100 * n / estudantes) -> Q10
plotQ10 <- ggplot(data = Q10, mapping = aes(x = Q10, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q10 - Você tem em sua casa? Microcomputador") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q10.png",grid.arrange(plotQ10))

dataFrame %>% count(Q11) %>% mutate(percentual = 100 * n / estudantes) -> Q11
plotQ11 <- ggplot(data = Q11, mapping = aes(x = Q11, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q11 - Você tem em sua casa? Automóvel") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q11.png",grid.arrange(plotQ11))

dataFrame %>% count(Q12) %>% mutate(percentual = 100 * n / estudantes) -> Q12
plotQ12 <- ggplot(data = Q12, mapping = aes(x = Q12, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q12 - Você tem em sua casa? Máquina de lavar roupa") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q12.png",grid.arrange(plotQ12))

dataFrame %>% count(Q13) %>% mutate(percentual = 100 * n / estudantes) -> Q13
plotQ13 <- ggplot(data = Q13, mapping = aes(x = Q13, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q13 - Você tem em sua casa? Geladeira") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q13.png",grid.arrange(plotQ13))

dataFrame %>% count(Q14) %>% mutate(percentual = 100 * n / estudantes) -> Q14
plotQ14 <- ggplot(data = Q14, mapping = aes(x = Q14, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q14 - Você tem em sua casa? Freezer \n(aparelho independente ou parte da geladeira duplex) ") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q14.png",grid.arrange(plotQ14))

dataFrame %>% count(Q15) %>% mutate(percentual = 100 * n / estudantes) -> Q15
plotQ15 <- ggplot(data = Q15, mapping = aes(x = Q15, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q15 - Você tem em sua casa? Telefone fixo") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q15.png",grid.arrange(plotQ15))

dataFrame %>% count(Q16) %>% mutate(percentual = 100 * n / estudantes) -> Q16
plotQ16 <- ggplot(data = Q16, mapping = aes(x = Q16, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q16 - Você tem em sua casa? Telefone celular") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q16.png",grid.arrange(plotQ16))

dataFrame %>% count(Q17) %>% mutate(percentual = 100 * n / estudantes) -> Q17
plotQ17 <- ggplot(data = Q17, mapping = aes(x = Q17, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q17 - Você tem em sua casa? Acesso à internet") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q17.png",grid.arrange(plotQ17))

dataFrame %>% count(Q18) %>% mutate(percentual = 100 * n / estudantes) -> Q18
plotQ18 <- ggplot(data = Q18, mapping = aes(x = Q18, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q18 - Você tem em sua casa? TV por assinatura") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q18.png",grid.arrange(plotQ18))

dataFrame %>% count(Q19) %>% mutate(percentual = 100 * n / estudantes) -> Q19
plotQ19 <- ggplot(data = Q19, mapping = aes(x = Q19, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q19 - Você tem em sua casa? Aspirador de pó") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q19.png",grid.arrange(plotQ19))

dataFrame %>% count(Q20) %>% mutate(percentual = 100 * n / estudantes) -> Q20
plotQ20 <- ggplot(data = Q20, mapping = aes(x = Q20, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q20 - Você tem em sua casa? Emprega mensalista") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q20.png",grid.arrange(plotQ20))

dataFrame %>% count(Q21) %>% mutate(percentual = 100 * n / estudantes) -> Q21
plotQ21 <- ggplot(data = Q21, mapping = aes(x = Q21, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D"),
                   labels=c("A - 1",
                            "B - 2",
                            "C - 3 ou mais",
                            "D - Não tenho")) +
  labs(y="Percentual", x="Q21 - Você tem em sua casa? Banheiro") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q21.png",grid.arrange(plotQ21))

dataFrame %>% count(Q22) %>% mutate(percentual = 100 * n / estudantes) -> Q22
plotQ22 <- ggplot(data = Q22, mapping = aes(x = Q22, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C"),
                   labels=c("A - Sim, estou trabalhando",
                            "B - Sim, já trabalhei, \nmas não estou trabalhando",
                            "C - Não, nunca trabalhei")) +
  labs(y="Percentual", x="Q22 - Você exerce ou já exerceu atividade remunerada?") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q22.png", grid.arrange(plotQ22))

dataFrame %>% count(Q23) %>% mutate(percentual = 100 * n / estudantes) -> Q23
plotQ23<-ggplot(data = Q23, mapping = aes(x = Q23, y = percentual)) + 
  labs(y="Percentual", x="Q23 - Indique os motivos que levaram você a participar do ENEM: \nTestar meus conhecimentos\n 0 - menos relevante / 5 - mais relevante") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q23.png", grid.arrange(plotQ23))

dataFrame %>% count(Q24) %>% mutate(percentual = 100 * n / estudantes) -> Q24
plotQ24 <- ggplot(data = Q24, mapping = aes(x = Q24, y = percentual)) + 
  labs(y="Percentual", x="Q24 - Indique os motivos que levaram você a participar do ENEM: \n Aumentar a possibilidade de conseguir um emprego\n 0 - menos relevante / 5 - mais relevante") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q24.png", grid.arrange(plotQ24))

dataFrame %>% count(Q25) %>% mutate(percentual = 100 * n / estudantes) -> Q25
plotQ25 <- ggplot(data = Q25, mapping = aes(x = Q25, y = percentual)) + 
  labs(y="Percentual", x="Q25 - Indique os motivos que levaram você a participar do ENEM: \n Progredir no meu emprego atual\n 0 - menos relevante / 5 - mais relevante") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q25.png", grid.arrange(plotQ25))

dataFrame %>% count(Q26) %>% mutate(percentual = 100 * n / estudantes) -> Q26
plotQ26 <- ggplot(data = Q26, mapping = aes(x = Q26, y = percentual)) + 
  labs(y="Percentual", x="Q26 - Indique os motivos que levaram você a participar do ENEM: \n Ingressar na Educação Superior Pública\n 0 - menos relevante / 5 - mais relevante") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q26.png", grid.arrange(plotQ26))

dataFrame %>% count(Q27) %>% mutate(percentual = 100 * n / estudantes) -> Q27
plotQ27 <- ggplot(data = Q27, mapping = aes(x = Q27, y = percentual)) + 
  labs(y="Percentual", x="Q27 - Indique os motivos que levaram você a participar do ENEM: \n Ingressar na Educação Superior Privada\n 0 - menos relevante / 5 - mais relevante") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q27.png", grid.arrange(plotQ27))

dataFrame %>% count(Q28) %>% mutate(percentual = 100 * n / estudantes) -> Q28
plotQ28 <- ggplot(data = Q28, mapping = aes(x = Q28, y = percentual)) + 
  labs(y="Percentual", x="Q28 - Indique os motivos que levaram você a participar do ENEM: \n Conseguir uma bolsa de estudos (ProUni, outras)\n 0 - menos relevante / 5 - mais relevante") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q28.png", grid.arrange(plotQ28))

dataFrame %>% count(Q29) %>% mutate(percentual = 100 * n / estudantes) -> Q29
plotQ29 <- ggplot(data = Q29, mapping = aes(x = Q29, y = percentual)) + 
  labs(y="Percentual", x="Q29 - Indique os motivos que levaram você a participar do ENEM: \n Participar do Programa de Financiamento Estudantil FIES\n 0 - menos relevante / 5 - mais relevante") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q29.png", grid.arrange(plotQ29))

dataFrame %>% count(Q30) %>% mutate(percentual = 100 * n / estudantes) -> Q30
plotQ30 <- ggplot(data = Q30, mapping = aes(x = Q30, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D","E","F","G","H"),
                   labels=c("A - Menos de 8 anos", 
                            "B - 8 anos", 
                            "C - 9 anos", 
                            "D - 10 anos", 
                            "E - 11 anos", 
                            "F - Mais de 11 anos", 
                            "G - Não conclui", 
                            "H - Não cursei \n(Passe para a pergunta 33)")) +
  labs(y="Percentual", x="Q30 - Quantos anos você levou para concluir o Ensino Fundamental?") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q30.png", grid.arrange(plotQ30))

dataFrame %>% count(Q31) %>% mutate(percentual = 100 * n / estudantes) -> Q31
plotQ31 <- ggplot(data = Q31, mapping = aes(x = Q31, y = percentual)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D","E"),
                   labels=c("A - Não", 
                            "B - Sim, por um ano", 
                            "C - Sim, por dois anos", 
                            "D - Sim, por três anos", 
                            "E - Sim, por quatro anos ou mais")) +
  labs(y="Percentual", x="Q31 - Você deixou de estudar durante o Ensino Fundamental?") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q31.png", grid.arrange(plotQ31))

dataFrame %>% count(Q32) %>% mutate(percentual = 100 * n / estudantes) -> Q32
plotQ32 <- ggplot(data = Q32, mapping = aes(x = Q32, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D","E","F","G","H"),
                   labels=c("A - Somente em escola pública",
                            "B - Maior parte em escola pública",
                            "C - Somente em escola particular",
                            "D - Maior parte em escola particular",
                            "E - Somente em escola indígena",
                            "F - Maior parte em escola indígena",
                            "G - Somente em escola situada \n em comunidade quilombola",
                            "H - Maior parte em escola \n situada em comunidade quilombola")) +
  labs(y="Percentual", x="Q32 - Em que tipo de escola você cursou o Ensino Fundamental?") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q32.png", grid.arrange(plotQ32))


dataFrame %>% count(Q33) %>% mutate(percentual = 100 * n / estudantes) -> Q33
plotQ33<-ggplot(data = Q33, mapping = aes(x = Q33, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D","E","F","G"),
                   labels=c("A - Menos de 3 anos",
                            "B - 3 anos",
                            "C - 4 anos",
                            "D - 5 anos",
                            "E - 6 anos ou mais",
                            "F - Não conclui.",
                            "G - Não cursei. \n(Passe para a pergunta 36)")) +
  labs(y="Percentual", x="Q33 - Quantos anos você levou para concluir o Ensino Médio?") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q33.png", grid.arrange(plotQ33))


dataFrame %>% count(Q34) %>% mutate(percentual = 100 * n / estudantes) -> Q34
plotQ34 <- ggplot(data = Q34, mapping = aes(x = Q34, y = percentual)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D","E"),
                   labels=c("A - Não",
                            "B - Sim, por um ano",
                            "C - Sim, por dois anos",
                            "D - Sim, por três anos",
                            "E - Sim, por quatro \n anos ou mais")) +
  labs(y="Percentual", x="Q34 - Você deixou de estudar durante o Ensino Médio?") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q34.png", grid.arrange(plotQ34))


dataFrame %>% count(Q35) %>% mutate(percentual = 100 * n / estudantes) -> Q35
plotQ35 <- ggplot(data = Q35, mapping = aes(x = Q35, y = percentual)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D","E","F","G","H"),
                   labels=c("A - Somente em escola pública",
                            "B - Maior parte em escola pública",
                            "C - Somente em escola particular",
                            "D - Maior parte em escola particular",
                            "E - Somente em escola indígena",
                            "F - Maior parte em escola indígena",
                            "G - Somente em escola situada \n em comunidade quilombola",
                            "H - Maior parte em escola \n situada em comunidade quilombola")) +
  labs(y="Percentual", x="Q35 - Em que tipo de escola você cursou o Ensino Médio?") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q35.png", grid.arrange(plotQ35))


dataFrame %>% count(Q36) %>% mutate(percentual = 100 * n / estudantes) -> Q36
plotQ36 <- ggplot(data = Q36, mapping = aes(x = Q36, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B"),
                   labels=c("A - Sim",
                            "B - Não")) +
  labs(y="Percentual", x="Q36 - Caso você ingresse no Ensino Superior privado pretende recorrer aos \nauxílios abaixo para custeio das mensalidades? \n Pró-Uni") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q36.png", grid.arrange(plotQ36))

dataFrame %>% count(Q37) %>% mutate(percentual = 100 * n / estudantes) -> Q37
plotQ37 <- ggplot(data = Q37, mapping = aes(x = Q37, y = percentual)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B"),
                   labels=c("A - Sim",
                            "B - Não")) +
  labs(y="Percentual", x="Q37 - Caso você ingresse no Ensino Superior privado pretende recorrer aos \nauxílios abaixo para custeio das mensalidades? \n Bolsa de estudos da própria Instituição de Ensino Superior") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q37.png", grid.arrange(plotQ37))


dataFrame %>% count(Q38) %>% mutate(percentual = 100 * n / estudantes) -> Q38
plotQ38 <- ggplot(data = Q38, mapping = aes(x = Q38, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B"),
                   labels=c("A - Sim",
                            "B - Não")) +
  labs(y="Percentual", x="Q38 - Caso você ingresse no Ensino Superior privado pretende recorrer aos \nauxílios abaixo para custeio das mensalidades? \n Bolsa de estudos da empresa onde trabalho") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q38.png", grid.arrange(plotQ38))

dataFrame %>% count(Q39) %>% mutate(percentual = 100 * n / estudantes) -> Q39
plotQ39 <- ggplot(data = Q39, mapping = aes(x = Q39, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B"),
                   labels=c("A - Sim",
                            "B - Não")) +
  labs(y="Percentual", x="Q39 - Caso você ingresse no Ensino Superior privado pretende recorrer aos \nauxílios abaixo para custeio das mensalidades? \n Auxílio do Programa de Financiamento Estudantil - FIES ") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q39.png", grid.arrange(plotQ39))

# ANALISAR
dataFrame %>% count(Q40) %>% mutate(percentual = 100 * n / estudantes) -> Q40
plotQ40 <- ggplot(data = Q40, mapping = aes(x = Q40, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q40 - Você cursa ou já cursou a Educação de Jovens e Adultos - EJA") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q40.png", grid.arrange(plotQ40))


dataFrame %>% count(Q41) %>% mutate(percentual = 100 * n / estudantes) -> Q41
plotQ41 <- ggplot(data = Q41, mapping = aes(x = Q41, y = percentual)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D","E","F","Nao Informado"),
                   labels=c("A - Curso presencial em \n escola pública",
                            "B - Curso presencial em escola privada",
                            "C - Curso presencial na empresa em que trabalhei, \ninstituição filantrópica ou religiosa",
                            "D - Curso a distância (via rádio, televisão, internet, \ncorreio, com apostilas)",
                            "E - Curso semi-presencial em escola pública",
                            "F -  Curso semi-presencial em escola privada",
                            "Não Informado")) +
  labs(y="Percentual", x="Q41 - Como é ou era o principal curso de EJA que você frequenta ou frequentou?") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q41.png", grid.arrange(plotQ41))


dataFrame %>% count(Q42) %>% mutate(percentual = 100 * n / estudantes) -> Q42
plotQ42 <- ggplot(data = Q42, mapping = aes(x = Q42, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q42 - Indique o que levou você a deixar de cursar a EJA \n (Se você não deixou de cursar a EJA, vá para a questão...): \nTrabalhava, não tinha tempo de estudar.") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q42.png", grid.arrange(plotQ42))


dataFrame %>% count(Q43) %>% mutate(percentual = 100 * n / estudantes) -> Q43
plotQ43 <- ggplot(data = Q43, mapping = aes(x = Q43, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q43 - Indique o que levou você a deixar de cursar a EJA \n (Se você não deixou de cursar a EJA, vá para a questão...): \nEstudava no curso da empresa e foi interrompido.") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q43.png", grid.arrange(plotQ43))

dataFrame %>% count(Q44) %>% mutate(percentual = 100 * n / estudantes) -> Q44
plotQ44 <- ggplot(data = Q44, mapping = aes(x = Q44, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q44 - Indique o que levou você a deixar de cursar a EJA \n (Se você não deixou de cursar a EJA, vá para a questão...): \nOcorreram problemas de saúde ou acidentes comigo familiares") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q44.png", grid.arrange(plotQ44))

dataFrame %>% count(Q45) %>% mutate(percentual = 100 * n / estudantes) -> Q45
plotQ45 <- ggplot(data = Q45, mapping = aes(x = Q45, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q45 - Indique o que levou você a deixar de cursar a EJA \n (Se você não deixou de cursar a EJA, vá para a questão...): \nMudei de bairro, cidade ou município") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q45.png", grid.arrange(plotQ45))


dataFrame %>% count(Q46) %>% mutate(percentual = 100 * n / estudantes) -> Q46
plotQ46 <- ggplot(data = Q46, mapping = aes(x = Q46, y = percentual)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q46 - Indique o que levou você a deixar de cursar a EJA \n (Se você não deixou de cursar a EJA, vá para a questão...): \nPor motivos pessoais, casamento, filhos, etc") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q46.png", grid.arrange(plotQ46))


dataFrame %>% count(Q47) %>% mutate(percentual = 100 * n / estudantes) -> Q47
plotQ47 <- ggplot(data = Q47, mapping = aes(x = Q47, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q47 - Indique o que levou você a deixar de cursar a EJA \n (Se você não deixou de cursar a EJA, vá para a questão...): \nFaltava-me interesse, desisti") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q47.png", grid.arrange(plotQ47))


dataFrame %>% count(Q48) %>% mutate(percentual = 100 * n / estudantes) -> Q48
plotQ48 <- ggplot(data = Q48, mapping = aes(x = Q48, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q48 - Indique o que levou você a deixar de cursar a EJA \n (Se você não deixou de cursar a EJA, vá para a questão...): \nSenti-me discriminado(a)") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q48.png", grid.arrange(plotQ48))


dataFrame %>% count(Q49) %>% mutate(percentual = 100 * n / estudantes) -> Q49
plotQ49 <- ggplot(data = Q49, mapping = aes(x = Q49, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q49 - Indique o que levou você a deixar de cursar a EJA \n (Se você não deixou de cursar a EJA, vá para a questão...): \nTemi/Sofri violência") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q49.png", grid.arrange(plotQ49))

dataFrame %>% count(Q50) %>% mutate(percentual = 100 * n / estudantes) -> Q50
plotQ50 <- ggplot(data = Q50, mapping = aes(x = Q50, y = percentual)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q50 - Você já frequentou o ensino regular?") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q50.png", grid.arrange(plotQ50))


dataFrame %>% count(Q51) %>% mutate(percentual = 100 * n / estudantes) -> Q51
plotQ51 <- ggplot(data = Q51, mapping = aes(x = Q51, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q51 - Indique o que levou você a deixar de cursar o ensino regular: \nFalta de vaga em escola pública") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q51.png", grid.arrange(plotQ51))


dataFrame %>% count(Q52) %>% mutate(percentual = 100 * n / estudantes) -> Q52
plotQ52 <- ggplot(data = Q52, mapping = aes(x = Q52, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q52 - Indique o que levou você a deixar de cursar o ensino regular: \nAusência de escola perto de casa") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q52.png", grid.arrange(plotQ52))


dataFrame %>% count(Q53) %>% mutate(percentual = 100 * n / estudantes) -> Q53
plotQ53 <- ggplot(data = Q53, mapping = aes(x = Q53, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q53 - Indique o que levou você a deixar de cursar o ensino regular: \nDificuldades após reprovação") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q53.png", grid.arrange(plotQ53))

dataFrame %>% count(Q54) %>% mutate(percentual = 100 * n / estudantes) -> Q54
plotQ54 <- ggplot(data = Q54, mapping = aes(x = Q54, y = percentual)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q54 - Indique o que levou você a deixar de cursar o ensino regular: \nFalta de interesse em estudar") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q54.png", grid.arrange(plotQ54))


dataFrame %>% count(Q55) %>% mutate(percentual = 100 * n / estudantes) -> Q55
plotQ55 <- ggplot(data = Q55, mapping = aes(x = Q55, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q55 - Indique o que levou você a deixar de cursar o ensino regular: \nFalta de condições adequadas na escola") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q55.png", grid.arrange(plotQ55))

dataFrame %>% count(Q56) %>% mutate(percentual = 100 * n / estudantes) -> Q56
plotQ56 <- ggplot(data = Q56, mapping = aes(x = Q56, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q56 - Indique o que levou você a deixar de cursar o ensino regular: \nTrabalho, falta de tempo para estudar") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q56.png", grid.arrange(plotQ56))


dataFrame %>% count(Q57) %>% mutate(percentual = 100 * n / estudantes) -> Q57
plotQ57 <- ggplot(data = Q57, mapping = aes(x = Q57, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q57 - Indique o que levou você a deixar de cursar o ensino regular: \nMotivos pessoais, casamento / filhos, etc") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q57.png", grid.arrange(plotQ57))


dataFrame %>% count(Q58) %>% mutate(percentual = 100 * n / estudantes) -> Q58
plotQ58 <- ggplot(data = Q58, mapping = aes(x = Q58, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q58 - Indique o que levou você a deixar de cursar o ensino regular: \nFalta de apoio familiar") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q58.png", grid.arrange(plotQ58))

dataFrame %>% count(Q59) %>% mutate(percentual = 100 * n / estudantes) -> Q59
plotQ59 <- ggplot(data = Q59, mapping = aes(x = Q59, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q59 - Indique o que levou você a deixar de cursar o ensino regular: \nProblemas de saúde ou acidente comigo ou familiares") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q59.png", grid.arrange(plotQ59))


dataFrame %>% count(Q60) %>% mutate(percentual = 100 * n / estudantes) -> Q60
plotQ60 <- ggplot(data = Q60, mapping = aes(x = Q60, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q60 - Indique o que levou você a deixar de cursar o ensino regular: \nDiscriminação / Preconceitos (sexo, raça, idade, classe etc.)") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q60.png", grid.arrange(plotQ60))


dataFrame %>% count(Q61) %>% mutate(percentual = 100 * n / estudantes) -> Q61
plotQ61 <- ggplot(data = Q61, mapping = aes(x = Q61, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","Nao Informado"),
                   labels=c("A - Sim",
                            "B - Não",
                            "Não Informado")) +
  labs(y="Percentual", x="Q61 - Indique o que levou você a deixar de cursar o ensino regular: \nMedo de sofrer violência") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q61.png", grid.arrange(plotQ61))


dataFrame %>% count(Q62) %>% mutate(percentual = 100 * n / estudantes) -> Q62
plotQ62 <- ggplot(data = Q62, mapping = aes(x = Q62, y = percentual)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks=c("A","B","C","D","E","F","G","Nao Informado"),
                   labels=c("A - Menos de 10 anos",
                            "B - Entre 10 e 14 anos",
                            "C - Entre 15 e 18 anos",
                            "D - Entre 19 e 24 anos",
                            "E - Entre 25 e 30 anos",
                            "F - Mais de 30 anos",
                            "G - Não deixei de frequentar",
                            "Não Informado")) +
  labs(y="Percentual", x="Q62 - Quantos anos você tinha quando deixou de frequentar o ensino regular?") +
  geom_bar(stat = 'identity')
ggsave("./analise-descritiva/q62.png", grid.arrange(plotQ62))


# análises bivariadas - cruzamentos entre variáveis socioeconômicas e o desempenho nas notas
# a metodologia adotada foi o boxplot, para ver a distribuição das notas conforme as classes
# também foi aplicado o teste qui-quadrado para verificação da hipótese de independência

ggplot(data = dataFrame, mapping = aes(x = TP_SEXO, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = TP_SEXO, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = TP_SEXO, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = TP_SEXO, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = TP_SEXO, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = ST_CONCLUSAO, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = ST_CONCLUSAO, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = ST_CONCLUSAO, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = ST_CONCLUSAO, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = ST_CONCLUSAO, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = AVG_NOTA, fill = TP_ESCOLA)) + 
  geom_histogram() +
  scale_fill_discrete(name = 'Escola', labels = c('Não Informado', 'Particular', 'Pública'))
summarize(group_by(dataFrame, TP_ESCOLA), means = mean(AVG_NOTA), skew = skewness(AVG_NOTA), kurt = kurtosis(AVG_NOTA))

ggplot(data = dataFrame, mapping = aes(x = AVG_NOTA, fill = TP_ESCOLA)) + 
  geom_histogram() +
  scale_fill_discrete(name = 'Escola', labels = c('Não Informado', 'Particular', 'Pública'))
summarize(dataFrame, means = mean(AVG_NOTA), skew = skewness(AVG_NOTA), kurt = kurtosis(AVG_NOTA), sd=sd(AVG_NOTA))

ggplot(data = dataFrame, mapping = aes(x = IDADE, fill = IDADE)) + 
  geom_histogram()
summarize(dataFrame, means = mean(IDADE), skew = skewness(IDADE), kurt = kurtosis(IDADE), 
          sd=sd(IDADE),
          lowerOutliers = quantile(dataFrame$IDADE, 0.25) - 1.5*IQR(dataFrame$IDADE),
          upperOutliers = quantile(dataFrame$IDADE, 0.75) + 1.5*IQR(dataFrame$IDADE),
          max(IDADE))
plotIdade <- ggplot(data = dataFrame, mapping = aes(x = " ", y = IDADE)) + geom_boxplot()

hist_boxplot(dataFrame$AVG_NOTA,col="lightgrey",freq=TRUE,xlab="AVG_NOTA",main="")

lowerOutliers <- quantile(dataFrame$AVG_NOTA, 0.25) - 1.5*IQR(dataFrame$AVG_NOTA)
upperOutliers <- quantile(dataFrame$AVG_NOTA, 0.75) + 1.5*IQR(dataFrame$AVG_NOTA)
(nrow(select(filter(dataFrame, AVG_NOTA < lowerOutliers |  AVG_NOTA > upperOutliers), colnames(data)))/nrow(dataFrame))*100

# nrow(dataFrame)

####
my_variable=c(rnorm(1000 , 0 , 2) , rnorm(1000 , 9 , 2))

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(dataFrame , horizontal=TRUE , ylim=c(-10,20), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="value of the variable", xlim=c(-10,20))

####

7.31
ggsave("./analise-descritiva/boxplot-idade.png", grid.arrange(plotIdade))

ggplot(data = dataFrame, mapping = aes(x = IN_TP_ENSINO, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = IN_TP_ENSINO, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = IN_TP_ENSINO, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = IN_TP_ENSINO, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = IN_TP_ENSINO, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = TP_ESTADO_CIVIL, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = TP_ESTADO_CIVIL, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = TP_ESTADO_CIVIL, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = TP_ESTADO_CIVIL, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = TP_ESTADO_CIVIL, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = TP_COR_RACA, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = TP_COR_RACA, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = TP_COR_RACA, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = TP_COR_RACA, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = TP_COR_RACA, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = ID_DEPENDENCIA_ADM, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = ID_DEPENDENCIA_ADM, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = ID_DEPENDENCIA_ADM, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = ID_DEPENDENCIA_ADM, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = ID_DEPENDENCIA_ADM, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = ID_LOCALIZACAO, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = ID_LOCALIZACAO, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = ID_LOCALIZACAO, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = ID_LOCALIZACAO, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = ID_LOCALIZACAO, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = IN_STATUS_REDACAO, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = IN_STATUS_REDACAO, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = IN_STATUS_REDACAO, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = IN_STATUS_REDACAO, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = IN_STATUS_REDACAO, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = AVG_NOTA, fill = Q01)) + 
  geom_histogram() +
  scale_fill_discrete(name = 'Até quando seu pai estudou?', 
                      labels = c('A - Não Estudou', 
                                 'B - Ens Fundamental I', 
                                 'C - Ens Fundamental II', 
                                 'D - Ens Médio Incompleto', 
                                 'E - Ens Médio', 
                                 'F - Ens Superior Incompleto', 
                                 'G - Ens Superior', 
                                 'H - Pós-Graduação', 
                                 'I - Não Sei'))
summarize(group_by(dataFrame, Q01), means = mean(AVG_NOTA), skew = skewness(AVG_NOTA), kurt = kurtosis(AVG_NOTA))

ggplot(data = dataFrame, mapping = aes(x = AVG_NOTA, fill = Q02)) + 
  geom_histogram() +
  scale_fill_discrete(name = 'Até quando sua mãe estudou?', 
                      labels = c('A - Não Estudou', 
                                 'B - Ens Fundamental I', 
                                 'C - Ens Fundamental II', 
                                 'D - Ens Médio Incompleto', 
                                 'E - Ens Médio', 
                                 'F - Ens Superior Incompleto', 
                                 'G - Ens Superior', 
                                 'H - Pós-Graduação', 
                                 'I - Não Sei'))
summarize(group_by(dataFrame, Q02), means = mean(AVG_NOTA), skew = skewness(AVG_NOTA), kurt = kurtosis(AVG_NOTA))

ggplot(data = dataFrame, mapping = aes(x = AVG_NOTA, fill = Q03)) + 
  geom_histogram() +
  scale_fill_discrete(name = 'Qual a sua renda familiar?', 
                      labels = c('A - Nenhuma', 
                                 'B - 1 Salário', 
                                 'C - 1 a 1,5 Salários', 
                                 'D - 1,5 a 2 Salários', 
                                 'E - 2 a 2,5 Salários', 
                                 'F - 2,5 a 3 Salários', 
                                 'G - 3 a 4 Salários', 
                                 'H - 4 a 5 Salários',
                                 'I - 5 a 6 Salários',
                                 'J - 6 a 7 Salários',
                                 'K - 7 a 8 Salários',
                                 'L - 9 a 9 Salários',
                                 'M - 9 a 10 Salários',
                                 'N - 10 a 12 Salários',
                                 'O - 12 a 15 Salários',
                                 'P - 15 a 20 Salários',
                                 'Q - Mais de 20 Salários'))
summarize(group_by(dataFrame, Q03), means = mean(AVG_NOTA), skew = skewness(AVG_NOTA), kurt = kurtosis(AVG_NOTA))

ggplot(data = dataFrame, mapping = aes(x = Q04, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q04, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q04, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q04, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q04, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q05, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q05, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q05, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q05, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q05, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q06, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q06, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q06, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q06, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q06, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q07, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q07, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q07, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q07, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q07, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q08, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q08, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q08, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q08, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q08, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q09, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q09, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q09, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q09, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q09, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q10, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q10, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q10, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q10, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q10, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q11, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q11, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q11, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q11, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q11, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q12, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q12, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q12, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q12, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q12, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q13, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q13, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q13, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q13, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q13, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q14, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q14, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q14, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q14, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q14, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q15, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q15, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q15, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q15, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q15, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q16, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q16, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q16, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q16, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q16, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q17, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q17, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q17, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q17, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q17, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q18, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q18, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q18, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q18, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q18, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q19, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q19, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q19, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q19, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q19, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q20, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q20, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q20, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q20, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q20, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q21, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q21, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q21, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q21, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q21, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = AVG_NOTA, fill = Q22)) + 
  geom_histogram() +
  scale_fill_discrete(name = 'Trabalha ou já trabalhou?', 
                      labels = c('A - Sim', 
                                 'B - Já Trabalhei', 
                                 'C - Nunca'))
summarize(group_by(dataFrame, Q22), means = mean(AVG_NOTA), skew = skewness(AVG_NOTA), kurt = kurtosis(AVG_NOTA))

ggplot(data = dataFrame, mapping = aes(x = Q23, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q23, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q23, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q23, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q23, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q24, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q24, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q24, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q24, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q24, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q25, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q25, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q25, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q25, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q25, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q26, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q26, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q26, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q26, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q26, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q27, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q27, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q27, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q27, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q27, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q28, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q28, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q28, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q28, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q28, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q29, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q29, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q29, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q29, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q29, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = AVG_NOTA, fill = Q30)) + 
  geom_histogram() +
  scale_fill_discrete(name = 'Conclusão Ens Fundamental', 
                      labels = c('A - < 8 anos', 
                                 'B - 8 anos', 
                                 'C - 9 anos', 
                                 'D - 10 anos', 
                                 'E - 11 anos', 
                                 'F - > 11 anos', 
                                 'G - Não Concluiu', 
                                 'H - Não Cursou'))
summarize(group_by(dataFrame, Q30), means = mean(AVG_NOTA), skew = skewness(AVG_NOTA), kurt = kurtosis(AVG_NOTA))

ggplot(data = dataFrame, mapping = aes(x = AVG_NOTA, fill = Q31)) + 
  geom_histogram() +
  scale_fill_discrete(name = 'Abandono Ens Fundamental', 
                      labels = c('A - Não', 
                                 'B - 1 ano', 
                                 'C - 2 anos', 
                                 'D - 3 anos', 
                                 'E - > 4 anos'))
summarize(group_by(dataFrame, Q31), means = mean(AVG_NOTA), skew = skewness(AVG_NOTA), kurt = kurtosis(AVG_NOTA))

ggplot(data = dataFrame, mapping = aes(x = AVG_NOTA, fill = Q32)) + 
  geom_histogram() +
  scale_fill_discrete(name = 'Escola Ens Fundamental', 
                      labels = c('A - Pública', 
                                 'B - Maior parte pública', 
                                 'C - Particular', 
                                 'D - Maior parte particular', 
                                 'E - Indígena', 
                                 'F - Maior parte indígena', 
                                 'G - Quilombola', 
                                 'H - Maior parte quilombola'))
summarize(group_by(dataFrame, Q32), means = mean(AVG_NOTA), skew = skewness(AVG_NOTA), kurt = kurtosis(AVG_NOTA))

ggplot(data = dataFrame, mapping = aes(x = AVG_NOTA, fill = Q33)) + 
  geom_histogram() +
  scale_fill_discrete(name = 'Conclusão Ens Médio', 
                      labels = c('A - < 3 anos', 
                                 'B - 3 anos', 
                                 'C - 4 anos', 
                                 'D - 5 anos', 
                                 'E - > 6 anos', 
                                 'F - Não Concluiu', 
                                 'G - Não Cursou'))
summarize(group_by(dataFrame, Q33), means = mean(AVG_NOTA), skew = skewness(AVG_NOTA), kurt = kurtosis(AVG_NOTA))

ggplot(data = dataFrame, mapping = aes(x = AVG_NOTA, fill = Q34)) + 
  geom_histogram() +
  scale_fill_discrete(name = 'Abandono Ens Médio', 
                      labels = c('A - Não', 
                                 'B - 1 ano', 
                                 'C - 2 anos', 
                                 'D - 3 anos', 
                                 'E - > 4 anos'))
summarize(group_by(dataFrame, Q34), means = mean(AVG_NOTA), skew = skewness(AVG_NOTA), kurt = kurtosis(AVG_NOTA))

ggplot(data = dataFrame, mapping = aes(x = AVG_NOTA, fill = Q35)) + 
  geom_histogram() +
  scale_fill_discrete(name = 'Escola Ens Médio', 
                      labels = c('A - Pública', 
                                 'B - Maior parte pública', 
                                 'C - Particular', 
                                 'D - Maior parte particular', 
                                 'E - Indígena', 
                                 'F - Maior parte indígena', 
                                 'G - Quilombola', 
                                 'H - Maior parte quilombola'))
summarize(group_by(dataFrame, Q35), means = mean(AVG_NOTA), skew = skewness(AVG_NOTA), kurt = kurtosis(AVG_NOTA))

ggplot(data = dataFrame, mapping = aes(x = Q36, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q36, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q36, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q36, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q36, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q37, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q37, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q37, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q37, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q37, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q38, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q38, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q38, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q38, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q38, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q39, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q39, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q39, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q39, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q39, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q40, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q40, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q40, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q40, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q40, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q41, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q41, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q41, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q41, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q41, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q42, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q42, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q42, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q42, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q42, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q43, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q43, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q43, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q43, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q43, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q44, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q44, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q44, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q44, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q44, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q45, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q45, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q45, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q45, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q45, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q46, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q46, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q46, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q46, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q46, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q47, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q47, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q47, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q47, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q47, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q48, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q48, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q48, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q48, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q48, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q49, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q49, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q49, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q49, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q49, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q50, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q50, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q50, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q50, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q50, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q51, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q51, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q51, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q51, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q51, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q52, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q52, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q52, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q52, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q52, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q53, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q53, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q53, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q53, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q53, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q54, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q54, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q54, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q54, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q54, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q55, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q55, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q55, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q55, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q55, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q56, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q56, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q56, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q56, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q56, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q57, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q57, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q57, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q57, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q57, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q58, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q58, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q58, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q58, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q58, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q59, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q59, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q59, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q59, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q59, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q60, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q60, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q60, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q60, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q60, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q61, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q61, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q61, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q61, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q61, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = dataFrame, mapping = aes(x = Q62, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q62, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q62, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q62, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = dataFrame, mapping = aes(x = Q62, y = NU_NOTA_REDACAO)) + geom_boxplot()

p <- rbind(ggplot(data = spestudantes, mapping = aes(x = 'SP', y = spestudantes$AVG_NOTA)), 
           ggplot(data = rjestudantes, mapping = aes(x = 'RJ', y = spestudantes$AVG_NOTA)))
ggplot(p, aes(x =c('SP','RJ'), y = AVG_NOTA)) + geom_boxplot()

data2 <- rbind(
  amestudantes %>% mutate(UF = 'AM'), 
  spestudantes %>% mutate(UF = 'SP'), 
  rjestudantes %>% mutate(UF = 'RJ'),
  rjestudantes %>% mutate(UF = 'RJ'),
)
ggplot(data2, aes(x = UF , y = AVG_NOTA)) +
  geom_boxplot ()

hist(spestudantes$AVG_NOTA)
hist(amestudantes$AVG_NOTA)
