# para executar este script deve-se 
# executar primeiro o script 
# importa-filtra-basededados.R

library(tidyverse)

# ADD da base de estudantes que fizeram o ENEM em 2012,
# residentes em São Paulo, SP

estudantes <- nrow(spCandidatos)

# análises univariadas - atributos categóricos - distribuições de frequência

# o atributo color de um plot exige que seja um <fct>

ggplot(data = spCandidatos, mapping = aes(x = IDADE)) + geom_bar()
# ggsave('../output/IDADE.png')

spCandidatos %>% count(TP_SEXO) %>% mutate(percentual = 100 * n / estudantes) -> genders
ggplot(data = genders, mapping = aes(x = TP_SEXO, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(ST_CONCLUSAO) %>% mutate(percentual = 100 * n / estudantes) -> conclusion
ggplot(data = conclusion, mapping = aes(x = ST_CONCLUSAO, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(TP_ESCOLA) %>% mutate(percentual = 100 * n / estudantes) -> schools
ggplot(data = schools, mapping = aes(x = TP_ESCOLA, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(IN_TP_ENSINO) %>% mutate(percentual = 100 * n / estudantes) -> teach
ggplot(data = teach, mapping = aes(x = IN_TP_ENSINO, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(TP_COR_RACA) %>% mutate(percentual = 100 * n / estudantes) -> ethnics
ggplot(data = ethnics, mapping = aes(x = TP_COR_RACA, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(ID_DEPENDENCIA_ADM) %>% mutate(percentual = 100 * n / estudantes) -> dependency
ggplot(data = dependency, mapping = aes(x = ID_DEPENDENCIA_ADM, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(ID_LOCALIZACAO) %>% mutate(percentual = 100 * n / estudantes) -> location
ggplot(data = location, mapping = aes(x = ID_LOCALIZACAO, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(NO_MUNICIPIO_PROVA) %>% mutate(percentual = 100 * n / estudantes) -> mun
ggplot(data = mun, mapping = aes(x = NO_MUNICIPIO_PROVA, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(IN_PRESENCA_CN) %>% mutate(percentual = 100 * n / estudantes) -> cn
ggplot(data = cn, mapping = aes(x = IN_PRESENCA_CN, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(IN_PRESENCA_CH) %>% mutate(percentual = 100 * n / estudantes) -> ch
ggplot(data = ch, mapping = aes(x = IN_PRESENCA_CH, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(IN_PRESENCA_LC) %>% mutate(percentual = 100 * n / estudantes) -> lc
ggplot(data = lc, mapping = aes(x = IN_PRESENCA_LC, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(IN_PRESENCA_MT) %>% mutate(percentual = 100 * n / estudantes) -> mt
ggplot(data = mt, mapping = aes(x = IN_PRESENCA_MT, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(IN_STATUS_REDACAO) %>% mutate(percentual = 100 * n / estudantes) -> red
ggplot(data = red, mapping = aes(x = IN_STATUS_REDACAO, y = percentual)) + geom_bar(stat = 'identity')

# análises univariadas - atributos contínuos - histogramas
ggplot(data = spCandidatos, mapping = aes(x = AVG_NOTA)) + geom_bar(binwidth = 10) + geom_boxplot(width=1000,color="blue")

ggplot(data = spCandidatos, mapping = aes(x = NU_NOTA_COMP1)) + geom_histogram(binwidth = 20)

ggplot(data = spCandidatos, mapping = aes(x = NU_NOTA_COMP2)) + geom_histogram(binwidth = 20)

ggplot(data = spCandidatos, mapping = aes(x = NU_NOTA_COMP3)) + geom_histogram(binwidth = 20)

ggplot(data = spCandidatos, mapping = aes(x = NU_NOTA_COMP4)) + geom_histogram(binwidth = 20)

ggplot(data = spCandidatos, mapping = aes(x = NU_NOTA_COMP5)) + geom_histogram(binwidth = 20)

ggplot(data = spCandidatos, mapping = aes(x = NU_NOTA_REDACAO)) + geom_histogram(binwidth = 20)

# análises univariadas - atributos categóricos - questionário socioeconômico

# vale analisar se as perguntas de 40 em diante, do EJA, possuem algum impacto

spCandidatos %>% count(Q01) %>% mutate(percentual = 100 * n / estudantes) -> Q01
ggplot(data = Q01, mapping = aes(x = Q01, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q02) %>% mutate(percentual = 100 * n / estudantes) -> Q02
ggplot(data = Q02, mapping = aes(x = Q02, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q03) %>% mutate(percentual = 100 * n / estudantes) -> Q03
ggplot(data = Q03, mapping = aes(x = Q03, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q04) %>% mutate(percentual = 100 * n / estudantes) -> Q04
ggplot(data = Q04, mapping = aes(x = as.numeric(Q04), y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q05) %>% mutate(percentual = 100 * n / estudantes) -> Q05
ggplot(data = Q05, mapping = aes(x = Q05, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q06) %>% mutate(percentual = 100 * n / estudantes) -> Q06
ggplot(data = Q06, mapping = aes(x = Q06, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q07) %>% mutate(percentual = 100 * n / estudantes) -> Q07
ggplot(data = Q07, mapping = aes(x = Q07, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q08) %>% mutate(percentual = 100 * n / estudantes) -> Q08
ggplot(data = Q08, mapping = aes(x = Q08, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q09) %>% mutate(percentual = 100 * n / estudantes) -> Q09
ggplot(data = Q09, mapping = aes(x = Q09, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q10) %>% mutate(percentual = 100 * n / estudantes) -> Q10
ggplot(data = Q10, mapping = aes(x = Q10, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q11) %>% mutate(percentual = 100 * n / estudantes) -> Q11
ggplot(data = Q11, mapping = aes(x = Q11, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q12) %>% mutate(percentual = 100 * n / estudantes) -> Q12
ggplot(data = Q12, mapping = aes(x = Q12, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q13) %>% mutate(percentual = 100 * n / estudantes) -> Q13
ggplot(data = Q13, mapping = aes(x = Q13, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q14) %>% mutate(percentual = 100 * n / estudantes) -> Q14
ggplot(data = Q14, mapping = aes(x = Q14, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q15) %>% mutate(percentual = 100 * n / estudantes) -> Q15
ggplot(data = Q15, mapping = aes(x = Q15, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q16) %>% mutate(percentual = 100 * n / estudantes) -> Q16
ggplot(data = Q16, mapping = aes(x = Q16, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q17) %>% mutate(percentual = 100 * n / estudantes) -> Q17
ggplot(data = Q17, mapping = aes(x = Q17, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q18) %>% mutate(percentual = 100 * n / estudantes) -> Q18
ggplot(data = Q18, mapping = aes(x = Q18, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q19) %>% mutate(percentual = 100 * n / estudantes) -> Q19
ggplot(data = Q19, mapping = aes(x = Q19, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q20) %>% mutate(percentual = 100 * n / estudantes) -> Q20
ggplot(data = Q20, mapping = aes(x = Q20, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q21) %>% mutate(percentual = 100 * n / estudantes) -> Q21
ggplot(data = Q21, mapping = aes(x = Q21, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q22) %>% mutate(percentual = 100 * n / estudantes) -> Q22
ggplot(data = Q22, mapping = aes(x = Q22, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q23) %>% mutate(percentual = 100 * n / estudantes) -> Q23
ggplot(data = Q23, mapping = aes(x = Q23, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q24) %>% mutate(percentual = 100 * n / estudantes) -> Q24
ggplot(data = Q24, mapping = aes(x = Q24, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q25) %>% mutate(percentual = 100 * n / estudantes) -> Q25
ggplot(data = Q25, mapping = aes(x = Q25, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q26) %>% mutate(percentual = 100 * n / estudantes) -> Q26
ggplot(data = Q26, mapping = aes(x = Q26, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q27) %>% mutate(percentual = 100 * n / estudantes) -> Q27
ggplot(data = Q27, mapping = aes(x = Q27, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q28) %>% mutate(percentual = 100 * n / estudantes) -> Q28
ggplot(data = Q28, mapping = aes(x = Q28, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q29) %>% mutate(percentual = 100 * n / estudantes) -> Q29
ggplot(data = Q29, mapping = aes(x = Q29, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q30) %>% mutate(percentual = 100 * n / estudantes) -> Q30
ggplot(data = Q30, mapping = aes(x = Q30, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q31) %>% mutate(percentual = 100 * n / estudantes) -> Q31
ggplot(data = Q31, mapping = aes(x = Q31, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q32) %>% mutate(percentual = 100 * n / estudantes) -> Q32
ggplot(data = Q32, mapping = aes(x = Q32, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q33) %>% mutate(percentual = 100 * n / estudantes) -> Q33
ggplot(data = Q33, mapping = aes(x = Q33, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q34) %>% mutate(percentual = 100 * n / estudantes) -> Q34
ggplot(data = Q34, mapping = aes(x = Q34, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q35) %>% mutate(percentual = 100 * n / estudantes) -> Q35
ggplot(data = Q35, mapping = aes(x = Q35, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q36) %>% mutate(percentual = 100 * n / estudantes) -> Q36
ggplot(data = Q36, mapping = aes(x = Q36, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q37) %>% mutate(percentual = 100 * n / estudantes) -> Q37
ggplot(data = Q37, mapping = aes(x = Q37, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q38) %>% mutate(percentual = 100 * n / estudantes) -> Q38
ggplot(data = Q38, mapping = aes(x = Q38, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q39) %>% mutate(percentual = 100 * n / estudantes) -> Q39
ggplot(data = Q39, mapping = aes(x = Q39, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q40) %>% mutate(percentual = 100 * n / estudantes) -> Q40
ggplot(data = Q40, mapping = aes(x = Q40, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q41) %>% mutate(percentual = 100 * n / estudantes) -> Q41
ggplot(data = Q41, mapping = aes(x = Q41, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q42) %>% mutate(percentual = 100 * n / estudantes) -> Q42
ggplot(data = Q42, mapping = aes(x = Q42, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q43) %>% mutate(percentual = 100 * n / estudantes) -> Q43
ggplot(data = Q43, mapping = aes(x = Q43, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q44) %>% mutate(percentual = 100 * n / estudantes) -> Q44
ggplot(data = Q44, mapping = aes(x = Q44, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q45) %>% mutate(percentual = 100 * n / estudantes) -> Q45
ggplot(data = Q45, mapping = aes(x = Q45, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q46) %>% mutate(percentual = 100 * n / estudantes) -> Q46
ggplot(data = Q46, mapping = aes(x = Q46, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q47) %>% mutate(percentual = 100 * n / estudantes) -> Q47
ggplot(data = Q47, mapping = aes(x = Q47, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q48) %>% mutate(percentual = 100 * n / estudantes) -> Q48
ggplot(data = Q48, mapping = aes(x = Q48, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q49) %>% mutate(percentual = 100 * n / estudantes) -> Q49
ggplot(data = Q49, mapping = aes(x = Q49, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q50) %>% mutate(percentual = 100 * n / estudantes) -> Q50
ggplot(data = Q50, mapping = aes(x = Q50, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q51) %>% mutate(percentual = 100 * n / estudantes) -> Q51
ggplot(data = Q51, mapping = aes(x = Q51, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q52) %>% mutate(percentual = 100 * n / estudantes) -> Q52
ggplot(data = Q52, mapping = aes(x = Q52, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q53) %>% mutate(percentual = 100 * n / estudantes) -> Q53
ggplot(data = Q53, mapping = aes(x = Q53, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q54) %>% mutate(percentual = 100 * n / estudantes) -> Q54
ggplot(data = Q54, mapping = aes(x = Q54, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q55) %>% mutate(percentual = 100 * n / estudantes) -> Q55
ggplot(data = Q55, mapping = aes(x = Q55, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q56) %>% mutate(percentual = 100 * n / estudantes) -> Q56
ggplot(data = Q56, mapping = aes(x = Q56, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q57) %>% mutate(percentual = 100 * n / estudantes) -> Q57
ggplot(data = Q57, mapping = aes(x = Q57, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q58) %>% mutate(percentual = 100 * n / estudantes) -> Q58
ggplot(data = Q58, mapping = aes(x = Q58, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q59) %>% mutate(percentual = 100 * n / estudantes) -> Q59
ggplot(data = Q59, mapping = aes(x = Q59, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q60) %>% mutate(percentual = 100 * n / estudantes) -> Q60
ggplot(data = Q60, mapping = aes(x = Q60, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q61) %>% mutate(percentual = 100 * n / estudantes) -> Q61
ggplot(data = Q61, mapping = aes(x = Q61, y = percentual)) + geom_bar(stat = 'identity')

spCandidatos %>% count(Q62) %>% mutate(percentual = 100 * n / estudantes) -> Q62
ggplot(data = Q62, mapping = aes(x = Q62, y = percentual)) + geom_bar(stat = 'identity')

# análises bivariadas - cruzamentos entre variáveis socioeconômicas e o desempenho nas notas
# a metodologia adotada foi o boxplot, para ver a distribuição das notas conforme as classes
# também foi aplicado o teste qui-quadrado para verificação da hipótese de independência

ggplot(data = spCandidatos, mapping = aes(x = TP_SEXO, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_SEXO, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_SEXO, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_SEXO, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_SEXO, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = ST_CONCLUSAO, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = ST_CONCLUSAO, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = ST_CONCLUSAO, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = ST_CONCLUSAO, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = ST_CONCLUSAO, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = TP_ESCOLA, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_ESCOLA, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_ESCOLA, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_ESCOLA, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_ESCOLA, y = NU_NOTA_REDACAO)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = AVG_NOTA, fill = TP_ESCOLA)) + geom_histogram()

ggplot(data = spCandidatos, mapping = aes(x = IN_TP_ENSINO, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = IN_TP_ENSINO, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = IN_TP_ENSINO, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = IN_TP_ENSINO, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = IN_TP_ENSINO, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = TP_ESTADO_CIVIL, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_ESTADO_CIVIL, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_ESTADO_CIVIL, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_ESTADO_CIVIL, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_ESTADO_CIVIL, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = TP_COR_RACA, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_COR_RACA, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_COR_RACA, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_COR_RACA, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = TP_COR_RACA, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = ID_DEPENDENCIA_ADM, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = ID_DEPENDENCIA_ADM, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = ID_DEPENDENCIA_ADM, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = ID_DEPENDENCIA_ADM, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = ID_DEPENDENCIA_ADM, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = ID_LOCALIZACAO, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = ID_LOCALIZACAO, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = ID_LOCALIZACAO, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = ID_LOCALIZACAO, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = ID_LOCALIZACAO, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = IN_STATUS_REDACAO, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = IN_STATUS_REDACAO, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = IN_STATUS_REDACAO, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = IN_STATUS_REDACAO, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = IN_STATUS_REDACAO, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q01, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q01, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q01, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q01, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q01, y = AVG_NOTA)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q02, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q02, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q02, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q02, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q02, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q03, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q03, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q03, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q03, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q03, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q04, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q04, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q04, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q04, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q04, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q05, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q05, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q05, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q05, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q05, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q06, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q06, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q06, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q06, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q06, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q07, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q07, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q07, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q07, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q07, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q08, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q08, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q08, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q08, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q08, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q09, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q09, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q09, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q09, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q09, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q10, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q10, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q10, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q10, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q10, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q11, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q11, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q11, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q11, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q11, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q12, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q12, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q12, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q12, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q12, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q13, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q13, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q13, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q13, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q13, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q14, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q14, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q14, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q14, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q14, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q15, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q15, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q15, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q15, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q15, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q16, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q16, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q16, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q16, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q16, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q17, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q17, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q17, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q17, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q17, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q18, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q18, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q18, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q18, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q18, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q19, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q19, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q19, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q19, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q19, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q20, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q20, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q20, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q20, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q20, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q21, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q21, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q21, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q21, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q21, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q22, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q22, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q22, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q22, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q22, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q23, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q23, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q23, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q23, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q23, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q24, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q24, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q24, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q24, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q24, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q25, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q25, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q25, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q25, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q25, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q26, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q26, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q26, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q26, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q26, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q27, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q27, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q27, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q27, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q27, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q28, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q28, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q28, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q28, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q28, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q29, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q29, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q29, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q29, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q29, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q30, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q30, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q30, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q30, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q30, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q31, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q31, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q31, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q31, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q31, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q32, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q32, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q32, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q32, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q32, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q33, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q33, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q33, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q33, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q33, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q34, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q34, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q34, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q34, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q34, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q35, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q35, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q35, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q35, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q35, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q36, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q36, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q36, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q36, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q36, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q37, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q37, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q37, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q37, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q37, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q38, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q38, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q38, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q38, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q38, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q39, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q39, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q39, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q39, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q39, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q40, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q40, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q40, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q40, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q40, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q41, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q41, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q41, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q41, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q41, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q42, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q42, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q42, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q42, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q42, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q43, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q43, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q43, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q43, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q43, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q44, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q44, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q44, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q44, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q44, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q45, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q45, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q45, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q45, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q45, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q46, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q46, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q46, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q46, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q46, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q47, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q47, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q47, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q47, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q47, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q48, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q48, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q48, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q48, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q48, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q49, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q49, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q49, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q49, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q49, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q50, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q50, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q50, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q50, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q50, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q51, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q51, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q51, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q51, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q51, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q52, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q52, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q52, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q52, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q52, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q53, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q53, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q53, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q53, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q53, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q54, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q54, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q54, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q54, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q54, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q55, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q55, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q55, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q55, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q55, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q56, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q56, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q56, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q56, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q56, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q57, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q57, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q57, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q57, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q57, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q58, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q58, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q58, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q58, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q58, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q59, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q59, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q59, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q59, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q59, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q60, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q60, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q60, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q60, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q60, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q61, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q61, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q61, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q61, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q61, y = NU_NOTA_REDACAO)) + geom_boxplot()

ggplot(data = spCandidatos, mapping = aes(x = Q62, y = NU_NT_CN)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q62, y = NU_NT_CH)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q62, y = NU_NT_LC)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q62, y = NU_NT_MT)) + geom_boxplot()
ggplot(data = spCandidatos, mapping = aes(x = Q62, y = NU_NOTA_REDACAO)) + geom_boxplot()