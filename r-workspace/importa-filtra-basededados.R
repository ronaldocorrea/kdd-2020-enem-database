library("BBmisc")
# library(dplyr) -- já está inclusa no tidyverse
library(tidyverse)
library("tidyr")

# Filtering staudents from Sao Paulo capital
loadDataLocale <- function(data, state, capital, atributosSelecionados, searchType = "estado") {
  if (searchType == "estado"){
    return (select(filter(data, UF_INSC == state &
                            IN_PRESENCA_CN == 1 &
                            IN_PRESENCA_CH == 1 &
                            IN_PRESENCA_LC == 1 &
                            IN_PRESENCA_MT == 1), 
                   all_of(atributosSelecionados)))
  } else if (searchType == "capital"){
    return (select(filter(data, UF_INSC == state &
                            IN_PRESENCA_CN == 1 &
                            IN_PRESENCA_CH == 1 &
                            IN_PRESENCA_LC == 1 &
                            IN_PRESENCA_MT == 1), 
                   all_of(atributosSelecionados)))
  }
  return (select(filter(data, UF_INSC == state &
                          NO_MUNICIPIO_INSC != capital &
                          IN_PRESENCA_CN == 1 &
                          IN_PRESENCA_CH == 1 &
                          IN_PRESENCA_LC == 1 &
                          IN_PRESENCA_MT == 1), 
                 all_of(atributosSelecionados)))
}

# eliminando atributos que não serão usados, nem considerados
removeAttribute <- function(data) {
  
  result <- data %>% 
    select(-NO_MUNICIPIO_ESC, -UF_ESC, -ANO_CONCLUIU, -UF_INSC, -NU_INSCRICAO, -NU_ANO, -COD_MUNICIPIO_INSC, -NO_MUNICIPIO_INSC, -IN_UNIDADE_HOSPITALAR, -PK_COD_ENTIDADE, -COD_MUNICIPIO_ESC, -COD_MUNICIPIO_PROVA)
  
  return (result)
}

modifyAttributes <- function(data) {
  # modificando valores numéricos (posteriormente serão todos transformados em numéricos)
  data <- mutate(data, TP_SEXO = replace(TP_SEXO, TP_SEXO == 0, 'Masculino')) %>%
    mutate(TP_SEXO = replace(TP_SEXO, TP_SEXO == 1, 'Feminino')) %>%
    mutate(ST_CONCLUSAO = replace(ST_CONCLUSAO, ST_CONCLUSAO == 1, 'Concluido')) %>%
    mutate(ST_CONCLUSAO = replace(ST_CONCLUSAO, ST_CONCLUSAO == 2, 'Conclui 2012')) %>%
    mutate(ST_CONCLUSAO = replace(ST_CONCLUSAO, ST_CONCLUSAO == 3, 'Depois 2012')) %>%
    mutate(ST_CONCLUSAO = replace(ST_CONCLUSAO, ST_CONCLUSAO == 4, 'Nao cursando')) %>%
    mutate(TP_ESCOLA = replace(TP_ESCOLA, TP_ESCOLA == 1, 'Publica')) %>%
    mutate(TP_ESCOLA = replace(TP_ESCOLA, TP_ESCOLA == 2, 'Privada')) %>%
    mutate(IN_TP_ENSINO = replace(IN_TP_ENSINO, IN_TP_ENSINO == 1, 'Regular')) %>%
    mutate(IN_TP_ENSINO = replace(IN_TP_ENSINO, IN_TP_ENSINO == 2, 'EJA')) %>%
    mutate(IN_TP_ENSINO = replace(IN_TP_ENSINO, IN_TP_ENSINO == 3, 'Profissionalizante')) %>%
    mutate(IN_TP_ENSINO = replace(IN_TP_ENSINO, IN_TP_ENSINO == 4, 'Especial')) %>%
    mutate(TP_ESTADO_CIVIL = replace(TP_ESTADO_CIVIL, TP_ESTADO_CIVIL == 0, 'Solteiro')) %>%
    mutate(TP_ESTADO_CIVIL = replace(TP_ESTADO_CIVIL, TP_ESTADO_CIVIL == 1, 'Casado')) %>%
    mutate(TP_ESTADO_CIVIL = replace(TP_ESTADO_CIVIL, TP_ESTADO_CIVIL == 2, 'Divorciado')) %>%
    mutate(TP_ESTADO_CIVIL = replace(TP_ESTADO_CIVIL, TP_ESTADO_CIVIL == 3, 'Viuvo')) %>%
    mutate(TP_COR_RACA = replace(TP_COR_RACA, TP_COR_RACA == 0, 'Nao declarado')) %>%
    mutate(TP_COR_RACA = replace(TP_COR_RACA, TP_COR_RACA == 1, 'Branca')) %>%
    mutate(TP_COR_RACA = replace(TP_COR_RACA, TP_COR_RACA == 2, 'Preta')) %>%
    mutate(TP_COR_RACA = replace(TP_COR_RACA, TP_COR_RACA == 3, 'Parda')) %>%
    mutate(TP_COR_RACA = replace(TP_COR_RACA, TP_COR_RACA == 4, 'Amarela')) %>%
    mutate(TP_COR_RACA = replace(TP_COR_RACA, TP_COR_RACA == 5, 'Indigena')) %>%
    mutate(ID_DEPENDENCIA_ADM = replace(ID_DEPENDENCIA_ADM, ID_DEPENDENCIA_ADM == 1, 'Federal')) %>%
    mutate(ID_DEPENDENCIA_ADM = replace(ID_DEPENDENCIA_ADM, ID_DEPENDENCIA_ADM == 2, 'Estadual')) %>%
    mutate(ID_DEPENDENCIA_ADM = replace(ID_DEPENDENCIA_ADM, ID_DEPENDENCIA_ADM == 3, 'Municipal')) %>%
    mutate(ID_DEPENDENCIA_ADM = replace(ID_DEPENDENCIA_ADM, ID_DEPENDENCIA_ADM == 4, 'Privada')) %>%
    mutate(ID_LOCALIZACAO = replace(ID_LOCALIZACAO, ID_LOCALIZACAO == 1, 'Urbana')) %>%
    mutate(ID_LOCALIZACAO = replace(ID_LOCALIZACAO, ID_LOCALIZACAO == 2, 'Rural')) %>%
    mutate(SIT_FUNC = replace(SIT_FUNC, SIT_FUNC == 1, 'Atividade')) %>%
    mutate(SIT_FUNC = replace(SIT_FUNC, SIT_FUNC == 2, 'Paralisada')) %>%
    mutate(SIT_FUNC = replace(SIT_FUNC, SIT_FUNC == 3, 'Extinta')) %>%
    mutate(SIT_FUNC = replace(SIT_FUNC, SIT_FUNC == 4, 'Anterior')) %>%
    mutate(IN_PRESENCA_CN = replace(IN_PRESENCA_CN, IN_PRESENCA_CN == 0, 'Faltou')) %>%
    mutate(IN_PRESENCA_CN = replace(IN_PRESENCA_CN, IN_PRESENCA_CN == 1, 'Presente')) %>%
    mutate(IN_PRESENCA_CN = replace(IN_PRESENCA_CN, IN_PRESENCA_CN == 2, 'Eliminado')) %>%
    mutate(IN_PRESENCA_CH = replace(IN_PRESENCA_CH, IN_PRESENCA_CH == 0, 'Faltou')) %>%
    mutate(IN_PRESENCA_CH = replace(IN_PRESENCA_CH, IN_PRESENCA_CH == 1, 'Presente')) %>%
    mutate(IN_PRESENCA_CH = replace(IN_PRESENCA_CH, IN_PRESENCA_CH == 2, 'Eliminado')) %>%
    mutate(IN_PRESENCA_LC = replace(IN_PRESENCA_LC, IN_PRESENCA_LC == 0, 'Faltou')) %>%
    mutate(IN_PRESENCA_LC = replace(IN_PRESENCA_LC, IN_PRESENCA_LC == 1, 'Presente')) %>%
    mutate(IN_PRESENCA_LC = replace(IN_PRESENCA_LC, IN_PRESENCA_LC == 2, 'Eliminado')) %>%
    mutate(IN_PRESENCA_MT = replace(IN_PRESENCA_MT, IN_PRESENCA_MT == 0, 'Faltou')) %>%
    mutate(IN_PRESENCA_MT = replace(IN_PRESENCA_MT, IN_PRESENCA_MT == 1, 'Presente')) %>%
    mutate(IN_PRESENCA_MT = replace(IN_PRESENCA_MT, IN_PRESENCA_MT == 2, 'Eliminado')) %>%
    mutate(TP_ESCOLA = replace(TP_ESCOLA, is.na(TP_ESCOLA), 'Nao Informado')) %>%
    mutate(IN_TP_ENSINO = replace(IN_TP_ENSINO, is.na(IN_TP_ENSINO), 'Nao Informado')) %>%
    mutate(ID_DEPENDENCIA_ADM = replace(ID_DEPENDENCIA_ADM, is.na(ID_DEPENDENCIA_ADM), 'Nao Informado')) %>%
    mutate(ID_LOCALIZACAO = replace(ID_LOCALIZACAO, is.na(ID_LOCALIZACAO), 'Nao Informado')) %>%
    mutate(SIT_FUNC = replace(SIT_FUNC, is.na(SIT_FUNC), 'Nao Informado')) %>%
    mutate(NU_NOTA_COMP1 = replace(NU_NOTA_COMP1, is.na(NU_NOTA_COMP1), 0)) %>%
    mutate(NU_NOTA_COMP2 = replace(NU_NOTA_COMP2, is.na(NU_NOTA_COMP2), 0)) %>%
    mutate(NU_NOTA_COMP3 = replace(NU_NOTA_COMP3, is.na(NU_NOTA_COMP3), 0)) %>%
    mutate(NU_NOTA_COMP4 = replace(NU_NOTA_COMP4, is.na(NU_NOTA_COMP4), 0)) %>%
    mutate(NU_NOTA_COMP5 = replace(NU_NOTA_COMP5, is.na(NU_NOTA_COMP5), 0)) %>%
    mutate(NU_NOTA_REDACAO = replace(NU_NOTA_REDACAO, is.na(NU_NOTA_REDACAO), 0)) %>%
    mutate(AVG_NOTA = rowMeans(select(.,NU_NT_CN, NU_NT_CH, NU_NT_LC, NU_NT_MT, NU_NOTA_REDACAO)))
  # transformando colunas <fct> em <char> (<fct> é complicado de manipular)
  
  data <- transform(data, Q01 = as.character(Q01)) %>%
    transform(Q02 = as.character(Q02)) %>%
    transform(Q03 = as.character(Q03)) %>%
    transform(Q04 = as.character(Q04)) %>%
    transform(Q05 = as.character(Q05)) %>%
    transform(Q06 = as.character(Q06)) %>%
    transform(Q07 = as.character(Q07)) %>%
    transform(Q08 = as.character(Q08)) %>%
    transform(Q09 = as.character(Q09)) %>%
    transform(Q10 = as.character(Q10)) %>%
    transform(Q11 = as.character(Q11)) %>%
    transform(Q12 = as.character(Q12)) %>%
    transform(Q13 = as.character(Q13)) %>%
    transform(Q14 = as.character(Q14)) %>%
    transform(Q15 = as.character(Q15)) %>%
    transform(Q16 = as.character(Q16)) %>%
    transform(Q17 = as.character(Q17)) %>%
    transform(Q18 = as.character(Q18)) %>%
    transform(Q19 = as.character(Q19)) %>%
    transform(Q20 = as.character(Q20)) %>%
    transform(Q21 = as.character(Q21)) %>%
    transform(Q22 = as.character(Q22)) %>%
    transform(Q23 = as.character(Q23)) %>%
    transform(Q24 = as.character(Q24)) %>%
    transform(Q25 = as.character(Q25)) %>%
    transform(Q26 = as.character(Q26)) %>%
    transform(Q27 = as.character(Q27)) %>%
    transform(Q28 = as.character(Q28)) %>%
    transform(Q29 = as.character(Q29)) %>%
    transform(Q30 = as.character(Q30)) %>%
    transform(Q31 = as.character(Q31)) %>%
    transform(Q32 = as.character(Q32)) %>%
    transform(Q33 = as.character(Q33)) %>%
    transform(Q34 = as.character(Q34)) %>%
    transform(Q35 = as.character(Q35)) %>%
    transform(Q36 = as.character(Q36)) %>%
    transform(Q37 = as.character(Q37)) %>%
    transform(Q38 = as.character(Q38)) %>%
    transform(Q39 = as.character(Q39)) %>%
    transform(Q40 = as.character(Q40)) %>%
    transform(Q41 = as.character(Q41)) %>%
    transform(Q42 = as.character(Q42)) %>%
    transform(Q43 = as.character(Q43)) %>%
    transform(Q44 = as.character(Q44)) %>%
    transform(Q45 = as.character(Q45)) %>%
    transform(Q46 = as.character(Q46)) %>%
    transform(Q47 = as.character(Q47)) %>%
    transform(Q48 = as.character(Q48)) %>%
    transform(Q49 = as.character(Q49)) %>%
    transform(Q50 = as.character(Q50)) %>%
    transform(Q51 = as.character(Q51)) %>%
    transform(Q52 = as.character(Q52)) %>%
    transform(Q53 = as.character(Q53)) %>%
    transform(Q54 = as.character(Q54)) %>%
    transform(Q55 = as.character(Q55)) %>%
    transform(Q56 = as.character(Q56)) %>%
    transform(Q57 = as.character(Q57)) %>%
    transform(Q58 = as.character(Q58)) %>%
    transform(Q59 = as.character(Q59)) %>%
    transform(Q60 = as.character(Q60)) %>%
    transform(Q61 = as.character(Q61)) %>%
    transform(Q62 = as.character(Q62))
  
  # substituindo valores ausentes no questionário social
  
  data <- mutate(data, Q40 = replace(Q40, is.na(Q40), 'Nao Informado')) %>%
    mutate(Q41 = replace(Q41, is.na(Q41), 'Nao Informado')) %>%
    mutate(Q42 = replace(Q42, is.na(Q42), 'Nao Informado')) %>%
    mutate(Q43 = replace(Q43, is.na(Q43), 'Nao Informado')) %>%
    mutate(Q44 = replace(Q44, is.na(Q44), 'Nao Informado')) %>%
    mutate(Q45 = replace(Q45, is.na(Q45), 'Nao Informado')) %>%
    mutate(Q46 = replace(Q46, is.na(Q46), 'Nao Informado')) %>%
    mutate(Q47 = replace(Q47, is.na(Q47), 'Nao Informado')) %>%
    mutate(Q48 = replace(Q48, is.na(Q48), 'Nao Informado')) %>%
    mutate(Q49 = replace(Q49, is.na(Q49), 'Nao Informado')) %>%
    mutate(Q50 = replace(Q50, is.na(Q50), 'Nao Informado')) %>%
    mutate(Q51 = replace(Q51, is.na(Q51), 'Nao Informado')) %>%
    mutate(Q52 = replace(Q52, is.na(Q52), 'Nao Informado')) %>%
    mutate(Q53 = replace(Q53, is.na(Q53), 'Nao Informado')) %>%
    mutate(Q54 = replace(Q54, is.na(Q54), 'Nao Informado')) %>%
    mutate(Q55 = replace(Q55, is.na(Q55), 'Nao Informado')) %>%
    mutate(Q56 = replace(Q56, is.na(Q56), 'Nao Informado')) %>%
    mutate(Q57 = replace(Q57, is.na(Q57), 'Nao Informado')) %>%
    mutate(Q58 = replace(Q58, is.na(Q58), 'Nao Informado')) %>%
    mutate(Q59 = replace(Q59, is.na(Q59), 'Nao Informado')) %>%
    mutate(Q60 = replace(Q60, is.na(Q60), 'Nao Informado')) %>%
    mutate(Q61 = replace(Q61, is.na(Q61), 'Nao Informado')) %>%
    mutate(Q62 = replace(Q62, is.na(Q62), 'Nao Informado'))
  
  return(unnest(data))
}

PATH_FILE_DADOS_ENEM_2012 <- '../database/DADOS_ENEM_2012.csv'
PATH_FILE_DADOS_QUESTIONARIO_2012 <- '../database/QUESTIONARIO_ENEM_2012.csv'

# os dados foram transformados em tibble para facilitar manuseio com o tidyverse
dfDadosEnem2012 <- read.csv(PATH_FILE_DADOS_ENEM_2012, na.strings=c(".",""))
dfDadosEnem2012 <- unnest(dfDadosEnem2012) # transformando em tibble
dfDadosEnem2012Colnames <- c(colnames(dfDadosEnem2012))
dfDadosQuestionatioEnem2012 <- read.csv(PATH_FILE_DADOS_QUESTIONARIO_2012, na.strings=c(".",""))
dfDadosQuestionatioEnem2012 <- unnest(dfDadosQuestionatioEnem2012) # transformando em tibble
dfDadosQuestionatioEnem2012Colnames <- c(colnames(dfDadosQuestionatioEnem2012))

# unificando bases de dados pelo número de inscrição
dfDatabaseEnem2012 <- merge(dfDadosEnem2012, dfDadosQuestionatioEnem2012, by=c("NU_INSCRICAO"))
summary(dfDatabaseEnem2012)

# seleção de atributos - pode mudar conforme o interesse da análise, mas isto fica para outro script
dadosDoInscrito <- dfDadosEnem2012Colnames[1:14]
necessidadesEspeciais <- dfDadosEnem2012Colnames[15:28]
atendimentoNecessidadesEspeciais <- dfDadosEnem2012Colnames[29:39]
certificacao <- dfDadosEnem2012Colnames[40:42]
dadosDaEscola <- dfDadosEnem2012Colnames[43:49]
dadosMunicipioAplicacaoProva <- dfDadosEnem2012Colnames[50:52]
dadosDaProvaObjetiva <- dfDadosEnem2012Colnames[53:60]
dadosDaRedacao <- dfDadosEnem2012Colnames[74:80]

# atributos relativos ao questionário socioeconômico
questionarioSocioEconomico <- c(
  dfDadosQuestionatioEnem2012Colnames[3],  #"Q01" -> Q1 Até quando seu pai estudou?
  dfDadosQuestionatioEnem2012Colnames[4],  #"Q02" -> Q2 Até quando sua mãe estudou?
  dfDadosQuestionatioEnem2012Colnames[5],  #"Q03" -> Q3 Qual é a renda mensal de sua família? (Some a sua renda com a dos seus familiares.)
  dfDadosQuestionatioEnem2012Colnames[6],  #"Q04" -> Q4 Quantas pessoas moram em sua casa (incluindo você)?
  dfDadosQuestionatioEnem2012Colnames[7],  #"Q05" -> Q5 A residência de sua família é? C
  dfDadosQuestionatioEnem2012Colnames[8],  #"Q06" -> Q6 A residência de sua família está localizada em?
  dfDadosQuestionatioEnem2012Colnames[9],  #"Q07" -> Q7 Você tem em sua casa? TV em cores
  dfDadosQuestionatioEnem2012Colnames[10], #"Q08" -> Q8 Você tem em sua casa? Videocassete e/ou DVD
  dfDadosQuestionatioEnem2012Colnames[11], #"Q09" -> Q9 Você tem em sua casa? Rádio
  dfDadosQuestionatioEnem2012Colnames[12], #"Q10" -> Q10 Você tem em sua casa? Microcomputador
  dfDadosQuestionatioEnem2012Colnames[13], #"Q11" -> Q11 Você tem em sua casa? Automóvel
  dfDadosQuestionatioEnem2012Colnames[14], #"Q12" -> Q12 Você tem em sua casa? Máquina de lavar roupa
  dfDadosQuestionatioEnem2012Colnames[15], #"Q13" -> Q13 Você tem em sua casa? Geladeira
  dfDadosQuestionatioEnem2012Colnames[16], #"Q14" -> Q14 Você tem em sua casa? Freezer (aparelho independente ou parte da geladeira duplex)
  dfDadosQuestionatioEnem2012Colnames[17], #"Q15" -> Q15 Você tem em sua casa? Telefone fixo
  dfDadosQuestionatioEnem2012Colnames[18], #"Q16" -> Q16 Você tem em sua casa? Telefone celular
  dfDadosQuestionatioEnem2012Colnames[19], #"Q17" -> Q17 Você tem em sua casa? Acesso a internet
  dfDadosQuestionatioEnem2012Colnames[20], #"Q18" -> Q18 Você tem em sua casa? TV por assinatura
  dfDadosQuestionatioEnem2012Colnames[21], #"Q19" -> Q19 Você tem em sua casa? Aspirador em pó
  dfDadosQuestionatioEnem2012Colnames[22], #"Q20" -> Q20 Você tem em sua casa? Empregada mensalista
  dfDadosQuestionatioEnem2012Colnames[23], #"Q21" -> Q21 Você tem em sua casa? Banheiro
  dfDadosQuestionatioEnem2012Colnames[24], #"Q22" -> Q22 Você exerce ou já exerceu atividade remunerada?
  dfDadosQuestionatioEnem2012Colnames[25], #"Q23" -> Q23 Indique os motivos que levaram você a participar do ENEM:Testar meus conhecimentos
  dfDadosQuestionatioEnem2012Colnames[26], #"Q24" -> Q24 Indique os motivos que levaram você a participar do ENEM:Aumentar a possibilidade de conseguir um emprego
  dfDadosQuestionatioEnem2012Colnames[27], #"Q25" -> Q25 Indique os motivos que levaram você a participar do ENEM:Progredir no meu emprego atual
  dfDadosQuestionatioEnem2012Colnames[28], #"Q26" -> Q26 Indique os motivos que levaram você a participar do ENEM:Ingressar na Educação Superior Pública
  dfDadosQuestionatioEnem2012Colnames[29], #"Q27" -> Q27 Indique os motivos que levaram você a participar do ENEM:Ingressar na Educação Superior Privada
  dfDadosQuestionatioEnem2012Colnames[30], #"Q28" -> Q28 Indique os motivos que levaram você a participar do ENEM:Conseguir uma bolsa de estudos (ProUni, outras)
  dfDadosQuestionatioEnem2012Colnames[31], #"Q29" -> Q29 Indique os motivos que levaram você a participar do ENEM:Participar do Programa de Financiamento Estudantil FIES
  dfDadosQuestionatioEnem2012Colnames[32], #"Q30" -> Q30 Quantos anos você levou para concluir o Ensino Fundamental?
  dfDadosQuestionatioEnem2012Colnames[33], #"Q31" -> Q31 Você deixou de estudar durante o Ensino Fundamental?
  dfDadosQuestionatioEnem2012Colnames[34], #"Q32" -> Q32 Em que tipo de escola você cursou o Ensino Fundamental?
  dfDadosQuestionatioEnem2012Colnames[35], #"Q33" -> Q33 Quantos anos você levou para concluir o Ensino Médio?
  dfDadosQuestionatioEnem2012Colnames[36], #"Q34" -> Q34 Você deixou de estudar durante o Ensino Médio?
  dfDadosQuestionatioEnem2012Colnames[37], #"Q35" -> Q35 Em que tipo de escola você cursou o Ensino Médio?
  dfDadosQuestionatioEnem2012Colnames[38], #"Q36" -> Q36 Caso você ingresse no Ensino Superior privado pretende recorrer aos auxílios abaixo para custeio das mensalidades? Pró-Uni (Programa Universidade para Todos)
  dfDadosQuestionatioEnem2012Colnames[39], #"Q37" -> Q37 Caso você ingresse no Ensino Superior privado pretende recorrer aos auxílios abaixo para custeio das mensalidades? Bolsa de estudos da própria Instituição de Ensino Superior
  dfDadosQuestionatioEnem2012Colnames[40], #"Q38" -> Q38 Caso você ingresse no Ensino Superior privado pretende recorrer aos auxílios abaixo para custeio das mensalidades? Bolsa de estudos da empresa onde trabalho.
  dfDadosQuestionatioEnem2012Colnames[41], #"Q39" -> Q39 Caso você ingresse no Ensino Superior privado pretende recorrer aos auxílios abaixo para custeio das mensalidades? Auxílio do Programa de Financiamento Estudantil - FIES
  dfDadosQuestionatioEnem2012Colnames[42], #"Q40" -> Q40 Você cursa ou já cursou a Educação de Jovens e Adultos - EJA?
  dfDadosQuestionatioEnem2012Colnames[43], #"Q41" -> Q41 Como é ou era o principal curso de EJA que você frequenta ou frequentou?
  dfDadosQuestionatioEnem2012Colnames[44], #"Q42" -> Q42 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Trabalhava, não tinha tempo de estudar
  dfDadosQuestionatioEnem2012Colnames[45], #"Q43" -> Q43 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Estudava no curso da empresa e foi interrompido.
  dfDadosQuestionatioEnem2012Colnames[46], #"Q44" -> Q44 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Ocorreram problemas de saúde ou acidentes comigo familiares
  dfDadosQuestionatioEnem2012Colnames[47], #"Q45" -> Q45 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Mudei de bairro, cidade ou município.
  dfDadosQuestionatioEnem2012Colnames[48], #"Q46" -> Q46 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Por motivos pessoais, casamento, filhos, etc.
  dfDadosQuestionatioEnem2012Colnames[49], #"Q47" -> Q47 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Faltava-me interesse, desisti.
  dfDadosQuestionatioEnem2012Colnames[50], #"Q48" -> Q48 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Senti-me discriminado(a).
  dfDadosQuestionatioEnem2012Colnames[51], #"Q49" -> Q49 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Temi/Sofri violência.
  dfDadosQuestionatioEnem2012Colnames[52], #"Q50" -> Q50 Você já frequentou o ensino regular?
  dfDadosQuestionatioEnem2012Colnames[53], #"Q51" -> Q51 Indique o que levou você a deixar de frequentar o ensino regular: Falta de vaga em escola pública.
  dfDadosQuestionatioEnem2012Colnames[54], #"Q52" -> Q52 Indique o que levou você a deixar de frequentar o ensino regular: Ausência de escola perto de casa.
  dfDadosQuestionatioEnem2012Colnames[55], #"Q53" -> Q53 Indique o que levou você a deixar de frequentar o ensino regular: Dificuldades após reprovação.
  dfDadosQuestionatioEnem2012Colnames[56], #"Q54" -> Q54 Indique o que levou você a deixar de frequentar o ensino regular: Falta de interesse em estudar.
  dfDadosQuestionatioEnem2012Colnames[57], #"Q55" -> Q55 Indique o que levou você a deixar de frequentar o ensino regular: Falta de condições adequadas na escola.
  dfDadosQuestionatioEnem2012Colnames[58], #"Q56" -> Q56 Indique o que levou você a deixar de frequentar o ensino regular: Trabalho, falta de tempo para estudar.
  dfDadosQuestionatioEnem2012Colnames[59], #"Q57" -> Q57 Indique o que levou você a deixar de frequentar o ensino regular: Motivos pessoais, casamento / filhos, etc.
  dfDadosQuestionatioEnem2012Colnames[60], #"Q58" -> Q58 Indique o que levou você a deixar de frequentar o ensino regular: Falta de apoio familiar.
  dfDadosQuestionatioEnem2012Colnames[61], #"Q59" -> Q59 Indique o que levou você a deixar de frequentar o ensino regular: Problemas de saúde ou acidente comigo ou familiares.
  dfDadosQuestionatioEnem2012Colnames[62], #"Q60" -> Q60 Indique o que levou você a deixar de frequentar o ensino regular: Discriminação / Preconceitos (sexo, raça, idade, classe etc.)
  dfDadosQuestionatioEnem2012Colnames[63], #"Q61" -> Q61 Indique o que levou você a deixar de frequentar o ensino regular: Medo de sofrer violência.
  dfDadosQuestionatioEnem2012Colnames[64]  #"Q62" -> Q62 Quantos anos você tinha quando deixou de frequentar o ensino regular?
)

# Attributes selection
atributosSelecionados <- c(dadosDoInscrito,
                        dadosDaEscola,
                        dadosMunicipioAplicacaoProva,
                        dadosDaProvaObjetiva,
                        dadosDaRedacao,
                        questionarioSocioEconomico)

length(atributosSelecionados) - length(necessidadesEspeciais) + length(atendimentoNecessidadesEspeciais) + length(certificacao)

# #AC
# acStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "AC", "", atributosSelecionados,"estado"))
# acStudents <- removeAttribute(acStudents)
# acStudents <- modifyAttributes(acStudents)
# write.csv(acStudents, './output/acStudents2012.csv', row.names = FALSE)
# #AL
# alStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "AL", "", atributosSelecionados,"estado"))
# alStudents <- removeAttribute(alStudents)
# alStudents <- modifyAttributes(alStudents)
# write.csv(alStudents, './output/alStudents2012.csv', row.names = FALSE)
# #AL
# apStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "AP", "", atributosSelecionados,"estado"))
# apStudents <- removeAttribute(apStudents)
# apStudents <- modifyAttributes(apStudents)
# write.csv(apStudents, './output/apStudents2012.csv', row.names = FALSE)
# #AM
# amStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "AM", "", atributosSelecionados,"estado"))
# amStudents <- removeAttribute(amStudents)
# amStudents <- modifyAttributes(amStudents)
# write.csv(amStudents, './output/amStudents2012.csv', row.names = FALSE)
# #BA
# baStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "BA", "", atributosSelecionados,"estado"))
# baStudents <- removeAttribute(baStudents)
# baStudents <- modifyAttributes(baStudents)
# write.csv(baStudents, './output/baStudents2012.csv', row.names = FALSE)
# #CE
# ceStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "CE", "", atributosSelecionados,"estado"))
# ceStudents <- removeAttribute(ceStudents)
# ceStudents <- modifyAttributes(ceStudents)
# write.csv(ceStudents, './output/ceStudents2012.csv', row.names = FALSE)
# #DF
# dfStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "DF", "", atributosSelecionados,"estado"))
# dfStudents <- removeAttribute(dfStudents)
# dfStudents <- modifyAttributes(dfStudents)
# write.csv(dfStudents, './output/dfStudents2012.csv', row.names = FALSE)
# #ES
# esStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "ES", "", atributosSelecionados,"estado"))
# esStudents <- removeAttribute(esStudents)
# esStudents <- modifyAttributes(esStudents)
# write.csv(esStudents, './output/esStudents2012.csv', row.names = FALSE)
# #GO
# goStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "GO", "", atributosSelecionados,"estado"))
# goStudents <- removeAttribute(goStudents)
# goStudents <- modifyAttributes(goStudents)
# write.csv(goStudents, './output/goStudents2012.csv', row.names = FALSE)
# #MA
# maStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "MA", "", atributosSelecionados,"estado"))
# maStudents <- removeAttribute(maStudents)
# maStudents <- modifyAttributes(maStudents)
# write.csv(maStudents, './output/maStudents2012.csv', row.names = FALSE)
# #MT
# mtStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "MT", "", atributosSelecionados,"estado"))
# mtStudents <- removeAttribute(mtStudents)
# mtStudents <- modifyAttributes(mtStudents)
# write.csv(mtStudents, './output/mtStudents2012.csv', row.names = FALSE)
# #MS
# msStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "MS", "", atributosSelecionados,"estado"))
# msStudents <- removeAttribute(msStudents)
# msStudents <- modifyAttributes(msStudents)
# write.csv(msStudents, './output/msStudents2012.csv', row.names = FALSE)
# #MG
# mgStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "MG", "", atributosSelecionados,"estado"))
# mgStudents <- removeAttribute(mgStudents)
# mgStudents <- modifyAttributes(mgStudents)
# write.csv(mgStudents, './output/mgStudents2012.csv', row.names = FALSE)
# #PA
# paStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "PA", "", atributosSelecionados,"estado"))
# paStudents <- removeAttribute(paStudents)
# paStudents <- modifyAttributes(paStudents)
# write.csv(paStudents, './output/paStudents2012.csv', row.names = FALSE)
# #PB
# pbStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "PB", "", atributosSelecionados,"estado"))
# pbStudents <- removeAttribute(pbStudents)
# pbStudents <- modifyAttributes(pbStudents)
# write.csv(pbStudents, './output/pbStudents2012.csv', row.names = FALSE)
# #PR
# prStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "PR", "", atributosSelecionados,"estado"))
# prStudents <- removeAttribute(prStudents)
# prStudents <- modifyAttributes(prStudents)
# write.csv(prStudents, './output/prStudents2012.csv', row.names = FALSE)
# #PE
# peStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "PE", "", atributosSelecionados,"estado"))
# peStudents <- removeAttribute(peStudents)
# peStudents <- modifyAttributes(peStudents)
# write.csv(peStudents, './output/peStudents2012.csv', row.names = FALSE)
# #PI
# piStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "PI", "", atributosSelecionados,"estado"))
# piStudents <- removeAttribute(piStudents)
# piStudents <- modifyAttributes(piStudents)
# write.csv(piStudents, './output/piStudents2012.csv', row.names = FALSE)
# #RJ
# rjStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "RJ", "", atributosSelecionados,"estado"))
# rjStudents <- removeAttribute(rjStudents)
# rjStudents <- modifyAttributes(rjStudents)
# write.csv(rjStudents, './output/rjStudents2012.csv', row.names = FALSE)
# #RN
# rnStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "RN", "", atributosSelecionados,"estado"))
# rnStudentsjStudents <- removeAttribute(rnStudents)
# rnStudents <- modifyAttributes(rnStudents)
# write.csv(rnStudents, './output/rnStudents2012.csv', row.names = FALSE)
# #RS
# rsStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "RS", "", atributosSelecionados,"estado"))
# rsStudents <- removeAttribute(rsStudents)
# rsStudents <- modifyAttributes(rsStudents)
# write.csv(rsStudents, './output/rsStudents2012.csv', row.names = FALSE)
# #RO
# roStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "RO", "", atributosSelecionados,"estado"))
# roStudents <- removeAttribute(roStudents)
# roStudents <- modifyAttributes(roStudents)
# write.csv(roStudents, './output/roStudents2012.csv', row.names = FALSE)
# #RR
# rrStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "RR", "", atributosSelecionados,"estado"))
# rrStudents <- removeAttribute(rrStudents)
# rrStudents <- modifyAttributes(rrStudents)
# write.csv(rrStudents, './output/rrStudents2012.csv', row.names = FALSE)
# #SC
# scStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "SC", "", atributosSelecionados,"estado"))
# scStudents <- removeAttribute(scStudents)
# scStudents <- modifyAttributes(scStudents)
# write.csv(scStudents, './output/scStudents2012.csv', row.names = FALSE)
#SP
spCandidatos <- unnest(loadDataLocale(dfDatabaseEnem2012, "SP", "", atributosSelecionados,"estado"))
spCandidatos <- removeAttribute(spCandidatos)
spCandidatos <- modifyAttributes(spCandidatos)
write.csv(spCandidatos, './output/spCandidatos2012.csv', row.names = FALSE)
# #SE
# seStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "SE", "", atributosSelecionados,"estado"))
# seStudents <- removeAttribute(seStudents)
# seStudents <- modifyAttributes(seStudents)
# write.csv(seStudents, './output/seStudents2012.csv', row.names = FALSE)
# #TO
# toStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "TO", "", atributosSelecionados,"estado"))
# toStudents <- removeAttribute(toStudents)
# toStudents <- modifyAttributes(toStudents)
# write.csv(toStudents, './output/toStudents2012.csv', row.names = FALSE)
# 
# spCapitalStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "SP", "SAO PAULO", atributosSelecionados, "capital"))
# spNotCapitalStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "SP", "SAO PAULO", atributosSelecionados, "interior"))
# 
# ceCapitalStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "CE", "FORTALEZA", atributosSelecionados))
# ceNotCapitalStudents <- unnest(loadDataLocale(dfDatabaseEnem2012, "SP", "FORTALEZA", atributosSelecionados, F))
# 


# spStudents <- removeAttribute(spStudents)
# spCapitalStudents <- removeAttribute(spCapitalStudents)
# spNotCapitalStudents <- removeAttribute(spNotCapitalStudents)
# ceCapitalStudents <- removeAttribute(ceCapitalStudents)
# ceNotCapitalStudents <- removeAttribute(ceNotCapitalStudents)


# spStudents <- modifyAttributes(spStudents)
# spCapitalStudents <- modifyAttributes(spCapitalStudents)
# spNotCapitalStudents <- modifyAttributes(spNotCapitalStudents)
# ceCapitalStudents <- modifyAttributes(ceCapitalStudents)
# ceNotCapitalStudents <- modifyAttributes(ceNotCapitalStudents)
# summary(spCapitalStudents)

# contando número de valores ausentes por coluna
# spCapitalNA <- spCapitalStudents %>%
#   select(everything()) %>%
#   summarise_all(funs(sum(is.na(.))))
# 

# write.csv(spStudents, './output/spStudents2012.csv', row.names = FALSE)
# write.csv(spCapitalStudents, './output/spCapitalStudents2012.csv', row.names = FALSE)
# write.csv(spNotCapitalStudents, './output/spNotCapitalStudents2012.csv', row.names = FALSE)
# write.csv(ceCapitalStudents, './output/ceCapitalStudents2012.csv', row.names = FALSE)
# write.csv(ceNotCapitalStudents, './output/ceNotCapitalStudents2012.csv', row.names = FALSE)

# execute depois de rodar o clustering.R

# Tests

# filter(dfDadosEnem2012, UF_INSC == 'AC') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'AL') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'AP') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'AM') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'BA') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'CE') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'DF') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'ES') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'GO') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'MA') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'MT') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'MS') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'MG') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'PA') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'PB') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'PR') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'PE') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'PI') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'RJ') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'RN') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'RS') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'RO') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'RR') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'SC') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'SP') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'SE') %>% nrow(.)
# filter(dfDadosEnem2012, UF_INSC == 'TO') %>% nrow(.)
# sampleSp <- filter(dfDadosEnem2012, UF_INSC == 'SP') %>% 
# hist(sample_n(dfDadosEnem2012, 15000)$IDADE)

