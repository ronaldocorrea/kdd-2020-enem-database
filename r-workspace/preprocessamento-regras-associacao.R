## FUNCOES
removeOutliers <- function(data) {
  lowerOutliers <- quantile(data$IDADE, 0.25) - 1.5*IQR(data$IDADE)
  upperOutliers <- quantile(data$IDADE, 0.75) + 1.5*IQR(data$IDADE)
  return (select(filter(data, IDADE >= lowerOutliers &  IDADE <= upperOutliers), colnames(data)))
}

countOutliers <- function(data) {
  lowerOutliers <- quantile(data$IDADE, 0.25) - 1.5*IQR(data$IDADE)
  upperOutliers <- quantile(data$IDADE, 0.75) + 1.5*IQR(data$IDADE)
  return (nrow(select(filter(data, IDADE < lowerOutliers &  IDADE > upperOutliers), colnames(data))))
}

filterByAvgNota <- function(data, nota) {
  return (select(filter(data, AVG_NOTA == nota), colnames(data)))
}

convertAvgNota <- function(data) {
  return (data %>%
            mutate(AVG_NOTA = case_when(AVG_NOTA <= 450 ~ 'A', 
                                        AVG_NOTA > 450 & AVG_NOTA <= 700 ~ 'B',
                                        AVG_NOTA > 700 ~ 'C'))) %>%
            mutate(Q23 = case_when(Q23 == 5 ~ 'I', Q23 != 5 ~ 'NI')) %>%
            mutate(Q24 = case_when(Q23 == 5 ~ 'I', Q23 != 5 ~ 'NI')) %>%
            mutate(Q25 = case_when(Q23 == 5 ~ 'I', Q23 != 5 ~ 'NI')) %>%
            mutate(Q26 = case_when(Q23 == 5 ~ 'I', Q23 != 5 ~ 'NI')) %>%
            mutate(Q27 = case_when(Q23 == 5 ~ 'I', Q23 != 5 ~ 'NI')) %>%
            mutate(Q28 = case_when(Q23 == 5 ~ 'I', Q23 != 5 ~ 'NI')) %>%
            mutate(Q29 = case_when(Q23 == 5 ~ 'I', Q23 != 5 ~ 'NI'))
}


removerAtributosAnaliseRegrasDeAssoicao <- function(data) {
  return (select(data, 
                 -TP_ESCOLA,
                 -TP_ESTADO_CIVIL,
                 -IDADE, 
                 -TP_SEXO,
                 -ST_CONCLUSAO,
                 -TP_COR_RACA,
                 -SIT_FUNC,
                 -NO_MUNICIPIO_PROVA,
                 -UF_MUNICIPIO_PROVA,
                 -IN_PRESENCA_CN,
                 -IN_PRESENCA_CH,
                 -IN_PRESENCA_LC,
                 -IN_PRESENCA_MT,
                 -ID_DEPENDENCIA_ADM,
                 -ID_LOCALIZACAO,
                 -IN_TP_ENSINO, #EJA, Especial e Regular
                 # removendo notas e usando somente AVG_NOTA
                 -NU_NT_CN,
                 -NU_NT_CH,
                 -NU_NT_LC,
                 -NU_NT_MT,
                 # Removendo Redacao
                 -IN_STATUS_REDACAO,
                 -NU_NOTA_COMP1, 
                 -NU_NOTA_COMP2, 
                 -NU_NOTA_COMP3, 
                 -NU_NOTA_COMP4, 
                 -NU_NOTA_COMP5,
                 -NU_NOTA_REDACAO,
                 # Questionario socioeconomico
                 #-Q01, #Q1 Até quando seu pai estudou?
                 #-Q02, #Q2 Até quando sua mãe estudou?
                 #-Q03, #Q3 Qual é a renda mensal de sua família? (Some a sua renda com a dos seus familiares.)
                 #-Q04, #Q4 Quantas pessoas moram em sua casa (incluindo você)?
                 # -Q05, #Q5 A residência de sua família é? propria, alugada etc
                 # removido por estarmos analisando sao paulo capital
                 -Q06, #Q6 A residência de sua família está localizada em? rural urbana etc
                 -Q07, #Q7 Você tem em sua casa? TV em cores
                 -Q08, #Q8 Você tem em sua casa? Videocassete e/ou DVD
                 -Q09, #Q9 Você tem em sua casa? Rádio
                 -Q10, #Q10 Você tem em sua casa? Microcomputador
                 -Q11, #Q11 Você tem em sua casa? Automóvel
                 -Q12, #Q12 Você tem em sua casa? Máquina de lavar roupa
                 -Q13, #Q13 Você tem em sua casa? Geladeira
                 -Q14, #Q14 Você tem em sua casa? Freezer (aparelho independente ou parte da geladeira duplex)
                 -Q15, #Q15 Você tem em sua casa? Telefone fixo
                 -Q16, #Q16 Você tem em sua casa? Telefone celular
                 -Q17, #Q17 Você tem em sua casa? Acesso a internet
                 -Q18, #Q18 Você tem em sua casa? TV por assinatura
                 -Q19, #Q19 Você tem em sua casa? Aspirador em pó
                 -Q20, #Q20 Você tem em sua casa? Empregada mensalista
                 -Q21, #Q21 Você tem em sua casa? Banheiro
                 # -Q22, #Q22 Você exerce ou já exerceu atividade remunerada?
                 # As perguntas Q23 a Q29 indicam um objetivo e nao um fato que pode se relacionar ao resultado da nota
                 -Q23, #Q23 Indique os motivos que levaram você a participar do ENEM:Testar meus conhecimentos
                 -Q24, #Q24 Indique os motivos que levaram você a participar do ENEM:Aumentar a possibilidade de conseguir um emprego
                 -Q25, #Q25 Indique os motivos que levaram você a participar do ENEM:Progredir no meu emprego atual
                 -Q26, #Q26 Indique os motivos que levaram você a participar do ENEM:Ingressar na Educação Superior Pública
                 -Q27, #Q27 Indique os motivos que levaram você a participar do ENEM:Ingressar na Educação Superior Privada
                 -Q28, #Q28 Indique os motivos que levaram você a participar do ENEM:Conseguir uma bolsa de estudos (ProUni, outras)
                 -Q29, #Q29 Indique os motivos que levaram você a participar do ENEM:Participar do Programa de Financiamento Estudantil FIES
                 # -Q30, #Q30 Quantos anos você levou para concluir o Ensino Fundamental?
                 # -Q31, #Q31 Você deixou de estudar durante o Ensino Fundamental?
                 # -Q32, #Q32 Em que tipo de escola você cursou o Ensino Fundamental?
                 # -Q33, #Q33 Quantos anos você levou para concluir o Ensino Médio?
                 # -Q34, #Q34 Você deixou de estudar durante o Ensino Médio?
                 # -Q35, #Q35 Em que tipo de escola você cursou o Ensino Médio?
                 # -Q36, #Q36 Caso você ingresse no Ensino Superior privado pretende recorrer aos auxílios abaixo para custeio das mensalidades? Pró-Uni (Programa Universidade para Todos)
                 # -Q37, #Q37 Caso você ingresse no Ensino Superior privado pretende recorrer aos auxílios abaixo para custeio das mensalidades? Bolsa de estudos da própria Instituição de Ensino Superior
                 # -Q38, #Q38 Caso você ingresse no Ensino Superior privado pretende recorrer aos auxílios abaixo para custeio das mensalidades? Bolsa de estudos da empresa onde trabalho.
                 # -Q39, #Q39 Caso você ingresse no Ensino Superior privado pretende recorrer aos auxílios abaixo para custeio das mensalidades? Auxílio do Programa de Financiamento Estudantil - FIES
                 # -Q40, #Q40 Você cursa ou já cursou a Educação de Jovens e Adultos - EJA?
                 # -Q41, #Q41 Como é ou era o principal curso de EJA que você frequenta ou frequentou?
                 # -Q42, #Q42 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Trabalhava, não tinha tempo de estudar
                 # -Q43, #Q43 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Estudava no curso da empresa e foi interrompido.
                 # -Q44, #Q44 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Ocorreram problemas de saúde ou acidentes comigo familiares
                 # -Q45, #Q45 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Mudei de bairro, cidade ou município.
                 # -Q46, #Q46 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Por motivos pessoais, casamento, filhos, etc.
                 # -Q47, #Q47 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Faltava-me interesse, desisti.
                 # -Q48, #Q48 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Senti-me discriminado(a).
                 # -Q49, #Q49 Indique o que levou você a deixar de cursar a EJA (Se você não deixou de cursar a EJA, vá para a questão...): Temi/Sofri violência.
                 # -Q50, #Q50 Você já frequentou o ensino regular?
                 # -Q51, #Q51 Indique o que levou você a deixar de frequentar o ensino regular: Falta de vaga em escola pública.
                 # -Q52, #Q52 Indique o que levou você a deixar de frequentar o ensino regular: Ausência de escola perto de casa.
                 # -Q53, #Q53 Indique o que levou você a deixar de frequentar o ensino regular: Dificuldades após reprovação.
                 # -Q54, #Q54 Indique o que levou você a deixar de frequentar o ensino regular: Falta de interesse em estudar.
                 # -Q55, #Q55 Indique o que levou você a deixar de frequentar o ensino regular: Falta de condições adequadas na escola.
                 # -Q56, #Q56 Indique o que levou você a deixar de frequentar o ensino regular: Trabalho, falta de tempo para estudar.
                 # -Q57, #Q57 Indique o que levou você a deixar de frequentar o ensino regular: Motivos pessoais, casamento / filhos, etc.
                 # -Q58, #Q58 Indique o que levou você a deixar de frequentar o ensino regular: Falta de apoio familiar.
                 # -Q59, #Q59 Indique o que levou você a deixar de frequentar o ensino regular: Problemas de saúde ou acidente comigo ou familiares.
                 # -Q60, #Q60 Indique o que levou você a deixar de frequentar o ensino regular: Discriminação / Preconceitos (sexo, raça, idade, classe etc.)
                 # -Q61, #Q61 Indique o que levou você a deixar de frequentar o ensino regular: Medo de sofrer violência.
                 # -Q62, #Q62 Quantos anos você tinha quando deixou de frequentar o ensino regular?
  ))
}

###


spCandidatos2012 <- read.csv('./output/spCandidatos2012.csv', na.strings=c(".","")) 

# Removendo outliers
sp.all <- removeOutliers(spCandidatos2012)
sp.all <- convertAvgNota(sp.all)

# removendo atributos não utilizados para a mineração de regras de associação
sp.all <- removerAtributosAnaliseRegrasDeAssoicao(sp.all)
write.csv(sp.all, './output/sp_dfRegrasAssociacao.csv', row.names = FALSE)

