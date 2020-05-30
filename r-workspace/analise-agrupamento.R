# necessário executar primeiro importa-filtra-basededados para obter o df spStudents

spSelectedStudents <- spCandidatos %>%
  select(AVG_NOTA, Q01, Q02, Q03, Q22, Q30, Q31, Q32, Q33, Q34, Q35)

# primeiro é necessário substituir valores ausentes por 0 nas colunas que os possuem
# atributos <fct> serão transformados em numéricos

spSelectedStudents <- spSelectedStudents %>%
  mutate(Q01 = as.factor(Q01)) %>%
  mutate(Q02 = as.factor(Q02)) %>%
  mutate(Q03 = as.factor(Q03)) %>%
  mutate(Q22 = as.factor(Q22)) %>%
  mutate(Q30 = as.factor(Q30)) %>%
  mutate(Q31 = as.factor(Q31)) %>%
  mutate(Q32 = as.factor(Q32)) %>%
  mutate(Q33 = as.factor(Q33)) %>%
  mutate(Q34 = as.factor(Q34)) %>%
  mutate(Q35 = as.factor(Q35))

spSelectedStudents <- spSelectedStudents %>%
  mutate(Q01 = as.numeric(Q01)) %>%
  mutate(Q02 = as.numeric(Q02)) %>%
  mutate(Q03 = as.numeric(Q03)) %>%
  mutate(Q22 = as.numeric(Q22)) %>%
  mutate(Q30 = as.numeric(Q30)) %>%
  mutate(Q31 = as.numeric(Q31)) %>%
  mutate(Q32 = as.numeric(Q32)) %>%
  mutate(Q33 = as.numeric(Q33)) %>%
  mutate(Q34 = as.numeric(Q34)) %>%
  mutate(Q35 = as.numeric(Q35))

spSelectedStudents <- spSelectedStudents %>%
  mutate(AVG_NOTA = replace(AVG_NOTA, is.na(AVG_NOTA), 0)) %>%
  mutate(Q01 = replace(Q01, is.na(Q01), 0)) %>%
  mutate(Q02 = replace(Q02, is.na(Q02), 0)) %>%
  mutate(Q03 = replace(Q03, is.na(Q03), 0)) %>%
  mutate(Q22 = replace(Q22, is.na(Q22), 0)) %>%
  mutate(Q30 = replace(Q30, is.na(Q30), 0)) %>%
  mutate(Q31 = replace(Q31, is.na(Q31), 0)) %>%
  mutate(Q32 = replace(Q32, is.na(Q32), 0)) %>%
  mutate(Q33 = replace(Q33, is.na(Q33), 0)) %>%
  mutate(Q34 = replace(Q34, is.na(Q34), 0)) %>%
  mutate(Q35 = replace(Q35, is.na(Q35), 0))

# normalização dos atributos entre 0 e 1

normalizedStudents <- spSelectedStudents %>%
  mutate(AVG_NOTA = normalize(AVG_NOTA, method = 'range')) %>%
  mutate(Q01 = normalize(Q01, method = 'range')) %>%
  mutate(Q02 = normalize(Q02, method = 'range')) %>%
  mutate(Q03 = normalize(Q03, method = 'range')) %>%
  mutate(Q22 = normalize(Q22, method = 'range')) %>%
  mutate(Q30 = normalize(Q30, method = 'range')) %>%
  mutate(Q31 = normalize(Q31, method = 'range')) %>%
  mutate(Q32 = normalize(Q32, method = 'range')) %>%
  mutate(Q33 = normalize(Q33, method = 'range')) %>%
  mutate(Q34 = normalize(Q34, method = 'range')) %>%
  mutate(Q35 = normalize(Q35, method = 'range'))
  
# aplicação do algoritmo k-means e definição do número ótimo de grupos

library(factoextra)
library(cluster)

# sampledStudents <- sample_n(normalizedStudents, 10000, replace = FALSE)

set.seed(123)

wss <- function(k) {
  print(k)
  withinss <- kmeans(normalizedStudents, k, nstart = 10 )$tot.withinss
  return(withinss)
}

k.values <- 1:20
wss_values <- map_dbl(k.values, wss)

w_map <- data.frame(k.values, wss_values)
w_map <- as_tibble(w_map)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

optimal_clusters <- function(w_map) {
  
  x0 <- w_map$k.values[1]
  y0 <- w_map$wss_values[1]
  
  x1 <- w_map$k.values[20]
  y1 <- w_map$wss_values[20]
  
  distances <- c()
  
  for (k in w_map$k.values) {
    distances[k] <- abs((y1 - y0) * k - (x1 - x0) * w_map$wss_values[k] + x1 * y0 - x0 * y1) / sqrt((y1 - y0)^2 + (x1 - x0)^2)
  }
  
  maximum <- max(distances)
  
  return(which(distances == maximum))
  
}

optimal_clusters(w_map)

cluster <- kmeans(normalizedStudents, optimal_clusters(w_map), nstart = 25)
prototypes <- cluster$centers
prototypes <- as_tibble(prototypes)

ggplot() + 
  geom_point(data = normalizedStudents, mapping = aes(x = Q62, y = NU_NT), color = 'blue') +
  geom_point(data = prototypes, mapping = aes(x = Q62, y = NU_NT), color = 'red')
   
clusteredStudents <- as_tibble(cbind(spStudents, cluster = as.factor(cluster$cluster)))
write.csv(clusteredStudents, '../output/clusteredStudents2012.csv', row.names = FALSE)  
  
# caracterização dos grupos

library(moments)

ggplot(data = filter(clusteredStudents, cluster == 3), mapping = aes(x = Q01, fill = cluster)) + 
  geom_bar()

# calculando modas, tabela geral de clusters

moda <- function(cArray) {
  return(names(which(table(cArray) == max(table(cArray))))[1])
}

summarize(group_by(clusteredStudents, cluster), 
          means = mean(AVG_NOTA),
          Q01 = moda(Q01),
          Q02 = moda(Q02),
          Q03 = moda(Q03),
          Q22 = moda(Q22),
          Q30 = moda(Q30),
          Q31 = moda(Q31),
          Q32 = moda(Q32),
          Q33 = moda(Q33),
          Q34 = moda(Q34),
          Q35 = moda(Q35))
 
  
  
  
  
  
  
  
  

