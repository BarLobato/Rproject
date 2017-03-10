
'''
Adrián García Moreno
Martín Garrido Rodríguez-Córdoba
Bárbara Lobato Delgado

Final project: selection bias
Statistics & R Programming
MSc Bioinformatics & Computational Biology
ENS-ISCIII
'''

# En caso de que diera problemas, abrir RStudio como superusuario. 
install.packages("randomForest")
library(randomForest)

create_gene_expression_dataset <- function (nsubjects, ngenes, gene_sim_datasets) {
  sim_dataset <- matrix(data = rnorm(nsubjects*ngenes), nrow = nsubjects, ncol = ngenes)
  sim_dataset <- as.data.frame(sim_dataset)
  names(sim_dataset) <- gene_sim_datasets
  sim_dataset <- cbind("status" = rbinom(nsubjects, 1, 0.5), sim_dataset)
  return(sim_dataset)
}

gene_expression_df <- create_gene_expression_dataset(nsubjects = 1000, ngenes = 50, gene_sim_datasets = NA)

# Hace una t test para cada gen. Sin embargo, en todos los casos se rechaza la hipótesis
# nula. Esto es un problema porque no todos los genes pueden ser significativos. 
# Hay que elegir unos cuantos. 
for (i in 2:51) {
  result_t_test <- t.test(gene_expression_df[,i] ~ status, data = gene_expression_df, conf.level = 0.99)
  print(result_t_test)
}
# Si el problema del test de Student fuesen los datos, porque se han generado
# aleatoriamente, podríamos buscar en Internet datos de expresión de genes de
# pacientes con cáncer y sin cáncer. Si la tabla fuese demasiado grande, podemos 
# extraer una submuestra de forma aleatoria, utilizando la función sample sin
# reemplazamiento. 
# En un caso real, la gran mayoría de los genes medidos no van a ser relevantes en el
# desarrollo del cáncer. Es por ello que creo que deberíamos solucionar el problema
# de los valores de la t de Student.
