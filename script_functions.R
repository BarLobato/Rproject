
'''
Adrián García Moreno
Martín Garrido Rodríguez-Córdoba
Bárbara Lobato Delgado

Final project: selection bias
Statistics & R Programming
MSc Bioinformatics & Computational Biology
ENS-ISCIII
'''

library(randomForest)
library(cars)
library(rms)

number_of_genes <- 1000
number_of_subjects <- 50
k_fold <- 10



create_data <- function(number_of_genes, number_of_subjects){
  gene_expression_data <- data.frame(matrix(nrow = number_of_subjects, ncol = number_of_genes))
  
  # To create data without a signal, we use a for loop.
  for (i in 1:number_of_genes){
    gene_expression_data[, i] <- rnorm(number_of_subjects, mean = 0, sd = 1)
  }
  
  status <- sample(x = c("nC", "C"), number_of_subjects, replace = TRUE)
  levels(status) <- c(0, 1)
  gene_expression_data <- cbind(status, gene_expression_data)
  return(gene_expression_data)
}

simmulated_data <- create_data(number_of_genes = 1000, number_of_subjects = 50)
head(simmulated_data[,1:6])


select_data_t_test <- function (DF){
  results_table <- data.frame()
  
  for (j in 2:number_of_genes){
    t_test <- t.test(DF[,j] ~ status, data = DF, conf.level = 0.95)
    p_value <- t_test$p.value
    results_table <- rbind(results_table, c(j, p_value))
    names(results_table) <- c("Gene column", "p-value")
  }

# We subset those p-values less than or equal to 0.05. We save them in a data
# frame along with the indexes of the genes in the original dataset. This way, 
# we can subset the dataset and take those 10 genes with lowest p-value.
good_p_values_table <- results_table[which(results_table$`p-value` <= 0.05), ]
good_p_values_table <- good_p_values_table[order(good_p_values_table$`p-value`), ]
best_10_p_values <- good_p_values_table[1:10, ]

return(filtered_simmulated_data)
}

t_test_results <- select_data_t_test(simmulated_data)

# Se rechaza la hipótesis nula cuando el p-valor es menor o igual al nivel de 
# significación (0.05 en este caso). Por lo tanto, primero tenemos que eliminar
# los p-valores mayores de 0.05 y después elegimos los p-valores de menor valor.




