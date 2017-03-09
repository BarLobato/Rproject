
'''
Adrián García Moreno
Martín Garrido Rodríguez-Córdoba
Bárbara Lobato Delgado

Final project: selection bias
Statistics & R Programming
MSc Bioinformatics & Computational Biology
ENS-ISCIII
'''

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
print(head(gene_expression_df))
