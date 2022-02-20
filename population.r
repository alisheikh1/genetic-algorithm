constructPopulation <- function(pop_max, mutation_rate, target){
  population <- list()
  population$population <- list()
  population$generations <- 0
  population$finished <- F
  population$target <- target
  population$mutation_rate <- mutation_rate
  population$perfect_score <- 1
  population$best <- ""
  
  for(i in 1:pop_max){
    population$population[[as.character(i)]] = constructDNA(DNA, target = target)
  }
  population$matingPool = c()
  population <- calcFitness(population)
  return(population)
}

calcFitness <- function(population){
  for(i in 1:length(population$population)){
    population$population[[as.character(i)]] <- calculateFitness(DNA = population$population[[as.character(i)]], target = population$target)
    population$population[[as.character(i)]] <- getPhrase(population$population[[as.character(i)]])
  }
  
  return(population)
}

naturalSelection <- function(population){
  
  next_population <- population
  next_population$generations <- next_population$generations+1L
  fitness_vector <- unlist(lapply(population$population, function(x) x[["fitness"]]))
  avg_fitness <- mean(fitness_vector)
  fitness_vector2 <- floor(fitness_vector*100)
  gene_pool <- c()
  for(i in 1:length(fitness_vector2)){
    dna_n <- names(fitness_vector2)[i]
    gene_pool <- c(gene_pool, rep(dna_n, fitness_vector2[i]))
  }
  
  next_population$population <- list()
  
  for( i in 1:length(population$population)){
   
    parentDNA1 <- sample(gene_pool, 1)
    parentDNA1 <- population$population[[parentDNA1]]
    parentDNA2 <- sample(gene_pool, 1)
    parentDNA2 <- population$population[[parentDNA2]]
    
    childDNA <- crossOver(parentDNA1, parentDNA2)
    childDNA <- mutateGene(DNA = childDNA, mutationRate = population$mutation_rate)
    next_population$population[[as.character(i)]] <- childDNA 
    
    #fitness_table[,`:=`(score1 = NULL, score2 = NULL)]
  }
  next_population$avg_fitness <- avg_fitness
  next_population$best <- max(fitness_vector)
  return(next_population)
}
