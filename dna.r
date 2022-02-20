# Generate a new character
generateNewChar <- function(){
  c = floor(runif(1, 63, 122))
  if (c == 63) c = 32
  if (c == 64) c = 46

  return(rawToChar(as.raw(c)))

}

# Construct a DNA with random phrase 
constructDNA <- function(DNA, target){
  for(i in 1:length(target)){
    DNA$value[i] = generateNewChar()}
  
   DNA$fitness = 0
   return(DNA)
}

# convert character vector into string
getPhrase <- function(DNA){
  DNA$phrase <- paste0(DNA$value, collapse = "")
  return(DNA)
}

# calculate how close a particular DNA (phrase) is to the target 
calculateFitness <- function(DNA, target){
  score = 0
  for(i in 1:length(target)){
    if(DNA$value[i] == target[i])
      DNA$fitness <- DNA$fitness + 1L
  }
  DNA$fitness <- DNA$fitness / length(target)
  return(DNA)
}

# return the DNA that has highest fitness score
getBestDNA <- function(gene_pool){
  fitness_table <- data.table(
    dna_n = names(gene_pool),
    fitness = as.numeric(unlist(lapply(gene_pool, function(x) x[["fitness"]])))
  )
  max_fitness = max(fitness_table$fitness)
  return(list(max_fitness = max_fitness, max_fitness_dna = head(fitness_table[fitness == max_fitness, dna_n], 1)))
}


# stick together two parent DNAs and return a new child dna
crossOver <- function(DNA1, DNA2){
  childDNA = list(
    value = c(""),
    fitness = 0,
    phrase = ""
  )
  
  midpoint = floor(runif(1, 1, length(target)))
  
  for(i in 1:length(target)){
    if(i > midpoint){
      childDNA$value[i] = DNA1$value[i]
    } else {
      childDNA$value[i] = DNA2$value[i]
    }
  }
  
  return(childDNA)
}

# mutate gene in a dna
mutateGene <- function(DNA, mutationRate){
  for(i in 1:length(DNA$value)){
    if(runif(1)<mutationRate){
      DNA$value[i] <- generateNewChar()
    }
  }
  return(DNA)
}


