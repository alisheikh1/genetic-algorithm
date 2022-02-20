library(data.table)
library(tictoc)
DNA <- list(
  value = c(""),
  fitness = 0,
  phrase = ""
)
source("C:/Users/12027/Desktop/studies/geneticAlgorithm/R/dna.r")
source("C:/Users/12027/Desktop/studies/geneticAlgorithm/R/population.r")
target_phrase <- "to be or not to be."
target <- unlist(strsplit(target_phrase, ""))
max_population <- 200
mutation_rate = 0.004

population <- constructPopulation(max_population, mutation_rate = mutation_rate, target = target)
# samp<-population
# samp$population[["1"]]$phrase <- "hellohello"
# samp$population[["2"]]$phrase <- "worldworld"
# samp$population[["1"]]$value <- unlist(strsplit(samp$population[["1"]]$phrase, ""))
# samp$population[["2"]]$value <- unlist(strsplit(samp$population[["2"]]$phrase, ""))
# 
# 
# 
# population <- calcFitness(samp)
while(population$finished == F){
  tic()
population <- naturalSelection(population)
#toc()
population <- calcFitness(population)
# toc()
best_dna_id <- getBestDNA(population$population)
# toc()
best_dna <- population$population[[best_dna_id$max_fitness_dna]]
if(best_dna$phrase == target_phrase){
  population$finished = T
}
toc()
cat("Population Generation", population$generations, "\n")
cat("Population Best Fitness", best_dna$phrase, " fitness score : ", population$best, " avg fitness : ", population$avg_fitness, "\n")
}

