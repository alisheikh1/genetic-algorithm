generateDictionaryGrid <- function(type = "callgroup.dictionary", dictionary.settings){
  #browser()
  stopifnot(is.list(dictionary.settings))
  if(str_detect(type, "callgroup")){
    columns <- c("tree_num", "is_tree_separate_fold", "description", "target_metric", 
                 "data_filter", "balance_callgroup_data", "balance_callgroup_metric", 
                 "variable_filter", "filteration_calls_perc", "filteration_threshold", 
                 "selection_calls_perc", "selection_threshold", "se_thresh", "min_split_value", 
                 "cp", "method", "parent_split", "callgroup_count", "training_calls_count", 
                 "unknown_calls_count", "skip_variable_selection")
    counter = 1
    for(i in names(dictionary.settings)){
      if(counter == 1){
        setting.name <- i
        setting.values <- dictionary.settings[[i]]
        t <- data.table(id = 1, values = setting.values)
        colnames(t)[2] <- setting.name
      } else {
        setting.name <- i
        setting.values <- dictionary.settings[[i]]
        t1 <- data.table(id = 1, values = setting.values)
        colnames(t1)[2] <- setting.name
        t <- merge(t, t1, by = "id", allow.cartesian = T)
      }
      counter <- counter + 1
    }
    
    t[, id := NULL][]
    t[, tree_num := 1:.N][]
    t[, description := paste0("CG:",tree_num)][]
    
  } else if(str_detect(type, "training")){
    columns <- c("number", "description", "stan_model", "stan_type", "stan_num_chains", 
                 "stan_num_samples", "stan_num_warmup", "stan_callgroup_tree_num", 
                 "stan_is_tree_separate_fold", "stan_breakpoint_col", "stan_data_filter", 
                 "stan_target_metric", "stan_pinning_metric", "stan_fullsave_metric", 
                 "stan_disconnect_metric", "stan_compliment_ln_outcome", "stan_filtered_out_callgroup_criteria", 
                 "stan_agent_calls_cutoff", "stan_zero_agent_calls_min", "agent_relevancy_threshold" )
    counter = 1
    for(i in names(dictionary.settings)){
      if(counter == 1){
        setting.name <- i
        setting.values <- dictionary.settings[[i]]
        t <- data.table(id = 1, values = setting.values)
        colnames(t)[2] <- setting.name
      } else {
        setting.name <- i
        setting.values <- dictionary.settings[[i]]
        t1 <- data.table(id = 1, values = setting.values)
        colnames(t1)[2] <- setting.name
        t <- merge(t, t1, by = "id", allow.cartesian = T)
      }
      counter <- counter + 1
    }
    
    t[, id := NULL][]
    t[, number := 1:.N][]
    t <- merge(t, callgroups.dictionary[,.(tree_num, is_tree_separate_fold)], by.x = "stan_callgroup_tree_num", by.y = "tree_num", all.x = T)
    t[, `:=`(stan_is_tree_separate_fold = is_tree_separate_fold, is_tree_separate_fold = NULL)]
    t[, description := paste0("Model:",number)][]
    t[, stan_data_filter := ifelse(is.na(stan_data_filter) | stan_data_filter == "" | stan_data_filter == T, paste0("!is.na(", stan_target_metric, ")"),
                                   paste0(stan_data_filter, " & !is.na(", stan_target_metric, ")"))]
  }else if(str_detect(type, "validation")){
    columns <- c("number", "description", "agent_training_number", "callgroup_training_number", 
                 "target_metric", "unknown_agent_percentile", "unknown_agent_calls_cutoff", 
                 "unknown_agent_method", "agent_split", "agent_lookahead_filter", 
                 "agent_ranking_method", "use_pca_params", "splitwise_params", 
                 "rate_card", "rate_card_param", "rate_card_filter", "raw_callgroup_tree", 
                 "raw_callgroup_parameter", "raw_callgroup_filter", "np_callgroup_tree", 
                 "np_agent_parameter", "np_agent_filter", "callgroup_subsplit", 
                 "callgroup_ranking_method", "is_tree_separate_fold", "stan_compliment_ln_outcome", 
                 "unknown_callgroup_percentile", "callgroup_split", "callgroup_lookahead_filter", 
                 "data_filter", "eval_score_filter", "validation_method", "validation_split", 
                 "validation_lift_type", "split_bm", "zero_agent_percentile")
    counter = 1
    for(i in names(dictionary.settings)){
      if(counter == 1){
        setting.name <- i
        setting.values <- dictionary.settings[[i]]
        t <- data.table(id = 1, values = setting.values)
        colnames(t)[2] <- setting.name
      } else {
        setting.name <- i
        setting.values <- dictionary.settings[[i]]
        t1 <- data.table(id = 1, values = setting.values)
        colnames(t1)[2] <- setting.name
        t <- merge(t, t1, by = "id", allow.cartesian = T)
      }
      counter <- counter + 1
    }
    #browser()
    t[, id := NULL][]
    t[, number := 1:.N][]
    t <- merge(t, models$training.dictionary[,.(number, stan_is_tree_separate_fold)], by.x = "agent_training_number", by.y = "number", all.x = T)
    t[, `:=`(is_tree_separate_fold = stan_is_tree_separate_fold, stan_is_tree_separate_fold = NULL)]
    t[, description := paste0("Validation:",number)][]
    t[, data_filter := ifelse(is.na(data_filter) | data_filter == "" | data_filter == T, paste0("!is.na(", target_metric, ")"),
                              paste0(data_filter, " & !is.na(", target_metric, ")"))]
  }
  
  return(t)
}


dictionary.settings <- list("tree_num" = 1, "is_tree_separate_fold" = c(T, F), "description" = 1, "target_metric" = c("issued_value", "mt_pred_rc", "st_pred_issave"), 
                            "data_filter" = T, "balance_callgroup_data" = F, "balance_callgroup_metric" = NA, 
                            "variable_filter" = T, "filteration_calls_perc" = 0.5, "filteration_threshold" = 0.00001, 
                            "selection_calls_perc" = 0.5, "selection_threshold" = 0.0001, "se_thresh" = 0, "min_split_value" = c(0.01, 0.1, 0.0002), 
                            "cp" = 0.000001, "method" = "anova", "parent_split" = NA, "skip_variable_selection" = c(T,F))

callgroups.dictionary <- generateDictionaryGrid("callgroup.dictionary", dictionary.settings = dictionary.settings)


dictionary.settings <- list("number" = 1, "description" = 1, "stan_model" = "binomial_1pl_1", "stan_type" = "sample", "stan_num_chains" = 4, 
                            "stan_num_samples" =300, "stan_num_warmup" = 300, "stan_callgroup_tree_num" = callgroups.dictionary$tree_num, 
                            "stan_is_tree_separate_fold" = T, "stan_breakpoint_col" = NA, "stan_data_filter" = "zero_agent_filter == F & remove_day_filter == F & model_agent_filter == T & dev_test_set == F", 
                            "stan_target_metric" = c("phn_issave", "st_pred_issave"), "stan_pinning_metric" = NA, "stan_fullsave_metric" = NA, 
                            "stan_disconnect_metric" = NA, "stan_compliment_ln_outcome" = NA, "stan_filtered_out_callgroup_criteria" = NA, 
                            "stan_agent_calls_cutoff" = 30, "stan_zero_agent_calls_min" = NA, "agent_relevancy_threshold" = c(NA, 3000)
)

models$training.dictionary <- generateDictionaryGrid("training.dictionary", dictionary.settings = dictionary.settings)
View(models$training.dictionary)


dictionary.settings <- list("number" = 1, "description" = 1, "agent_training_number" = models$training.dictionary$number, "callgroup_training_number" = NA, 
                            "target_metric" = c("issued_value", "mt_pred_rc"), "unknown_agent_percentile" = 0.5, "unknown_agent_calls_cutoff" = NA, 
                            "unknown_agent_method" = NA, "agent_split" = NA, "agent_lookahead_filter" = T, 
                            "agent_ranking_method" ="z_score", "use_pca_params" = F, "splitwise_params" = F, 
                            "rate_card" = NA, "rate_card_param" = NA, "rate_card_filter" = NA, "raw_callgroup_tree"= 1, 
                            "raw_callgroup_parameter" = "issued_value", "raw_callgroup_filter" = T, "np_callgroup_tree" = T, 
                            "np_agent_parameter" = T, "np_agent_filter" = T, "callgroup_subsplit" = NA, 
                            "callgroup_ranking_method" ="-1*callgroup_z_score_raw", "is_tree_separate_fold" = T, "stan_compliment_ln_outcome" = NA, 
                            "unknown_callgroup_percentile" = 0.5, "callgroup_split" = NA, "callgroup_lookahead_filter" = T, 
                            "data_filter" = "!is.na(mt_pred_rc) & dev_test_set == F", "eval_score_filter" = "eval_score_filter == 1", "validation_method" = "differential", "validation_split" = NA, 
                            "validation_lift_type" = "relative", "split_bm" = 0.8, "zero_agent_percentile" = 0.99)

models$validation.dictionary <- generateDictionaryGrid("validation.dictionary", dictionary.settings = dictionary.settings)



population_models <- models$training.dictionary$number
validation_folders <- models$validation.dictionary[agent_training_number %in% population_models, .(number, agent_training_number)]
validation_folders <- validation_folders[,max_number := max(number), agent_training_number][number == max_number][, number]
validation_folders <- paste0("validation_", validation_folders)
validation_paths <- glue::glue("{model.settings$location}/{validation_folders}")

x <- validation_paths[1]
folders <- "fold1" # read name of folders in the validation path
fold_validation_folder <- glue::glue("{x}/{folders}/agent_ranks.rds")
agent_ranks <- lapply(validation_paths, function(x) {
  folders <- "fold1" # read name of folders in the validation path
  fold_validation_folder <- glue::glue("{x}/{folders}/agent_ranks.rds")
  return(readRDS(fold_validation_folder))
  
})

names(agent_ranks) <- validation_folders 
agent_ranks <- lapply(agent_ranks, function(x) x[, rank := frank(agent_z_score, ties.method = "dense")])
agent_ranks <- lapply(names(agent_ranks), function(x) agent_ranks[[x]][, model_ := x])
names(agent_ranks) <- validation_folders


View(agent_ranks)
population <- agent_ranks 
population <- lapply(population, function(x) x[,.(model_, agentid, rank)])
population <- rbindlist(population)
population_og <- dcast(population, formula = agentid~model_, value.var = "rank")
# View(population)
population_length <- length(population)
validation_data <- complete.data[training_fold_1 == F]

population <- copy(population_og)
for(i in colnames(population)){
  if(i != "agentid"){
    # population[, (i) := as.character(get(i))]
    population[, (i) := paste0(i, "_", get(i))]
  }
}




getbacktoranks <- function(population){
  pop <- copy(population)
  agentid_vector <- pop$agentid
  rank_df <- as.data.frame(do.call(cbind, lapply(pop[,!c("agentid")], function(x) as.numeric(str_remove(x, "validation_[0-9]+_")))))
  dna_df <- as.data.frame(do.call(cbind, lapply(pop[,!c("agentid")], function(x) as.character(str_extract(x, "validation_[0-9]+")))))
  rank_df$agentid <- agentid_vector
  dna_df$agentid <- agentid_vector
  
  return(list(rank_df = setDT(rank_df), dna_df = setDT(dna_df)))
}


population_alt <- getbacktoranks(population)


calculateFitness <- function(rank_df, validation_data, opt = "st_pred_issave", generation = 1L){
  pop <- copy(rank_df)
  dnas <- colnames(pop)[! (colnames(pop) %like% 'agentid')]
  fitness_record <- data.table()
  validation_data <- validation_data[on_off == 0]
  FitnessLoop <- function(i){
    cols <- c("agentid", i)
    temp <- pop[,..cols]
    temp[, rank := frank(get(i), ties.method = "dense")]
    temp[, percentile := (rank - 0.5)/.N]
    v <- copy(validation_data)
    cols <- c("agentid", opt)
    v <- merge(v[,..cols], temp[,.(agentid, percentile)], by = "agentid", all.x = T)
    fitness_df <- v[!is.na(percentile) & !is.na(get(opt)), .(target = mean(get(opt), na.rm = T)), keyby = .(ap_bucket = floor(percentile * 50))]
    fitness <- cor(fitness_df$ap_bucket, fitness_df$target)
    return(data.table(dna = i, generation = generation, fitness = fitness))
  }
  
  x <- rbindlist(mclapply(dnas, FitnessLoop, mc.cores = 10L))
  x[, prob := fitness/sum(fitness)]
  x[, cumulative_prob := cumsum(prob)]
}

fitness_df <- calculateFitness(rank_df = population_alt$rank_df, validation_data = validation_data, opt = "issued_value", generation = 1L)

selectdna <- function(fitness_df){
  fitness_df <- copy(fitness_df)
  r <- runif(1)
  fitness_df <- copy(fitness_df)
  fitness_df[, score := abs(cumulative_prob - r)]
  return(fitness_df[score == min(score), dna])
}

crossover <- function(n, fitness_df, population){
  print(paste0("crossover number :", n))
  dna1 <- selectdna(fitness_df)
  dna1 <- population[, get(dna1)]
  dna2 <- selectdna(fitness_df)
  dna2 <- population[, get(dna2)]
  dnachild <- c()
  midpoint = floor(runif(1, 1, nrow(population)))
  
  for(i in 1:nrow(population)){
    if(i > midpoint){
      dnachild <- c(dnachild, dna1[i])
    } else {
      dnachild <- c(dnachild, dna2[i])
    }
  }
  dnachild <- mutategene(dnachild, 0.005)
  childdna <- data.table(dnachild)
  
  return(childdna)
}

mutategene <- function(dna, mutationrate){
  
  swapdna <- function(dna, from, to){
    temp_from <- dna[from]
    temp_to <- dna[to]
    
    dna[from] = temp_to
    dna[to] = temp_from
    return(dna)
  }
  
  for(i in 1:length(dna)){
    if(runif(1) < mutationrate){
      print("Mutating Gene")
      dna <- swapdna(dna, from = i, to = as.integer(runif(1, 1, length(dna))))
    }
  }
  return(dna)
}
p <- population
generation_fitness_tracker <- data.table()
best_dna <- c()
best_fitness <- 0
for(counter in 1:20){
  newpopulation <- do.call(cbind, mclapply(1:ncol(population), crossover, fitness_df, population, mc.cores = 1L))
  colnames(newpopulation) <- as.character(paste0("gene_",1:ncol(population)))
  newpopulation$agentid <- population$agentid
  newpopulation_alt <- getbacktoranks(newpopulation)
  newfitness_df <- calculateFitness(newpopulation_alt$rank_df, validation_data, opt = "issued_value", generation = counter)
  generation_fitness_tracker <- rbind(generation_fitness_tracker, data.table(generation = counter, average_fitness = mean(newfitness_df$fitness), max_fitness = max(newfitness_df$fitness)))
  max_fitness <- head(newfitness_df[fitness == max(fitness), .(dna, fitness)],1)
  max_fitness_dna <- newpopulation[, get(max_fitness$dna)]
  if(max_fitness$fitness > best_fitness){
    best_dna <- max_fitness_dna
    best_fitness <- max_fitness$fitness
  }
  
  print(generation_fitness_tracker)
  population <- newpopulation
  fitness_df <- newfitness_df
}


foldagentranks <- function(fold, validation_folders){
  validation_paths <- glue::glue("{model.settings$location}/{validation_folders}")
  fold <- paste0("fold",fold)
  agent_ranks <- lapply(validation_paths, function(x) {
    folders <- fold # read name of folders in the validation path
    fold_validation_folder <- glue::glue("{x}/{folders}/agent_ranks.rds")
    return(readRDS(fold_validation_folder))
    
  })
  names(agent_ranks) <- validation_folders
  agent_ranks <- lapply(agent_ranks, function(x) x[, rank := frank(agent_z_score, ties.method = "dense")])
  agent_ranks <- lapply(validation_folders, function(x) agent_ranks[[x]][, model_ := x])
  names(agent_ranks) <- validation_folders
  agent_ranks <- lapply(agent_ranks, function(x) x[,.(model_, agentid, rank)])
  agent_ranks <- rbindlist(agent_ranks)
  agent_ranks <- dcast(agent_ranks, formula = agentid~model_, value.var = "rank")
  return(agent_ranks)
}
fold_agent_ranks <- lapply(1:5, foldagentranks, validation_folders = validation_folders)

StichResults <- function(fold){
  print(fold)
  agent_rank <- fold_agent_ranks[[fold]]
  this_fold_optimized_agent_ranks <- data.table()
  for(j in 1:nrow(fold_agent_ranks[[1]])){
    ag <- x$dna_df[j, agentid]
    gene <- as.character(x$dna_df[j, gene])
    optimized_rank <- agent_rank[agentid == ag, get(gene)]
    this_fold_optimized_agent_ranks <- rbind(this_fold_optimized_agent_ranks, data.table(agentid = ag, gene = gene, rank = optimized_rank))
  }
  return(this_fold_optimized_agent_ranks)
}

p <- lapply(1:5, StichResults)
names(p) <- paste0("fold",1:(data.config$num.folds+1))

path <- normalizePath(model.settings$location)
system(glue::glue("mkdir {path}/validation_-1"))

for(i in names(p)){
  saveRDS(p[[i]], glue::glue("{path}/validation_-1/{i}.rds"))
}
