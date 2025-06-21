library(httr)

setwd("~/Library/CloudStorage/Box-Box/Xi/LLM")

######################################################################################
######################################################################################
# rare disease
######################################################################################
######################################################################################
# read data
train <- readRDS('data/rare_train.rds')
test <- readRDS('data/rare_test.rds')
orphar <- read.csv('data/orphar.csv')
colnames(orphar)[3] <- 'content'


# read embedding
embedding.train <- readRDS('result/embedding.train.rds')
embedding.test <- readRDS('result/embedding.test.rds')
embedding.orphar <- readRDS('result/embedding.orpha.rds')

# distance function
euclidean_distance <- function(v1, v2) {
  sqrt(sum((v1 - v2)^2))
}

######################################################################################
# distance between each train test and all test text
######################################################################################
distances <- c()
for(i in 1:length(embedding.train)){
  #i <- 4
  distances[i] <- mean(sapply(embedding.test, euclidean_distance, v2 = embedding.train[[i]]))
}

######################################################################################
# analyze rare disease
######################################################################################
task <- 'Task: Identify the names of rare diseases from the following text.\n'
guidance <- 'Definition: A rare disease is a health condition that affects a small percentage of the population. In the U.S., a disease is considered rare if it affects fewer than 200,000 people. In European Union, a disease is considered rare if it affects fewer than 1 in 2,000 people.\n'
format <- 'Output format: Output only the exact rare disease names without any additional changes. If there are multiple rare diseases, separate their names with commas. If there is no rare disease, output \'none\'.\n'
error <- 'Guidance: Treat abbreviations as separate rare disease names. Do not identify regular diseases as rare diseases.\n\n'
prefix <- '\nThe text from which you need to exact the names of rare diseases is: '
shot.train <- 4
shot.orphar <- 2
result.rare <- c()

system.time({
  set.seed(2024)
  for(i in 1:208){
    #i <- 1
    print(i)
    a <- test$text[[i]]
    question <- paste(task, format, prefix, a, sep = '')
    #cat(question)

    # knn pick up orphar
    distances <- sapply(embedding.orphar, euclidean_distance, v2 = embedding.test[[i]])
    neighbor <- order(distances, decreasing = F)[1:shot.orphar]  # nearest
    snippets <- c()
    for(j in neighbor){
      #j <- neighbor[1]
      text <- orphar$content[j]
      snippets <- c(snippets, text)
    }
    snippets <- paste0("\nSnippet ", seq_along(snippets), ".\n", snippets)
    snippets <- paste(snippets, collapse = ""); snippets
    snippets <- paste('Here are knowledge snippets:', snippets, sep = '')
    question <- paste(task, guidance, format, error, snippets, prefix, a, sep = '\n')
    #cat(question)
    
    # knn pick up
    distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
    neighbor <- order(distances, decreasing = F)[1:shot.train]  # nearest
    examples <- c()
    for(j in neighbor){
      #j <- neighbor[1]
      text <- train[["text"]][[j]]
      type <- train[["label"]][[j]]
      entry <- paste(type$entry[type$type=='RAREDISEASE'], collapse = ', '); entry
      example <- paste('Input text: ', text, ' Output: ', entry, '.\n',sep = ''); example
      examples <- c(examples, example)
    }
    examples <- paste0("Example ", seq_along(examples), ".\n", examples)
    examples <- paste(examples, collapse = "")
    examples <- paste('\n\n Here are demonstration shots:\n', examples, sep = '')
    question <- paste(task, guidance, format, error, snippets, examples, prefix, a, sep = ' ')
    #question <- paste(task, guidance, format, error, examples, prefix, a, sep = ' ')
    #cat(question)

    response <- POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        #model = "gpt-3.5-turbo",
        model = "gpt-4o-2024-05-13",
        #model = "ft:gpt-4o-mini-2024-07-18:loyola-university-chicago:1:9quhR4mO",
        temperature = 0,
        messages = list(list(
          role = "user", 
          content = question
        ))
      )
    )
    if(!is.null(content(response)$choices[[1]]$message$content)){
      result.rare[i] <- content(response)$choices[[1]]$message$content
    }
  }
})
result.rare
result.rare <- tolower(result.rare)
saveRDS(result.rare, 'result/rare/KB2_pubmed.shot4.rds')

######################################################################################
# precision
######################################################################################
text <- c()
model <- c()
compare <- c()

for(i in 1:208){
  #i <- 1
  result <- result.rare[i]; result
  # only check identified entry
  if(result!='none'){
    result <- trimws(unlist(strsplit(result, split = ","))); result
    text <- c(text, rep(names(test$label)[i], length(result)))
    for(r in result){
      #r <- result[1]; r
      type <- test$label[[i]]; type
      type <- type[type$type=='RAREDISEASE',]; type
      compare <- c(compare, any(r%in%type$entry))
      model <- c(model, r)
    }
  }
}
result.prec <- data.frame(text, model, compare)
prec <- mean(result.prec$compare); prec

######################################################################################
# recall
######################################################################################
text <- c()
entry <- c()
compare <- c()
for(i in 1:208){
  #i <- 190
  result <- result.rare[i]  
  result <- trimws(unlist(strsplit(result, split = ",")))
  type <- test$label[[i]]
  type <- type[type$type=='RAREDISEASE',]
  if(dim(type)[1]>0){
    entry <- c(entry, type$entry)
    text <- c(text, rep(names(test$label)[i], length(type$entry)))
    for(e in type$entry){
      #e <- entry[1]
      compare <- c(compare, any(e%in%result))
    }
  } 
}
result.recall <- data.frame(text, entry, compare)
recall <- mean(result.recall$compare); recall
F1 <- 2*prec*recall/(prec+recall); F1

######################################################################################
# error analysis
######################################################################################
result.rare <- readRDS('result/rare/basic.guidance.error.knn16.4o.rds')
result.disease <- readRDS('result/disease/basic.guidance.error.knn8.withrare.4o.rds')
result.sign <- readRDS('result/sign/basic.knn10.4o.rds')
result.symptom <- readRDS('result/symptom/basic.guidance.error.counter.knn14.4o.rds')

# rare
compare.rare <- data.frame()
for(i in 1:208){
  #i <- 2
  type <- test$label[[i]]; type
  type <- type[type$type=='RAREDISEASE',]; type
  if(dim(type)[1]==0){
    type <- data.frame(type_raw='none', entry='none', type='none')
  }
  type$number <- i
  result <- result.rare[i]; result
  type$result <- result
  compare.rare <- rbind(compare.rare, type)
}

# disease
compare.disease <- data.frame()
for(i in 1:208){
  #i <- 2
  type <- test$label[[i]]; type
  type <- type[type$type=='DISEASE',]; type
  if(dim(type)[1]==0){
    type <- data.frame(type_raw='none', entry='none', type='none')
  }
  type$number <- i
  result <- result.disease[i]; result
  type$result <- result
  compare.disease <- rbind(compare.disease, type)
}

# sign
compare.sign <- data.frame()
for(i in 1:208){
  #i <- 2
  type <- test$label[[i]]; type
  type <- type[type$type=='SIGN',]; type
  if(dim(type)[1]==0){
    type <- data.frame(type_raw='none', entry='none', type='none')
  }
  type$number <- i
  result <- result.sign[i]; result
  type$result <- result
  compare.sign <- rbind(compare.sign, type)
}

# symptom
compare.symptom <- data.frame()
for(i in 1:208){
  #i <- 2
  type <- test$label[[i]]; type
  type <- type[type$type=='SYMPTOM',]; type
  if(dim(type)[1]==0){
    type <- data.frame(type_raw='none', entry='none', type='none')
  }
  type$number <- i
  result <- result.symptom[i]; result
  type$result <- result
  compare.symptom <- rbind(compare.symptom, type)
}

compare <- rbind(compare.rare, compare.disease, compare.sign, compare.symptom)
compare <- compare[order(compare$number), ]

############################################################
## Five-way error analysis script
## (Correct · Boundary · Type · Boundary+Type · Spurious · Missed)
############################################################

library(tidyverse)
library(stringr)

## ---------- 1 · load ---------------------------------------------------------
# columns expected: number, type, entry, result
df <- compare

## ---------- 2 · helper functions --------------------------------------------
split_pred <- function(x) {
  x <- trimws(tolower(x))
  if (x %in% c("none", "null", "")) character(0)
  else str_trim(unlist(str_split(x, ",")))
}

tokenise <- function(x) str_split(x, "\\s+")[[1]]

classify_pair <- function(gold, pred, wrong_type) {
  exact <- str_to_lower(gold) == str_to_lower(pred)
  if (exact  && !wrong_type) "Correct"
  else if (exact)            "Type"
  else if (!wrong_type)      "Boundary"
  else                       "Boundary+Type"
}

## ---------- 3 · sentence × class aggregation --------------------------------
pair_tbl <- df %>%
  mutate(entry = na_if(tolower(entry),  "none"),
         entry = na_if(entry,  "null")) %>%
  group_by(number, type) %>%
  summarise(
    gold = list(na.omit(entry)),                 # vector of gold strings
    pred = list(split_pred(first(result))),      # vector of predictions
    .groups = "drop"
  )

## ---------- 4 · per-entity comparison ---------------------------------------
process_group <- function(gold_vec, pred_vec, this_type, full_df) {
  used_pred <- rep(FALSE, length(pred_vec))
  out <- tibble(entity = character(), status = character())
  
  # loop over gold entities
  for (g in seq_along(gold_vec)) {
    g_tok <- tokenise(gold_vec[g])
    overlap <- map_lgl(pred_vec, ~ length(intersect(g_tok, tokenise(.x))) > 0)
    
    if (any(overlap)) {
      idx <- which(overlap)[1]
      used_pred[idx] <- TRUE
      wrong_type <- any(full_df$entry == gold_vec[g] &
                          full_df$type  != this_type)
      lab <- classify_pair(gold_vec[g], pred_vec[idx], wrong_type)
      out <- add_row(out, entity = gold_vec[g], status = lab)
    } else {                                   # Missed gold
      out <- add_row(out, entity = gold_vec[g], status = "Missed")
    }
  }
  
  # remaining predictions are Spurious
  if (any(!used_pred)) {
    out <- bind_rows(out,
                     tibble(entity = pred_vec[!used_pred],
                            status = "Spurious"))
  }
  out
}

err_df <- pair_tbl %>%
  mutate(err = pmap(list(gold, pred, type),
                    ~ process_group(..1, ..2, ..3, df))) %>%
  select(number, type, err) %>%
  unnest(err)

## ---------- 5 · summary & visualisation -------------------------------------
plot_df <- err_summary %>% 
  filter(type != "none") %>% 
  mutate(
    type   = factor(type,
                    levels = c("RAREDISEASE","DISEASE","SIGN","SYMPTOM"),
                    labels = c("Rare disease","Disease","Sign","Symptom")),
    status = factor(status,
                    levels = rev(c("Correct","Boundary","Boundary+Type",
                               "Type","Missed","Spurious")))
  )

# colour-blind–safe palette from Okabe & Ito
err_cols <- c(
  Correct        = "#009E73",
  Boundary       = "#E69F00",
  `Boundary+Type`= "#A6761D",
  Type           = "#CC79A7",
  Missed         = "#56B4E9",
  Spurious       = "#0072B2"
)

## 1 · find a common right-hand limit (max proportion rounded up)
x_max <- plot_df$prop %>% max() %>% scales::pretty_breaks(n = 5)() %>% max()

## 2 · redraw
ggplot(plot_df, aes(status, prop, fill = status)) +
  geom_col(width = 0.65) +
  facet_wrap(~ type, ncol = 2) +        # same scale for all panels
  coord_flip() +
  scale_fill_manual(values = err_cols, name = NULL) +
  scale_y_continuous(limits = c(0, x_max), expand = expansion(mult = c(0, .02))) +
  labs(x = NULL, y = "\nProportion") +
  theme_minimal(base_size = 13) +       # larger default text
  theme(
    axis.text   = element_text(colour = "black"),  
    strip.text  = element_text(face = "bold", colour = "black", size=12),
    legend.position = "none", panel.spacing.x  = unit(1.5, "cm")
  )

## ---------- 6 · export detailed errors --------------------------------------
write_csv(err_df, "error_breakdown_per_entity.csv")



######################################################################################
######################################################################################
# Disease
######################################################################################
######################################################################################
# read data
train <- readRDS('data/rare_train.rds')
test <- readRDS('data/rare_test.rds')

# read embedding
embedding.train <- readRDS('result/embedding.train.rds')
embedding.test <- readRDS('result/embedding.test.rds')

# distance function
euclidean_distance <- function(v1, v2) {
  sqrt(sum((v1 - v2)^2))
}

######################################################################################
# distance between each train test and all test text
######################################################################################
distances <- c()
for(i in 1:length(embedding.train)){
  #i <- 4
  distances[i] <- mean(sapply(embedding.test, euclidean_distance, v2 = embedding.train[[i]]))
}

######################################################################################
# analyze disease
######################################################################################
task <- 'Task: Identify the names of diseases mentioned in the following text.\n'
guidance <- 'Definition: A disease is a condition of the body or mind that impairs normal functioning and is characterized by specific signs and symptoms. Diseases can be caused by a variety of factors, including infections, genetic mutations, environmental factors, and lifestyle choices.\n'
format <- 'Output format: Output only the exact disease names without any additional changes. If there are multiple diseases, separate their names with commas. If there is no disease, output \'none\'.\n'
error <- 'Guidance: Differentiate between rare diseases and diseases. A rare disease is a health condition that affects a small percentage of the population. Rare diseases are a subset of diseases. Only output diseases, not rare diseases.\n'
prefix <- 'The text from which you need to exact the names of diseases is: '
shot.train <- 1
shot.orphar <- 2
result.rare <- c()

system.time({
  set.seed(2024)
  for(i in 1:208){
    #i <- 1
    print(i)
    a <- test$text[[i]]
    question <- paste(task, guidance, error, format, prefix, a, sep = '')
    #cat(question)
    
    # knn pick up orphar
    distances <- sapply(embedding.orphar, euclidean_distance, v2 = embedding.test[[i]])
    neighbor <- order(distances, decreasing = F)[1:shot.orphar]  # nearest
    snippets <- c()
    for(j in neighbor){
      #j <- neighbor[1]
      text <- orphar$content[j]
      snippets <- c(snippets, text)
    }
    snippets <- paste0("\nSnippet ", seq_along(snippets), ".\n", snippets)
    snippets <- paste(snippets, collapse = "")
    snippets <- paste('Here are knowledge snippets:', snippets, sep = '')
    #question <- paste(task, guidance, format, error, snippets, prefix, a, sep = '\n')
    #cat(question)
    
    # knn pick up
    distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
    neighbor <- order(distances, decreasing = F)[1:shot.train]  # nearest
    examples <- c()
    for(j in neighbor){
      #j <- neighbor[1]
      text <- train[["text"]][[j]]
      type <- train[["label"]][[j]]
      entry.disease <- paste(type$entry[type$type=='DISEASE'], collapse = ', '); entry.disease # possible empty return
      entry.rare <- paste(type$entry[type$type=='RAREDISEASE'], collapse = ', '); entry.rare # possible empty return
      example <- paste('In the following text "', text, '" The diseases mentioned in this text are: ', entry.disease,
                          '; The rare diseases mentioned in this text are: ', entry.rare, '.\n',sep = ''); #cat(example)
      #example <- paste('Input text: ', text, ' Output: ', entry, '.\n',sep = ''); example
      examples <- c(examples, example)
    }
    examples <- paste0("Example ", seq_along(examples), ".\n", examples)
    examples <- paste(examples, collapse = "")
    examples <- paste('\n\n Here are demonstration shots:\n', examples, sep = '')
    question <- paste(task, guidance, format, error, snippets, examples, prefix, a, sep = ' ')
    #cat(question)
    
    # knn pick up + rare example
    # distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
    # neighbor <- order(distances, decreasing = F)[1:shot]  # nearest
    # #neighbor <- order(distances, decreasing = T)[1:shot]  # furthest
    # examples <- c()
    # for(j in neighbor){
    #   #j <- neighbor[1]
    #   text <- train[["text"]][[j]]
    #   type <- train[["label"]][[j]]
    #   entry.disease <- paste(type$entry[type$type=='DISEASE'], collapse = ', '); entry.disease # possible empty return
    #   entry.rare <- paste(type$entry[type$type=='RAREDISEASE'], collapse = ', '); entry.rare # possible empty return
    #   example <- paste('In the following text "', text, '" The diseases mentioned in this text are: ', entry.disease, 
    #                    '; The rare diseases mentioned in this text are: ', entry.rare, '.\n',sep = ''); #cat(example)
    #   examples <- c(examples, example)
    # }
    # examples <- paste0("Example ", seq_along(examples), ".\n", examples)
    # examples <- paste(examples, collapse = ""); examples
    # examples <- paste('Here are examples:\n', examples, sep = '')
    # question <- paste(task, guidance, format, error, examples, prefix, a, sep = ' '); question
    #cat(question)
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        #model = "gpt-3.5-turbo",
        model = "gpt-4o-2024-05-13",
        #model = "ft:gpt-4o-2024-08-06:purdue-university:disease:A6RMGYBL",
        #model = "ft:gpt-4o-mini-2024-07-18:purdue-university::A6RWSDDL",
        temperature = 0,
        messages = list(list(
          role = "user", 
          content = question
        ))
      )
    )
    if(!is.null(content(response)$choices[[1]]$message$content)){
      result.rare[i] <- content(response)$choices[[1]]$message$content
    }
  }
})
result.rare
result.rare <- tolower(result.rare)
saveRDS(result.rare, 'result/disease/KB2.pubmed.shot1.rds')
#result.rare <- readRDS('result/disease/basic.guidance.error.knn8.withrare.4o.rds')

######################################################################################
# precision
######################################################################################
text <- c()
model <- c()
compare <- c()

for(i in 1:208){
  #i <- 1
  result <- result.rare[i]  
  # only check identified entry
  if(result!='none'){
    result <- trimws(unlist(strsplit(result, split = ",")))
    text <- c(text, rep(names(test$label)[i], length(result)))
    for(r in result){
      #r <- result[1]; r
      type <- test$label[[i]]; type
      type <- type[type$type=='DISEASE',]; type
      compare <- c(compare, any(r%in%type$entry))
      model <- c(model, r)
    }
  }
}
result.prec <- data.frame(text, model, compare)
prec <- mean(result.prec$compare); round(prec,3)

######################################################################################
# recall
######################################################################################
text <- c()
entry <- c()
compare <- c()
for(i in 1:208){
  #i <- 190
  result <- result.rare[i]  
  result <- trimws(unlist(strsplit(result, split = ",")))
  type <- test$label[[i]]
  type <- type[type$type=='DISEASE',]
  if(dim(type)[1]>0){
    entry <- c(entry, type$entry)
    text <- c(text, rep(names(test$label)[i], length(type$entry)))
    for(e in type$entry){
      #e <- entry[1]
      compare <- c(compare, any(e%in%result))
    }
  } 
}
result.recall <- data.frame(text, entry, compare)
recall <- mean(result.recall$compare); round(recall,3)
F1 <- 2*prec*recall/(prec+recall); round(F1,3)

######################################################################################
######################################################################################
# Sign
######################################################################################
######################################################################################
# read data
train <- readRDS('data/rare_train.rds')
test <- readRDS('data/rare_test.rds')

# read embedding
embedding.train <- readRDS('result/embedding.train.rds')
embedding.test <- readRDS('result/embedding.test.rds')

# distance function
euclidean_distance <- function(v1, v2) {
  sqrt(sum((v1 - v2)^2))
}

######################################################################################
# distance between each train test and all test text
######################################################################################
distances <- c()
for(i in 1:length(embedding.train)){
  #i <- 4
  distances[i] <- mean(sapply(embedding.test, euclidean_distance, v2 = embedding.train[[i]]))
}

######################################################################################
# analyze sign
######################################################################################
task <- 'Task: Identify the signs of diseases mentioned in the following text.\n'
guidance <- 'Definition: A sign of a disease is an objective evidence of disease that can be observed or detected by someone other than the individual affected by the disease. It includes measurable indicators such as physical findings, laboratory test results, and imaging studies, which provide concrete evidence of a medical condition.\n'
format <- 'Output format: Output only the exact sign names without any additional changes. If there are multiple signs, separate their names with commas. If there is no sign, output \'none\'.\n'
error <- 'Guidance: Differentiate between signs and symptoms. Symptoms are subjective experiences of disease reported by the patient and cannot be directly measured by healthcare providers. Only output signs, not symptoms.\n'
prefix <- 'The text from which you need to exact the signs of diseases is: '
shot.train <- 4
shot.orphar <- 2
result.rare <- c()

system.time({
  set.seed(2024)
  for(i in 1:208){
    #i <- 1
    print(i)
    a <- test$text[[i]]
    question <- paste(task, guidance, error, format, prefix, a, sep = '')
    #cat(question)
    
    # knn pick up orphar
    distances <- sapply(embedding.orphar, euclidean_distance, v2 = embedding.test[[i]])
    neighbor <- order(distances, decreasing = F)[1:shot.orphar]  # nearest
    snippets <- c()
    for(j in neighbor){
      #j <- neighbor[1]
      text <- orphar$content[j]
      snippets <- c(snippets, text)
    }
    snippets <- paste0("\nSnippet ", seq_along(snippets), ".\n", snippets)
    snippets <- paste(snippets, collapse = "")
    snippets <- paste('Here are knowledge snippets:', snippets, sep = '')
    question <- paste(task, guidance, format, error, snippets, prefix, a, sep = '\n')
    #cat(question)
    
    # knn pick up
    # distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
    # neighbor <- order(distances, decreasing = F)[1:shot.train]  # nearest
    # examples <- c()
    # for(j in neighbor){
    #   #j <- neighbor[1]
    #   text <- train[["text"]][[j]]
    #   type <- train[["label"]][[j]]
    #   entry.disease <- paste(type$entry[type$type=='SIGN'], collapse = ', '); entry.disease # possible empty return
    #   entry.rare <- paste(type$entry[type$type=='SYMPTOM'], collapse = ', '); entry.rare # possible empty return
    #   # example <- paste('In the following text "', text, '" The signs of diseases mentioned in this text are: ', entry.disease,
    #   #                   '; The symptoms of diseases mentioned in this text are: ', entry.rare, '.\n',sep = ''); #cat(example)
    #   example <- paste('In the following text "', text, '" The signs of diseases mentioned in this text are: ', entry.disease,
    #                    '.\n',sep = ''); #cat(example)
    #   examples <- c(examples, example)
    # }
    # examples <- paste0("Example ", seq_along(examples), ".\n", examples)
    # examples <- paste(examples, collapse = "")
    # examples <- paste('\n\n Here are demonstration shots:\n', examples, sep = '')
    # question <- paste(task, guidance, format, error, snippets, examples, prefix, a, sep = ' ')
    #cat(question)

    # knn pick up + counter example
    # distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
    # neighbor <- order(distances, decreasing = F)[1:shot]  # nearest
    # examples <- c()
    # for(j in neighbor){
    #   #j <- neighbor[1]
    #   text <- train[["text"]][[j]]
    #   type <- train[["label"]][[j]]
    #   entry.disease <- paste(type$entry[type$type=='SIGN'], collapse = ', '); entry.disease # possible empty return
    #   entry.rare <- paste(type$entry[type$type=='SYMPTOM'], collapse = ', '); entry.rare # possible empty return
    #   example <- paste('In the following text "', text, '" The signs of diseases mentioned in this text are: ', entry.disease,
    #                    '; The symptoms of diseases mentioned in this text are: ', entry.rare, '.\n',sep = ''); #cat(example)
    #   examples <- c(examples, example)
    # }
    # examples <- paste0("Example ", seq_along(examples), ".\n", examples)
    # examples <- paste(examples, collapse = ""); examples
    # examples <- paste('Here are examples:\n', examples, sep = '')
    # #question <- paste(task, guidance, format, error, examples, prefix, a, sep = ' '); question
    # question <- paste(task, format, examples, prefix, a, sep = ' '); question
    #cat(question)
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        #model = "gpt-3.5-turbo",
        model = "gpt-4o-2024-05-13",
        #model = 'ft:gpt-4o-mini-2024-07-18:purdue-university::A6iPC4EJ',
        temperature = 0,
        messages = list(list(
          role = "user", 
          content = question
        ))
      )
    )
    if(!is.null(content(response)$choices[[1]]$message$content)){
      result.rare[i] <- content(response)$choices[[1]]$message$content
    }
  }
})
result.rare
result.rare <- tolower(result.rare)
saveRDS(result.rare, 'result/sign/KB2.rds')

#result.rare <- readRDS('result/sign/basic.knn10.4o.rds')

#sort(table(result.rare), decreasing = F)
#which(result.rare=='none')

######################################################################################
# precision
######################################################################################
text <- c()
model <- c()
compare <- c()

for(i in 1:208){
  #i <- 1
  result <- result.rare[i]  
  # only check identified entry
  if(result!='none'){
    result <- trimws(unlist(strsplit(result, split = ",")))
    text <- c(text, rep(names(test$label)[i], length(result)))
    for(r in result){
      #r <- result[1]; r
      type <- test$label[[i]]; type
      type <- type[type$type=='SIGN',]; type
      compare <- c(compare, any(r%in%type$entry))
      model <- c(model, r)
    }
  }
}
result.prec <- data.frame(text, model, compare)
prec <- mean(result.prec$compare); round(prec,3)

######################################################################################
# recall
######################################################################################
text <- c()
entry <- c()
compare <- c()
for(i in 1:208){
  #i <- 190
  result <- result.rare[i]  
  result <- trimws(unlist(strsplit(result, split = ",")))
  type <- test$label[[i]]
  type <- type[type$type=='SIGN',]
  if(dim(type)[1]>0){
    entry <- c(entry, type$entry)
    text <- c(text, rep(names(test$label)[i], length(type$entry)))
    for(e in type$entry){
      #e <- entry[1]
      compare <- c(compare, any(e%in%result))
    }
  } 
}
result.recall <- data.frame(text, entry, compare)
recall <- mean(result.recall$compare); round(recall,3)
F1 <- 2*prec*recall/(prec+recall); round(F1,3)


######################################################################################
######################################################################################
# Symptom
######################################################################################
######################################################################################
# read data
train <- readRDS('data/rare_train.rds')
test <- readRDS('data/rare_test.rds')

# read embedding
embedding.train <- readRDS('result/embedding.train.rds')
embedding.test <- readRDS('result/embedding.test.rds')

# distance function
euclidean_distance <- function(v1, v2) {
  sqrt(sum((v1 - v2)^2))
}

######################################################################################
# distance between each train test and all test text
######################################################################################
distances <- c()
for(i in 1:length(embedding.train)){
  #i <- 4
  distances[i] <- mean(sapply(embedding.test, euclidean_distance, v2 = embedding.train[[i]]))
}

######################################################################################
# analyze symptom
######################################################################################
task <- 'Task: Identify the symptoms of diseases mentioned in the following text.\n'
guidance <- 'Definition: Symptoms are subjective experiences reported by the patient, which cannot be directly observed or measured by others. They reflect what the patient feels, such as pain, fatigue, or nausea. Symptoms are experienced internally and rely on the patient’s description.\n'
format <- 'Output format: Output only the exact symptom names without any additional changes. If there are multiple symptoms, separate their names with commas. If there is no symptom, output \'none\'.\n'
error <- 'Guidance: Differentiate between symptoms and signs. Signs are objective indicators of a disease that can be observed, measured, or detected by someone other than the patient, such as a doctor or medical professional. Only output symptoms, not signs.\n'
prefix <- 'The text from which you need to exact the symptoms of diseases is: '
#supplement <- 'Extra guidelines: Be conservative when you identify symptoms. You made too many false positives.\n'
shot.train <- 4
shot.orphar <- 2
result.rare <- c()

system.time({
  set.seed(2024)
  for(i in 1:208){
    #i <- 1
    print(i)
    a <- test$text[[i]]
    question <- paste(task, guidance, error, format, prefix, a, sep = '')
    #cat(question)
    
    # knn pick up orphar
    distances <- sapply(embedding.orphar, euclidean_distance, v2 = embedding.test[[i]])
    neighbor <- order(distances, decreasing = F)[1:shot.orphar]  # nearest
    snippets <- c()
    for(j in neighbor){
      #j <- neighbor[1]
      text <- orphar$content[j]
      snippets <- c(snippets, text)
    }
    snippets <- paste0("\nSnippet ", seq_along(snippets), ".\n", snippets)
    snippets <- paste(snippets, collapse = "")
    snippets <- paste('Here are knowledge snippets:', snippets, sep = '')
    #question <- paste(task, guidance, format, error, snippets, prefix, a, sep = '\n')
    #cat(question)
    
    #knn pick up + counter example
    distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
    neighbor <- order(distances, decreasing = F)[1:shot.train]  # nearest
    #neighbor <- sample(1:729, shot)   # random
    examples <- c()
    for(j in neighbor){
      #j <- neighbor[1]
      text <- train[["text"]][[j]]
      type <- train[["label"]][[j]]
      entry.disease <- paste(type$entry[type$type=='SIGN'], collapse = ', '); entry.disease # possible empty return
      entry.rare <- paste(type$entry[type$type=='SYMPTOM'], collapse = ', '); entry.rare # possible empty return
      example <- paste('In the following text "', text, '" The signs of diseases mentioned in this text are: ', entry.disease,
                       '; The symptoms of diseases mentioned in this text are: ', entry.rare, '.\n',sep = ''); #cat(example)
      examples <- c(examples, example)
    }
    examples <- paste0("Example ", seq_along(examples), ".\n", examples)
    examples <- paste(examples, collapse = "")
    examples <- paste('Here are examples:\n', examples, sep = '')
    #question <- paste(task, guidance, format, error, supplement, examples, prefix, a, sep = ' '); question
    question <- paste(task, guidance, format, error, snippets, examples, prefix, a, sep = ' ')
    #cat(question)
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        #model = "gpt-3.5-turbo",
        model = "gpt-4o-2024-05-13",
        #model = "ft:gpt-4o-mini-2024-07-18:purdue-university::A6RnCso5",
        #model = 'ft:gpt-4o-2024-08-06:purdue-university::A6ij89ng',
        temperature = 0,
        messages = list(list(
          role = "user", 
          content = question
        ))
      )
    )
    if(!is.null(content(response)$choices[[1]]$message$content)){
      result.rare[i] <- content(response)$choices[[1]]$message$content
    }
  }
})
result.rare
result.rare <- tolower(result.rare)
saveRDS(result.rare, 'result/symptom/KB2.shot4.rds')

#result.rare <- readRDS('result/symptom/basic.guidance.error.counter.knn14.4o.rds')

#sort(table(result.rare), decreasing = F)
#which(result.rare=='none')

######################################################################################
# precision
######################################################################################
text <- c()
model <- c()
compare <- c()

for(i in 1:208){
  #i <- 1
  result <- result.rare[i]  
  # only check identified entry
  if(result!='none'){
    result <- trimws(unlist(strsplit(result, split = ",")))
    text <- c(text, rep(names(test$label)[i], length(result)))
    for(r in result){
      #r <- result[1]; r
      type <- test$label[[i]]; type
      type <- type[type$type=='SYMPTOM',]; type
      compare <- c(compare, any(r%in%type$entry))
      model <- c(model, r)
    }
  }
}
result.prec <- data.frame(text, model, compare)
prec <- mean(result.prec$compare); round(prec,3)

######################################################################################
# recall
######################################################################################
text <- c()
entry <- c()
compare <- c()
for(i in 1:208){
  #i <- 190
  result <- result.rare[i]  
  result <- trimws(unlist(strsplit(result, split = ",")))
  type <- test$label[[i]]
  type <- type[type$type=='SYMPTOM',]
  if(dim(type)[1]>0){
    entry <- c(entry, type$entry)
    text <- c(text, rep(names(test$label)[i], length(type$entry)))
    for(e in type$entry){
      #e <- entry[1]
      compare <- c(compare, any(e%in%result))
    }
  } 
}
result.recall <- data.frame(text, entry, compare)
recall <- mean(result.recall$compare); round(recall,3)
F1 <- 2*prec*recall/(prec+recall); round(F1,3)

