library(httr)
library(umap)
library(NbClust)

setwd("C:/Users/mxi1/OneDrive - Loyola University Chicago/LLM")

######################################################################################
######################################################################################
# rare disease
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
# k-means clustering on test
######################################################################################
embedding.test.frame <- do.call(rbind.data.frame, embedding.test)
#embedding.test.umap <- umap(embedding.test.frame, n_components=5)
#embedding.test.umap <- embedding.test.umap$layout

# kmeans
cl <- 64
set.seed(2024)
km.out <- kmeans(embedding.test.frame, centers = cl, nstart = 25)
table(km.out$cluster)

embedding.test.kmeans <- list()
for(i in 1:cl){
  embedding.test.kmeans[[i]] <- embedding.test[which(km.out$cluster==i)]
}

######################################################################################
# distance between each train test and all test text per cluster
######################################################################################
distances <- list()
for(j in 1:length(embedding.test.kmeans)){
  #j <- 1
  d <- c()
  for(i in 1:length(embedding.train)){
    d[i] <- mean(sapply(embedding.test.kmeans[[j]], euclidean_distance, v2 = embedding.train[[i]]))
  }
  distances[[j]] <- d
}

######################################################################################
# analyze rare disease
######################################################################################
task <- 'Task: Identify the names of rare diseases from the following text.\n'
guidance <- 'Definition: A rare disease is a health condition that affects a small percentage of the population. In the U.S., a disease is considered rare if it affects fewer than 200,000 people. In European Union, a disease is considered rare if it affects fewer than 1 in 2,000 people.\n'
format <- 'Output format: Output only the exact rare disease names without any additional changes. If there are multiple rare diseases, separate their names with commas. If there is no rare disease, output \'none\'.\n'
error <- 'Guidance: Treat abbreviations as seperate rare disease names. Do not identify regular diseases as rare diseases.\n'
prefix <- 'The text from which you need to exact the names of rare diseases is: '
shot <- 16
result.rare <- c()

system.time({
  set.seed(2024)
  for(i in 1:208){
    #i <- 3
    print(i)
    a <- test$text[[i]]
    #question <- paste(task, format, prefix, a, sep = '')
    #cat(question)
    
    # knn pick up
    #distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
    c <- km.out$cluster[i]; c
    neighbor <- order(distances[[c]], decreasing = F)[1:shot]  # nearest
    #neighbor <- order(distances, decreasing = T)[1:shot]  # furthest
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
    examples <- paste(examples, collapse = ""); examples
    examples <- paste('Here are examples:\n', examples, sep = '')
    question <- paste(task, guidance, format, error, examples, prefix, a, sep = ' '); question
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        #model = "gpt-3.5-turbo",
        model = "gpt-4o",
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
saveRDS(result.rare, 'result/basic.guidance.error.knn16_k64.4o.rds')
#result.rare <- readRDS('result/basic.guidance.error.knn4_k64.4o.rds')

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
      type <- type[type$type=='RAREDISEASE',]
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
######################################################################################
# disease
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
# k-means clustering on test
######################################################################################
embedding.test.frame <- do.call(rbind.data.frame, embedding.test)

# kmeans
cl <- 64
set.seed(2024)
km.out <- kmeans(embedding.test.frame, centers = cl, nstart = 25)
table(km.out$cluster)

embedding.test.kmeans <- list()
for(i in 1:cl){
  embedding.test.kmeans[[i]] <- embedding.test[which(km.out$cluster==i)]
}

######################################################################################
# distance between each train test and all test text per cluster
######################################################################################
distances <- list()
for(j in 1:length(embedding.test.kmeans)){
  #j <- 1
  d <- c()
  for(i in 1:length(embedding.train)){
    d[i] <- mean(sapply(embedding.test.kmeans[[j]], euclidean_distance, v2 = embedding.train[[i]]))
  }
  distances[[j]] <- d
}

######################################################################################
# analyze disease
######################################################################################
task <- 'Task: Identify the names of diseases mentioned in the following text.\n'
guidance <- 'Definition: A disease is a condition of the body or mind that impairs normal functioning and is characterized by specific signs and symptoms. Diseases can be caused by a variety of factors, including infections, genetic mutations, environmental factors, and lifestyle choices.\n'
format <- 'Output format: Output only the exact disease names without any additional changes. If there are multiple diseases, separate their names with commas. If there is no disease, output \'none\'.\n'
error <- 'Guidance: Differentiate between rare diseases and diseases. A rare disease is a health condition that affects a small percentage of the population. Rare diseases are a subset of diseases. Only output diseases, not rare diseases.\n'
prefix <- 'The text from which you need to exact the names of diseases is: '
shot <- 16
result.rare <- c()

system.time({
  set.seed(2025)
  for(i in 1:208){
    #i <- 3
    print(i)
    a <- test$text[[i]]
    #question <- paste(task, format, prefix, a, sep = '')
    #cat(question)
    
    # knn pick up
    #distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
    c <- km.out$cluster[i]; c
    neighbor <- order(distances[[c]], decreasing = F)[1:shot]  # nearest
    #neighbor <- order(distances, decreasing = T)[1:shot]  # furthest
    examples <- c()
    for(j in neighbor){
      #j <- neighbor[1]
      text <- train[["text"]][[j]]
      type <- train[["label"]][[j]]
      entry.disease <- paste(type$entry[type$type=='DISEASE'], collapse = ', '); entry.disease # possible empty return
      entry.rare <- paste(type$entry[type$type=='RAREDISEASE'], collapse = ', '); entry.rare # possible empty return
      example <- paste('In the following text "', text, '" The diseases mentioned in this text are: ', entry.disease, 
                       '; The rare diseases mentioned in this text are: ', entry.rare, '.\n',sep = ''); #cat(example)
      examples <- c(examples, example)
    }
    examples <- paste0("Example ", seq_along(examples), ".\n", examples)
    examples <- paste(examples, collapse = ""); examples
    examples <- paste('Here are examples:\n', examples, sep = '')
    question <- paste(task, guidance, format, error, examples, prefix, a, sep = ' '); question
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        #model = "gpt-3.5-turbo",
        model = "gpt-4o",
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
saveRDS(result.rare, 'result/disease/basic.guidance.error.knn16_k64.4o.rds')
#result.rare <- readRDS('result/disease/basic.guidance.error.knn1_k32.4o.rds')

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
      type <- type[type$type=='DISEASE',]
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
recall <- mean(result.recall$compare); recall
F1 <- 2*prec*recall/(prec+recall); F1


######################################################################################
######################################################################################
# sign
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
# k-means clustering on test
######################################################################################
embedding.test.frame <- do.call(rbind.data.frame, embedding.test)

# kmeans
cl <- 64
set.seed(2024)
km.out <- kmeans(embedding.test.frame, centers = cl, nstart = 25)
table(km.out$cluster)

embedding.test.kmeans <- list()
for(i in 1:cl){
  embedding.test.kmeans[[i]] <- embedding.test[which(km.out$cluster==i)]
}

######################################################################################
# distance between each train test and all test text per cluster
######################################################################################
distances <- list()
for(j in 1:length(embedding.test.kmeans)){
  #j <- 1
  d <- c()
  for(i in 1:length(embedding.train)){
    d[i] <- mean(sapply(embedding.test.kmeans[[j]], euclidean_distance, v2 = embedding.train[[i]]))
  }
  distances[[j]] <- d
}

######################################################################################
# analyze sign
######################################################################################
task <- 'Task: Identify the signs of diseases mentioned in the following text.\n'
guidance <- 'Definition: A sign of a disease is an objective evidence of disease that can be observed or detected by someone other than the individual affected by the disease. It includes measurable indicators such as physical findings, laboratory test results, and imaging studies, which provide concrete evidence of a medical condition.\n'
format <- 'Output format: Output only the exact sign names without any additional changes. If there are multiple signs, separate their names with commas. If there is no sign, output \'none\'.\n'
error <- 'Guidance: Differentiate between signs and symptoms. Symptoms are subjective experiences of disease reported by the patient and cannot be directly measured by healthcare providers. Only output signs, not symptoms.\n'
prefix <- 'The text from which you need to exact the signs of diseases is: '
shot <- 14
result.rare <- c()

system.time({
  set.seed(2024)
  for(i in 1:208){
    #i <- 3
    print(i)
    a <- test$text[[i]]
    #question <- paste(task, format, prefix, a, sep = '')
    #cat(question)
    
    # knn pick up
    #distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
    c <- km.out$cluster[i]; c
    neighbor <- order(distances[[c]], decreasing = F)[1:shot]  # nearest
    #neighbor <- order(distances, decreasing = T)[1:shot]  # furthest
    examples <- c()
    for(j in neighbor){
      #j <- neighbor[1]
      text <- train[["text"]][[j]]
      type <- train[["label"]][[j]]
      entry <- paste(type$entry[type$type=='SIGN'], collapse = ', '); entry # possible empty return
      # if(entry==''){
      #   entry <- 'none'
      # }
      example <- paste('Input text: ', text, ' Output: ', entry, '.\n',sep = ''); example
      examples <- c(examples, example)
    }
    examples <- paste0("Example ", seq_along(examples), ".\n", examples)
    examples <- paste(examples, collapse = ""); examples
    examples <- paste('Here are examples:\n', examples, sep = '')
    question <- paste(task, format, examples, prefix, a, sep = ' '); question
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        #model = "gpt-3.5-turbo",
        model = "gpt-4o",
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
#result.rare
result.rare <- tolower(result.rare)
saveRDS(result.rare, 'result/sign/basic.knn14_k64.4o.rds')
#result.rare <- readRDS('result/disease/basic.guidance.error.knn1_k32.4o.rds')

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
      type <- type[type$type=='SIGN',]
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
# symptom
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
# k-means clustering on test
######################################################################################
embedding.test.frame <- do.call(rbind.data.frame, embedding.test)

# kmeans
cl <- 64
set.seed(2024)
km.out <- kmeans(embedding.test.frame, centers = cl, nstart = 25)
table(km.out$cluster)

embedding.test.kmeans <- list()
for(i in 1:cl){
  embedding.test.kmeans[[i]] <- embedding.test[which(km.out$cluster==i)]
}

######################################################################################
# distance between each train test and all test text per cluster
######################################################################################
distances <- list()
for(j in 1:length(embedding.test.kmeans)){
  #j <- 1
  d <- c()
  for(i in 1:length(embedding.train)){
    d[i] <- mean(sapply(embedding.test.kmeans[[j]], euclidean_distance, v2 = embedding.train[[i]]))
  }
  distances[[j]] <- d
}

######################################################################################
# analyze symptoms
######################################################################################
task <- 'Task: Identify the symptoms of diseases mentioned in the following text.\n'
guidance <- 'Definition: Symptoms are subjective experiences reported by the patient, which cannot be directly observed or measured by others. They reflect what the patient feels, such as pain, fatigue, or nausea. Symptoms are experienced internally and rely on the patient’s description.\n'
format <- 'Output format: Output only the exact symptom names without any additional changes. If there are multiple symptoms, separate their names with commas. If there is no symptom, output \'none\'.\n'
error <- 'Guidance: Differentiate between symptoms and signs. Signs are objective indicators of a disease that can be observed, measured, or detected by someone other than the patient, such as a doctor or medical professional. Only output symptoms, not signs.\n'
prefix <- 'The text from which you need to exact the symptoms of diseases is: '
supplement <- 'Be conservative when you identify symptoms. You made too many false positives.'
shot <- 14
result.rare <- c()

system.time({
  set.seed(2024)
  for(i in 1:208){
    #i <- 3
    print(i)
    a <- test$text[[i]]
    #question <- paste(task, format, prefix, a, sep = '')
    #cat(question)
    
    # knn pick up
    #distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
    c <- km.out$cluster[i]; c
    neighbor <- order(distances[[c]], decreasing = F)[1:shot]  # nearest
    #neighbor <- order(distances, decreasing = T)[1:shot]  # furthest
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
    examples <- paste(examples, collapse = ""); examples
    examples <- paste('Here are examples:\n', examples, sep = '')
    question <- paste(task, guidance, format, error, examples, supplement, prefix, a, sep = ' '); question
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        #model = "gpt-3.5-turbo",
        model = "gpt-4o",
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
#result.rare
result.rare <- tolower(result.rare)
saveRDS(result.rare, 'result/symptom/basic.guidance.error.counter.knn16_k64.4o.rds')
#result.rare <- readRDS('result/disease/basic.guidance.error.knn1_k32.4o.rds')

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
      type <- type[type$type=='SYMPTOM',]
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
