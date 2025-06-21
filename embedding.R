library(httr)
setwd("C:/Users/mxi1/Box/Xi/LLM")

######################################################
# read test data
######################################################
test <- readRDS('data/rare_test.rds')

embeddings <- list()
system.time({
  for(i in 14:208){
    #i <- 1
    print(i)
    text <- test$text[[i]]; text
    response <- POST(
      url = "https://api.openai.com/v1/embeddings", 
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "text-embedding-3-large",
        input = text
      )
    )
    e <- unlist(content(response)$data[[1]]$embedding)
    embeddings[[i]] <- e
  }
})

saveRDS(embeddings, 'result/embedding.test.rds')

######################################################
# read training data
######################################################
train <- readRDS('data/rare_train.rds')

embeddings <- list()
system.time({
  for(i in 25:729){
    #i <- 1
    print(i)
    text <- train$text[[i]]; text
    response <- POST(
      url = "https://api.openai.com/v1/embeddings", 
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "text-embedding-3-large",
        input = text
      )
    )
    e <- unlist(content(response)$data[[1]]$embedding)
    embeddings[[i]] <- e
  }
})

saveRDS(embeddings, 'result/embedding.train.rds')

######################################################
# read validation data
######################################################
train <- readRDS('data/rare_dev.rds')

embeddings <- list()
system.time({
  for(i in 1:104){
    #i <- 1
    print(i)
    text <- train$text[[i]]; text
    response <- POST(
      url = "https://api.openai.com/v1/embeddings", 
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "text-embedding-3-large",
        input = text
      )
    )
    e <- unlist(content(response)$data[[1]]$embedding)
    embeddings[[i]] <- e
  }
})

saveRDS(embeddings, 'result/embedding.val.rds')
                
######################################################
# read orpha data
######################################################
train <- read.csv('data/orphar.csv')
summary(train$n_tokens)

embeddings <- list()
system.time({
  for(i in 3845:dim(train)[1]){
    #i <- 1
    print(i)
    text <- train$content[[i]]; text
    response <- POST(
      url = "https://api.openai.com/v1/embeddings", 
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "text-embedding-3-large",
        input = text
      )
    )
    e <- unlist(content(response)$data[[1]]$embedding)
    embeddings[[i]] <- e
  }
})

saveRDS(embeddings, 'result/embedding.orpha.rds')
