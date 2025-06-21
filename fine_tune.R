setwd("C:/Users/mxi1/OneDrive - Loyola University Chicago/LLM")

################################################################
# rare disease
################################################################
# read data
train <- readRDS('data/rare_train.rds')
val <- readRDS('data/rare_dev.rds')

# prompt
task <- 'Identify the names of rare diseases from the following text.'
guidance <- 'A rare disease is a health condition that affects a small percentage of the population. A disease is classified as rare if it affects fewer than a certain number of individuals within a population.'
format <- 'Output only the exact rare disease names without any additional changes. If there are multiple rare diseases, separate their names with commas.'
error <- 'Treat abbreviations as seperate rare disease names.'
prefix <- 'The text from which you need to exact the names of rare diseases is:'

# create json file
jf <- c()
for(i in 1:length(val$text)){
  #i <- 10
  text <- paste(task, guidance, format, error, prefix, train$text[[i]], sep = ' ')
  text <- gsub('"', '', text); cat(text)
  text <- paste('"', text, '"', sep = ''); cat(text)
  text <- paste("{\"role\": \"user\", \"content\": ", text, '}', sep = ''); cat(text)
  type <- train$label[[i]]
  entry <- type$entry[type$type=='RAREDISEASE']; entry
  if(length(entry)>0){
    entry <- paste(type$entry[type$type=='RAREDISEASE'], collapse = ', '); cat(entry)
  } else {
    entry <- 'none'
  }
  entry <- paste('"', entry, '"', sep = ''); cat(entry) 
  entry <- paste("{\"role\": \"assistant\", \"content\": ", entry, '}', sep = ''); cat(entry)
  message <- '{"messages": [{"role": "system", "content": "You are an AI assistant specialized in identifying rare diseases from text."}'; cat(message)
  d <- paste(message, text, entry, sep = ', '); cat(d)
  d <- paste(d, ']}', sep = ''); cat(d)
  jf <- c(jf, d)
}
writeLines(jf, con = 'finetune/data/val_rare.jsonl', sep = '\n')


################################################################
# disease
################################################################
# read data
train <- readRDS('data/rare_train.rds')
val <- readRDS('data/rare_dev.rds')

# prompt
task <- 'Identify the names of diseases mentioned in the following text.'
guidance <- 'A disease is a condition of the body or mind that impairs normal functioning and is characterized by specific signs and symptoms. Diseases can be caused by a variety of factors, including infections, genetic mutations, environmental factors, and lifestyle choices.'
format <- 'Output only the exact disease names without any additional changes. If there are multiple diseases, separate their names with commas. If there is no disease, output \'none\'.'
error <- 'Differentiate between rare diseases and diseases. A rare disease is a health condition that affects a small percentage of the population. Rare diseases are a subset of diseases. Only output diseases, not rare diseases.'
prefix <- 'The text from which you need to exact the names of diseases is: '

# create json file
jf <- c()
for(i in 1:length(val$text)){
  #i <- 10
  text <- paste(task, guidance, format, error, prefix, train$text[[i]], sep = ' ')
  text <- gsub('"', '', text); #cat(text)
  text <- paste('"', text, '"', sep = ''); #cat(text)
  text <- paste("{\"role\": \"user\", \"content\": ", text, '}', sep = ''); #cat(text)
  type <- train$label[[i]]
  entry <- type$entry[type$type=='DISEASE']; entry
  if(length(entry)>0){
    entry <- paste(type$entry[type$type=='DISEASE'], collapse = ', '); #cat(entry)
  } else {
    entry <- 'none'
  }
  entry <- paste('"', entry, '"', sep = ''); #cat(entry) 
  entry <- paste("{\"role\": \"assistant\", \"content\": ", entry, '}', sep = ''); #cat(entry)
  message <- '{"messages": [{"role": "system", "content": "You are an AI assistant specialized in identifying diseases from text."}'; #cat(message)
  d <- paste(message, text, entry, sep = ', '); #cat(d)
  d <- paste(d, ']}', sep = ''); #cat(d)
  jf <- c(jf, d)
}
writeLines(jf, con = 'data/finetune/val_disease.jsonl', sep = '\n')


################################################################
# sign
################################################################
# read data
train <- readRDS('data/rare_train.rds')
val <- readRDS('data/rare_dev.rds')

# prompt
task <- 'Identify the signs of diseases mentioned in the following text.'
guidance <- 'A sign of a disease is an objective evidence of disease that can be observed or detected by someone other than the individual affected by the disease. It includes measurable indicators such as physical findings, laboratory test results, and imaging studies, which provide concrete evidence of a medical condition.'
format <- 'Output only the exact sign names without any additional changes. If there are multiple signs, separate their names with commas. If there is no sign, output \'none\'.'
error <- 'Differentiate between signs and symptoms. Symptoms are subjective experiences of disease reported by the patient and cannot be directly measured by healthcare providers. Only output signs, not symptoms.'
prefix <- 'The text from which you need to exact the signs of diseases is: '

# create json file
jf <- c()
for(i in 1:length(val$text)){
  #i <- 14
  text <- paste(task, guidance, format, error, prefix, train$text[[i]], sep = ' ')
  text <- gsub('"', '', text); #cat(text)
  text <- paste('"', text, '"', sep = ''); #cat(text)
  text <- paste("{\"role\": \"user\", \"content\": ", text, '}', sep = ''); #cat(text)
  type <- train$label[[i]]
  entry <- type$entry[type$type=='SIGN']; entry
  entry <- gsub('"', '', entry); entry
  if(length(entry)>0){
    entry <- paste(gsub('"', '', type$entry[type$type=='SIGN']), collapse = ', '); #cat(entry)
  } else {
    entry <- 'none'
  }
  entry <- paste('"', entry, '"', sep = ''); #cat(entry) 
  entry <- paste("{\"role\": \"assistant\", \"content\": ", entry, '}', sep = ''); #cat(entry)
  message <- '{"messages": [{"role": "system", "content": "You are an AI assistant specialized in identifying disease signs from text."}'; #cat(message)
  d <- paste(message, text, entry, sep = ', '); #cat(d)
  d <- paste(d, ']}', sep = ''); #cat(d)
  jf <- c(jf, d)
}
writeLines(jf, con = 'data/finetune/val_sign.jsonl', sep = '\n')


################################################################
# symptom
################################################################
# read data
train <- readRDS('data/rare_train.rds')
val <- readRDS('data/rare_dev.rds')

# prompt
task <- 'Identify the symptoms of diseases mentioned in the following text.'
guidance <- 'Symptoms are subjective experiences reported by the patient, which cannot be directly observed or measured by others. They reflect what the patient feels, such as pain, fatigue, or nausea. Symptoms are experienced internally and rely on the patient’s description.'
format <- 'Output only the exact symptom names without any additional changes. If there are multiple symptoms, separate their names with commas. If there is no symptom, output \'none\'.'
error <- 'Differentiate between symptoms and signs. Signs are objective indicators of a disease that can be observed, measured, or detected by someone other than the patient, such as a doctor or medical professional. Only output symptoms, not signs.'
prefix <- 'The text from which you need to exact the symptoms of diseases is: '

# create json file
jf <- c()
for(i in 1:length(train$text)){
  #i <- 10
  text <- paste(task, guidance, format, error, prefix, train$text[[i]], sep = ' ')
  text <- gsub('"', '', text)
  text <- paste('"', text, '"', sep = '')
  text <- paste("{\"role\": \"user\", \"content\": ", text, '}', sep = '')
  type <- train$label[[i]]
  entry <- type$entry[type$type=='SYMPTOM']
  if(length(entry)>0){
    entry <- paste(type$entry[type$type=='SYMPTOM'], collapse = ', ')
  } else {
    entry <- 'none'
  }
  entry <- paste('"', entry, '"', sep = '')
  entry <- paste("{\"role\": \"assistant\", \"content\": ", entry, '}', sep = '')
  message <- '{"messages": [{"role": "system", "content": "You are an AI assistant specialized in identifying disease symptoms from text."}'
  d <- paste(message, text, entry, sep = ', ')
  d <- paste(d, ']}', sep = '')
  jf <- c(jf, d)
}
writeLines(jf, con = 'data/finetune/train_symptom.jsonl', sep = '\n')








