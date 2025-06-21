library(readxl)

setwd("~/Library/CloudStorage/Box-Box/Xi/LLM")

######################################################################################
# read ann files
######################################################################################
# result list
labels <- list()

# read ann files
ann_files <- list.files(path = 'RareDis-v1-without-texts/RareDis-v1-without-texts/test', 
                        pattern = "\\.ann$", full.names = F)

for(f in ann_files){
  #f <- ann_files[39]; f
  # read one ann
  data <- read.table(paste("RareDis-v1-without-texts/RareDis-v1-without-texts/test/", f, sep = ''), header = F, sep = "\t", quote = '')
  
  # pick up entry type; remove index
  split_data <- strsplit(data$V2, " ")
  first_elements <- sapply(split_data, function(x) x[1])
  data$`type` <- first_elements
  
  # change 'SKINRAREDISEASE' to 'RAREDISEASE'
  data$`type` <- gsub('SKIN', "", data$`type`)
  
  # only keep 4 types
  data <- data[data$type%in%c('RAREDISEASE', 'DISEASE', 'SIGN', 'SYMPTOM'),]
  data <- data[,2:4]
  colnames(data)[1:2] <- c('type_raw','entry')
  
  # entry to lowercase
  data$entry <- tolower(data$entry)
  
  # remove duplicated entries
  data <- data[!duplicated(data$entry),]
  
  # add to list
  labels[[gsub('.ann', "", f)]] <- data
}


######################################################################################
# read text
######################################################################################
# result list
texts <- list()

# read ann files
text_files <- list.files(path = 'RareDis-v1-without-texts/test', pattern = "\\.txt$", full.names = F)

for(f in text_files){
  #f <- text_files[1]
  # read one text
  data <- readLines(paste("RareDis-v1/test/", f, sep = ''))

  # add to list
  texts[[gsub('.txt', "", f)]] <- data
}

######################################################################################
# combine text and type and save
######################################################################################
data <- list(text=texts, label=labels)
saveRDS(data, 'rare_test.rds')









