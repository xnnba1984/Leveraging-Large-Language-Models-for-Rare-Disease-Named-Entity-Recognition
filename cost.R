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
result <- list()

for(shot.train in c(0,1,2,4,6,8,10,12,14,16)){
  questions <- c()
  #shot.train <- 0
  print(shot.train)
  system.time({
    set.seed(2024)
    for(i in 1:208){
      #i <- 1
      #print(i)
      a <- test$text[[i]]
      question <- paste(task, format, prefix, a, sep = '')
      #cat(question)
      
      # knn pick up
      distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
      neighbor <- order(distances, decreasing = F)[0:shot.train]  # nearest
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
      #question <- paste(task, guidance, format, error, snippets, examples, prefix, a, sep = ' ')
      question <- paste(task, guidance, format, error, examples, prefix, a, sep = ' ')
      #cat(question)
      questions <- c(questions, question)
    }
  })
  result[[as.character(shot.train)]] <- questions
}

df <- data.frame(
  k = rep(names(result), times = sapply(result, length)),
  text = unlist(result, use.names = FALSE),
  stringsAsFactors = FALSE
)
df$token_count <- sapply(strsplit(df$text, "\\s+"), length)
df$cost <- df$token_count / 0.75 * 5 / 1e6
df_avg_cost <- aggregate(cost ~ k, data = df, FUN = sum)
str(df_avg_cost)
df_avg_cost$k <- as.numeric(df_avg_cost$k)
df_avg_cost <- df_avg_cost[order(df_avg_cost$k), ]
colnames(df_avg_cost)[1] <- 'k-shot'
F1 <- c(0.702, 0.750,0.746,0.760,0.759,0.768,0.762,0.765,0.758,0.774)
df_avg_cost$F1 <- F1

############################################################
## Asymptotic-exponential cost-performance analysis
############################################################
library(tidyverse)

## 1 · Data  (unchanged) -----------------------------------
df <- tribble(
  ~k_shot, ~cost_usd,      ~F1,
  0, 0.001883910, 0.702,
  1, 0.002991603, 0.750,
  2, 0.004001920, 0.746,
  4, 0.006355224, 0.760,
  6, 0.008588365, 0.759,
  8, 0.010695417, 0.768,
  10, 0.012896827, 0.762,
  12, 0.015110032, 0.765,
  14, 0.017309263, 0.758,
  16, 0.019385160, 0.774
) %>%
  mutate(cost_ct = cost_usd * 100)          # convert to cents

## 2 · Fit asymptotic-exponential model --------------------
fit <- nls(F1 ~ Asym - R0 * exp(-lambda * cost_ct),
           data  = df,
           start = list(Asym = 0.78, R0 = 0.12, lambda = 3.5))
confint(fit)

## 3 · Smooth curve ----------------------------------------
grid <- tibble(cost_ct = seq(min(df$cost_ct),
                             max(df$cost_ct),
                             length.out = 400)) %>%
  mutate(F1_hat = predict(fit, newdata = .))

## 4 · Plot with clear x-axis ------------------------------
ggplot(df, aes(cost_ct, F1)) +
  geom_point(size = 3, colour = "steelblue4") +
  geom_line(data = grid, aes(cost_ct, F1_hat),
            colour = "firebrick", size = 1.2) +
  scale_x_continuous(
    breaks = seq(0, 2, by = 0.25),          # every quarter-cent
    labels = function(x) paste0(x, "¢")     # append cent symbol
  ) +
  labs(title    = "Asymptotic exponential fit: cost vs. F1",
       x = "Per-query cost (US cents)",
       y = "F1 score",
       subtitle = sprintf("Plateau F∞ ≈ %.3f  |  half-rise cost ≈ %.2f ¢",
                          coef(fit)["Asym"],
                          log(2) / coef(fit)["lambda"])) +
  theme_minimal(base_size = 12) +
  geom_text(aes(label = k_shot),
            vjust = -0.8, size = 3, colour = "grey30")


######################################################################################
# analyze disease
######################################################################################
task <- 'Task: Identify the names of diseases mentioned in the following text.\n'
guidance <- 'Definition: A disease is a condition of the body or mind that impairs normal functioning and is characterized by specific signs and symptoms. Diseases can be caused by a variety of factors, including infections, genetic mutations, environmental factors, and lifestyle choices.\n'
format <- 'Output format: Output only the exact disease names without any additional changes. If there are multiple diseases, separate their names with commas. If there is no disease, output \'none\'.\n'
error <- 'Guidance: Differentiate between rare diseases and diseases. A rare disease is a health condition that affects a small percentage of the population. Rare diseases are a subset of diseases. Only output diseases, not rare diseases.\n'
prefix <- 'The text from which you need to exact the names of diseases is: '
result <- list()

for(shot.train in c(0,1,2,4,6,8,10,12,14,16)){
  questions <- c()
  #shot.train <- 0
  print(shot.train)
  system.time({
    set.seed(2024)
    for(i in 1:208){
      #i <- 1
      #print(i)
      a <- test$text[[i]]
      question <- paste(task, format, prefix, a, sep = '')
      #cat(question)
      
      # knn pick up
      distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
      neighbor <- order(distances, decreasing = F)[0:shot.train]  # nearest
      examples <- c()
      for(j in neighbor){
        #j <- neighbor[1]
        text <- train[["text"]][[j]]
        type <- train[["label"]][[j]]
        entry <- paste(type$entry[type$type=='DISEASE'], collapse = ', '); entry
        example <- paste('Input text: ', text, ' Output: ', entry, '.\n',sep = ''); example
        examples <- c(examples, example)
      }
      examples <- paste0("Example ", seq_along(examples), ".\n", examples)
      examples <- paste(examples, collapse = "")
      examples <- paste('\n\n Here are demonstration shots:\n', examples, sep = '')
      question <- paste(task, guidance, format, error, examples, prefix, a, sep = ' ')
      #cat(question)
      questions <- c(questions, question)
    }
  })
  result[[as.character(shot.train)]] <- questions
}

df <- data.frame(
  k = rep(names(result), times = sapply(result, length)),
  text = unlist(result, use.names = FALSE),
  stringsAsFactors = FALSE
)
df$token_count <- sapply(strsplit(df$text, "\\s+"), length)
df$cost <- df$token_count / 0.75 * 5 / 1e6
df_avg_cost <- aggregate(cost ~ k, data = df, FUN = mean)
str(df_avg_cost)
df_avg_cost$k <- as.numeric(df_avg_cost$k)
df_avg_cost <- df_avg_cost[order(df_avg_cost$k), ]
colnames(df_avg_cost)[1] <- 'k-shot'
F1 <- c(0.314, 0.392,0.467,0.471,0.498,0.518,0.485,0.488,0.494,0.487)
df_avg_cost$F1 <- F1

############################################################
## Asymptotic-exponential cost-performance analysis
############################################################
library(tidyverse)

## 1 · Data  (unchanged) -----------------------------------
df <- tribble(
  ~k_shot, ~cost_usd,      ~F1,
  0, 0.001937244, 0.314,
  1, 0.003038109, 0.392,
  2, 0.004141891, 0.467,
  4, 0.006387468, 0.471,
  6, 0.008604872, 0.498,
  8, 0.010696506, 0.518,
  10, 0.012884679, 0.485,
  12, 0.015089135, 0.488,
  14, 0.017276122, 0.494,
  16, 0.019345064, 0.487
) %>%
  mutate(cost_ct = cost_usd * 100)          # convert to cents

## 2 · Fit asymptotic-exponential model --------------------
fit <- nls(F1 ~ Asym - R0 * exp(-lambda * cost_ct),
           data  = df,
           start = list(Asym = 0.78, R0 = 0.12, lambda = 3.5))
confint(fit)

## 3 · Smooth curve ----------------------------------------
grid <- tibble(cost_ct = seq(min(df$cost_ct),
                             max(df$cost_ct),
                             length.out = 400)) %>%
  mutate(F1_hat = predict(fit, newdata = .))

## 4 · Plot with clear x-axis ------------------------------
ggplot(df, aes(cost_ct, F1)) +
  geom_point(size = 3, colour = "steelblue4") +
  geom_line(data = grid, aes(cost_ct, F1_hat),
            colour = "firebrick", size = 1.2) +
  scale_x_continuous(
    breaks = seq(0, 2, by = 0.25),          # every quarter-cent
    labels = function(x) paste0(x, "¢")     # append cent symbol
  ) +
  labs(title    = "Asymptotic exponential fit: cost vs. F1",
       x = "Per-query cost (US cents)",
       y = "F1 score",
       subtitle = sprintf("Plateau F∞ ≈ %.3f  |  half-rise cost ≈ %.2f ¢",
                          coef(fit)["Asym"],
                          log(2) / coef(fit)["lambda"])) +
  theme_minimal(base_size = 12) +
  geom_text(aes(label = k_shot),
            vjust = -0.8, size = 3, colour = "grey30")

######################################################################################
# analyze sign
######################################################################################
task <- 'Task: Identify the signs of diseases mentioned in the following text.\n'
guidance <- 'Definition: A sign of a disease is an objective evidence of disease that can be observed or detected by someone other than the individual affected by the disease. It includes measurable indicators such as physical findings, laboratory test results, and imaging studies, which provide concrete evidence of a medical condition.\n'
format <- 'Output format: Output only the exact sign names without any additional changes. If there are multiple signs, separate their names with commas. If there is no sign, output \'none\'.\n'
error <- 'Guidance: Differentiate between signs and symptoms. Symptoms are subjective experiences of disease reported by the patient and cannot be directly measured by healthcare providers. Only output signs, not symptoms.\n'
prefix <- 'The text from which you need to exact the signs of diseases is: '
result <- list()

for(shot.train in c(0,1,2,4,6,8,10,12,14,16)){
  questions <- c()
  #shot.train <- 0
  print(shot.train)
  system.time({
    set.seed(2024)
    for(i in 1:208){
      #i <- 1
      #print(i)
      a <- test$text[[i]]
      question <- paste(task, format, prefix, a, sep = '')
      #cat(question)
      
      # knn pick up
      distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
      neighbor <- order(distances, decreasing = F)[0:shot.train]  # nearest
      examples <- c()
      for(j in neighbor){
        #j <- neighbor[1]
        text <- train[["text"]][[j]]
        type <- train[["label"]][[j]]
        entry <- paste(type$entry[type$type=='SIGN'], collapse = ', '); entry
        example <- paste('Input text: ', text, ' Output: ', entry, '.\n',sep = ''); example
        examples <- c(examples, example)
      }
      examples <- paste0("Example ", seq_along(examples), ".\n", examples)
      examples <- paste(examples, collapse = "")
      examples <- paste('\n\n Here are demonstration shots:\n', examples, sep = '')
      question <- paste(task, guidance, format, error, examples, prefix, a, sep = ' ')
      #cat(question)
      questions <- c(questions, question)
    }
  })
  result[[as.character(shot.train)]] <- questions
}

df <- data.frame(
  k = rep(names(result), times = sapply(result, length)),
  text = unlist(result, use.names = FALSE),
  stringsAsFactors = FALSE
)
df$token_count <- sapply(strsplit(df$text, "\\s+"), length)
df$cost <- df$token_count / 0.75 * 5 / 1e6
df_avg_cost <- aggregate(cost ~ k, data = df, FUN = mean)
str(df_avg_cost)
df_avg_cost$k <- as.numeric(df_avg_cost$k)
df_avg_cost <- df_avg_cost[order(df_avg_cost$k), ]
colnames(df_avg_cost)[1] <- 'k-shot'
F1 <- c(0.278, 0.408,0.430,0.460,0.460,0.468,0.471,0.466,0.463,0.465)
df_avg_cost$F1 <- F1

############################################################
## Asymptotic-exponential cost-performance analysis
############################################################
library(tidyverse)

## 1 · Data  (unchanged) -----------------------------------
df <- tribble(
  ~k_shot, ~cost_usd,      ~F1,
  0, 0.001957244, 0.278,
  1, 0.003146891, 0.408,
  2, 0.004329071, 0.430,
  4, 0.006736250, 0.460,
  6, 0.009128846, 0.460,
  8, 0.011368205, 0.468,
  10, 0.013725128, 0.471,
  12, 0.016079904, 0.466,
  14, 0.018418910, 0.463,
  16, 0.020622756, 0.465
) %>%
  mutate(cost_ct = cost_usd * 100)          # convert to cents

## 2 · Fit asymptotic-exponential model --------------------
fit <- nls(F1 ~ Asym - R0 * exp(-lambda * cost_ct),
           data  = df,
           start = list(Asym = 0.78, R0 = 0.12, lambda = 3.5))
confint(fit)

## 3 · Smooth curve ----------------------------------------
grid <- tibble(cost_ct = seq(min(df$cost_ct),
                             max(df$cost_ct),
                             length.out = 400)) %>%
  mutate(F1_hat = predict(fit, newdata = .))

## 4 · Plot with clear x-axis ------------------------------
ggplot(df, aes(cost_ct, F1)) +
  geom_point(size = 3, colour = "steelblue4") +
  geom_line(data = grid, aes(cost_ct, F1_hat),
            colour = "firebrick", size = 1.2) +
  scale_x_continuous(
    breaks = seq(0, 2, by = 0.25),          # every quarter-cent
    labels = function(x) paste0(x, "¢")     # append cent symbol
  ) +
  labs(title    = "Asymptotic exponential fit: cost vs. F1",
       x = "Per-query cost (US cents)",
       y = "F1 score",
       subtitle = sprintf("Plateau F∞ ≈ %.3f  |  half-rise cost ≈ %.2f ¢",
                          coef(fit)["Asym"],
                          log(2) / coef(fit)["lambda"])) +
  theme_minimal(base_size = 12) +
  geom_text(aes(label = k_shot),
            vjust = -0.8, size = 3, colour = "grey30")


######################################################################################
# analyze symptom
######################################################################################
task <- 'Task: Identify the symptoms of diseases mentioned in the following text.\n'
guidance <- 'Definition: Symptoms are subjective experiences reported by the patient, which cannot be directly observed or measured by others. They reflect what the patient feels, such as pain, fatigue, or nausea. Symptoms are experienced internally and rely on the patient’s description.\n'
format <- 'Output format: Output only the exact symptom names without any additional changes. If there are multiple symptoms, separate their names with commas. If there is no symptom, output \'none\'.\n'
error <- 'Guidance: Differentiate between symptoms and signs. Signs are objective indicators of a disease that can be observed, measured, or detected by someone other than the patient, such as a doctor or medical professional. Only output symptoms, not signs.\n'
prefix <- 'The text from which you need to exact the symptoms of diseases is: '
result <- list()

for(shot.train in c(0,1,2,4,6,8,10,12,14,16)){
  questions <- c()
  #shot.train <- 0
  print(shot.train)
  system.time({
    set.seed(2024)
    for(i in 1:208){
      #i <- 1
      #print(i)
      a <- test$text[[i]]
      question <- paste(task, format, prefix, a, sep = '')
      #cat(question)
      
      # knn pick up
      distances <- sapply(embedding.train, euclidean_distance, v2 = embedding.test[[i]])
      neighbor <- order(distances, decreasing = F)[0:shot.train]  # nearest
      examples <- c()
      for(j in neighbor){
        #j <- neighbor[1]
        text <- train[["text"]][[j]]
        type <- train[["label"]][[j]]
        entry <- paste(type$entry[type$type=='SYMPTOM'], collapse = ', '); entry
        example <- paste('Input text: ', text, ' Output: ', entry, '.\n',sep = ''); example
        examples <- c(examples, example)
      }
      examples <- paste0("Example ", seq_along(examples), ".\n", examples)
      examples <- paste(examples, collapse = "")
      examples <- paste('\n\n Here are demonstration shots:\n', examples, sep = '')
      question <- paste(task, guidance, format, error, examples, prefix, a, sep = ' ')
      #cat(question)
      questions <- c(questions, question)
    }
  })
  result[[as.character(shot.train)]] <- questions
}

df <- data.frame(
  k = rep(names(result), times = sapply(result, length)),
  text = unlist(result, use.names = FALSE),
  stringsAsFactors = FALSE
)
df$token_count <- sapply(strsplit(df$text, "\\s+"), length)
df$cost <- df$token_count / 0.75 * 5 / 1e6
df_avg_cost <- aggregate(cost ~ k, data = df, FUN = mean)
str(df_avg_cost)
df_avg_cost$k <- as.numeric(df_avg_cost$k)
df_avg_cost <- df_avg_cost[order(df_avg_cost$k), ]
colnames(df_avg_cost)[1] <- 'k-shot'
F1 <- c(0.230, 0.189,0.193,0.194,0.205,0.209,0.207,0.212,0.223,0.195)
df_avg_cost$F1 <- F1

############################################################
## Asymptotic-exponential cost-performance analysis
############################################################
library(tidyverse)

## 1 · Data  (unchanged) -----------------------------------
df <- tribble(
  ~k_shot, ~cost_usd,      ~F1,
  0, 0.001950577, 0.230,
  1, 0.003036218, 0.189,
  2, 0.004124391, 0.193,
  4, 0.006331987, 0.194,
  6, 0.008518141, 0.205,
  8, 0.010581090, 0.209,
  10, 0.012737212, 0.207,
  12, 0.014905962, 0.212,
  14, 0.017058782, 0.223,
  16, 0.019092436, 0.195
) %>%
  mutate(cost_ct = cost_usd * 100)          # convert to cents

## 2 · Fit asymptotic-exponential model --------------------
fit <- nls(F1 ~ Asym - R0 * exp(-lambda * cost_ct),
           data  = df,
           start = list(Asym = 0.78, R0 = 0.12, lambda = 3.5))
confint(fit)

## 3 · Smooth curve ----------------------------------------
grid <- tibble(cost_ct = seq(min(df$cost_ct),
                             max(df$cost_ct),
                             length.out = 400)) %>%
  mutate(F1_hat = predict(fit, newdata = .))

## 4 · Plot with clear x-axis ------------------------------
ggplot(df, aes(cost_ct, F1)) +
  geom_point(size = 3, colour = "steelblue4") +
  geom_line(data = grid, aes(cost_ct, F1_hat),
            colour = "firebrick", size = 1.2) +
  scale_x_continuous(
    breaks = seq(0, 2, by = 0.25),          # every quarter-cent
    labels = function(x) paste0(x, "¢")     # append cent symbol
  ) +
  labs(title    = "Asymptotic exponential fit: cost vs. F1",
       x = "Per-query cost (US cents)",
       y = "F1 score",
       subtitle = sprintf("Plateau F∞ ≈ %.3f  |  half-rise cost ≈ %.2f ¢",
                          coef(fit)["Asym"],
                          log(2) / coef(fit)["lambda"])) +
  theme_minimal(base_size = 12) +
  geom_text(aes(label = k_shot),
            vjust = -0.8, size = 3, colour = "grey30")



ggplot(df, aes(cost_ct, F1)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.75, se = TRUE) +
  labs(title = "Symptom: loess smoother reveals no systematic cost gain")


############################################################
## plot together
############################################################
df_rare <- tribble(
  ~k_shot, ~cost_usd,      ~F1,
  0, 0.001883910, 0.702,
  1, 0.002991603, 0.750,
  2, 0.004001920, 0.746,
  4, 0.006355224, 0.760,
  6, 0.008588365, 0.759,
  8, 0.010695417, 0.768,
  10, 0.012896827, 0.762,
  12, 0.015110032, 0.765,
  14, 0.017309263, 0.758,
  16, 0.019385160, 0.774
) %>%
  mutate(cost_ct = cost_usd * 100)          # convert to cents

df_disease <- tribble(
  ~k_shot, ~cost_usd,      ~F1,
  0, 0.001937244, 0.314,
  1, 0.003038109, 0.392,
  2, 0.004141891, 0.467,
  4, 0.006387468, 0.471,
  6, 0.008604872, 0.498,
  8, 0.010696506, 0.518,
  10, 0.012884679, 0.485,
  12, 0.015089135, 0.488,
  14, 0.017276122, 0.494,
  16, 0.019345064, 0.487
) %>%
  mutate(cost_ct = cost_usd * 100)          # convert to cents

df_sign <- tribble(
  ~k_shot, ~cost_usd,      ~F1,
  0, 0.001957244, 0.278,
  1, 0.003146891, 0.408,
  2, 0.004329071, 0.430,
  4, 0.006736250, 0.460,
  6, 0.009128846, 0.460,
  8, 0.011368205, 0.468,
  10, 0.013725128, 0.471,
  12, 0.016079904, 0.466,
  14, 0.018418910, 0.463,
  16, 0.020622756, 0.465
) %>%
  mutate(cost_ct = cost_usd * 100)          # convert to cents

df_symptom <- tribble(
  ~k_shot, ~cost_usd,      ~F1,
  0, 0.001950577, 0.230,
  1, 0.003036218, 0.189,
  2, 0.004124391, 0.193,
  4, 0.006331987, 0.194,
  6, 0.008518141, 0.205,
  8, 0.010581090, 0.209,
  10, 0.012737212, 0.207,
  12, 0.014905962, 0.212,
  14, 0.017058782, 0.223,
  16, 0.019092436, 0.195
) %>%
  mutate(cost_ct = cost_usd * 100)          # convert to cents

library(tidyverse)

## 0 · Bind the four data frames and tag with entity ----------
all_df <- bind_rows(
  df_rare    %>% mutate(entity = "Rare disease"),
  df_disease %>% mutate(entity = "Disease"),
  df_sign    %>% mutate(entity = "Sign"),
  df_symptom %>% mutate(entity = "Symptom")
) %>%
  mutate(cost_ct = cost_usd * 100)      # → cents

## 1 · Split into a list *after* entity column is present ------
entity_list <- all_df %>% group_split(entity)

## 2 · Fit one model per list element --------------------------
fit_fun <- function(d) {
  if (unique(d$entity) == "Symptom") {
    loess(F1 ~ cost_ct, data = d, span = 0.75)
  } else {
    nls(F1 ~ Asym - R0 * exp(-lambda * cost_ct),
        data  = d,
        start = list(Asym   = max(d$F1),
                     R0     = max(d$F1) - min(d$F1),
                     lambda = 3))
  }
}

models <- map(entity_list, fit_fun)
names(models) <- map_chr(entity_list, ~ unique(.x$entity))

## 3 · Prediction grid (one row per entity × cost value) -------
grid <- map_dfr(entity_list, function(d) {
  ent   <- unique(d$entity)
  cost_seq <- seq(min(d$cost_ct), max(d$cost_ct), length.out = 300)
  tibble(entity = ent,
         cost_ct = cost_seq,
         F1_hat  = predict(models[[ent]],
                           newdata = data.frame(cost_ct = cost_seq)))
})

## 4 · Overlay plot -------------------------------------------
ggplot(all_df, aes(cost_ct, F1, colour = entity)) +
  geom_point(size = 2.5) +
  geom_line(data = grid, aes(cost_ct, F1_hat, colour = entity), size = 1.1) +
  geom_text(aes(label = k_shot),
            vjust = -0.9, size = 3, colour = "black", show.legend = FALSE) +
  scale_colour_manual(values = c("steelblue4", "firebrick",
                                 "seagreen4", "purple")) +
  scale_x_continuous(breaks = seq(0, 2, 0.25),
                     labels = function(x) paste0(x, "¢")) +
  labs(title   = NULL,
       x       = "\nPer-query cost (US cents)",
       y       = "F1 score",
       colour  = "Entity") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.title=element_blank(),
        axis.text   = element_text(colour = "black"),  
        strip.text  = element_text(face = "bold", colour = "black", size=12))

