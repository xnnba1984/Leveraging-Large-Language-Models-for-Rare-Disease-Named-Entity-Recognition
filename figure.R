library(ggplot2)
library(readxl)

setwd("~/Library/CloudStorage/Box-Box/Xi/LLM")

##########################################################################################################
# F1
##########################################################################################################
# F1 rare
data <- data.frame(read_excel("figure.xlsx", sheet = 1))
str(data)
data$example <- factor(data$example, levels=as.character(c(1, seq(2, 16, 2))))
data$method <- factor(data$method, levels=c('Inquiry-Random','Inquiry-KNN','Cluster-KNN-32','Cluster-KNN-64'))
data <- data[data$type=='rare',]

pdf('figure/rare_F1.pdf', width = 10, height = 7)
ggplot(data, aes(x = example, y=value, group=method)) +
  geom_line(size=1.5, aes(color=method)) +
  geom_point(aes(color=method), size = 2.5) +
  scale_color_manual(values=c("#66A61E", "#D95F02", "#7570B3", '#E6AB02')) +
  labs(y = "", x = "Learning Examples") +
  scale_y_continuous(limits = c(0.66, 0.78), breaks = seq(0.66, 0.78, by = 0.02)) +
  geom_hline(yintercept=0.704,linetype=2, color='black', size=0.8) +
  geom_hline(yintercept=0.702,linetype=2, color='black', size=0.8) +
  ggtitle("Rare Disease") +
  geom_text(x=7.5, y=0.709, label="SOTA=0.704", color='black', size=6) +
  geom_text(x=7.4, y=0.697, label="Zero-shot=0.702", color='black', size=6) +
  theme(axis.text.x = element_text(color = 'black'), 
        axis.text.y = element_text(color = 'black'),
        text = element_text(size = 18),
        legend.title=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# F1 disease
data <- data.frame(read_excel("figure.xlsx", sheet = 1))
str(data)
data$example <- factor(data$example, levels=as.character(c(1, seq(2, 16, 2))))
data$method <- factor(data$method, levels=c('Inquiry-Random','Inquiry-KNN','Cluster-KNN-32','Cluster-KNN-64'))
data <- data[data$type=='disease',]

pdf('figure/disease_F1.pdf', width = 10, height = 7)
ggplot(data, aes(x = example, y=value, group=method)) +
  geom_line(size=1.5, aes(color=method)) +
  geom_point(aes(color=method), size = 2.5) +
  scale_color_manual(values=c("#66A61E", "#D95F02", "#7570B3", '#E6AB02')) +
  labs(y = "", x = "Learning Examples") +
  scale_y_continuous(limits = c(0.29, 0.55), breaks = seq(0.3, 0.55, by = 0.05)) +
  geom_hline(yintercept=0.491,linetype=2, color='black', size=0.8) +
  geom_hline(yintercept=0.314,linetype=2, color='black', size=0.8) +
  ggtitle("Disease") +
  geom_text(x=2, y=0.5, label="SOTA=0.491", color='black', size=6) +
  geom_text(x=2, y=0.305, label="Zero-shot=0.314", color='black', size=6) +
  theme(axis.text.x = element_text(color = 'black'), 
        axis.text.y = element_text(color = 'black'),
        text = element_text(size = 18),
        legend.title=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()


# F1 sign
data <- data.frame(read_excel("figure.xlsx", sheet = 1))
data$example <- factor(data$example, levels=as.character(c(1, seq(2, 16, 2))))
data$method <- factor(data$method, levels=c('Inquiry-Random','Inquiry-KNN','Cluster-KNN-32','Cluster-KNN-64'))
data <- data[data$type=='sign',]

pdf('figure/sign_F1.pdf', width = 10, height = 7)
ggplot(data, aes(x = example, y=value, group=method)) +
  geom_line(size=1.5, aes(color=method)) +
  geom_point(aes(color=method), size = 2.5) +
  scale_color_manual(values=c("#66A61E", "#D95F02", "#7570B3", '#E6AB02')) +
  labs(y = "", x = "Learning Examples") +
  scale_y_continuous(limits = c(0.375, 0.54), breaks = seq(0.4, 0.54, by = 0.02)) +
  geom_hline(yintercept=0.538,linetype=2, color='black', size=0.8) +
  geom_hline(yintercept=0.392,linetype=2, color='black', size=0.8) +
  ggtitle("Sign") +
  geom_text(x=8, y=0.53, label="SOTA=0.538", color='black', size=6) +
  geom_text(x=8, y=0.385, label="Zero-shot=0.392", color='black', size=6) +
  theme(axis.text.x = element_text(color = 'black'), 
        axis.text.y = element_text(color = 'black'),
        text = element_text(size = 18),
        legend.title=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()


# F1 symptom
data <- data.frame(read_excel("figure.xlsx", sheet = 1))
data$example <- factor(data$example, levels=as.character(c(1, seq(2, 16, 2))))
data$method <- factor(data$method, levels=c('Inquiry-Random','Inquiry-KNN','Cluster-KNN-32','Cluster-KNN-64'))
data <- data[data$type=='symptom',]

pdf('figure/symptom_F1.pdf', width = 10, height = 7)
ggplot(data, aes(x = example, y=value, group=method)) +
  geom_line(size=1.5, aes(color=method)) +
  geom_point(aes(color=method), size = 2.5) +
  scale_color_manual(values=c("#66A61E", "#D95F02", "#7570B3", '#E6AB02')) +
  labs(y = "", x = "Learning Examples") +
  scale_y_continuous(limits = c(0.17, 0.25), breaks = seq(0.17, 0.23, by = 0.01)) +
  geom_hline(yintercept=0.25,linetype=2, color='black', size=0.8) +
  geom_hline(yintercept=0.23,linetype=2, color='black', size=0.8) +
  ggtitle("Symptom") +
  geom_text(x=8, y=0.247, label="SOTA=0.648", color='black', size=6) +
  geom_text(x=8, y=0.227, label="Zero-Shot=0.230", color='black', size=6) +
  theme(axis.text.x = element_text(color = 'black'), 
        axis.text.y = element_text(color = 'black'),
        text = element_text(size = 18),
        legend.title=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

##########################################################################################################
# Precision
##########################################################################################################
# Precision rare
data <- data.frame(read_excel("figure.xlsx", sheet = 2))
str(data)
data$example <- factor(data$example, levels=as.character(c(1, seq(2, 16, 2))))
data$method <- factor(data$method, levels=c('Inquiry-Random','Inquiry-KNN','Cluster-KNN-32','Cluster-KNN-64'))
data <- data[data$type=='rare',]
summary(data$value)

pdf('figure/rare_prec.pdf', width = 10, height = 7)
ggplot(data, aes(x = example, y=value, group=method)) +
  geom_line(size=1.5, aes(color=method)) +
  geom_point(aes(color=method), size = 2.5) +
  scale_color_manual(values=c("#66A61E", "#D95F02", "#7570B3", '#E6AB02')) +
  labs(y = "", x = "Learning Examples") +
  scale_y_continuous(limits = c(0.82, 0.93), breaks = seq(0.85, 0.93, by = 0.02)) +
  geom_hline(yintercept=0.83,linetype=2, color='black', size=0.8) +
  geom_hline(yintercept=0.897,linetype=2, color='black', size=0.8) +
  ggtitle("Rare Disease") +
  geom_text(x=7.5, y=0.827, label="SOTA=0.689", color='black', size=6) +
  geom_text(x=7.6, y=0.9, label="Zero-shot=0.897", color='black', size=6) +
  theme(axis.text.x = element_text(color = 'black'), 
        axis.text.y = element_text(color = 'black'),
        text = element_text(size = 18),
        legend.title=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# precision disease
data <- data.frame(read_excel("figure.xlsx", sheet = 2))
str(data)
data$example <- factor(data$example, levels=as.character(c(1, seq(2, 16, 2))))
data$method <- factor(data$method, levels=c('Inquiry-Random','Inquiry-KNN','Cluster-KNN-32','Cluster-KNN-64'))
data <- data[data$type=='disease',]
summary(data$value)

pdf('figure/disease_prec.pdf', width = 10, height = 7)
ggplot(data, aes(x = example, y=value, group=method)) +
  geom_line(size=1.5, aes(color=method)) +
  geom_point(aes(color=method), size = 2.5) +
  scale_color_manual(values=c("#66A61E", "#D95F02", "#7570B3", '#E6AB02')) +
  labs(y = "", x = "Learning Examples") +
  scale_y_continuous(limits = c(0.39, 0.56), breaks = seq(0.39, 0.56, by = 0.04)) +
  geom_hline(yintercept=0.494,linetype=2, color='black', size=0.8) +
  geom_hline(yintercept=0.545,linetype=2, color='black', size=0.8) +
  ggtitle("Disease") +
  geom_text(x=7.4, y=0.489, label="SOTA=0.494", color='black', size=6) +
  geom_text(x=7.5, y=0.549, label="Zero-shot=0.545", color='black', size=6) +
  theme(axis.text.x = element_text(color = 'black'), 
        axis.text.y = element_text(color = 'black'),
        text = element_text(size = 18),
        legend.title=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# precision sign
data <- data.frame(read_excel("figure.xlsx", sheet = 2))
data$example <- factor(data$example, levels=as.character(c(1, seq(2, 16, 2))))
data$method <- factor(data$method, levels=c('Inquiry-Random','Inquiry-KNN','Cluster-KNN-32','Cluster-KNN-64'))
data <- data[data$type=='sign',]
summary(data$value)

pdf('figure/sign_prec.pdf', width = 10, height = 7)
ggplot(data, aes(x = example, y=value, group=method)) +
  geom_line(size=1.5, aes(color=method)) +
  geom_point(aes(color=method), size = 2.5) +
  scale_color_manual(values=c("#66A61E", "#D95F02", "#7570B3", '#E6AB02')) +
  labs(y = "", x = "Learning Examples") +
  scale_y_continuous(limits = c(0.37, 0.51), breaks = seq(0.37, 0.48, by = 0.02)) +
  geom_hline(yintercept=0.5,linetype=2, color='black', size=0.8) +
  geom_hline(yintercept=0.377,linetype=2, color='black', size=0.8) +
  ggtitle("Sign") +
  geom_text(x=7, y=0.505, label="SOTA=0.561", color='black', size=6) +
  geom_text(x=7, y=0.382, label="Zero-shot=0.377", color='black', size=6) +
  theme(axis.text.x = element_text(color = 'black'), 
        axis.text.y = element_text(color = 'black'),
        text = element_text(size = 18),
        legend.title=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# prec symptom
data <- data.frame(read_excel("figure.xlsx", sheet = 2))
data$example <- factor(data$example, levels=as.character(c(1, seq(2, 16, 2))))
data$method <- factor(data$method, levels=c('Inquiry-Random','Inquiry-KNN','Cluster-KNN-32','Cluster-KNN-64'))
data <- data[data$type=='symptom',]
summary(data$value)

pdf('figure/symptom_prec.pdf', width = 10, height = 7)
ggplot(data, aes(x = example, y=value, group=method)) +
  geom_line(size=1.5, aes(color=method)) +
  geom_point(aes(color=method), size = 2.5) +
  scale_color_manual(values=c("#66A61E", "#D95F02", "#7570B3", '#E6AB02')) +
  labs(y = "", x = "Learning Examples") +
  scale_y_continuous(limits = c(0.1, 0.155), breaks = seq(0.1, 0.14, by = 0.01)) +
  geom_hline(yintercept=0.155,linetype=2, color='black', size=0.8) +
  geom_hline(yintercept=0.142,linetype=2, color='black', size=0.8) +
  ggtitle("Symptom") +
  geom_text(x=8, y=0.153, label="SOTA=0.667", color='black', size=6) +
  geom_text(x=8, y=0.14, label="Zero-shot=0.142", color='black', size=6) +
  theme(axis.text.x = element_text(color = 'black'), 
        axis.text.y = element_text(color = 'black'),
        text = element_text(size = 18),
        legend.title=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()


##########################################################################################################
# recall
##########################################################################################################
# recall rare
data <- data.frame(read_excel("figure.xlsx", sheet = 3))
str(data)
data$example <- factor(data$example, levels=as.character(c(1, seq(2, 16, 2))))
data$method <- factor(data$method, levels=c('Inquiry-Random','Inquiry-KNN','Cluster-KNN-32','Cluster-KNN-64'))
data <- data[data$type=='rare',]
summary(data$value)

pdf('figure/rare_recall.pdf', width = 10, height = 7)
ggplot(data, aes(x = example, y=value, group=method)) +
  geom_line(size=1.5, aes(color=method)) +
  geom_point(aes(color=method), size = 2.5) +
  scale_color_manual(values=c("#66A61E", "#D95F02", "#7570B3", '#E6AB02')) +
  labs(y = "", x = "Learning Examples") +
  scale_y_continuous(limits = c(0.53, 0.73), breaks = seq(0.53, 0.73, by = 0.03)) +
  geom_hline(yintercept=0.720,linetype=2, color='black', size=0.8) +
  geom_hline(yintercept=0.576,linetype=2, color='black', size=0.8) +
  ggtitle("Rare Disease") +
  geom_text(x=7.5, y=0.715, label="SOTA=0.720", color='black', size=6) +
  geom_text(x=7.6, y=0.581, label="Zero-shot=0.576", color='black', size=6) +
  theme(axis.text.x = element_text(color = 'black'), 
        axis.text.y = element_text(color = 'black'),
        text = element_text(size = 18),
        legend.title=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# recall disease
data <- data.frame(read_excel("figure.xlsx", sheet = 3))
str(data)
data$example <- factor(data$example, levels=as.character(c(1, seq(2, 16, 2))))
data$method <- factor(data$method, levels=c('Inquiry-Random','Inquiry-KNN','Cluster-KNN-32','Cluster-KNN-64'))
data <- data[data$type=='disease',]
summary(data$value)

pdf('figure/disease_recall.pdf', width = 10, height = 7)
ggplot(data, aes(x = example, y=value, group=method)) +
  geom_line(size=1.5, aes(color=method)) +
  geom_point(aes(color=method), size = 2.5) +
  scale_color_manual(values=c("#66A61E", "#D95F02", "#7570B3", '#E6AB02')) +
  labs(y = "", x = "Learning Examples") +
  scale_y_continuous(limits = c(0.21, 0.52), breaks = seq(0.24, 0.52, by = 0.04)) +
  geom_hline(yintercept=0.488,linetype=2, color='black', size=0.8) +
  geom_hline(yintercept=0.221,linetype=2, color='black', size=0.8) +
  ggtitle("Disease") +
  geom_text(x=7.4, y=0.496, label="SOTA=0.488", color='black', size=6) +
  geom_text(x=7.5, y=0.23, label="Zero-shot=0.221", color='black', size=6) +
  theme(axis.text.x = element_text(color = 'black'), 
        axis.text.y = element_text(color = 'black'),
        text = element_text(size = 18),
        legend.title=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# recall sign
data <- data.frame(read_excel("figure.xlsx", sheet = 3))
data$example <- factor(data$example, levels=as.character(c(1, seq(2, 16, 2))))
data$method <- factor(data$method, levels=c('Inquiry-Random','Inquiry-KNN','Cluster-KNN-32','Cluster-KNN-64'))
data <- data[data$type=='sign',]
summary(data$value)

pdf('figure/sign_recall.pdf', width = 10, height = 7)
ggplot(data, aes(x = example, y=value, group=method)) +
  geom_line(size=1.5, aes(color=method)) +
  geom_point(aes(color=method), size = 2.5) +
  scale_color_manual(values=c("#66A61E", "#D95F02", "#7570B3", '#E6AB02')) +
  labs(y = "", x = "Learning Examples") +
  scale_y_continuous(limits = c(0.35, 0.52), breaks = seq(0.37, 0.52, by = 0.02)) +
  geom_hline(yintercept=0.516,linetype=2, color='black', size=0.8) +
  geom_hline(yintercept=0.35,linetype=2, color='black', size=0.8) +
  ggtitle("Sign") +
  geom_text(x=7, y=0.510, label="SOTA=0.516", color='black', size=6) +
  geom_text(x=7, y=0.355, label="Zero-shot=0.221", color='black', size=6) +
  theme(axis.text.x = element_text(color = 'black'), 
        axis.text.y = element_text(color = 'black'),
        text = element_text(size = 18),
        legend.title=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# prec symptom
data <- data.frame(read_excel("figure.xlsx", sheet = 3))
data$example <- factor(data$example, levels=as.character(c(1, seq(2, 16, 2))))
data$method <- factor(data$method, levels=c('Inquiry-Random','Inquiry-KNN','Cluster-KNN-32','Cluster-KNN-64'))
data <- data[data$type=='symptom',]
summary(data$value)

pdf('figure/symptom_recall.pdf', width = 10, height = 7)
ggplot(data, aes(x = example, y=value, group=method)) +
  geom_line(size=1.5, aes(color=method)) +
  geom_point(aes(color=method), size = 2.5) +
  scale_color_manual(values=c("#66A61E", "#D95F02", "#7570B3", '#E6AB02')) +
  labs(y = "", x = "Learning Examples") +
  scale_y_continuous(limits = c(0.56, 0.68), breaks = seq(0.56, 0.68, by = 0.02)) +
  geom_hline(yintercept=0.630,linetype=2, color='black', size=0.8) +
  geom_hline(yintercept=0.612,linetype=2, color='black', size=0.8) +
  ggtitle("Symptom") +
  geom_text(x=8, y=0.627, label="SOTA=0.630", color='black', size=6) +
  geom_text(x=8, y=0.609, label="Zero-shot=0.612", color='black', size=6) +
  theme(axis.text.x = element_text(color = 'black'), 
        axis.text.y = element_text(color = 'black'),
        text = element_text(size = 18),
        legend.title=element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()
