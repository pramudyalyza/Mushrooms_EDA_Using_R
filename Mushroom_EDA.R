# ==============GSLC================
# Alyza Rahima Pramudya - 2502032125
# Faishal Kamil         - 2502001063
# Shafa Amira Qonitatin - 2502009173
# ==================================

library(magrittr)
library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)

#=================1A================
mushrooms <- read.csv("C:/Users/Alyza/Downloads/mushrooms.csv") # load msuhroom dataset
dim(mushrooms) # show number of rows and columns (row, column)
colnames(mushrooms)  # show all columns name
head(mushrooms) # show the first six rows
tail(mushrooms) # show the last six rows
str(mushrooms) # show the structure of the dataset
View(mushrooms)  # open a new window to show us all the data from mushroom dataset

# counts the number of each unique value in column class
table(mushrooms$class)

mushroom_table <- lapply(seq(from=2, to=ncol(mushrooms)), 
                         function(x) {table(mushrooms$class, mushrooms[,x])})
names(mushroom_table) <- colnames(mushrooms)[2:ncol(mushrooms)]
for(i in 1:length(mushroom_table)) {
  print("======================================")
  print(names(mushroom_table)[i])
  print(mushroom_table[[i]]) 
}

mushrooms <- mushrooms %>% select(- veil.type)
colnames(mushrooms)

#=================1B================
mushrooms[mushrooms == "?"] <- NA

for(i in 1:ncol(mushrooms)){
  null_count <- sum(is.na(mushrooms[,i]))
  print(paste(colnames(mushrooms)[i], "contains", null_count, "null data."))
}

mushrooms <- mushrooms[, colSums(is.na(mushrooms)) == 0]

for(i in 1:ncol(mushrooms)){
  null_count <- sum(is.na(mushrooms[,i]))
  print(paste(colnames(mushrooms)[i], "contains", null_count, "null data."))
}

duplicated_rows <- duplicated(mushrooms)
sum(duplicated_rows)

#=================1C================

# Plot 1
counts <- table(mushrooms$class)

ggplot(data = data.frame(Class = names(counts), Count = as.vector(counts)), aes(x = Class, y = Count)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(x = "Class", y = "Count", title = "Number of Mushrooms by Class")


# Plot 2
columns <- c('cap.color', 'odor', 'cap.shape', 'veil.color')
par(mfrow = c(2,2))
for(i in columns){
  freq_table <- prop.table(table(mushrooms[,i]))
  barplot(freq_table, main=i, ylab="Proportion")
}

# Create function datavis
dataVis <- function(data, x, y, col) {
  x <- rlang::sym(x)
  y <- rlang::sym(y)
  col <- rlang::sym(col)
  
  ggplot(data = data, aes(x = !!x , y = !!y , col = !!col)) + 
    geom_jitter(alpha = 0.5) + 
    scale_color_manual(values = c("blue", "red"))
}

set.seed(1)
# Plot 3
dataVis(data = mushrooms, x = 'class', y = 'odor', col = 'class')
# Plot 4
dataVis(data = mushrooms, x = 'class', y = 'stalk.surface.above.ring', col = 'class')
# Plot 5
dataVis(data = mushrooms, x = 'class', y = 'stalk.surface.below.ring', col = 'class')
