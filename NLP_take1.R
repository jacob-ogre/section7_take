# First round of NLP analysis for FWS section 7 take.
# Copyright (c) 2016 Defenders of Wildlife, jmalcom@defenders.org

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.
# 

library(cluster)
library(fpc)
library(ggdendro)
library(ggplot2)
library(ggthemes)
library(NLP)
library(quanteda)
library(tm)

##############################################################################
# Get the data ready
load("~/Repos/Defenders/working_papers/_data/take_data.RData")
dim(full)

# What are all of the bull trout consults in this set?
pos <- full[full$BO_species == "Bull Trout (Salvelinus confluentus)" &
            !is.na(full$BO_species), ]
dim(pos)

sub <- full[full$BO_species == "Bull Trout (Salvelinus confluentus)" &
            full$with_take == 1 & !is.na(full$BO_species), ]
dim(sub)
length(unique(sub$take))

##############################################################################
# Start working with take text

# first, how long are these take statements
sub$parts <- sapply(sub$take, FUN = strsplit, split = " ")
sub$n_words <- sapply(sub$parts, FUN = length)
sub$long <- ifelse(sub$n_words > 1, 1, 0)

qplot(sub$n_words, 
      geom = "histogram", 
      xlab = "# words", 
      main = "Length of bull trout take statements") + 
theme_hc()

summary(sub$n_words)

long <- sub[sub$long == 1, ]
take_corp <- corpus(long$take)
docvars(take_corp, "Consult") <- long$activity_code
sum_take_corp <- summary(take_corp)

# look at keywords in context (kwic)
options(width = 200)
num_ctxt <- kwic(take_corp, "[0-9]+", window = 5, valuetype = "regex")
acre_ctxt <- kwic(take_corp, "acre", window = 5, valuetype = "regex")
feet_ctxt <- kwic(take_corp, "(feet)|(ft)", window = 5, valuetype = "regex")
take_ctxt <- kwic(take_corp, "take", window = 5, valuetype = "regex")

take_wtok <- tokenize(take_corp, removePunct = TRUE, ngrams = c(1,2))
take_stok <- tokenize(take_corp, what = "sentence")

take_dfm <- dfm(take_corp, 
                removePunct = TRUE, 
                stem = TRUE, 
                ngrams = c(1,2),
                ignoredFeatures = stopwords("english"))

options(width = 100)
topfeatures(take_dfm, 20)
word_sums <- colSums(take_dfm)

trim_dfm <- trim(take_dfm, minCount = 4, minDoc = 3)
dim(trim_dfm)
dist_mat <- dist(as.matrix(weight(trim_dfm, "relFreq")))
clust <- hclust(dist_mat)
clust$labels <- long$activity_code

par(mar=c(3,5,5,4))
ggdendrogram(clust)

# Let's take a look at k-means clustering
trim_kmean_3_1 <- kmeans(trim_dfm, centers = 3)
trim_kmean_3_2 <- kmeans(weight(trim_dfm, "relFreq"), centers = 3)
table(trim_kmean_3_1$cluster)
table(trim_kmean_3_2$cluster)

tk31_df <- as.data.frame(trim_kmean_3_1$cluster)
tk31_df <- cbind(tk31_df, summary(take_corp, 215))
names(tk31_df) <- c("cluster", "text", "types", "tokens", "sentences", "consult")
head(tk31_df, 20)

ggplot(data = tk31_df, aes(x = factor(cluster), y = tokens)) +
    geom_boxplot() +
    theme_hc()

tk32_df <- as.data.frame(trim_kmean_3_2$cluster)
tk32_df <- cbind(tk32_df, summary(take_corp, 215))
names(tk32_df) <- c("cluster", "text", "types", "tokens", "sentences", "consult")
head(tk32_df, 20)

ggplot(data = tk32_df, aes(x = factor(cluster), y = tokens)) +
    geom_boxplot() +
    theme_hc()

head(trim_kmean_3$cluster, 20)

trim_kmean_4 <- kmeans(trim_dfm, centers = 4)
table(trim_kmean_4$cluster)
trim_kmean_5 <- kmeans(trim_dfm, centers = 5)
table(trim_kmean_5$cluster)
trim_kmean_6 <- kmeans(trim_dfm, centers = 6)
table(trim_kmean_6$cluster)
trim_kmean_8 <- kmeans(trim_dfm, centers = 8)
table(trim_kmean_8$cluster)

trim_kmean_10 <- kmeans(weight(trim_dfm, "relFreq"), centers = 10)
table(trim_kmean_10$cluster)
tk10_df <- as.data.frame(trim_kmean_10$cluster)
tk10_df <- cbind(tk10_df, summary(take_corp, 215))
names(tk10_df) <- c("cluster", "text", "types", "tokens", "sentences", "consult")
head(tk10_df, 20)
ggplot(data = tk10_df, aes(x = factor(cluster), y = tokens)) +
    geom_boxplot() +
    theme_hc()

trim_kmean_15 <- kmeans(weight(trim_dfm, "relFreq"), centers = 15)
table(trim_kmean_15$cluster)
tk15_df <- as.data.frame(trim_kmean_15$cluster)
tk15_df <- cbind(tk15_df, summary(take_corp, 215))
names(tk15_df) <- c("cluster", "text", "types", "tokens", "sentences", "consult")
head(tk15_df, 20)
ggplot(data = tk15_df, aes(x = factor(cluster), y = tokens)) +
    geom_boxplot() +
    theme_hc()


plotcluster(trim_dfm, trim_kmean_3_2$cluster)

similarity(trim_dfm, 
           c("acr", "feet", "take", "individu"), 
           method = "correlation", 
           margin = "features", 
           n = 20)

