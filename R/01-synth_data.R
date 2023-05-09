
################### load in synthesised data ############

## Dist data

file <- "data-raw/data_dist.txt"
data_dist <- read_table(file)


#add group variable to categorise
data_dist <- data_dist %>%
  add_column("group" = 1:nrow(data_dist))


data_dist$group[1:40] <- "Group 1"
data_dist$group[41:80] <- "Group 2"
data_dist$group[81:120] <- "Group 3"


## Comorb data

file <- "data-raw/data_comorb.txt"
data_comorb <- read_table(file)

#add group variable to categorise
data_comorb <- data_comorb %>%
  add_column("group" = 1:nrow(data_comorb))

data_comorb$group[1:40] <- "Group 1"
data_comorb$group[41:80] <- "Group 2"
data_comorb$group[81:120] <- "Group 3"


## One data

file <- "data-raw/data_one.txt"
data_one <- read_table(file)

#add group variable to categorise
data_one <- data_one %>%
  add_column("group" = 1:nrow(data_one))

data_one$group[1:40] <- "Group 1"
data_one$group[41:80] <- "Group 2"
data_one$group[81:120] <- "Group 3"



# colour scheme for synthesised data

synth_colour <- c("#2587be", "#d5bb2a", "#cf1609")


################## PCA #####################

#PCA on dist data

pca <- data_dist %>%
  select(!group) %>%
  prcomp(scale. = TRUE)

pca_labelled <- data.frame(pca$x, group = data_dist$group)

#plot the first two principle components
pca_labelled %>%
  ggplot(aes(x = PC1, y = PC2, color = group)) +
  geom_point(size = 2.5) +
  theme_cognitive() +
  labs(color = "Group") +
  scale_color_manual(values = synth_colour)



#PCA on comorb data

pca <- data_comorb %>%
  select(!group) %>%
  prcomp(scale. = TRUE)

pca_labelled <- data.frame(pca$x, group = data_comorb$group)

#plot the first two principle components
pca_labelled %>%
  ggplot(aes(x = PC1, y = PC2, color = group)) +
  geom_point(size = 2.5) +
  theme_cognitive() +
  labs(color = "Group") +
  scale_color_manual(values = synth_colour)



#PCA on ONE data

pca <- data_one %>%
  select(!group) %>%
  prcomp(scale. = TRUE)

pca_labelled <- data.frame(pca$x, group = data_one$group)

#plot the first two principle components
pca_labelled %>%
  ggplot(aes(x = PC1, y = PC2, color = group)) +
  geom_point(size = 2.5) +
  theme_cognitive() +
  labs(color = "Group") +
  scale_color_manual(values = synth_colour)



################## MDS #####################


# run mds on the DISTINC data

mds <- data_dist %>%
  select(!group) %>%
  dist() %>%
  cmdscale() %>%
  data.frame()

#add names to the columns
colnames(mds) <- c("Dimension 1", "Dimension 2")

#add the group variable to colour
mds <- mds %>%
  add_column(group = data_dist$group)

#plot the 2 dimensions
mds %>%
  ggplot(aes(x = `Dimension 1`, y = `Dimension 2`, color = group)) +
  geom_point(size = 1.5) +
  theme_cognitive()+
  labs(color = "Group") +
  scale_color_manual(values = synth_colour)




# run mds on the COMORB data

mds <- data_comorb %>%
  select(!group) %>%
  dist() %>%
  cmdscale() %>%
  data.frame()

#add names to the columns
colnames(mds) <- c("Dimension 1", "Dimension 2")

#add the group variable to colour
mds <- mds %>%
  add_column(group = data_comorb$group)

#plot the 2 dimensions
mds %>%
  ggplot(aes(x = `Dimension 1`, y = `Dimension 2`, color = group)) +
  geom_point(size = 1.5) +
  theme_cognitive() +
  labs(color = "Group") +
  scale_color_manual(values = synth_colour)



# run mds on the ONE data

mds <- data_one %>%
  select(!group) %>%
  dist() %>%
  cmdscale() %>%
  data.frame()

#add names to the columns
colnames(mds) <- c("Dimension 1", "Dimension 2")

#add the group variable to colour
mds <- mds %>%
  add_column(group = data_one$group)

#plot the 2 dimensions
mds %>%
  ggplot(aes(x = `Dimension 1`, y = `Dimension 2`, color = group)) +
  geom_point(size = 1.5) +
  theme_cognitive() +
  labs(color = "Group") +
  scale_color_manual(values = synth_colour)



################## T-SNE ###################

#run the tSNE on DIST data

#exclude group as it is categorical
tsne <- data_dist %>%
  select(!group) %>%
  Rtsne(perplexity = 30,
        check_duplicates = FALSE)

#store tsne data (Y) and group as labels in dat
dat <- data.frame(tsne$Y, type = data_dist$group)

#plot the tsne
dat %>%
  ggplot(aes(x = X1, y = X2, colour = type)) +
  geom_point(size = 2) +
  theme_cognitive() +
  labs(color = "Group") +
  scale_color_manual(values = synth_colour)


#run the tSNE on COMORB data

#exclude group as it is categorical
tsne <- data_comorb %>%
  select(!group) %>%
  Rtsne(perplexity = 30,
        check_duplicates = FALSE)

#store tsne data (Y) and group as labels in dat
dat <- data.frame(tsne$Y, type = data_comorb$group)

#plot the tsne
dat %>%
  ggplot(aes(x = X1, y = X2, colour = type)) +
  geom_point(size = 2) +
  theme_cognitive() +
  labs(color = "Group") +
  scale_color_manual(values = synth_colour)



#run the tSNE on ONE data

#exclude group as it is categorical
tsne <- data_one %>%
  select(!group) %>%
  Rtsne(perplexity = 30,
        check_duplicates = FALSE)

#store tsne data (Y) and group as labels in dat
dat <- data.frame(tsne$Y, type = data_one$group)

#plot the tsne
dat %>%
  ggplot(aes(x = X1, y = X2, colour = type)) +
  geom_point(size = 2) +
  theme_cognitive() +
  labs(color = "Group") +
  scale_color_manual(values = synth_colour)

