#run the tSNE on the Matlab subset

#exclude dyslexia as it is categorical
tsne <- matlab_data %>%
  select(!dyslexia) %>%
  Rtsne(perplexity = 40,
        check_duplicates = FALSE)


#create scatterplot of results of t-SNE analysis

#store tsne data (Y) and dyslexia as labels in dat
dat <- data.frame(tsne$Y, dyslexia = matlab_data$dyslexia)

#order so that dyslexia group is plotted on-top of no dyslexia group
dat <- dat[order(dat$dyslexia, decreasing=TRUE),]

#plot dat (tsne with labels)
tsne_plot <- dat %>%
  ggplot(aes(x = X1, y = X2, colour = dyslexia)) +
  geom_point(size = 2.5) +
  theme_cognitive() +
  scale_color_manual(values = col_sch,
                     name = "Dyslexia",
                     labels = c("Dyslexia", "No Dyslexia"))


file <- "figures/t_sne.jpeg"
ggsave(file, tsne_plot)



