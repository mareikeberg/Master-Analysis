pca <- matlab_data %>%
  select(!dyslexia) %>%
  prcomp(scale. = TRUE)

pca_labelled <- data.frame(pca$x, dyslexia = matlab_data$dyslexia)

pca_labelled <- pca_labelled[order(pca_labelled$dyslexia, decreasing=TRUE),]

#plot the first two principle components
pc1pc2 <-pca_labelled %>%
  ggplot(aes(x = PC1, y = PC2, color = dyslexia)) +
  geom_point(size = 2) +
  theme_cognitive() +
  labs(color = "Dyslexia") +
  scale_color_manual(values = col_sch)

file <- "figures/pc1pc2.jpeg"
ggsave(file, pc1pc2)

#store variance of each principal component in data frame
pca_variance <- data.frame(summary(pca)$importance) %>%
  filter(!row_number() %in% c(1,3)) %>%
  pivot_longer(cols = starts_with("PC"),
               values_to = "variance",
               names_to = "pc")

#make the pca lables ordered factor so ggplot does not sort them alphabetically
pca_variance$pc <- factor(pca_variance$pc, levels = pca_variance$pc)

#plot the variance
pc_variance <- ggplot(pca_variance, aes(x = pc, y = variance, group = 1)) +
  geom_line(color = '#5A5A5A') +
  geom_point() +
  theme_cognitive() +
  xlab("Principal Component") +
  ylab("Eigenvalue")

file <- "figures/pc_variance.jpeg"
ggsave(file, pc_variance,
       unit = "in",
       height = 3,
       width = 8)





#look at the loadings
pca$rotation

#plot PC1 and PC4
pc1pc4 <- pca_labelled %>%
  ggplot(aes(x = PC1, y = PC4, color = dyslexia)) +
  geom_point(size = 2.4) +
  theme_cognitive() +
  labs(color = "Dyslexia") +
  scale_color_manual(values = col_sch)

file <- "figures/pc1pc4.jpeg"
ggsave(file, pc1pc4)


#save loadings into table
loadings <- data.frame(round(pca$rotation, digits = 3))

rownames(loadings) <- c("Social Skills",
                        "Attention Switching",
                        "Attention to Detail",
                        "Communication",
                        "Imagination",
                        "Depression",
                        "Not Distracting",
                        "Attention Regulation",
                        "Trait Anxiety",
                        "Social Anxiety",
                        "Letter Verbal Fluency",
                        "Category Verbal Fluency")

# knitr::kable(loadings,
#              caption = "Factor Loadings",
#              booktabs = TRUE,
#              escape = FALSE,
#              "html") %>%
#   kableExtra::kable_styling(font_size = 6) %>%
#   save_kable("loadings.pdf")
#
#
#


