#calculate the euclidean distance (dist()) and pipe it into the cmdscale function and
#make the output into a tibble

mds <- matlab_data %>%
  select(!dyslexia) %>%
  dist() %>%
  cmdscale() %>%
  data.frame()

#add names to the columns
colnames(mds) <- c("Dimension 1", "Dimension 2")

#add the categorical dyslexia variable to colour the data points
mds <- mds %>%
  add_column(dyslexia = matlab_data$dyslexia)

#order so that dyslexia group is plotted ontop of no dyslexia group
mds <- mds[order(mds$dyslexia, decreasing=TRUE),]

#plot the 2 dimensions
mds_plot <- mds %>%
  ggplot(aes(x = `Dimension 1`, y = `Dimension 2`, color = dyslexia)) +
  geom_point(size = 2.5) +
  theme_cognitive() +
  scale_color_manual(values = col_sch,
                     name = "Dyslexia",
                     labels = c("Dyslexia", "No Dyslexia"))

file <- "figures/mds.jpeg"
ggsave(file, mds_plot)

##### distance from origin of mds plot #########

#calculate the distances of each participant to the origin
distances <- mds %>%
  mutate(orig_dist = sqrt(`Dimension 1`^2 + `Dimension 2`^2)) %>%
  add_column("pp" = 1:nrow(data),
             .before = 1)

#order it by the distance from the origin
distances <- distances[order(distances$orig_dist),]

#prevent ordering by participant number
distances$pp <- factor(distances$pp, levels = distances$pp)

#plot the distances from the origin
mds_dist <- ggplot(distances, aes(x = pp, y = orig_dist, fill = dyslexia), group = 1) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_cognitive() +
  ylab("Distance from the origin") +
  xlab("Participants") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_fill_manual(values = col_sch)

file <- "figures/mds_dist.jpeg"
ggsave(file, mds_dist,
       unit = "in",
       height = 3,
       width = 7)


#t-test on whether groups differ in distance to origin
t_dist <- t.test(data = distances,
                 orig_dist ~ dyslexia,
                 paired = F,
                 var.equal = T)

format.pval(t_dist$p.value,
            digits = 2,
            eps = 0.001,
            nsmall = 3)


#plot the mean distances from the origin for each group

mean_dists <- matrix(c(1,2,3,4,5,6), ncol = 3)
colnames(mean_dists) <- c("dyslexia", "mean_distance", "stdv")
mean_dists <- as_tibble(mean_dists)

mean_dists$mean_distance[1] <- distances$orig_dist[distances$dyslexia == 'dyslexia'] %>%
  mean() %>%
  round(digits = 2)

mean_dists$mean_distance[2] <- distances$orig_dist[distances$dyslexia == 'no dyslexia'] %>%
  mean() %>%
  round(digits = 2)

mean_dists$stdv[1] <- distances$orig_dist[distances$dyslexia == 'dyslexia'] %>%
  sd() %>%
  round(digits = 2)

mean_dists$stdv[2] <- distances$orig_dist[distances$dyslexia == 'no dyslexia'] %>%
  sd() %>%
  round(digits = 2)

mean_dists$dyslexia[1] <- "dyslexia"
mean_dists$dyslexia[2] <- "no dyslexia"

mean_dists$dyslexia <- as.factor(mean_dists$dyslexia)
mean_dists$stdv <- as.numeric(mean_dists$stdv)


#plot

mds_mean_dist <- mean_dists %>%
  ggplot(aes(x = dyslexia, y = mean_distance, fill = dyslexia)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_distance - stdv, ymax = mean_distance + stdv), width = .2) +
  theme_cognitive() +
  scale_fill_manual(values = col_sch) +
  xlab(" ") +
  ylab("Distance from the origin")

#save plot
file <- "figures/mds_mean_dist.jpeg"
ggsave(file, mds_mean_dist,
       unit = "in",
       width = 2.2,
       height = 4.5)

#compare_means(orig_dist ~ dyslexia, data = distances, method = "t.test")


