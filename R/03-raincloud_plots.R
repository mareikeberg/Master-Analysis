

########################create raincloud plots to show distribution #################

level_order <- c("no dyslexia", "dyslexia")

#SOCIAL SKILLS

rc1_social_skills <- matlab_data %>%
  #make coordinate system
  ggplot(aes(x = factor(dyslexia, level = level_order), y = aq_social_skills,
             fill = dyslexia)) +
  #add half violin plots
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA) +
  #add box plots
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #add dots on left side
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.1,
            slab_linewidth = 0.1) +
  #visuals
  scale_fill_manual(values = col_sch,
                    name = "Dyslexia",
                    labels = c("Dyslexia", "No Dyslexia")) +
  theme_cognitive() +
  labs(y = "Social Skills", x = " ") +
  theme(axis.text.y = element_blank()) +
  coord_flip()

file <- "figures/rc1_social_skills.jpeg"
ggsave(file,
       rc1_social_skills,
       unit = "in",
       height = 2.862,
       width = 4.319)

#ATTENTION SWITCHING

rc2_attention_switching <- matlab_data %>%
  #make coordinate system
  ggplot(aes(x = factor(dyslexia, level = level_order), y = aq_attention_switching,
             fill = dyslexia)) +
  #add half violin plots
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA) +
  #add box plots
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #add dots on left side
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.1,
            slab_linewidth = 0.1) +
  #visuals
  scale_fill_manual(values = col_sch,
                    name = "Dyslexia",
                    labels = c("Dyslexia", "No Dyslexia")) +
  theme_cognitive() +
  labs(y = "Attention Switching", x = " ") +
  theme(axis.text.y = element_blank()) +
  coord_flip()

file <- "figures/rc2_attention_switching.jpeg"
ggsave(file,
       rc2_attention_switching,
       unit = "in",
       height = 2.862,
       width = 4.319)

#ATTENTION TO DETAIL

rc3_attention_to_detail <-matlab_data %>%
  #make coordinate system
  ggplot(aes(x = factor(dyslexia, level = level_order), y = aq_attention_to_detail,
             fill = dyslexia)) +
  #add half violin plots
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA) +
  #add box plots
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #add dots on left side
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.1,
            slab_linewidth = 0.1) +
  #visuals
  scale_fill_manual(values = col_sch,
                    name = "Dyslexia",
                    labels = c("Dyslexia", "No Dyslexia")) +
  theme_cognitive() +
  labs(y = "Attention to Detail", x = " ") +
  theme(axis.text.y = element_blank()) +
  coord_flip()

file <- "figures/rc3_attention_to_detail.jpeg"
ggsave(file,
       rc3_attention_to_detail,
       unit = "in",
       height = 2.862,
       width = 4.319)

#COMMUNICATION

rc4_communication <-matlab_data %>%
  #make coordinate system
  ggplot(aes(x = factor(dyslexia, level = level_order), y = aq_communication,
             fill = dyslexia)) +
  #add half violin plots
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA) +
  #add box plots
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #add dots on left side
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.1,
            slab_linewidth = 0.1) +
  #visuals
  scale_fill_manual(values = col_sch,
                    name = "Dyslexia",
                    labels = c("Dyslexia", "No Dyslexia")) +
  theme_cognitive() +
  labs(y = "Communication", x = " ") +
  theme(axis.text.y = element_blank()) +
  coord_flip()


file <- "figures/rc4_communication.jpeg"
ggsave(file,
       rc4_communication,
       unit = "in",
       height = 2.862,
       width = 4.319)

#IMAGINATION

rc5_imagination <-matlab_data %>%
  #make coordinate system
  ggplot(aes(x = factor(dyslexia, level = level_order), y = aq_imagination_dimension,
             fill = dyslexia)) +
  #add half violin plots
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA) +
  #add box plots
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #add dots on left side
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.1,
            slab_linewidth = 0.1) +
  #visuals
  scale_fill_manual(values = col_sch,
                    name = "Dyslexia",
                    labels = c("Dyslexia", "No Dyslexia")) +
  theme_cognitive() +
  labs(y = "Imagination", x = " ") +
  theme(axis.text.y = element_blank()) +
  coord_flip()


file <- "figures/rc5_imagination.jpeg"
ggsave(file,
       rc5_imagination,
       unit = "in",
       height = 2.862,
       width = 4.319)

#DEPRESSION

rc6_depression <-matlab_data %>%
  #make coordinate system
  ggplot(aes(x = factor(dyslexia, level = level_order), y = depression_score,
             fill = dyslexia)) +
  #add half violin plots
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA) +
  #add box plots
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #add dots on left side
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.6,
            slab_linewidth = 0.1) +
  #visuals
  scale_fill_manual(values = col_sch,
                    name = "Dyslexia",
                    labels = c("Dyslexia", "No Dyslexia")) +
  theme_cognitive() +
  labs(y = "Depression", x = " ") +
  theme(axis.text.y = element_blank()) +
  coord_flip()

file <- "figures/rc6_depression.jpeg"
ggsave(file,
       rc6_depression,
       unit = "in",
       height = 2.862,
       width = 4.319)

#NOT DISTRACTING

rc7_not_distracting <-matlab_data %>%
  #make coordinate system
  ggplot(aes(x = factor(dyslexia, level = level_order), y = maia_not_distracting,
             fill = dyslexia)) +
  #add half violin plots
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA) +
  #add box plots
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #add dots on left side
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.05,
            slab_linewidth = 0.1) +
  #visuals
  scale_fill_manual(values = col_sch,
                    name = "Dyslexia",
                    labels = c("Dyslexia", "No Dyslexia")) +
  theme_cognitive() +
  labs(y = "Not Distracting", x = " ") +
  theme(axis.text.y = element_blank()) +
  coord_flip()

file <- "figures/rc7_not_distracting.jpeg"
ggsave(file,
       rc7_not_distracting,
       unit = "in",
       height = 2.862,
       width = 4.319)

#ATTENTION REGULATION

rc8_attention_regulation <- matlab_data %>%
  #make coordinate system
  ggplot(aes(x = factor(dyslexia, level = level_order), y = maia_attention_regulation,
             fill = dyslexia)) +
  #add half violin plots
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA) +
  #add box plots
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #add dots on left side
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.04,
            slab_linewidth = 0.1) +
  #visuals
  scale_fill_manual(values = col_sch,
                    name = "Dyslexia",
                    labels = c("Dyslexia", "No Dyslexia")) +
  theme_cognitive() +
  labs(y = "Attention Regulation", x = " ") +
  theme(axis.text.y = element_blank()) +
  coord_flip()


file <- "figures/rc8_attention_regulation.jpeg"
ggsave(file,
       rc8_attention_regulation,
       unit = "in",
       height = 2.862,
       width = 4.319)

#TRAIT ANXIETY

rc9_trait_anxiety <-matlab_data %>%
  #make coordinate system
  ggplot(aes(x = factor(dyslexia, level = level_order), y = trait_anxiety,
             fill = dyslexia)) +
  #add half violin plots
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA) +
  #add box plots
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #add dots on left side
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.025,
            slab_linewidth = 0.1) +
  #visuals
  scale_fill_manual(values = col_sch,
                    name = "Dyslexia",
                    labels = c("Dyslexia", "No Dyslexia")) +
  theme_cognitive() +
  labs(y = "Trait Anxiety", x = " ") +
  theme(axis.text.y = element_blank()) +
  coord_flip()


file <- "figures/rc9_trait_anxiety.jpeg"
ggsave(file,
       rc9_trait_anxiety,
       unit = "in",
       height = 2.862,
       width = 4.319)

#SOCIAL ANXIETY

rc10_social_anxiety <-matlab_data %>%
  #make coordinate system
  ggplot(aes(x = factor(dyslexia, level = level_order), y = social_anxiety,
             fill = dyslexia)) +
  #add half violin plots
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA) +
  #add box plots
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #add dots on left side
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.15,
            slab_linewidth = 0.1) +
  #visuals
  scale_fill_manual(values = col_sch,
                    name = "Dyslexia",
                    labels = c("Dyslexia", "No Dyslexia")) +
  theme_cognitive() +
  labs(y = "Social Anxiety", x = " ") +
  theme(axis.text.y = element_blank()) +
  coord_flip()


file <- "figures/rc10_social_anxiety.jpeg"
ggsave(file,
       rc10_social_anxiety,
       unit = "in",
       height = 2.862,
       width = 4.319)

#VFT LETTERS

rc11_letters <-matlab_data %>%
  #make coordinate system
  ggplot(aes(x = factor(dyslexia, level = level_order), y = vft_letters,
             fill = dyslexia)) +
  #add half violin plots
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA) +
  #add box plots
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #add dots on left side
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.0028,
            slab_linewidth = 0.1) +
  #visuals
  scale_fill_manual(values = col_sch,
                    name = "Dyslexia",
                    labels = c("Dyslexia", "No Dyslexia")) +
  theme_cognitive() +
  labs(y = "Letter Verbal Fluency", x = " ") +
  theme(axis.text.y = element_blank()) +
  coord_flip()

file <- "figures/rc11_letters.jpeg"
ggsave(file,
       rc11_letters,
       unit = "in",
       height = 2.862,
       width = 4.319)

# VFT CATEGORY
rc12_categories <-matlab_data %>%
  #make coordinate system
  ggplot(aes(x = factor(dyslexia, level = level_order), y = vft_categories,
             fill = dyslexia)) +
  #add half violin plots
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA) +
  #add box plots
  geom_boxplot(width = 0.12,
               outlier.color = NA,
               alpha = 0.5) +
  #add dots on left side
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.0028,
            slab_linewidth = 0.1) +
  #visuals
  scale_fill_manual(values = col_sch,
                    name = "Dyslexia",
                    labels = c("Dyslexia", "No Dyslexia")) +
  theme_cognitive() +
  labs(y = "Category Verbal Fluency", x = " ") +
  theme(axis.text.y = element_blank()) +
  coord_flip()


file <- "figures/rc12_categories.jpeg"
ggsave(file,
       rc12_categories,
       unit = "in",
       height = 2.862,
       width = 4.319)

#define matrix of plot layout
lay <- rbind(c(1,2,3,3),
             c(4,5,6,6),
             c(7,8,9,9),
             c(10,11,12,12))



#################################### p-values ggpairs ##########################################
#create variable to store p-values in
p_val <- 1:12

#### SOCIAL SKILLS

#run unpaired t-test to investigate effect of dyslexia on social skills
t_test_dep <- t.test(data = matlab_data,
                     aq_social_skills ~ dyslexia,
                     paired = F,
                     var.equal = T)

#get p-value in appropriate format and save to p_val
p_val[1] <- format.pval(t_test_dep$p.value,
                        digits = 2, #significant figures after 0.0
                        eps = 0.001, #threshold over which replaces with < than it
                        nsmall = 3) #trail zeros to keep if value has less digits than defined


####ATTENTION SWITCHING

#run unpaired t-test to investigate effect of dyslexia on attention switching
t_test_dist <- t.test(data = matlab_data,
                      aq_attention_switching ~ dyslexia,
                      paired = F,
                      var.equal = T)

#get p-value in appropriate format and save to p_val
p_val[2] <- format.pval(t_test_dist$p.value,
                        digits = 2, #significant figures after 0.0
                        eps = 0.001, #threshold over which replaces with < than it
                        nsmall = 3) #trail zeros to keep if value has less digits than defined


####ATTENTION TO DETAIL

#run unpaired t-test to investigate effect of dyslexia on attention to detail
t_test_tanx <- t.test(data = matlab_data,
                      aq_attention_to_detail ~ dyslexia,
                      paired = F,
                      var.equal = T)

#get p-value in appropriate format and save to p_val
p_val[3] <- format.pval(t_test_tanx$p.value,
                        digits = 2, #significant figures after 0.0
                        eps = 0.001, #threshold over which replaces with < than it
                        nsmall = 3) #trail zeros to keep if value has less digits than defined

####COMMUNICATION SKILLS

#run unpaired t-test to investigate effect of dyslexia on communication skills


t_test_sanx <- t.test(data = matlab_data,
                      aq_communication ~ dyslexia,
                      paired = F,
                      var.equal = T)

#get p-value in appropriate format and save to p_val
p_val[4] <- format.pval(t_test_sanx$p.value,
                        digits = 2, #significant figures after 0.0
                        eps = 0.001, #threshold over which replaces with < than it
                        nsmall = 3) #trail zeros to keep if value has less digits than defined

####IMAGINATION

#run unpaired t-test to investigate effect of dyslexia on imagination


t_test_sanx <- t.test(data = matlab_data,
                      aq_imagination_dimension ~ dyslexia,
                      paired = F,
                      var.equal = T)

#get p-value in appropriate format and save to p_val
p_val[5] <- format.pval(t_test_sanx$p.value,
                        digits = 2, #significant figures after 0.0
                        eps = 0.001, #threshold over which replaces with < than it
                        nsmall = 3) #trail zeros to keep if value has less digits than defined

#### DEPRESSION

#run unpaired t-test to investigate effect of dyslexia on depression


t_test_sanx <- t.test(data = matlab_data,
                      depression_score ~ dyslexia,
                      paired = F,
                      var.equal = T)

#get p-value in appropriate format and save to p_val
p_val[6] <- format.pval(t_test_sanx$p.value,
                        digits = 2, #significant figures after 0.0
                        eps = 0.001, #threshold over which replaces with < than it
                        nsmall = 3) #trail zeros to keep if value has less digits than defined


##### NOT DISTRACTING

#run unpaired t-test to investigate effect of dyslexia on not distracting


t_test_sanx <- t.test(data = matlab_data,
                      maia_not_distracting ~ dyslexia,
                      paired = F,
                      var.equal = T)

#get p-value in appropriate format and save to p_val
p_val[7] <- format.pval(t_test_sanx$p.value,
                        digits = 2, #significant figures after 0.0
                        eps = 0.001, #threshold over which replaces with < than it
                        nsmall = 3) #trail zeros to keep if value has less digits than defined

##### ATTENTION REGULATION

#run unpaired t-test to investigate effect of dyslexia on attention regulation


t_test_sanx <- t.test(data = matlab_data,
                      maia_attention_regulation ~ dyslexia,
                      paired = F,
                      var.equal = T)

#get p-value in appropriate format and save to p_val
p_val[8] <- format.pval(t_test_sanx$p.value,
                        digits = 2, #significant figures after 0.0
                        eps = 0.001, #threshold over which replaces with < than it
                        nsmall = 3) #trail zeros to keep if value has less digits than defined

##### TRAIT ANXIETY

#run unpaired t-test to investigate effect of dyslexia on trait anxiety


t_test_sanx <- t.test(data = matlab_data,
                      trait_anxiety ~ dyslexia,
                      paired = F,
                      var.equal = T)

#get p-value in appropriate format and save to p_val
p_val[9] <- format.pval(t_test_sanx$p.value,
                        digits = 2, #significant figures after 0.0
                        eps = 0.001, #threshold over which replaces with < than it
                        nsmall = 3) #trail zeros to keep if value has less digits than defined

##### SOCIAL ANXIETY

#run unpaired t-test to investigate effect of dyslexia on social anxiety


t_test_sanx <- t.test(data = matlab_data,
                      social_anxiety ~ dyslexia,
                      paired = F,
                      var.equal = T)

#get p-value in appropriate format and save to p_val
p_val[10] <- format.pval(t_test_sanx$p.value,
                         digits = 2, #significant figures after 0.0
                         eps = 0.001, #threshold over which replaces with < than it
                         nsmall = 3) #trail zeros to keep if value has less digits than defined



##### VERBAL FLUENCY LETTERS

#run unpaired t-test to investigate effect of dyslexia on letter verbal fluency performance
t_test_lvf <- t.test(data = matlab_data,
                     vft_letters ~ dyslexia,
                     paired = F,
                     var.equal = T)

#get p-value in appropriate format and save to p_val
p_val[11] <- format.pval(t_test_lvf$p.value,
                         digits = 2, #significant figures after 0.0
                         eps = 0.001, #threshold over which replaces with < than it
                         nsmall = 3) #trail zeros to keep if value has less digits than defined

##### VERBAL FLUENCY CATEGORIES

#run unpaired t-test to investigate effect of dyslexia on category verbal fluency performance
t_test_cvf <- t.test(data = matlab_data,
                     vft_categories ~ dyslexia,
                     paired = F,
                     var.equal = T)

#get p-value in appropriate format and save to p_val
p_val[12] <- format.pval(t_test_cvf$p.value,
                         digits = 2, #significant figures after 0.0
                         eps = 0.001, #threshold over which replaces with < than it
                         nsmall = 3) #trail zeros to keep if value has less digits than defined



