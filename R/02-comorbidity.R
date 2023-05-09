
#plot dyslexia and adhd score against each other for each participant
#colour determined by dyslexia category (whether exceeded cut off or not)
comorb_adhd <- ggplot(data, aes(x = qd_dac, y = qd_adhd, color = dyslexia)) +
  geom_point(size = 1.5) +
  theme_cognitive() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  labs(x = "Dyslexia Score",
       y = "ADHD Score") +
  scale_color_manual(values = col_sch,
                     name = "Dyslexia",
                     labels = c("Dyslexia", "No Dyslexia")) +
  geom_vline(xintercept = 45, color = "grey", linetype = "dashed")

file <- "figures/comorb.jpeg"
ggsave(file, comorb_adhd)

# test whether difference significant

#t-test to evaluate whether exceeding cut off in dyslexia impacts ADHD performance
t_dys_adhd <- t.test(qd_adhd ~ dyslexia, data = data)
format.pval(t_dys_adhd$p.value,
            digits = 2, #significant figures after 0.0
            eps = 0.001, #threshold over which replaces with < than it
            nsmall = 3)



