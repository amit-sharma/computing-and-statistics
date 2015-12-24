## Source code for my answer on Quora:
#https://www.quora.com/What-is-an-intuitive-explanation-of-inverse-transform-sampling-method-in-statistics-and-how-does-it-relate-to-cumulative-distribution-function

## Generate samples from any arbitrary probability distribution.
## Motivating example: A new Star Wars game. 
## Task : Generate StormTroopers of varying fighting ability, according to a given distribution.

## Author: Amit Sharma
library(dplyr)
library(ggplot2)
library(cowplot)
library(RColorBrewer)

p_ability = c("Weak" = 0.2, "Standard" = 0.7, "Strong" = 0.1)
prob_df = data.frame(Ability=factor(names(p_ability), levels=names(p_ability)), 
                     Probability=p_ability)
plot_prob = ggplot(prob_df, aes(x=Ability, y=Probability)) + geom_bar(stat="identity", width=0.75) + 
  ylim(0,1) + ggtitle("Probability Distribution") +
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=16),
        title=element_text(size=20))
#ggsave(filename="figures/prob_distr.jpg", width=4, height=6)

cdf_df = mutate(prob_df, 
                Cumulative_Probability = cumsum(Probability))
plot_cdf = ggplot(cdf_df, aes(x=Ability, y=Cumulative_Probability)) + geom_bar(stat="identity", width=0.75) + 
  ylim(0,1) + ggtitle("Cumulative Distribution Function (CDF)") +
  theme(axis.text=element_text(size=14), 
      axis.title=element_text(size=16),
      title=element_text(size=20))
plot_grid(plot_prob, plot_cdf, align='h')
ggsave(filename="figures/prob_distr.jpg", width=9, height=3.8)


plot_cdf + 
  geom_segment(x=0, y=0.05, xend=1, yend=0.05, color="green", size=1) +
  geom_segment(x=0, y=0.35, xend=2, yend=0.35, color="blue", size=1) +
  geom_segment(x=0, y=0.65, xend=2, yend=0.65, color="blue", size=1) +
  geom_segment(x=0, y=0.95, xend=3, yend=0.95, color="red", size=1) +
  ggtitle("Inverse Transform Sampling (Discrete Distribution)")
ggsave(filename="figures/inverse_transform_sampling_discrete.jpg", width=6, height=3.8)

# Now assuming that abilities fall on a continuous scale.  
lambda=0.1
cont_abilities = rexp(1000, rate=lambda)
ggplot(as.data.frame(cont_abilities), aes(x=cont_abilities)) + stat_ecdf() + 
  geom_segment(x=-9, y=0.1, xend=1, yend=0.1, color="green", size=1) +
  geom_segment(x=-9, y=0.5, xend=7.7, yend=0.5, color="blue", size=1) +
  geom_segment(x=-9, y=0.9, xend=21.3, yend=0.9, color="red", size=1) +
  geom_segment(x=1, y=0.1, xend=1, yend=0, color="green", size=1, linetype=5) +
  geom_segment(x=7.7, y=0.5, xend=7.7, yend=0, color="blue", size=1, linetype=5) +
  geom_segment(x=21.3, y=0.9, xend=21.3, yend=0, color="red", size=1, linetype=5) +
  ylim(0,1) + ylab("Cumulative Probability") + xlab("Ability") +
  ggtitle("Inverse Transform Sampling (Continuous Distribution)") +
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=16),
        title=element_text(size=20))
ggsave(filename="figures/inverse_transform_sampling_continuous.jpg", width=6, height=3.8)


