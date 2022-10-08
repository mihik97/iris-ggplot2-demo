#Iris dataset ggplot demo
library(ggplot2)
library(cowplot)
library(ggrepel)
library(dplyr)

df = datasets::iris

find_outlier <- function(x) {
  return (x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}


#scatter plot for lengths correlation
p1 <- ggplot(df, aes(Sepal.Length, Petal.Length, color = factor(Species))) + 
      geom_point() +
      ggtitle("Petal Length vs Sepal Length") +
      labs(color = "Species") +
      theme(legend.key = element_rect(fill = "white", color = "black"))

p1

#scatter plot for widths correlation
p2 <- ggplot(df, aes(Sepal.Width, Petal.Width, color = factor(Species))) + 
      geom_point() +
      ggtitle("Petal Width vs Sepal Width") +
      labs(color = "Species") +
      theme(legend.key = element_rect(fill = "white", color = "black")) +
      scale_color_brewer(palette = "Paired")

p2

#box whisker plot for sepal lengths
df <- df %>%
  group_by(Species) %>%
  mutate(outlier_sl = ifelse(find_outlier(Sepal.Length), Sepal.Length, NA))

p3 <- ggplot(df, aes(x = factor(Species), y = Sepal.Length, fill = factor(Species))) + 
  geom_boxplot() +
  ggtitle("Sepal Lengths Variation") +
  xlab(paste0("Sepal Lengths Overall - Mean: ", round(mean(df$Sepal.Length), 2), " S.D: ", round(sd(df$Sepal.Length), 2))) +
  geom_text_repel(aes(label=outlier_sl), na.rm=TRUE) +
  labs(fill = "Species") +
  theme(legend.key = element_rect(fill = "white", color = "black")) +
  scale_color_brewer(palette = "Dark2")

p3

#box whisker plot for sepal widths
df <- df %>%
  group_by(Species) %>%
  mutate(outlier_sw = ifelse(find_outlier(Sepal.Width), Sepal.Width, NA))

p4 <- ggplot(df, aes(x = factor(Species), y = Sepal.Width, fill = factor(Species))) + 
  geom_boxplot() +
  ggtitle("Sepal Widths Variation") +
  xlab(paste0("Sepal Widths Overall - Mean: ", round(mean(df$Sepal.Width), 2), " S.D: ", round(sd(df$Sepal.Length), 2))) +
  geom_text_repel(aes(label=outlier_sw), na.rm=TRUE) +
  labs(fill = "Species") +
  theme(legend.key = element_rect(fill = "white", color = "black")) +
  scale_color_brewer(palette = "Dark2")

p4

#box whisker plot for petal lengths
df <- df %>%
  group_by(Species) %>%
  mutate(outlier_pl = ifelse(find_outlier(Petal.Length), Petal.Length, NA))

p5 <- ggplot(df, aes(x = factor(Species), y = Petal.Length, fill = factor(Species))) + 
  geom_boxplot() +
  ggtitle("Petal Lengths Variation") +
  xlab(paste0("Petal Lengths Overall - Mean: ", round(mean(df$Petal.Length), 2), " S.D: ", round(sd(df$Petal.Length), 2))) +
  geom_text_repel(aes(label=outlier_pl), na.rm=TRUE) +
  labs(fill = "Species") +
  theme(legend.key = element_rect(fill = "white", color = "black")) +
  scale_color_brewer(palette = "Dark2")

p5

#box whisker plot for petal widths
df <- df %>%
  group_by(Species) %>%
  mutate(outlier_pw = ifelse(find_outlier(Petal.Width), Petal.Width, NA))

p6 <- ggplot(df, aes(x = factor(Species), y = Petal.Width, fill = factor(Species))) + 
  geom_boxplot() +
  ggtitle("Petal Widths Variation") +
  xlab(paste0("Petal Widths Overall - Mean: ", round(mean(df$Petal.Width), 2), " S.D: ", round(sd(df$Petal.Length), 2))) +
  geom_text_repel(aes(label=outlier_pw), na.rm=TRUE) +
  labs(fill = "Species") +
  theme(legend.key = element_rect(fill = "white", color = "black")) +
  scale_color_brewer(palette = "Dark2")

p6