library(readr)
lr <- read_delim("linear_regression_dataset.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(linear_regression_dataset)

plot(lr)

lm <- lm(maas~deneyim,data = lr)
summary(lm)
plot(lm)
library(ggpubr)
library(ggplot2)
ggplot(lr,aes(lr$maas, lr$deneyim)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='red') +
  theme_minimal() +
  labs(x='Maas', y='Yıl', title='Maas ve Deneyim İlişkisi') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))+
  stat_regline_equation(label.x=20, label.y=12.5) +
  stat_cor(aes(label=..rr.label..), label.x=15, label.y=15)

