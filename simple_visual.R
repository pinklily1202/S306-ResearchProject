library(ggplot2)

s = student.mat

# Student.mat with selected variable
s_s = s %>%
  select(sex, age, Pstatus, Medu, Fedu, Mjob, Fjob,
         activities, freetime, absences, G1, G2, G3)


# Boxplots
# G3 ~ sex
ggplot(s_s, aes(x = sex, y = G3)) +
  geom_boxplot() +
  labs(title = "G3 by Sex")

# G3 ~ Pstatus
ggplot(s_s, aes(x = Pstatus, y = G3)) +
  geom_boxplot() +
  labs(title = "G3 by Pstatus")

# G3 ~ activities
ggplot(s_s, aes(x = activities, y = G3)) +
  geom_boxplot() +
  labs(title = "G3 by Activities")


# Scatterplots
# Age vs G3
ggplot(s_s, aes(x = age, y = G3)) +
  geom_point() +
  ggtitle("Age vs G3")

# Medu vs G3
ggplot(s_s, aes(x = Medu, y = G3)) +
  geom_point() +
  ggtitle("Medu vs G3")

# Fedu vs G3
ggplot(s_s, aes(x = Fedu, y = G3)) +
  geom_point() +
  ggtitle("Medu vs G3")

# Freetime vs G3
ggplot(s_s, aes(x = freetime, y = G3)) +
  geom_point() +
  ggtitle("Freetime vs G3")

# G1 vs G3
ggplot(s_s, aes(x = G1, y = G3)) +
  geom_point() +
  ggtitle("G1 vs G3")

# G2 vs G3
ggplot(s_s, aes(x = G2, y = G3)) +
  geom_point() +
  ggtitle("G2 vs G3")

# absences vs G3
ggplot(s_s, aes(x = absences, y = G3)) +
  geom_point() +
  ggtitle("Absences vs G3")


# Barplots
# Mjob vs mean_G3
s_s %>%
  group_by(Mjob) %>%
  summarise(mean_G3 = mean(G3)) %>%
  ggplot(aes(x = Mjob, y = mean_G3)) +
  geom_col() +
  ggtitle("Mean G3 by Mjob")

# Fjob vs mean_G3
s_s %>%
  group_by(Fjob) %>%
  summarise(mean_G3 = mean(G3)) %>%
  ggplot(aes(x = Fjob, y = mean_G3)) +
  geom_col() +
  ggtitle("Mean G3 by Fjob")


# G3 Histogram
ggplot(s_s, aes(x = G3)) +
  geom_histogram() +
  ggtitle("Histogram of G3")


lm_model = lm(G3 ~ sex + age + Pstatus + Medu + Fedu + Mjob + Fjob + activities + freetime + absences + G1 + G2, data = s)
summary(lm_model)