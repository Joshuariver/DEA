# 기술통계 - 신뢰성 측정의 다섯 가지 방법
# 출처: https://www.r-bloggers.com/five-ways-to-calculate-internal-consistency/
# 
# Average inter-item correlation
# Average item-total correlation
# Cronbach’s alpha
# Split-half reliability (adjusted using the Spearman–Brown prophecy formula)
# Composite reliability



rm(list=ls())
library(dplyr)


temp <- tempfile()
download.file("http://personality-testing.info/_rawdata/BIG5.zip", temp, mode="wb")
d <- read.table(unz(temp, "BIG5/data.csv"), header = TRUE, sep="\t")
unlink(temp); rm(temp)

d <- d[1:500, paste0("E", 1:10)]
str(d)


# Here is a list of the extraversion items that people are rating from 1 = Disagree to 5 = Agree:
# 즉, 역척도를 적용해야 할 변수 처리 방법 (E2, 4, 6, 8,10번 항목은 역척도 처리)  
# E1 I am the life of the party.
# E2 I don’t talk a lot.
# E3 I feel comfortable around people.
# E4 I keep in the background.
# E5 I start conversations.
# E6 I have little to say.
# E7 I talk to a lot of different people at parties.
# E8 I don’t like to draw attention to myself.
# E9 I don’t mind being the center of attention.
# E10 I am quiet around strangers.

d[, paste0("E", c(2, 4, 6, 8, 10))] <- 6 - d[, paste0("E", c(2, 4, 6, 8, 10))]


# Average inter-item correlation법
# 첫 번 째 방법

library(corrr)
d %>% correlate()
inter_item <- d %>% correlate() %>% select(-rowname) %>% colMeans(na.rm = TRUE)
inter_item
mean(inter_item)

library(ggplot2)

data.frame(inter_item) %>% 
  ggplot(aes(x = inter_item)) +
  geom_histogram(bins = 10, alpha = .5) +
  geom_vline(xintercept = mean(inter_item), color = "red") +
  xlab("Mean inter-item correlation") +
  theme_bw()


# Average item-total correlation
# 두 번 째 방법

d$score <- rowMeans(d)
head(d)
item_total <- d %>% correlate() %>% focus(score)
item_total
mean(item_total$score)
item_total %>% 
  ggplot(aes(x = score)) +
  geom_histogram(bins = 10, alpha = .5) +
  geom_vline(xintercept = mean(item_total$score), color = "red") +
  xlab("Mean item-total correlation") +
  theme_bw()


# Cronbach’s alpha
# 세 번 째 방법 (가장 기본적인 평가)

d$score <- NULL  # delete the score column we made earlier
psych::alpha(d)
psych::alpha(d)$total$std.alpha


# Split-half reliability (adjusted using the Spearman–Brown prophecy formula)

# Calculating total score...
score_e <- rowMeans(d[, c(TRUE, FALSE)])  # with even items
score_o <- rowMeans(d[, c(FALSE, TRUE)])  # with odd items

# Correlating scores from even and odd items
r <- cor(score_e, score_o)
r

# Adjusting with the Spearman-Brown prophecy formula
(2 * r) / (1 + r)


# Composite reliability

library(lavaan)

# Define the model
items <- paste(names(d), collapse = "+")
model <- paste("extraversion", items, sep = "=~")
model


# Fit the model
fit <- cfa(model, data = d)

# There are various ways to get to the composite reliability from this model. We’ll extract the standardized factor loadings and work with those:
  
sl <- standardizedSolution(fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(d)
sl  # These are the standardized factor loadings for each item


# We then obtain the composite reliability via the following:
  
# Compute residual variance of each item
re <- 1 - sl^2

# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))

