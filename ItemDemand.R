library(vroom)

item_train <- vroom("./Item-Demand-Challenge/train.csv")
item_test <- vroom("./Item-Demand-Challenge/test.csv")
#10 stores and 50 items
library(timetk)
library(tidyverse)

graph1 <- item_train %>%
  filter(store == 1, item == 1)
graph2 <- item_train %>%
  filter(store == 5, item == 1)
graph3 <- item_train %>%
  filter(store == 1, item == 20)
graph4 <- item_train %>%
  filter(store == 1, item == 50)

library(patchwork)
a <- graph1 %>%
  pull(sales) %>%
  forecast::ggAcf(lag.max = 3*365)
b <- graph2 %>%
  pull(sales) %>%
  forecast::ggAcf(lag.max = 3*365)
c <- graph3 %>%
  pull(sales) %>%
  forecast::ggAcf(lag.max = 3*365)
d <- graph4 %>%
  pull(sales) %>%
  forecast::ggAcf(lag.max = 3*365)
a <- a + xlab("Time") + ylab("Sales") + ggtitle("Store 1, Item 1")
b <- b + xlab("Time") + ylab("Sales") + ggtitle("Store 5, Item 1")
c <- c + xlab("Time") + ylab("Sales") + ggtitle("Store 1, Item 20")
d <- d + xlab("Time") + ylab("Sales") + ggtitle("Store 1, Item 50")
(a+b)/(c+d)