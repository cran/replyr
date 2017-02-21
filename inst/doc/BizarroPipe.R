## ----magrittr------------------------------------------------------------
library("babynames") # data package
library("dplyr")     # provides data manipulating functions.
library("magrittr")  # ceci n'est pas un pipe
library("ggplot2")   # for graphics

babynames %>%
    filter(name %>% substr(1, 3) %>% equals("Ste")) %>%
    group_by(year, sex) %>%
    summarize(total = sum(n)) %>%
    qplot(year, total, color = sex, data = ., geom = "line") %>%
    add(ggtitle('Names starting with "Ste"')) %>%
    print

## ----dots, eval=FALSE----------------------------------------------------
#  babynames %>%
#      filter(.,  name %>% substr(1, 3) %>% equals("Ste") ) %>%
#      group_by(., year, sex) %>%
#      summarize(., total = sum(n)) %>%
#      qplot(year, total, color = sex, data = ., geom = "line") %>%
#      add(., ggtitle('Names starting with "Ste"')) %>%
#      print(.)

## ----replaced------------------------------------------------------------
babynames ->.;
    filter(.,  name %>% substr(1, 3) %>% equals("Ste") ) ->.;
    group_by(., year, sex) ->.;
    summarize(., total = sum(n)) ->.;
    qplot(year, total, color = sex, data = ., geom = "line") ->.;
    add(., ggtitle('Names starting with "Ste"')) ->.;
    print(.)

