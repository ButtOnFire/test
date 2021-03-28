library(tidyverse)
library(ggrepel)
library(dslabs)
library(rvest)



library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")

tab_1 <-html_table(nodes[[10]]) %>% filter(X1!="No.")%>%select(-"X1") %>% setNames(.,c("Team", "Payroll", "Average"))
tab_2 <-html_table(nodes[[19]]) %>% filter(X1!="Team") %>% setNames(.,c("Team", "Payroll", "Average"))
head(tab_1)

full_join(tab_1, tab_2, by = "Team")

head(tab_2)

library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

h <- read_html(url)
tab <- html_nodes(h, "table")
length(tab)
tabs <- html_table(tab, fill = TRUE) 

n<-seq(1,40)
data <- function(n){
  ifelse(ncol(tabs[[n]]) == 9 & colnames(tabs[[n]][1]) =="Date(s) conducted", n, NA)
}
sapply(n, data)

# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

# demonstrate the effect of groups
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)

# demonstrate difference between str_match and str_extract
str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups)

# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

# final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head


s <- c("5'10", "6'1")
tab <- data.frame(x = s)
tab
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")









