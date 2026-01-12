install.packages(c("tidyverse","RSQLite"))
library(tidyverse)
library(RSQLite)

## Connect database
con <- dbConnect(SQLite(),"chinook.db")

## see tables in this db
dbListTables(con)

dbListFields(con,"customers")

## db get query

df <- dbGetQuery(con,"select firstname, lastname,city from customers limit 10")

df2 <- dbGetQuery(con, "select 
                    t1.customerid,
                    sum(total) as total_invoice
                  from customers t1
                  Join invoices t2 on t1.customerid = t2.customerid
                  group by 1
                  order by 2 desc
                  limit 20")


t1 <- dbGetQuery(con,"select * from customers")
t2 <- dbGetQuery(con, "select * from invoices")

t1 %>% 
  left_join(t2, by = "CustomerId") %>% 
  group_by(CustomerId, FirstName, LastName) %>%
  summarise((total_invoice = sum(Total)))

clean_names(t1) %>% 
  names()

## Gramma of graphic (ggplot2) 2=2D

library(ggplot2)

diamonds
View(diamonds)

base <- ggplot(data = diamonds,
       mapping = aes(x = price))
## Bins define box
base + geom_histogram(bins = 5)


base + geom_density()

## discrete(จำนวนเต็ม) vs continuous(ทศนิยม)

diamonds %>% 
  count(cut)

## one variable, factor (discrate)
ggplot(data = diamonds,aes(x = cut)) +
  geom_bar()

##shortcut function: qplot()

qplot(x = cut, data=diamonds, geom ="bar") +
  theme_minimal()

qplot(x = price, data=diamonds, geom ="density") +
  theme_minimal()

## built-in theme in ggplot2

install.packages("ggthemes")
library(ggthemes)
qplot(x=cut,data=diamonds,geom="bar") + theme_dark()

## TWo variavles, both number
## Scater Plot

set.seed(42)
mini_dia <- sample_n(diamonds,5000)

ggplot(mini_dia,aes(x=price,y=carat)) + geom_point()

ggplot(data = mini_dia,
       mapping = aes(x=price, y=carat)) +
  geom_point(color = "darkblue", 
             alpha=0.1,
             size = 1) +
  theme_minimal()

ggplot(data = mini_dia,
       mapping = aes(x=price,
                     y=carat,
                     color = depth)) +
  geom_point(alpha=0.35,size=3)+
  scale_color_gradient(low = "gold",
                       high = "#390ba3")+
  theme_minimal()

## multiple chart in one
base <- ggplot(sample_n(diamonds,500),aes(x=price , y=carat)) +
  theme_minimal()

base + 
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess",
              color = "blue",
              se =FALSE)

base + 
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess",
              color = "blue",
              se =FALSE)

## method 01: create new column

diamonds %>% 
  mutate(price_segment = if_else(price >= 10000, "high", "low" )) %>%
  sample_n(500) %>% 
  ggplot(data = .,
         mapping = aes(price,carat,
                       color = price_segment)) +
  geom_point() +
  scale_color_manual(values = c("red","gold")) +
  theme_minimal()

##mtcars
mtcars %>% 
  ggplot(data=., mapping=aes(x=mpg)) +
  geom_density()

## method 02: call ggplot() twice

df <- sample_n(diamonds,2000)
ggplot() +
    geom_point(
      data = filter(df,price >= 10000),
      mapping = aes(x=price, y=carat),
      color = "blue"
    ) +
    geom_point(
      data = filter(df,price < 10000),
      mapping = aes(x=price, y=carat),
      color = "gold"
    ) +
  theme_minimal()

## Stack bar in ggplot
## Two variable, both are discrete

diamonds %>% 
  count(cut,color)

ggplot(data = diamonds,
       mapping = aes(x=cut, fill=color)) +
  geom_bar() +
  theme_minimal()

ggplot(data = diamonds,
       mapping = aes(x=cut, fill=color)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(x = "Cut Quality",
      y = "Proportion",
      title = "Stackes Bar Chart for Cut X Color",
      subtitle = "Created by AdToy 2025" )

ggplot(data = diamonds,
       mapping = aes(x=cut, fill=color)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Cut Quality",
       y = "Proportion",
       title = "Stackes Bar Chart for Cut X Color",
       subtitle = "Created by AdToy 2025" )

## Box plot, violin plot
## Two variable, one dis one con

##five number summary
## min, q1, q2 ,q3, max

diamonds %>% 
  group_by(cut) %>% 
  summarise(min = min(price),
            q1 = quantile(price,0.25),
            q2 = median(price),
            q3 = quantile(price,0.75),
            max = max(price))

ggplot(data = diamonds,
       mapping = aes(x=cut, y=price, fill = cut)) +
  geom_boxplot()

ggplot(data = diamonds,
       mapping = aes(x=cut, y=price, fill =cut)) +
  geom_violin()

## facet -> the most powerful data viz technique in R
## Small 

set.seed(42)
ggplot(data = diamonds %>% 
         sample_n(5000),
       mapping = aes(x=carat, y=price)) +
  geom_point() +
  facet_wrap(~cut, ncol=5) +
  theme_minimal()

ggplot(data = diamonds %>% 
         sample_n(5000),
       mapping = aes(x=carat, y=price)) +
  geom_point(color = "red", alpha = 0.3) +
  geom_smooth(color = "black") +
  facet_grid(cut ~ color) +
  theme_minimal()

ggplot(diamonds %>% 
         sample_n(300),
       aes(x=carat,y=price,
                    color = cut)) +
  geom_point(size = 3, alpha = 0.35)+
  scale_color_brewer(type = "qual", palette = 1) +
  theme_minimal()
