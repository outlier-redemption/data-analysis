paleta = colorspace::rainbow_hcl(10, c=100, l=65)
library(rtweet)
library(tidyverse)
library(extrafont)
library(lubridate)
library(ggtext)
library(patchwork)

loadfonts(dev = "win")

tweets = rtweet::get_timeline("@year_progress", n = 18000)

tweets = tweets %>%
  filter(created_at >= lubridate::date("2015-05-08"),
         str_ends(text, "%")) %>%
  mutate(percentage = str_extract(text, "\\d+"),
         date = lubridate::date(created_at)) %>%
  select(text, percentage, created_at, date, retweet_count)

write_csv(tweets, "tweets.csv")

tweets = read_csv("tweets.csv") %>%
  filter(year(date) > 2017)

tweets %>%
  group_by(percentage) %>%
  summarise(RT = mean(retweet_count)) %>%
  ggplot() +
  geom_col(aes(x = as.numeric(percentage)/100, y = RT), fill = "black") +
  theme_minimal() +
  labs(y = "Retweet count", x = "",  title =  paste0("The most retweeted progress bars."),
       caption = "Source: @year_progress tweet list.") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot", 
        text = element_text(family = "Didact Gothic"),
        plot.title = element_markdown(size = 18),
        plot.subtitle = element_markdown(size = 15))


p1 = tweets %>%
  group_by(percentage) %>%
  summarise(RT = mean(retweet_count)) %>%
  ggplot() +
  geom_col(aes(x = as.numeric(percentage)/100, y = RT, fill = RT > 10000)) +
  geom_hline(yintercept = 10000, color = "black", linetype = 2, size = 0.8) +
  theme_minimal() +
  labs(y = "Retweet count", x = "",  title =  paste0("Threshold: 10000 RT"),
       caption = "Source: @year_progress tweet list.") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("black", paleta[8])) +
  theme(plot.title.position = "plot", 
        legend.position = "none",
        text = element_text(family = "Didact Gothic"),
        plot.title = element_markdown(size = 13))

p2 = tweets %>%
  group_by(percentage) %>%
  summarise(RT = mean(retweet_count)) %>%
  ggplot() +
  geom_col(aes(x = as.numeric(percentage)/100, y = RT, fill = RT > 5000)) +
  geom_hline(yintercept = 5000, color = "black", linetype = 2, size = 0.8) +
  theme_minimal() +
  labs(y = "Retweet count", x = "",  title =  paste0("Threshold: 5000 RT"),
       caption = "Source: @year_progress tweet list.") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("black", paleta[8])) +
  theme(plot.title.position = "plot", 
        legend.position = "none",
        text = element_text(family = "Didact Gothic"),
        plot.title = element_markdown(size = 13))

p3 = tweets %>%
  group_by(percentage) %>%
  summarise(RT = mean(retweet_count)) %>%
  ggplot() +
  geom_col(aes(x = as.numeric(percentage)/100, y = RT, fill = RT > 2000)) +
  geom_hline(yintercept = 2000, color = "black", linetype = 2, size = 0.8) +
  theme_minimal() +
  labs(y = "Retweet count", x = "",  title =  paste0("Threshold: 2000 RT"),
       caption = "Source: @year_progress tweet list.") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("black", paleta[8])) +
  theme(plot.title.position = "plot", 
        legend.position = "none",
        text = element_text(family = "Didact Gothic"),
        plot.title = element_markdown(size = 13))

table = tweets %>%
  group_by(percentage) %>%
  summarise(RT = mean(retweet_count)) 

#paste0(pull(filter(table, RT>=10000), percentage), collapse = ", ")
#paste0(pull(filter(table, RT>=5000), percentage), collapse = ", ")
#paste0(pull(filter(table, RT>=2000), percentage), collapse = ", ")

(p1+ grid::textGrob("0, 50, 69, 99, 100", gp=grid::gpar(col=paleta[8], fontfamily  = "Didact Gothic", fontface = "bold")))/
  (p2+ grid::textGrob("0, 25, 50, 69, 90,\n98, 99, 100", gp=grid::gpar(col=paleta[8], fontfamily  = "Didact Gothic", fontface = "bold")))/
  (p3+ grid::textGrob("0, 1, 5, 10, 20,\n25, 30, 33, 40, 50,\n60, 69, 70, 75, 80,\n85, 90, 91, 92, 93,\n94, 95, 96, 97, 98,\n99, 100", gp=grid::gpar(col=paleta[8], fontfamily  = "Didact Gothic", fontface = "bold")))


datis = tibble::tibble(number = 1 : 100, x = ((1:100)-1) %% 10 + 1, y = 10 - ((1:100)-1) %/%10) %>%
  bind_rows(tibble(number = 0, x = 0, y = 10))

expected = unique(c(setdiff(pull(filter(table, RT>=2000), percentage) , 69), seq(5, 95, by = 5), 66))

expected_plus = setdiff(pull(filter(table, RT>=2000), percentage) , 69)

p12 = ggplot(datis) +
  geom_point(aes(x, y, color = number %in% expected)) +
  geom_text(data = datis %>% filter(number %in% expected), aes(x, y + 0.5 , label = number), color = "black") +
  geom_point(data = datis %>% filter(number %in% expected_plus), aes(x, y) , color = paleta[8]) +
  geom_text(data = datis %>% filter(number %in% expected_plus), aes(x, y + 0.5 , label = number), color = paleta[8]) +
  coord_equal() +
  scale_color_manual(values = c("gray", "black")) +
  theme_void() +
  labs(title = "The ones that I was expecting") +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 13),
        text = element_text(family = "Didact Gothic"))

p13 = ggplot(datis) +
  geom_point(aes(x, y, color = ! number %in% expected)) +
  geom_point(data = datis %>% filter(number == 69), aes(x, y), color = paleta[8]) +
  geom_text(data = datis %>% filter(! number %in% expected), aes(x, y + 0.5 , label = number), color = "black") +
  geom_text(data = datis %>% filter(number == 69), aes(x, y + 0.5 , label = number), color = paleta[8]) +
  coord_equal() +
  scale_color_manual(values = c("gray", "black")) +
  theme_void() +
  labs(title = "The ones that I was not expecting") +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 13),
        text = element_text(family = "Didact Gothic"))

p12 + p13 +
  plot_annotation(title = "The most popular percentages of the year", theme = theme(plot.title = element_text(size = 18, family = "Didact Gothic", color = paleta[8])))


tweets %>%
  ggplot() +
  geom_line(aes(x = date, y = retweet_count), size = 1) +
  theme_minimal() +
  geom_vline(xintercept = pull(tweets %>% filter(percentage == "69"), 
                               date),
             color = paleta[1], 
             linetype = 2,
             size = 0.8) +
  geom_vline(xintercept = pull(tweets %>% filter(percentage == "50"), 
                               date),
             color = paleta[3], 
             linetype = 2,
             size = 0.8) +
  geom_vline(xintercept = pull(tweets %>% filter(percentage == "100"), 
                               date),
             color = paleta[6], 
             linetype = 2,
             size = 0.8) +
  labs(y = "Retweet count", x = "", 
       subtitle =  paste0("Daily number of retweets of @year_progress. The highlighted spikes represent<br><span style='color: ", paleta[3], ";'>50% </span>, <span style='color: ", paleta[1], ";'>69%</span>, and <span style='color: ", paleta[6], ";'>100%</span> progress of the year.") , title =  paste0("<span style='color: ", paleta[3], ";'>Half time </span>, <span style='color: ", paleta[1], ";'>sex time</span>, and <span style='color: ", paleta[6], ";'>the end</span>"),
       caption = "Source: @year_progress tweet list.") +
  scale_x_date(expand = c(0, 0), breaks = seq(as.Date("2015-12-01"), as.Date("2021-06-01"), by="6 months"),
               date_labels = "%b %Y", ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title.position = "plot", 
        text = element_text(family = "Didact Gothic"),
        plot.title = element_markdown(size = 18),
        plot.subtitle = element_markdown(size = 15))
