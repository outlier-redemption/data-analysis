library(tidyverse)
library(pageviews)
library(ggbump)
library(cowplot)
library(ggbump)

top_1 = pageviews::top_articles(project = "es.wikipedia", 
                                start =  as.Date("01/01/21", "%d/%m/%y"),
                                granularity = "monthly") %>%
  filter(rank<= 7, !rank %in% c(1, 2)) %>%
  mutate(month = "JAN")
top_2 = pageviews::top_articles(project = "es.wikipedia", 
                                start =  as.Date("01/02/21", "%d/%m/%y"),
                                granularity = "monthly") %>%
  filter(rank<= 7, !rank %in% c(1, 2)) %>%
  mutate(month = "FEB")
top_3 = pageviews::top_articles(project = "es.wikipedia", 
                                start =  as.Date("01/03/21", "%d/%m/%y"),
                                granularity = "monthly") %>%
  filter(rank<= 7, !rank %in% c(1, 2)) %>%
  mutate(month = "MAR")
top_4 = pageviews::top_articles(project = "es.wikipedia", 
                                start =  as.Date("01/04/21", "%d/%m/%y"),
                                granularity = "monthly") %>%
  filter(rank<= 7, !rank %in% c(1, 2)) %>%
  mutate(month = "APR")
top_5 = pageviews::top_articles(project = "es.wikipedia", 
                                start =  as.Date("01/05/21", "%d/%m/%y"),
                                granularity = "monthly") %>%
  filter(rank<= 7, !rank %in% c(1, 2)) %>%
  mutate(month = "MAY",
         article = case_when(article == "Wikipedia:Celebración_del_20.º_aniversario_de_Wikipedia_en_español" ~ "Wiki 20 años",
                             TRUE ~ article))
top = bind_rows(top_1, top_2, top_3, top_4, top_5) %>%
  select(month, rank, article)  %>%
  mutate(article = str_replace_all(article, "_", " "))

# following https://github.com/davidsjoberg/ggbump/wiki/My-year-on-Spotify
top_top =  tidyr::crossing(month = unique(top$month), article = unique(top$article)) %>%
  left_join(top,by = c("month" = "month", "article" = "article")) %>%
  mutate(rank = rank - 2,
         rank = if_else(is.na(rank),
                        6,
                        rank),
         order = case_when(month == "JAN" ~ 1,
                           month == "FEB" ~ 2,
                           month == "MAR" ~ 3,
                           month == "APR" ~ 4,
                           month == "MAY" ~ 5))

top_top = top_top %>%
  group_by(article) %>%
  mutate(first_top5 = min(order[rank <= 5]),
         last_top5 = max(order[rank <= 5]),
         d_first_top5 = if_else(order == first_top5,
                                1,
                                0)) %>%
  filter(!is.na(first_top5),
         order >= first_top5) %>%
  ungroup()

top_top <- top_top %>% 
  arrange(article, order) %>% 
  group_by(article) %>% 
  mutate(lag_zero = if_else(lag(rank) %in% c(6, NA) & rank <= 5, 1, 0, 0)) %>% 
  ungroup() %>% 
  mutate(group = cumsum(lag_zero))

top_top %>%
  write_csv("topviews.csv")


p <- top_top %>% 
  ggplot(aes(order, rank, group = article), color = "gray") +
  geom_bump(smooth = 15, size = 2, alpha = 0.1) +
  scale_y_reverse() 

p <- p +
  geom_bump(mapping = aes(order, rank, group = article), color = paleta[4], 
            data = top_top %>% filter(article == "Isabel II del Reino Unido"),
            smooth = 15, size = 2, alpha = 0.3) 

p <- p +
  geom_bump(data = top_top %>% filter(rank <= 5), 
            aes(order, rank, group = group, color = article), 
            smooth = 15, size = 2, inherit.aes = F) +
  scale_color_manual(values = paleta[c(1, 4, 8, 9)])

p <- p + 
  geom_point(data = top_top %>% filter(d_first_top5 == 1),
             aes(x = order - .2), color = "gray",
             size = 5) +
  geom_segment(data = top_top %>% filter(rank <=5),
               aes(x = order - .2, xend = order + .2, 
                   y = rank, yend = rank),  
               color = "gray",
               size = 2,
               lineend = "round") + 
  theme_minimal() 

p <- p + 
  geom_point(data = top_top %>% filter(d_first_top5 == 1) %>% filter(article %in% c("Cleopatra", "Tabla periódica de los elementos", "Isabel II del Reino Unido", "Traducción")),
             aes(x = order - .2, color = article),
             size = 5) +
  geom_segment(data = top_top %>% filter(article %in% c("Cleopatra", "Tabla periódica de los elementos", "Isabel II del Reino Unido", "Traducción")) %>% filter(rank<6),
               aes(x = order - .2, xend = order + .2, 
                   y = rank, yend = rank,  color = article),
               size = 2,
               lineend = "round") + 
  theme_minimal()

p+
  scale_x_continuous(breaks = top_top$order %>% unique() %>% sort(),
                     labels = c("Jan", "Feb", "Mar" , "Apr", "May"),
                     expand = expand_scale(mult = .1)) +
  geom_text(data = top_top %>% filter(d_first_top5 == 1,
                                      ! article %in% c("WandaVision",
                                                       "Ejército Nacional de Colombia", "Día Internacional de la Mujer")),
            aes(label = article, x = order-.2),
            family = "Didact Gothic",
            color = "black",
            nudge_y = .43,
            nudge_x = -.05,
            size = 3.5,
            fontface = 2,
            hjust = 0) +
  theme_void() +
  cowplot::theme_minimal_hgrid(font_size = 14) +
  labs(x = NULL,
       caption = "SOURCE: pageviews package") +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = "Didact Gothic", size = 14)) +
  geom_point(data = tibble(x = 0.55, y = 1:5), aes(x = x, y = y), 
             inherit.aes = F,
             color = "black",
             size = 10,
             pch = 21) +
  geom_text(data = tibble(x = .55, y = 1:5), aes(x = x, y = y, label = y), 
            inherit.aes = F,
            color = "black")

datos_2 <- article_pageviews(project = "es.wikipedia",
                             article = c("Cleopatra", "Marie Curie",
                                         "YouTube", 
                                         "Tabla periódica de los elementos",
                                         "COVID-19"),
                             start =  "2021010100",
                             end =  "2021062100")


datos_2 = read_csv("datos_wiki_2.csv")
filter(datos_2,! article %in%  c("Cleopatra", "Tabla_periódica_de_los_elementos")) %>%
  ggplot() +
  geom_line(aes(x = date, y = views, group = article), size = 1,  color = "gray" ) +
  geom_line(data = filter(datos_2, article == "Cleopatra"), 
            aes(x = date, y = views), size = 1.5,
            color = colorspace::rainbow_hcl(10)[1])+
  geom_line(data = filter(datos_2, article == "Tabla_periódica_de_los_elementos"), 
            aes(x = date, y = views), size = 1.5,
            color = paleta[8])+
  annotate("text", x = c(as.POSIXct("2021-04-01"), 
                         as.POSIXct("2021-01-02"),
                         as.POSIXct("2021-01-02")),
           y = c(200000, 100000+5000, 100000/2+5000),
           label = c("Marie Curie", 
                     "Cleopatra",
                     "Tabla periódica de los elementos"), family = "Didact Gothic", hjust = 0, face = "bold", size = 4.5) +
  scale_x_datetime(date_breaks = "1 month", date_minor_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Number of visits", x = "", 
       color = "Article", caption="SOURCE: pageviews package") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 14, family = "Didact Gothic"))