library(extrafont)
loadfonts(dev = "win")
library(ggplot2) 
library(tidyverse)
library(magrittr)

paleta = colorspace::rainbow_hcl(10, c=100, l=65)
paleta = colorspace::rainbow_hcl(10)

lighten <- function(color, factor=1.4){
    col <- col2rgb(color)
    col <- col*factor
    col <- rgb(t(col), maxColorValue=255)
    col
}

apps <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-24/apps_googleplaystore.csv")

ggplot(apps) +
  geom_histogram(aes(x=precio), fill = paleta[10], bins = 33) +
  ggforce::geom_ellipse(mapping = aes(x0 = 400, y0 = 0, a = 30, b = 1000, angle = 0), color = paleta[10], size = 1.2) +
  theme_minimal() +
  labs(y = "", x = "price") +
  theme(plot.background = element_rect(fill = "#272822", color = "#272822"),
        panel.background = element_rect(fill = lighten("#272822", 1.4), color = lighten("#272822", 1.4)),
        text = element_text(color = "#ffffff", family = "Didact Gothic"),
        axis.text = element_text(color = "#ffffff", family = "Didact Gothic"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1), panel.grid.minor.y = element_line(size = 0.1))

ggplot(apps, aes(x=precio)) +
  geom_histogram(fill = paleta[10], bins = 33) +
  ggforce::facet_zoom(xlim = c(350, 410), ylim = c(0, 15), horizontal = FALSE) +
  theme_minimal() +
  labs(y = "", x = "price") +
  theme(plot.background = element_rect(fill = "#272822", color = "#272822"),
        panel.background = element_rect(fill = lighten("#272822", 1.4), color = lighten("#272822", 1.4)),
        text = element_text(color = "#ffffff", family = "Didact Gothic"),
        axis.text = element_text(color = "#ffffff", family = "Didact Gothic"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1), panel.grid.minor.y = element_line(size = 0.1),
        strip.background = element_rect(fill = alpha(lighten("#272822", 1.4), 1), color = lighten("#272822", 1.4)))

apps %>%
  dplyr::filter(precio > 350) %>%
  dplyr::pull(app)

