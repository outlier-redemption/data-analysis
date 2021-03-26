library(tidyverse)
library(extrafont)
library(rgeos)
library(sf)
library(here)
loadfonts(dev = "win")

world <- rnaturalearth::ne_countries(scale = 10, returnclass = "sp")
box_cut <- rgeos::bbox2SP(n = 90, s = -90, w = -60, e = 90, proj4string = world@proj4string)
world_crop <- rgeos::gDifference(world, box_cut)

lighten <- function(color, factor=1.4){
    col <- col2rgb(color)
    col <- col*factor
    col <- rgb(t(col), maxColorValue=255)
    col
}

paleta = colorspace::rainbow_hcl(10)

pacific_crop <- world_crop %>% 
  st_as_sf() %>% # change from sp to sf object/class
  st_shift_longitude() %>% 
  st_crop(c(xmin = 90,
            xmax = 230,
            ymin = -50,
            ymax = 30))

tiny_countries <- rnaturalearthdata::tiny_countries50 %>% 
  st_as_sf() %>%
  st_shift_longitude() %>% 
  st_crop(c(xmin = 90, xmax = 230, ymin = -50, ymax = 30)) %>% 
  # Also adds the coordinates to be used for labeling with geom_text_repel
  bind_cols(st_coordinates(.) %>% as.data.frame())

tiny_countries2 <- rnaturalearthdata::tiny_countries50 %>% 
  st_as_sf() %>%
  filter(name == "Tuvalu") %>%
  st_shift_longitude() %>% 
  st_crop(c(xmin = 90, xmax = 230, ymin = -50, ymax = 30)) %>% 
  # Also adds the coordinates to be used for labeling with geom_text_repel
  bind_cols(st_coordinates(.) %>% as.data.frame())

tuvalu_years <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

tuvalu_GNI <- c(24960328.95, 33312887.1, 28110318.44, 36589277.83, 37400840.02, 37830798.19, 48173709.31, 49241989.6, 47385821.24, 50918730.51, 57742238.27, 49401532.41, 58017281.33, 49698846.22, 55661783.49, 55456437.7, 58095263.64, 64107890.02, 65326451.16)

data_tuvalu <- tibble(years = tuvalu_years, GNI = tuvalu_GNI) 

ggplot(data_tuvalu) +
  geom_line(aes(x = years, y = GNI), size = 1.5, color = "white") +
  geom_hline(yintercept = 5000000, color = paleta[1], size = 1.2, lty = 2) +
  scale_y_continuous(limits = c(0, 70000000)) +
  scale_x_continuous(limits = c(2011, 2019), breaks = c(2011,2013,2015,2017,2019)) +
  labs(x = "", y = "GNI (US dollars)", caption = "Source: data.worldbank.org") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = lighten("#272822", 1.4), color = lighten("#272822", 1.2)),
        #panel.border =  element_rect(fill = "#272822", color = "#272822"),
        text = element_text(color = "#ffffff", family = "Didact Gothic"),
        axis.text = element_text(color = "#ffffff", family = "Didact Gothic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "#272822", color = "#272822"))
