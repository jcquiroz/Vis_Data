

library(readxl)
library(tidyverse)
library(plotly)
library(ggthemes)

m3a <- read_excel("data/m3a_edad.xlsx", sheet = "Sheet1")

# Usualmente se busca indicadores por grupo
t1 <-m3a %>%
  filter(year >= 2009) %>%
  group_by(year,sex) %>%
  summarise(number = sum(n)/1e6, n = n())
    p1 <- ggplot(t1, aes(year,number)) + geom_bar(aes(fill=sex), stat='identity', position=position_dodge()) + 
      labs(title= 'Pesqueria Arrastre', x = 'year', y = 'Captura Numero [*1e6]') + guides(fill=FALSE) +
      scale_fill_manual(values=c("#999999", "#E69F00")) + theme_pander()
    ggplotly(p1)


# jitter data
t2 <- m3a %>% filter(year>= 2009) %>% mutate(n = n/1e6)
    p2 <- ggplot(t2,aes(x=age, y=n, colour=sex)) +
      geom_jitter(aes(text=paste("Year: ", year)), width=0.05, alpha=0.5 ) +
      geom_line(aes(group=year), alpha = 0.2) +
      theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
      labs(title = "Captura edad por AÑO", x = "Edad", y = "N [1*6]") + 
      guides(colour=FALSE) + theme_gdocs() + scale_color_wsj()
    ggplotly(p2)

    p3 <- ggplot(t2,aes(x=age, y=n, colour=factor(year))) +
      geom_jitter(aes(text=sex), width=0.05, alpha=0.5 ) +
      geom_line(aes(group=sex), alpha = 0.2) +
      theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
      labs(title = "Captura edad por SEXO", x = "Edad", y = "N [1*6]") + 
      guides(colour=FALSE) + theme_gdocs()+ scale_color_pander()
    ggplotly(p3)


t3 <-t2 %>% mutate(agef = factor(age))
    p4 <- ggplot(t3,aes(x=agef, y=n))+
      geom_jitter(aes(colour=year), alpha=0.5, width=0.25 ) +
      geom_boxplot(aes(fill=sex), alpha=0.8, outlier.shape = NA) +
      theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
      labs(title = "Captura edad por SEXO", x = "Edad", y = "N [1*6]") + 
      guides(fill=FALSE, colour=FALSE) + theme_gdocs() + 
      scale_fill_manual(values=c("#999999", "#E69F00")) +
      scale_color_gradientn(colours = terrain.colors(10))
      #scale_color_continuous(type = "viridis")
    ggplotly(p4, tooltip = c("x", "y", "fill")) %>%
      highlight("plotly_selected")


t4 <-t3 %>% mutate(ws = (weight-min(weight))/(max(weight)-min(weight)))
    p <- ggplot(t4,aes(x=age, y=n, colour=sex, fill=sex)) +
      geom_line(aes(frame=year)) + geom_point(aes(frame=year, size=weight)) + 
      scale_fill_manual(values=c("chocolate2", "dodgerblue1")) +
      scale_color_manual(values=c("chocolate2", "dodgerblue1")) + 
      scale_size(range = c(-1, 7)) + 
      labs(title = "Captura edad", x = "Edad", y = "N [1*6]") +
      guides(colour=FALSE, fill=FALSE, size=FALSE) + theme_pander() 
    ggplotly(p, tooltip = c("x", "y", "size", "fill")) %>% 
      animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Year: ", font = list(color="goldenrod2")))
    

t5 <-t4 %>% mutate(fishery2 = as.factor(fishery))
levels(t5$fishery2)[t5$year>=2014] <- "Trawl"
t5$fishery2[t5$year>=2014] <- "Trawl"
    p <- ggplot(t5,aes(x=age, y=n, colour=sex)) +
      geom_line(aes(frame=year)) + geom_point(aes(frame=year, size=weight)) + 
      facet_grid(. ~ sex) + 
      scale_color_manual(values=c("chocolate2", "dodgerblue1")) + 
      scale_size(range = c(-1, 7)) + 
      labs(title = "Captura edad", x = "Edad", y = "N [1*6]") +
      theme_few() + theme(legend.position = "none")
    ggplotly(p, tooltip = c("x", "y", "size")) %>% 
      animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Year: ", font = list(color="goldenrod2")))

    p <- ggplot(t5,aes(x=age, y=n, group=year, colour=year)) +
      geom_line() + geom_point(aes(size=weight)) + 
      facet_grid(fishery2 ~ sex) + 
      scale_color_continuous_tableau() + 
      scale_size(range = c(-1, 1)) + 
      labs(title = "Captura edad", x = "Edad", y = "N [1e6]") +
      theme_few() + theme(legend.position = "bottom") + guides(colour='colorbar')
    ggplotly(p, tooltip = c("x", "y", "size")) 



# Elementos estaticos -----------------------------------------------------

library(ggforce)
library(nycflights13)
library(readr)
erizo <- read_csv("data/erizoToy.csv")
glimpse(erizo)
    
tmp <- erizo %>% filter(ANO_REG >= 2016) %>% 
  select(poly = POLIGONO, lon = LONGITUD, lat = LATITUD)
glimpse(tmp)


p <- tmp %>%
  filter(lon != 0, lat != 0, poly >= 1) %>% mutate(polyf = factor(poly)) %>%
  ggplot(aes(lon, lat, color = polyf)) + 
  geom_jitter(show.legend = FALSE, width = 0.01, height = 0.02, size = 0.2) +
  theme_pander()
p

p +
  geom_mark_rect() 

p + 
  geom_mark_rect(aes(label = polyf))


p + 
  geom_mark_rect(aes(label = polyf), show.legend = FALSE) +
  theme_void() 


p + 
  geom_mark_hull(aes(label = polyf)) + theme_void() 


p + 
  geom_mark_hull(aes(label = polyf, fill = polyf), show.legend = FALSE) +
  theme_void() 


p + 
  geom_mark_hull(aes(label = polyf, fill = polyf), show.legend = FALSE, expand = unit(2, "mm")) +
  theme_void() 


p + 
  geom_mark_hull(aes(label = polyf, fill = polyf), show.legend = FALSE, expand = unit(2, "mm")) +
  theme_pander()


p +
  geom_mark_hull(aes(label = polyf, fill = polyf), show.legend = FALSE, expand = unit(2, "mm")) +
  theme_pander() +
  facet_zoom(xlim = c(-73.5, -73), ylim = c(-43, -42))


p +
  geom_mark_hull(aes(label = polyf, fill = polyf), show.legend = FALSE, expand = unit(2, "mm")) +
  theme_pander() +
  facet_zoom(xy = polyf == 4)




# Plotting with parallel --------------------------------------------------

glimpse(erizo)

tmp <- erizo %>% filter(ANO_REG >= 2018, POLIGONO >=1) %>% 
  select(puerto = PUERTO, poligono = POLIGONO, mes = MES_REG) %>%
  mutate(puerto = factor(puerto), poligono = factor(poligono), mes = factor(mes))
str(tmp)
tmp

tmp %>%
  gather_set_data(1:2) 


tmp %>%
  gather_set_data(c(1,2)) %>%
  ggplot(aes(x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = poligono)) 


tmp %>%
  gather_set_data(1:2) %>%
  ggplot(aes(x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = poligono)) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels()


tmp %>%
  gather_set_data(1:3) %>%
  ggplot(aes(x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = mes), show.legend = TRUE, alpha = 0.5) +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgrey", fill = "white") +
  geom_parallel_sets_labels(angle = 0) +
  theme_pander()


tmp %>%
  count(puerto) %>%
  ggplot() +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.65, r = 1, amount = n, fill = puerto), alpha = 0.7, stat = "pie")  +
  coord_fixed() + theme_no_axes() +
  scale_fill_pander()



tmp %>%
  count(puerto) %>%
  mutate(focus = ifelse(puerto %in% c("953","938"), 0.15, 0)) %>%
  ggplot() +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1, amount = n, fill = puerto, explode = focus), 
               alpha = 0.7, stat = "pie") +
  coord_fixed() + theme_no_axes() +
  scale_fill_pander()























































