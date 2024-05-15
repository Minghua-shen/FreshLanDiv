
library(tidyverse)

library(ggplot2)

getwd()#open script within project 'Datapaper'

####Distribution map#####

meta_data <- read.csv('metadata.csv')

meta_data  <- transform(meta_data, Latitude= as.double(Latitude),Longitude= as.double(Longitude))

source_data<- readxl::read_xlsx(path = "Land-use-and-freshwater-database.xlsx", sheet = 1)

plot_data<- readxl::read_xlsx(path = "Land-use-and-freshwater-database.xlsx", sheet = 2)

plot_data <- dplyr::select(plot_data,Plot_id,Site_id,Dataset_id,Stream_order,
                           Latitude,Longitude,Country,Continent)

plot_data $Latitude <-as.numeric(plot_data$Latitude)

plot_data $Longitude <-as.numeric(plot_data$Longitude)

#plot map
world <- map_data("world")

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)
  ) 
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) 


####plot with plot_id coordinates
plot_data$Dataset_id<-as.character(plot_data$Dataset_id)

meta_data$Latitude<-as.numeric(meta_data$Latitude)
meta_data$Longitude<-as.numeric(meta_data$Longitude)
#study level map
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  # geom_point(
  #   data = plot_data,#plots location
  #   aes(Longitude, Latitude),
  #   alpha = 0.015,size = 6
  # ) + 
  geom_point(data = meta_data,#study location
             aes(Longitude, Latitude
                 # , color =Taxa
                 ,fill=Taxa
                 ), 
            alpha = 0.9,size = 5
            , shape = 21,color="darkgrey"
  ) +
  labs(fill = "Taxon group",) +
  theme_void() +
  scale_colour_viridis_d(aesthetics = "fill")+
  guides(fill = guide_legend(nrow = 1))+
  #labs(x = NULL, y = NULL, color = NULL)+
  theme(legend.position =c(.5,.03), legend.direction = "horizontal",
        text = element_text(size=13))+
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

#sites level map
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = plot_data,#plots location
    aes(Longitude, Latitude,color=Dataset_id),
    alpha = 0.15,size = 4,shape=17
  ) +
  # geom_point(data = meta_data,#study location
  #            aes(Longitude, Latitude
  #                # , color =Taxa
  #                ,fill=Taxa
  #            ), 
  #            alpha = 0.9,size = 5
  #            , shape = 21,color="darkgrey"
  # ) +
  # labs(fill = "Taxon group",) +
  theme_void() +
  scale_colour_viridis_d()+
#   # guides(fill = guide_legend(nrow = 1))+
#   #labs(x = NULL, y = NULL, color = NULL)+
  theme(legend.position ="none")
# +
#   guides(color = guide_legend(nrow = 1, byrow = TRUE))

#count land use comparison group number
land_use <- readxl::read_xlsx("Land-use-categories.xlsx", sheet = 1)

table_lu<-as.data.frame(unclass(table(land_use$Dataset_id,land_use$Land_use)))

table_lu_com<-table_lu %>% as_tibble() %>% 
  mutate(Forest_Forestry = ifelse(`Natural vegetation` >0 & Forestry >0, 1, 0),
         Forest_Agriculture = ifelse(`Natural vegetation` >0 & Agriculture >0, 1, 0),
         Forest_Urban = ifelse(`Natural vegetation` >0 & Urban >0, 1, 0),
         Forest_Mining = ifelse(`Natural vegetation` >0 & Mining >0, 1, 0),
         Forestry_Agriculture = ifelse(Forestry >0 & Agriculture >0, 1, 0),
         Forestry_Urban = ifelse(Forestry >0 & Urban >0, 1, 0),
         Forestry_Mining = ifelse(Forestry >0 & Mining >0, 1, 0),
         Agriculture_Urban = ifelse(Agriculture >0 & Urban >0, 1, 0),
         Agriculture_Mining = ifelse(Agriculture >0 & Mining >0, 1, 0),
         Urban_Mining = ifelse(Urban  >0 & Mining >0, 1, 0))

table_lu_com<-table_lu_com %>% 
  select(-(1:5),)

table_lu_com_g<-
  table_lu_com %>% as.tibble %>% colSums() %>% as.data.frame()

table_lu_com_g$comparison<-rownames(table_lu_com_g)
table_lu_com_g$Study_number<-table_lu_com_g$.
table_lu_com_g$Study_number<-as.numeric(table_lu_com_g$Study_number)


####land_use_comparison figure#####  
lu_com<-readxl::read_xlsx("Land_use_comparison.xlsx", sheet = 1)

lu_com$`Mining`<-as.character(lu_com$`Mining`)

corm <- lu_com |>
  tidyr::pivot_longer(
    cols = -Land_use,
    names_to = "colname",
    values_to = "corr"
  ) |>
  dplyr::mutate(
    rowname = forcats::fct_inorder(Land_use),
    colname = forcats::fct_inorder(colname),
    label = dplyr::if_else(is.na(corr), "",corr)
  )

corm$corr<-as.numeric(corm$corr)
corm$label<-as.numeric(corm$label)

ggplot(corm, aes(rowname, fct_rev(colname),
                 fill = corr)) +
  geom_tile() +
  geom_text(aes(
    label = label
  )) +
  coord_fixed(expand = FALSE) +
  scale_fill_continuous(na.value = "white",
                        type = "viridis",
                        alpha =0.65,
                        name = "Study number")+
  labs(x="Land use",y="Land use") +
  theme(line = element_blank(),
    panel.border = element_rect(color = NA, fill = NA),
        legend.position = c(.85, .8),
    text = element_text(size=13))



#Taxon group#
Taxon_groups<- ggplot(meta_data,aes(x=fct_infreq(Taxa)))+
  stat_count(geom = "bar")+
  labs(x="Biological group",y="Number of studies")+
  theme(line = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        # panel.border = element_rect(colour = "black",fill = NA),
        # axis.text.x = element_text(angle = 20, hjust = 1),
        text = element_text(size=15))

#Basin type#
Basin_type<-ggplot(meta_data, aes(x=fct_infreq(Basin_type),na.rm = TRUE))+
  stat_count(geom = "bar")+
  labs(x="Ecosystem type")+
  theme(line = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        # panel.border = element_rect(colour = "black",fill = NA),
        axis.title.y=element_blank(),
        # axis.text.x = element_text(angle = -15),
        text = element_text(size=15))

cowplot::plot_grid(Taxon_groups, Basin_type,rel_widths = c(1.3, 1), labels = "AUTO")
