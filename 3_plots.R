rm(list=ls())
library(data.table)
library(ggplot2)
library(mapview)
library(sf)


# 1) od 1 plot-----
rm(list=ls())

od <- readr::read_rds("data/od_total_res7.rds")
res <- readr::read_rds("data/grid_toronto_res7.rds")

# destination
grid_dt <- function(dt,way = "origin"){
  dest <- copy(od)
  dest <- dest[,list("N" = sum(N)),by = way]
  dest$way <- way
  setnames(dest,way,"id_hex")
  # add geometry
  res <- setDT(res)
  dest <- data.table::merge.data.table(y = res
                                       ,x = dest
                                       ,by = "id_hex")
  dest <- sf::st_sf(dest)
  return(dest)
}

my_grid <- rbind(grid_dt(dt,"origin"),grid_dt(dt,"destination"))

my_grid[my_grid$id_hex == "872b9bc46ffffff",]
ggplot(my_grid)+
  geom_sf(aes(fill = N))+
  scale_fill_viridis_c(direction = -1,trans = "log10")+
  labs(fill = "Total trips")+
  facet_wrap(~way)

# 2) OD plot-----
rm(list=ls())
library(data.table)
library(ggplot2)
library(sf)
library(mapview)

# read od data
od <- readr::read_rds("data/od_res7.rds")
res <- readr::read_rds("data/grid_toronto_res7.rds")
# read Tiles & Boundaries
my_tile <- readr::read_rds("data_raw/maptiles_tor.rds")
my_bound <- readr::read_rds("data_raw/boundary_tor.rds")


add_geometry <- function(ress,od,way){
  # od <- tmp_od
  od <- as.data.table(od)
  ress<- as.data.table(ress)
  #way = "destination"
  geom_file <- data.table::merge.data.table(y = ress
                                            ,by.y = "id_hex"
                                            ,x = od
                                            ,by.x = way)
  geom_file[,way := way]
  setnames(geom_file,old = way,new = "id_hex")
  geom_file <- sf::st_sf(geom_file)
  return(geom_file)
}
tmp_od <- copy(od)
tmp_od <- tmp_od[,.N,by=.(origin)]
tmp_od[,prop := N/sum(N)]
#tmp_od <- tmp_od[serviceprovided %in% c("uberX","uberPOOL"),]

my_grid_o <- rbind(add_geometry(res,tmp_od,way = "origin"))

tmp_od <- copy(od)[,.N,by=.(destination)]
tmp_od[,prop := N/sum(N)]
my_grid_d <- rbind(add_geometry(res = res,tmp_od,way = "destination"))
# row bind
my_grid <- rbind(my_grid_d,my_grid_o)
my_grid <- sf::st_as_sf(my_grid)
my_grid

# adjust factors

my_grid$way <- factor(my_grid$way
                      ,levels = c("destination","origin")
                      ,labels = c("Destination","Origin"))
#my_grid <- my_grid[my_grid$way == "Origin",]
# transform data
my_grid <- sf::st_transform(my_grid,3857)
my_bound <- sf::st_transform(my_bound,3857)

# remove data out of the bbox
dt <- data.frame( "x" = c( min(my_tile$x),min(my_tile$x),max(my_tile$x),max(my_tile$x))
                  ,"y" = c( min(my_tile$y),max(my_tile$y),max(my_tile$y),min(my_tile$y)))
dt <- dt |> sfheaders::sf_polygon(,x = "x",y = "y") 
dt <- sf::st_set_crs(dt,3857)
st_crs(my_grid) == st_crs(my_grid)
ids <- sf::st_intersects(my_grid,dt,sparse = F) |> which()
my_grid <- my_grid[ids,]
my_grid1 <- sf::st_intersection(my_grid,dt)
#
library(mapview)
mapview(my_grid) 
plot(my_grid$geometry) 
mapview(dt)
mapview(dt)
mapview::mapview(dt) + mapview(my_grid[ids,])
# plot

ggplot(my_grid1)+
  # add raster
  geom_raster(data = my_tile, aes(x, y, fill = hex)
              , alpha = 0.75) +
  coord_equal() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~way)+
  # remaining data
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = log(as.numeric(N)))
          , colour = "transparent",alpha = .85)+  
  viridis::scale_fill_viridis(option = "D"
                              ,direction = -1)+
  geom_sf(data = my_bound,color = "black",alpha = 0)+
  labs(fill = "Total trips \n(log)
       ",title = "Trips generation")+
  theme_minimal() + 
  theme_void() +
  theme(legend.position = "bottom"#,c(0.85,0.225)
        ,legend.direction = "horizontal"
        ,text = element_text(family = "Times New Roman"),
        legend.box.background = element_rect(fill = "white"
                                             ,color = "white"),
        legend.key.size = ggplot2::unit(0.08250,"npc"),
        legend.box.margin = margin(3,3,3,3, "pt")) 

ggsave("figures/generation_trips.jpg"
       ,width = 20,height = 20/2,units = "cm",scale = .85)

# 3) hour plot-----
rm(list=ls())

od <- readr::read_rds("data/od_res7.rds")
res <- readr::read_rds("data/grid_toronto_res7.rds")

add_geometry <- function(ress,od,way){
  # od <- tmp_od
  od <- as.data.table(od)
  ress<- as.data.table(ress)
  #way = "destination"
  geom_file <- data.table::merge.data.table(y = ress
                                            ,by.y = "id_hex"
                                            ,x = od
                                            ,by.x = way)
  geom_file[,way := way]
  setnames(geom_file,old = way,new = "id_hex")
  geom_file <- sf::st_sf(geom_file)
  return(geom_file)
}
tmp_od <- copy(od)
tmp_od <- tmp_od[,.N,by=.(origin,hour)]
tmp_od[,prop := N/sum(N),by = .(hour)]
#tmp_od <- tmp_od[serviceprovided %in% c("uberX","uberPOOL"),]

my_grid_o <- rbind(add_geometry(res,tmp_od,way = "origin"))

tmp_od <- copy(od)
tmp_od <- tmp_od[,.N,by=.(destination,hour)]
tmp_od[,prop := N/sum(N),by = .(hour)]
#tmp_od[,prop := N/sum(N)]
#tmp_od <- tmp_od[serviceprovided %in% c("uberX","uberPOOL"),]
my_grid_d <- rbind(add_geometry(res = res,tmp_od,way = "destination"))

#my_grid <- melt.data.table(data = my_grid
#                ,measure.vars = c("origin","destination")[1]
#                ,value.name = "hex_id"
#                ,variable.name = "way")
##my_grid <- my_grid[way == "origin"]
#my_grid <- my_grid[,(N1 = sum(N)),by = .(serviceprovided,hex_id)]
my_grid <- rbind(my_grid_d,my_grid_o)
head(my_grid,1)
unique(my_grid$serviceprovided)

str(my_grid)
my_grid <- sf::st_as_sf(my_grid)
my_grid

#plot_grid <- my_grid[my_grid$serviceprovided %in% c("uberX","uberPOOL"),]

mapview(my_grid[my_grid$way=="origin","N"])

ggplot(my_grid[
  my_grid$hour %in% c(8,18),
])+
  geom_sf(aes(fill = 100*prop))+
  scale_fill_viridis_c(direction = -1)+
  labs(fill = "Prop trips")+
  facet_grid(rows = vars(way),cols = vars(hour))+
  theme_void()

# 4) text description -----
rm(list=ls())
library(data.table)
library(ggplot2)
#library(mapview)
#library(sf)

#od <- readr::read_rds("data/od_total_res7.rds")
#res <- readr::read_rds("data/grid_toronto_res7.rds")
dt <- fread("data_raw/feb6thtrips_cleaned_with_emissions.csv")

# check columns
names(dt)[names(dt) %like% "date"]  |>  paste0(", ",collapse = "")

dt[1,.SD,.SDcols = c('fareamount','pickup_long','pickup_lat','dropoff_long','dropoff_lat')]

dt$driverarrivaldatetime[1]
table(dt$serviceprovided)
str(dt)

as.Date("2020-02-06","%Y-%m-%d") |>  format("%A")

## 4.1) trips per hour ----
dt
dt[,typeService := fifelse(
  !(serviceprovided %in% c("uberX","standard")),
  "Other Services*",serviceprovided)]
label_caption <- dt[typeService == "Other Services*"
                    ,sort(unique(serviceprovided))] 
label_caption <- paste0("* "
                        ,paste0(c(label_caption[1:6]
                                  ,"\n"
                                  ,label_caption[7:13])
                                ,collapse = ", ")
                        ,collapse = "")
label_caption <- gsub(", shared"," shared",label_caption)
label_caption
dt[,.N,by = .(hour,typeService)]  |> 
  ggplot()+
  geom_col(aes(x = hour,y = N,fill = typeService))+
  scale_fill_manual(values = ggsci::pal_jama()(7)[c(2,3,6)])+
  labs(x = "Hour",y = "Number of trips"
       ,fill = "Type of Services"
       ,caption = label_caption)+
  # theme
  theme_light()+
  theme(axis.text.x = element_text(size = 9)
        ,text = element_text(family = "Times New Roman")
        ,strip.text = element_text(colour = 'black')
        #,legend.position = c(0.125,0.80)
        ,legend.position = "bottom"
  )+
  guides(fill = guide_legend(title.position = "top"))

dir.create("figures",showWarnings = FALSE)
ggsave("figures/total_trips_hour.jpg"
       ,width = 40/2,height = 30/2,units = "cm",scale = .7)

## 4.2) type of service ------
rm(list=ls())
library(data.table)
library(magrittr)
library(ggplot2)
library(mapview)
library(sf)

#od <- readr::read_rds("data/od_total_res7.rds")
#res <- readr::read_rds("data/grid_toronto_res7.rds")
dt <- fread("data_raw/feb6thtrips_cleaned_with_emissions.csv")

tmp_dt <- copy(dt)[,.N,by = .(serviceprovided)]
tmp_dt[serviceprovided %like% c("ber"),company := "Uber"]
tmp_dt[!(serviceprovided %like% c("ber")),company := "Lyft"]

tmp_dt <- tmp_dt[,sum(N),by = .(serviceprovided,company)]
tmp_dt <- tmp_dt[order(V1,decreasing = TRUE),]

tmp_dt <- rbind(tmp_dt[company == "Lyft",],tmp_dt[company == "Uber"])
tmp_dt[,prop := paste0(V1," (",100 * round(V1/sum(V1),3),"%)")]


# 4.3) mean, sd----

tmp_dt <- copy(dt)[serviceprovided %in% c("uberX","standard"),.SD
                   ,.SDcols = c("distance","fareamount","avg_speed_p3")]
tmp_dt[,id := 1:.N]
tmp_dt <- melt.data.table(data = tmp_dt
                          ,id.vars = c("id"))

var_names <- c(
  `distance` = "Distance (km)",
  `fareamount` = "Fare amount (CAD)",
  `avg_speed_p3` = "Avg. Speed (km/h)"
)

ggplot(tmp_dt[value < 85,]) + 
  geom_histogram(aes(x = value
                     ,fill = variable),bins = 30L
                 ,binwidth = .75)+
  facet_wrap(~variable,nrow = 3,scales = "free"
             ,labeller = as_labeller(var_names))+
  scale_fill_viridis_d()+
  labs(x = NULL,y = "Frequency")+
  theme_bw(base_family = "Times New Roman")+
  theme(legend.position = "none")

#dir.create("figures")
ggsave("figures/histogram_variables.jpg"
       ,width = 30/2,height = 30/2,units = "cm",scale = .95)

# 4.4) mean, sd / cost per km ----
rm(list=ls())
library(data.table)
library(magrittr)
library(ggplot2)
library(mapview)
library(sf)

dt <- fread("data_raw/feb6thtrips_cleaned_with_emissions.csv")

tmp_dt <- copy(dt)[serviceprovided %in% c("uberX","standard"),.SD
                   ,.SDcols = c("distance","fareamount")]
tmp_dt[,id := 1:.N]
tmp_dt[,cost_km := fareamount / distance]
tmp_dt <- tmp_dt[!is.na(cost_km) | !is.infinite(cost_km) | distance < 0.1
                 | cost_km < 50 | distance < 60,]
tmp_dt <- melt.data.table(data = tmp_dt
                          ,id.vars = c("id"))

var_names <- c(
  `distance` = "Distance (km)",
  `fareamount` = "Fare amount (CAD)",
  `cost_km` = "Cost per km (CAD/km)"
)

ggplot(tmp_dt[value < 85 ,]) + 
  geom_histogram(aes(x = value
                     ,fill = variable),bins = 40L
                 ,binwidth = .75)+
  facet_wrap(~variable,nrow = 3,scales = "free"
             ,labeller = as_labeller(var_names))+
  scale_fill_viridis_d()+
  labs(x = NULL,y = "Frequency")+
  theme_bw(base_family = "Times New Roman")+
  theme(legend.position = "none")

#dir.create("figures")
ggsave("figures/histogram_variables.jpg"
       ,width = 30/2,height = 30/2,units = "cm",scale = .95)

## analysis -----
tmp_dt <- copy(dt)[serviceprovided %in% c("uberX","standard"),.SD,
                   .SDcols = c("distance","fareamount","avg_speed_p3")]
tmp_dt <- tmp_dt[fareamount > 0 & distance > 0 & avg_speed_p3 > 0,]
tmp_dt[,id := 1:.N]
tmp_dt <- melt.data.table(data = tmp_dt,id.vars = c("id"))
tmp_dt <- tmp_dt[,lapply(.SD,function(i){
  tmp_vec <- c(min(i,na.rm = TRUE),
               quantile(i,0.25,na.rm = TRUE),
               median(i,na.rm = TRUE), 
               mean(i,na.rm = TRUE),  
               quantile(i,0.75,na.rm = TRUE), 
               max(i,na.rm = TRUE))
  tmp_vec <- round(tmp_vec,2)
  return(tmp_vec)
}),by = variable]

var_names <- c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
tmp_dt[,names := rep(var_names,3)]
tmp_dt[,id := NULL]

tmp_dt <- dcast(tmp_dt,formula = variable ~ names)
data.table::setcolorder(tmp_dt,neworder = c("variable",var_names))
tmp_dt
# 5) Analysis ----------------
# https://www.datanovia.com/en/lessons/anova-in-r/
# https://bookdown.org/steve_midway/DAR/understanding-anova-in-r.html
rm(list=ls())
library(data.table)
library(magrittr)
library(ggplot2)
library(mapview)
library(sf)
library(tidyverse)

dt <- fread("data_raw/feb6thtrips_cleaned_with_emissions.csv")

tmp_dt <- dt[serviceprovided %in% c("uberX","standard"),]
tmp_dt[,distance_round := round(distance,0)]
#tmp_dt[,period := fifelse(hour %in% c(8,9,17,18),"peak","off-peak")]
tmp_dt[,lapply(.SD,mean),by = .(serviceprovided,distance_round),.SDcols = "distance"]
tmp_dt[,cost_per_km := fareamount / distance]
tmp_dt <- tmp_dt[!(is.infinite(cost_per_km)) & !is.na(cost_per_km)]
tmp_dt <- tmp_dt[distance > 0.2,]
tmp_dt[,distance_type := fcase(distance > 30,"long",
                               distance <= 30 & distance > 10,"medium",
                               distance > 1 & distance <= 10,"short",
                               distance > 0.2 & distance <= 1,"very-short")]
#tmp_dt <- tmp_dt[,.SD,.SDcols = c("serviceprovided"
#                              ,"distance_type","cost_per_km","distance","period")]

head(tmp_dt,3)
var_names <- c(
  `long` = "Long (> 30 km)",
  `medium` = "Medium (10 < d < 30 km)",
  `short` = "Short (1 < d < 10 km)",
  `very-short` = "Very-short (0.2 < d < 1 km)"
)

tmp_dt[1:2,]
ggplot(tmp_dt) +
  geom_boxplot(aes(y = cost_per_km
                   , fill = serviceprovided)) +
  scale_fill_viridis_d(option = "C")+
  labs(x = "Periods", y= "Cost per kilometer (CAD/km)"
       ,fill = "Service \nProvided")+
  facet_wrap(~distance_type,scales = "free_y"
             ,labeller = as_labeller(var_names))+
  theme_bw(base_family = "Times New Roman")

ggsave("figures/prices_uber_lyft.jpg"
       ,width = 30/2,height = 30/2,units = "cm",scale = 1)

# 4.5) cost per km ----
rm(list=ls())
library(data.table)
library(magrittr)
library(ggplot2)
library(mapview)
library(sf)

dt <- fread("data_raw/feb6thtrips_cleaned_with_emissions.csv")

tmp_dt <- copy(dt)[serviceprovided %in% c("uberX","standard"),.SD
                   ,.SDcols = c("distance","fareamount")]
tmp_dt[,id := 1:.N]
tmp_dt[,cost_km := fareamount / distance]
tmp_dt <- tmp_dt[!is.na(cost_km) | !is.infinite(cost_km) | distance < 0.1
                 | cost_km < 50 | distance < 60,]
tmp_dt <- melt.data.table(data = tmp_dt
                          ,id.vars = c("id"))

var_names <- c(
  `distance` = "Distance (km)",
  `fareamount` = "Fare amount (CAD)",
  `cost_km` = "Cost per km (CAD/km)"
)

ggplot(tmp_dt[variable == "cost_km",]) + 
  geom_histogram(aes(x = value
                     ,fill = variable),bins = 40L
                 ,binwidth = .1250)+
  xlim(c(2,15))+
  facet_wrap(~variable,nrow = 3,scales = "free"
             ,labeller = as_labeller(var_names))+
  scale_fill_viridis_d()+
  labs(x = "Cost per km (CAD/km)",y = "Frequency")+
  theme_bw(base_family = "Times New Roman")+
  theme(legend.position = "none")

#dir.create("figures")
ggsave("figures/histogram_variables1.jpg"
       ,width = 30/2,height = 30/2,units = "cm",scale = .95)

# 4.6) distance vs. cost per km -------------
rm(list=ls())
library(data.table)
library(magrittr)
library(ggplot2)
library(mapview)
library(sf)

dt <- fread("data_raw/feb6thtrips_cleaned_with_emissions.csv")

tmp_dt <- copy(dt)[serviceprovided %in% c("uberX","standard"),.SD
                   ,.SDcols = c("distance","fareamount","serviceprovided")]
tmp_dt[,cost_per_km := fareamount / distance]

ggplot(tmp_dt[distance < 50 & fareamount < 100,])+
  geom_point(aes(x = distance,y = fareamount,color = cost_per_km)
             ,alpha = .4)+
  scale_color_viridis_c()+
  #xlim(c(0,50))+
  #ylim(c(0,100))+
  facet_wrap(~serviceprovided,scales = "fixed")+
  labs(x = "Distance (km)",y = "Fare amount (CAD)")+
  theme_bw(base_family = "Times New Roman")+
  theme(legend.position = "none")

# save
ggsave("figures/price_distance.jpg"
       ,width = 30/2,height = 17.5/2,units = "cm",scale = .95)

# 4.7) Hypothesis testing ------

rm(list=ls())
library(data.table)
library(magrittr)
library(ggplot2)
library(mapview)
library(sf)

dt <- fread("data_raw/feb6thtrips_cleaned_with_emissions.csv")
tmp_dt <- copy(dt)[serviceprovided %in% c("uberX","standard"),]
tmp_dt[,distance_0 := round(distance,0)]

names(tmp_dt)
stats <- data.table("distance" = 1:50,
                    "mean_uberX" = NA,
                    "mean_lyft" = NA,
                    "p_value" = NA,
                    "uber_normal" = NA,
                    "lyft_normal" = NA)

for(i in 1:50){ # i = 2
  #message(i)
  p_uber <- tmp_dt[serviceprovided == "uberX" & distance_0 == i
                   ,.SD,.SDcols = "fareamount"]
  p_lyft <- tmp_dt[serviceprovided == "standard" & distance_0 == i
                   ,.SD,.SDcols = "fareamount"]
  p_uber <- p_uber[[1]]
  p_lyft <- p_lyft[[1]]
  # normality
  if(length(p_uber) > 5000) p_uber <- p_uber[1:5000]
  if(length(p_lyft) > 5000) p_lyft <- p_lyft[1:5000]
  norm_uber <- shapiro.test(p_uber)
  norm_uber <- round(norm_uber$p.value,3)
  stats$uber_normal[i] <- norm_uber # (norm_uber > 0.05)
  norm_lyft <- shapiro.test(p_lyft)
  norm_lyft <- round(norm_lyft$p.value,3)
  stats$lyft_normal[i] <- norm_lyft# (norm_lyft > 0.05)
  # welch-t test
  test1 <- t.test(x = p_uber,y =   p_lyft)   
  test1 <- wilcox.test(x = p_uber,y =   p_lyft,alternative = "greater")   
  test1$method
  stats$mean_uberX[i] <- test1$estimate[[1]]
  stats$mean_lyft[i] <- test1$estimate[[2]]
  stats$p_value[i] <- test1$p.value
}
stats[,p_status := (p_value < 0.05)]
stats[mean_uberX > mean_lyft & p_status == TRUE,cheaper_price := "Lyft"]
stats[mean_uberX < mean_lyft & p_status == TRUE,cheaper_price := "UberX"]
stats[p_status != TRUE,cheaper_price := "Inconclusive"]
stats[,p := fcase(p_value < 0.001,"< 0.001",
                  p_value < 0.005,"< 0.005",
                  p_value < 0.05,"< 0.05")]
stats[is.na(p),p := round(p_value,2)]
stats[,mean_lyft := round(mean_lyft,2)]
stats[,mean_uberX := round(mean_uberX,2)]
stats[,p_value := NULL]
stats
View(stats)
# distance type
tmp_dt[distance_0 <= 16,type_distance := "Distance [0-16]"]
tmp_dt[distance_0 > 16 & distance_0 <= 33,type_distance := "Distance [17-33]"]
tmp_dt[distance_0 > 33 ,type_distance := "Distance [34-50]"]

# factor lyft-uber
tmp_dt[,serviceprovided := factor(serviceprovided
                                  ,levels = c("standard","uberX")
                                  ,labels = c("Lyft (Standard)",
                                              "Uber (uberX)"))]

tmp_dt %>%
  .[distance < 50 & fareamount < 100,] %>%
  .[!(type_distance == "Distance [0-16]" & fareamount > 60),] %>%
  ggplot()+
  geom_boxplot(aes(x = factor(distance_0)
                   ,y = (fareamount)
                   ,color = serviceprovided)
               ,alpha = .4)+
  scale_color_manual(values = c("red","purple"))+
  facet_wrap(~type_distance,scales = "free",ncol = 1)+
  labs(x = "Distance (km)",y = "Fare amount (CAD)"
       ,color = "Service Provided")+
  theme_bw(base_family = "Times New Roman")+
  theme(legend.position = "bottom")

ggsave("figures/fare_amount_distance.jpg"
       ,width = 30/2,height = 30/2,units = "cm",scale = .95)

# 4.8) Surge analysis -----
rm(list=ls())
library(data.table)
library(ggplot2)
library(mapview)
library(sf)
library(factoextra)
library(opendatatoronto)

dt <- fread("data_raw/feb6thtrips_cleaned_with_emissions.csv")
tmp_dt <- copy(dt)[serviceprovided %in% c("uberX","standard"),]
tmp_dt <- tmp_dt[,.SD
                 ,.SDcols = c("pickup_neighbourhood"
                              ,"dropoff_neighbourhood",
                              "hour","distance", "fareamount",
                              "serviceprovided"
                 )]
tmp_dt <- tmp_dt[!is.na(pickup_neighbourhood) & 
                   !is.na(dropoff_neighbourhood) & 
                   !is.na(serviceprovided) & 
                   !is.na(distance) & 
                   !is.na(fareamount) & 
                   !is.na(hour),]
tmp_dt[,distance := scale(distance)]
tmp_dt[,fareamount := scale(fareamount)]
tmp_dt[,serviceprovided := fifelse(serviceprovided == "standard",2,1)]


lmdt <- stats::glm(data = tmp_dt
                   ,formula =fareamount ~ pickup_neighbourhood + dropoff_neighbourhood+ hour +  distance
                   +serviceprovided
)
lmdt
summary(lmdt)
cluster4 <- stats::kmeans(x = tmp_dt
                          ,centers = 4
                          ,iter.max = 5)

unique(tmp_dt$pickup_neighbourhood) |> sort()

resources <- list_package_resources("neighbourhoods")
data <- opendatatoronto::get_resource(resource = resources[11,])
data$ID <- sort(unique(data$AREA_SH5))
data <- data[,"ID"]
data$ID <- as.numeric(data$ID)
data
data[data$X_id1==70,]
datastore_resources <- filter(resources, 
                              tolower(format) %in% c('csv', 'geojson'))
cluster4$cluster
fviz_nbclust(tmp_dt, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

ggplot(tmp_dt)+
  geom_point(aes(x = distance,y = fareamount))+
  viridis::scale_color_viridis(discrete = T)+
  facet_wrap(~serviceprovided)



standard_lm <- lm(data = tmp_dt[serviceprovided == "standard",]
                  ,formula = fareamount ~ distance)
uberx_lm <- lm(data = tmp_dt[serviceprovided == "uberX",]
               ,formula = fareamount ~ distance)

summary(standard_lm)
summary(uberx_lm)

uberx_lm$residuals |> mean()
standard_lm$residuals |> mean()

uberx_lm$coefficients
standard_lm$coefficients

uberx_lm$fitted.values

# Perform Breusch-Pagan test to check for homoscedasticity
library(car)
bp_test <- ncvTest(uberx_lm)
print(bp_test)

# 4.9) Proportion of trips -----
rm(list=ls())
library(data.table)
library(ggplot2)
library(mapview)
library(sf)
library(patchwork)

dt <- fread("data_raw/feb6thtrips_cleaned_with_emissions.csv")
tmp_dt <- copy(dt)[serviceprovided %in% c("uberX","standard"),]
tmp_dt <- tmp_dt[,.SD
                 ,.SDcols = c("pickup_neighbourhood"
                              ,"dropoff_neighbourhood",
                              "hour","distance", "fareamount",
                              "serviceprovided"
                 )]
tmp_dt <- tmp_dt[!is.na(pickup_neighbourhood) & 
                   !is.na(dropoff_neighbourhood) & 
                   !is.na(serviceprovided) & 
                   !is.na(distance) & 
                   !is.na(fareamount) & 
                   !is.na(hour),]
#tmp_dt[,distance_0 := round(distance,0)]
vec <- seq(0,40,2.5)
vec_names <- c()
tmp_dt[,dist_class := NA_character_]
for(i in seq_along(vec)[-length(vec)]){
  vec_names[i] <- paste0(vec[i]," - ",vec[i+1])
  tmp_dt[distance > vec[i] & distance <= vec[i+1]
         ,dist_class := vec_names[i] ]
}
tmp_dt <- tmp_dt[,.N,by = .(dist_class,serviceprovided)]
tmp_dt <- tmp_dt[!is.na(dist_class),]
# tmp_dt <- dcast(tmp_dt,formula = dist_class ~ serviceprovided
#       ,value.var = "N")
# tmp_dt[,prop_uberX := round(100 * uberX / (uberX+standard),2)]
# tmp_dt[,prop_standard := round(100 * standard / (uberX+standard),2)]

tmp_dt[,dist_class := factor(dist_class
                             ,levels = vec_names)]
tmp_dt[,prop := round(100 * N/sum(N),2),by = .(dist_class)]
tmp_dt[,N := as.numeric(N)]

tmp_dt1 <- melt(data = tmp_dt
                ,id.vars = c("serviceprovided","dist_class")
                ,measure.vars = c("N","prop"))
tmp_dt1[,variable_f := factor(variable
                              ,levels = c("N","prop")
                              ,labels = c("Absolute numbers (N)"
                                          ,"Proportion (%)"))]

ggplot(tmp_dt1)+
  geom_bar(aes(y = value
               ,x = factor(dist_class),fill = serviceprovided)
           ,stat = "identity")+
  scale_fill_manual(values = c("red","purple"))+
  facet_wrap(~variable_f,ncol = 1,scales = "free")+
  labs(x = "Distance (km)",fill = "Service Provided"
       ,y = NULL)+
  theme_bw(base_family = "Times New Roman")+
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 0.950, hjust=1),
        legend.position = "bottom")

ggsave("figures/fare_amount_proportion.jpg"
       ,width = 30/2,height = 30/2,units = "cm",scale = .95)
p1/p2

# 4.9) Type of distribution -----
rm(list=ls())
library(data.table)
library(ggplot2)
library(mapview)
library(sf)
library(patchwork)

dt <- fread("data_raw/feb6thtrips_cleaned_with_emissions.csv")
tmp_dt <- copy(dt)[serviceprovided %in% c("uberX","standard"),]


# lyft
lm_lyft <- lm(data = tmp_dt[serviceprovided == "standard",]
              ,formula = fareamount ~ distance)
summary(lm_lyft)
xx <- seq(1,45,by = 0.1)
yy <- lm_lyft$coefficients[[2]] * xx + lm_lyft$coefficients[[1]]
dt_lyft <- data.frame("x" = xx,"y" = yy,"service" = "Lyft")

# uber
lm_uber <- lm(data = tmp_dt[serviceprovided == "uberX",]
              ,formula = fareamount ~ distance)
xx <- seq(1,45,by = 0.1)
yy <- lm_lyft$coefficients[[2]] * xx + lm_lyft$coefficients[[1]]
dt_uber <- data.frame("x" = xx,"y" = yy,"service" = "uberX")

plot(dt_lyft$x,dt_lyft$y,type = "l",xlim = c(10,20),ylim = c(20,30))
lines(dt_uber$x,dt_uber$y,col = "red")
# dataframe
dt <- rbind(dt_lyft,dt_uber)

lm_lyft
tmp_dt[,id := 1:.N]
tmp_dt[,cost_km := fareamount / distance]
tmp_dt <- tmp_dt[!is.na(cost_km) | !is.infinite(cost_km) | distance < 0.1
                 | cost_km < 50 | distance < 60,]
tmp_dt[,distance_0  := round(distance,0)]
tmp_dt[,fareamount_0  := round(fareamount,0)]
tmp_dt[,speed_0  := round(avg_speed_p3,0)]
tmp_dt <- tmp_dt[,.N,by = .(distance_0)]
tmp_dt <- tmp_dt[order(N),]
tmp_dt

ggplot(tmp_dt)+geom_point(aes(x = distance_0,y= N))
llm <- stats::glm(data = tmp_dt
                  ,formula = N ~ exp(-distance_0)) 
llm$coefficients
# 5.0) trips by OD -----
rm(list=ls())
library(data.table)
library(ggplot2)
library(mapview)
library(sf)
library(patchwork)

dt <- fread("data_raw/feb6thtrips_cleaned_with_emissions.csv")
tmp_dt <- copy(dt)[serviceprovided %in% c("uberX","standard"),]
tmp_dt <- tmp_dt[,{list("N"= .N)}
                 ,by = .(pickup_neighbourhood,dropoff_neighbourhood
                         ,serviceprovided)]
tmp_dt <- tmp_dt[!is.na(pickup_neighbourhood) & 
                   !is.na(dropoff_neighbourhood),]
tmp_dt <- dcast(data = tmp_dt
                ,formula = pickup_neighbourhood + dropoff_neighbourhood
                ~ serviceprovided ,value.var = "N")
tmp_dt <- tmp_dt[!is.na(standard) & 
                   !is.na(uberX),]
data.table::setkeyv(tmp_dt
                    ,cols = c("standard","uberX"))
tmp_dt <- tmp_dt[standard > 5 & uberX > 5,]
tmp_dt[,pair_OD := paste0(pickup_neighbourhood,"-",dropoff_neighbourhood)]

tmp_dt1 <- copy(dt)[serviceprovided %in% c("uberX","standard"),]
tmp_dt1[,pair_OD := paste0(pickup_neighbourhood,"-",dropoff_neighbourhood)]
tmp_dt1 <- tmp_dt1[pair_OD %in% unique(tmp_dt$pair_OD),]
tmp_dt1

names(tmp_dt)
pair_OD <- unique(tmp_dt1$pair_OD)
stats <- data.table("pair_OD" = pair_OD,
                    "mean_uberX" = NA,
                    "mean_lyft" = NA,
                    "p_value" = NA)
stats[,origin := stringr::str_split(pair_OD,"-")[[1]][1],by = .(pair_OD)]
stats[,destination := stringr::str_split(pair_OD,"-")[[1]][2],by = .(pair_OD)]
for(i in seq_along(pair_OD)){ # i = 1
  test1 <- t.test(x = tmp_dt1[serviceprovided == "uberX" & pair_OD == pair_OD[i]
                             ,.SD,.SDcols = "fareamount"],
                  y = tmp_dt1[serviceprovided == "standard" & pair_OD == pair_OD[i]
                             ,.SD,.SDcols = "fareamount"]
  )
  test1$method
  stats$mean_uberX[i] <- test1$estimate[[1]]
  stats$mean_lyft[i] <- test1$estimate[[2]]
  stats$p_value[i] <- test1$p.value
}
stats[,p_status := (p_value < 0.05)]
stats[mean_uberX > mean_lyft & p_status == TRUE,cheaper_price := "Lyft"]
stats[mean_uberX < mean_lyft & p_status == TRUE,cheaper_price := "UberX"]
stats[p_status != TRUE,cheaper_price := "Inconclusive"]
stats[,p := fcase(p_value < 0.001,"< 0.001",
                  p_value < 0.005,"< 0.005",
                  p_value < 0.05,"< 0.05")]
stats[is.na(p),p := round(p_value,2)]
stats[,mean_lyft := round(mean_lyft,2)]
stats[,mean_uberX := round(mean_uberX,2)]
stats[,p_value := NULL]
stats

library(wesanderson)
ggplot(stats)+
  geom_tile(aes(x = origin,y = destination
                ,fill = factor(cheaper_price)))+
  scale_fill_manual(values = c(
    wes_palettes$Zissou1[2],
    wes_palettes$GrandBudapest1[3]
  ))+
  labs(x = "Origin",y = "Destination"
       ,fill = "Cheaper Service"
       ,caption = "Each squares represents an O-D pair;\nVoid squares represents non-observed trips in the dataset.")+
  theme_bw()+
  theme(axis.text = element_blank()
        ,legend.position = "bottom",
        axis.ticks.length = unit(0, "pt"), axis.ticks.length.x = NULL, 
        axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL, 
        axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL, 
        axis.ticks.length.y.right = NULL)

ggsave("figures/service_cheaper_OD.png"
       ,width = 30/2,height = 31/2,units = "cm"
       ,scale = .95)


# end -----


