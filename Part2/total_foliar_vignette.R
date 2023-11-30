library(trex)
library(sf)
library(ggplot2)
library(viridis)

# get polygons
chihuahuan_desert <- read_sf("Part2/MapLayers/ChihuahuanDesert.shp")
nm <- read_sf("Part2/MapLayers/NewMexico_Albers.shp")
sandy_esg <- read_sf("Part2/MapLayers/SandyESG.shp")

# extract data from LDC
data <- fetch_ldc_spatial(chihuahuan_desert,
                          data_type = "indicators")

header <-fetch_ldc(data_type = "header")

# get benchmark data
benchmark <- read.csv("Part2/benchmark_points.csv")


# note if data have been used in benchmarking
data <- data |> subset(!is.na(Longitude_NAD83)) |>
  dplyr::mutate(benchmark =
                  dplyr::case_when(PrimaryKey %in% benchmark$PrimaryKey~ "benchmark",
                                   !PrimaryKey %in% benchmark$PrimaryKey~ "new")
                ) |>
  dplyr::mutate(Longitude_NAD83 = as.numeric(Longitude_NAD83),
                Latitude_NAD83 = as.numeric(Latitude_NAD83))
write.csv(data, "ldc_data.csv")
# convert to spatial
data_sf <- sf::st_as_sf(data %>% subset(!is.na(Longitude_NAD83)),
                     coords = c("Longitude_NAD83", "Latitude_NAD83"),
                     crs = st_crs(chihuahuan_desert))
sf::st_write(data_sf, "ldc_data.shp", append=F)

# Subset to NM
data_nm <- data |> subset(State %in% "NM")
data_nm_sf <- sf::st_as_sf(data_nm,
                        coords = c("Longitude_NAD83", "Latitude_NAD83"),
                        crs = st_crs(chihuahuan_desert))
sf::st_write(data_nm_sf, "ldc_data_nm.shp", append=F)

data_nm <- data_nm |>
  dplyr::mutate(DateVisited = lubridate::as_date(DateVisited),
                GapCover_101 = GapCover_101_200 + GapCover_200_plus,
                Year = lubridate::year(DateVisited) |> as.factor(),
                # benchmark points
                evaluated = dplyr::case_when(GapCover_101 > 50 ~ "Not meeting",
                                             GapCover_101 <=50 ~ "Meeting")

  ) |>
  subset(GapCover_101 <=100)
  # benchmark points


ggplot(data_nm |> subset(benchmark== "new" & !Year %in% 2023), aes(x = Year,
                                                 y = GapCover_101,
                                                 group = Year)) +
  geom_boxplot()+
  geom_point(aes(color = evaluated))+
  viridis::scale_color_viridis(discrete = T)

#TODO Nelson, could you help me make this intersection work? I think I moved to R because it wasn't going well in R
# work with intersected data
nm_data <- sf::st_read("~/Papers/DataCommons/AEL_submission/R2/Vignette/ldc_data_nm_intersect.shp")

nm_data <- nm_data |> dplyr::mutate(NWERN_JER = dplyr::case_when(PrjctKy %in% "NWERN_JER"~ "yes"),
                                    Chihuahuan_desert = "yes",
                                    DateVisited = lubridate::as_date(DatVstd),
                                    Year = lubridate::year(DateVisited) |> as.factor(),
                                    # benchmark points
                                    evaluated = dplyr::case_when(TtlFlrC >30 ~ "Meeting",
                                                                 TtlFlrC <=30 ~ "Not meeting"
                                                                 )
)

nm_data_long <- as.data.frame(nm_data) |>
  tidyr::pivot_longer(cols = c("MLRA42","SandyESG", "NWERN_JER", "Chihuahuan_desert"),
                      names_to = "region",
                      values_to = "region_membership") |>
  subset(region_membership =="yes")|>
  dplyr::mutate(region = factor(region)|>
                  dplyr::recode( "NWERN_JER" = "Site",
                                 "SandyESG" = "ESG",
                                 "MLRA42" = "MLRA",
                                 "Chihuahuan_desert" = "Ecoregion"))

nm_data_long$region <- factor(nm_data_long$region,
                              levels = c( "Site", "ESG",
                                          "MLRA","Ecoregion")
                              )




# box plot
ggplot(nm_data_long |> subset(bnchmrk== "new" &
                                Year %in% c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022") &
                                TtlFlrC),
       aes(x = Year,
                                                                   y = TtlFlrC,
                                                                   group = Year)) +
  facet_grid(rows = "region")+
  geom_boxplot()+
  geom_point(aes(color = evaluated))+
  scale_color_manual(values = c("darkblue", "orange"))+
  theme_bw()+
  ylab("Total foliar cover (%)")+
  guides(color=guide_legend(title="Benchmark status"))

# bar chart
ggplot(nm_data_long |> subset(bnchmrk== "new" &
                                Year %in% c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022") &
                                TtlFlrC),
       aes(x = Year,
           y = TtlFlrC,
           fill = evaluated)) +
  facet_grid(rows = "region")+
  geom_bar(position = "fill", stat = "identity")+
  scale_fill_manual(values = c("darkblue", "orange"))+
  theme_bw()+
  ylab("Proportion of plots")+
  guides(fill=guide_legend(title="Benchmark status"))

# summarize
nm_data_summary <- nm_data_long |> subset(bnchmrk== "new" &
                         Year %in% c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022") &
                         TtlFlrC) |>
  dplyr::select(PrmryKy, region, Year) |>
  dplyr::mutate(Year = as.character(Year)) |>
  dplyr::distinct()|>
  dplyr::group_by(Year, region) |> dplyr::tally()

## get climate data
# join county area to US Drought Monitor
county <- read.csv("County_area.csv")
USDM <- read.csv("USDM_20150101_20221231.csv")

# summarize
USDM_summary <- USDM |> dplyr::group_by(MapDate) |>
  dplyr::summarise(
    None = sum(None),
    D0 = sum(D0),
    D1 = sum(D1),
    D2 = sum(D2),
    D3 = sum(D3),
    D4 = sum(D4)
  )|> tidyr::pivot_longer(cols = c(None:D4),
                          names_to = "Drought_Category",
                          values_to = "SQMI") |>
  dplyr::mutate(Drought_percent = SQMI/58180,
                MapDate = as.Date(as.character(MapDate),format = "%Y%m%d"))

USDM_extreme <- USDM_summary |> subset(Drought_Category %in% c("D3", "D4"))|>
  dplyr::group_by(MapDate)

usdm_color <- c("#ffff00", "#fcd37f", "#ffaa00", "#e60000", "#730000") %>% rev()
usdm_labels <- c("Exceptional", "Extreme", "Severe", "Moderate", "Abnormally Dry")

drought_plot <-ggplot(USDM_summary |> subset(!Drought_Category=="None" &
                                               MapDate < "2022-12-31"),
                      aes(x = MapDate, y = Drought_percent, fill = Drought_Category)) +
  geom_col()+
  scale_fill_manual(values = usdm_color,
                    labels = usdm_labels,
                    name = "Drought category",
                    guide = guide_legend(reverse = TRUE))+
  #scale_x_date(date_breaks = "years", date_labels = "%Y")+
  ylab("Proportion in drought") +
  xlab ("Year")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme_bw()

drought_plot
ggsave(drought_plot, filename = "ecoregion_drought.jpg")
