library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

#Load data. Bunch of Excel sheets state provided. 
#Note for future: 

df_1 <- read_excel("data/Active List 1.xls")

df_2 <- read_excel("data/Active List 2.xls")

df_3 <- read_excel("data/Active List 3.xls")

df_4 <- read_excel("data/Archive List 1.xls")

df_5 <- read_excel("data/Archive List 2.xls")

df_6 <- read_excel("data/Archive List 3.xls")

# Spreadsheet columns are inconsistent -- number of variables ranges from 39-47. Make consistent and drop columns we don't need.
# What we eventually need is just permit year and DCR location. 
# Newer spreadsheets include a second DCR location with more precise detail. But it's missing from later sheets, so we'll drop.
# But for fun, let's include who pulled the permit, event / project name and details about crew. 

df_1a <- df_1 %>% 
  select(Permit_year = `Permit Year`, 
         Issued_to = `ISSUED TO`,
         DCR_location = Combo253,
         Project = `EVENT NAME`, 
         Project_detail = SpecialEventInformation)

df_2a <- df_2 %>% 
  select(Permit_year = `Permit_Year`, 
         Issued_to = `ISSUED TO`,
         DCR_location = Combo253,
         Project = `EVENT NAME`, 
         Project_detail = SpecialEventInformation)

df_3a <- df_3 %>% 
  select(Permit_year = `Permit_Year`, 
         Issued_to = `ISSUED TO`,
         DCR_location = Combo253,
         Project = `EVENT NAME`, 
         Project_detail = SpecialEventInformation)

# Older spreadsheets don't have a Permit Year column. We'll include Permit Number and extract year from that later.
# Also, older spreadsheets don't have added detail column for DCR locations. 

df_4a <- df_4 %>% 
  select(Permit_year = `PERMIT NUMBER`, 
         Issued_to = `ISSUED TO`,
         DCR_location = Location,
         Project = `EVENT NAME`, 
         Project_detail = SpecialEventInformation)

df_5a <- df_5 %>% 
  select(Permit_year = `PERMIT NUMBER`, 
         Issued_to = `ISSUED TO`,
         DCR_location = Location,
         Project = `EVENT NAME`, 
         Project_detail = SpecialEventInformation)

df_6a <- df_6 %>% 
  select(Permit_year = `PERMIT NUMBER`, 
         Issued_to = `ISSUED TO`,
         DCR_location = Location,
         Project = `EVENT NAME`, 
         Project_detail = SpecialEventInformation)

# Let's deal with that permit year issue for df_4a-6a. Permit year format is 2007-0281; so, delete hyphen to end of string.

df_4a$Permit_year <- as.numeric(gsub("-.*$", "", df_4a$Permit_year))

df_5a$Permit_year <- as.numeric(gsub("-.*$", "", df_5a$Permit_year))

df_6a$Permit_year <- as.numeric(gsub("-.*$", "", df_6a$Permit_year))

# Now we have data frames 1a-6a, with common column names. Time to join them. 

dcr_permits <- bind_rows(df_1a, df_2a, df_3a, df_4a, df_5a, df_6a)

# Make sure there aren't duplicate rows. First iteration of dcr_permits has 1412 obs. Running distinct returns 835.

dcr_permits <- distinct(dcr_permits)

# Make a data table 

library(DT)

datatable(dcr_permits)

# Group by location to get totals. Although, ehh, looks like this messes up the bar chart. 

location_totals <- dcr_permits %>% 
  group_by(DCR_location) %>% 
  summarize(Location_total=n()) %>% 
  arrange(desc(Location_total))

# List locations with 10 or more permits

top_locations <- location_totals %>% 
  filter(Location_total>=10)

# Chart locations with 10 or more permits

library(ggplot2)
library(forcats)
library(scales)
library(extrafont)

ggplot(top_locations, aes(x=fct_reorder(DCR_location, Location_total), y=Location_total)) + 
  geom_bar(stat="identity", fill="#31a354", width = .6) +
  geom_text(aes(label=Location_total), hjust=-.5, family="BentonSans Light", size=3) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +
  labs(title="Film permits at Massachusetts DCR properties", 
       subtitle="Locations with 10 or more permits between 2007 and 2017", 
       y="", x="",
       caption="Source: Massachusetts Department of Conservation and Recreation") +
  theme(axis.line.y = element_line(color="grey80"), 
        axis.line.x = element_line(color="grey80"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(family="BentonSans Regular"),
        axis.text.x = element_text(family="BentonSans Light"), 
        plot.title = element_text(size=18, family="BentonSans Bold"),
        plot.subtitle = element_text(size=12, family="BentonSans Book"),
        plot.caption = element_text(size=8, family="BentonSans Light", hjust=1))

ggsave("top_locations.png", width = 30, height = 30, units = "cm")

# borrowed from nicholas field: aes(x=fct_reorder(`Branch Name`, percent_MLC), y = percent_MLC, fill=Tier)). Thanks! 

# Create Massachusetts map showing DCR locations, w/ dot size as number of permits. Load libraries / options, MA map.

library(tigris)
library(sf)
library(ggmap)
library(leaflet)

options(tigris_class="sf")

options(device = "X11") 
X11.options(type = "cairo")

ma <- states(cb=T) %>% 
  filter(NAME=="Massachusetts")

# Let's see if DCR_locations will geocode. 

location_geo <- location_totals %>% 
  mutate(address = paste0(DCR_location, ", ", "MA"))

geo <- mutate_geocode(location_geo, address)

# Holy crap that actually worked. Note: Could have tightened up number of data frames by mutating location_geo over itself.

# Map locations, check map. 

ggplot(ma) + 
  geom_sf() +
  geom_point(data=geo, aes(x=lon, y=lat, size=Location_total, fill="#31a354")) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'))

# Two locations didn't geocode correctly and are falling outside MA borders. 
# The southwest border of MA is at lon -73.496867. Find which locations geocoded at longitude west of -73.496867. 

mapcheck <- geo %>% 
  filter(lon < -73.496867)

# There was one DCR_location listed as "NA" that was way off. Also, a DCR_location listed as Riverdale Park was geocoded to Maryland.
# Update location_geo to exclude the NA value. For Riverdale park, add: Dedham, MA 02026 to Riverdale Park.  

location_geo <- location_geo %>% 
  filter(DCR_location != "NA")

location_geo$address[location_geo$address %in% "Riverdale Park, MA"] <- "Riverdale Park, Dedham, MA 02026"

# Geocode again. Pour another glass of bourbon while that runs. 

geo <- mutate_geocode(location_geo, address)

# Geocode failed once on Nonantum Road Branch. Worked on subsequent try, but looks like lat / lon is wrong. Add lat / lon manually. 
# Unclear exactly where filming took place, but permit lists Community Rowing: 42.358630, -71.165553  x[1, 4] = 5

geo[160, 4] = -71.165553

geo[160, 5] = 42.358630

# General note: Geocoding w/ raw DCR_location names worked OK -- but for a truly final version, will need to double-check locations. For example, Quabbin Watershed Land location appears to be geocoded incorrectly. 

# Latest geocode also returned an OVER_QUERY_LIMIT warning that gave NA lat / lon for Mary O'Malley Waterfront Park, Chelsea, MA.
# Fix that manually: 42.388772, -71.052152 

geo[54, 4] = -71.052152

geo[54, 5] = 42.388772

# Export geo dataframe for safekeeping. 

library(readr)

write_csv(geo, "geo_dcr_permits.csv")

# Ran str(geo) in the console to make sure lat / lon were OK. Noticed that Location_totals are integers. 

geo$Location_total <- as.numeric(geo$Location_total)

# Check map again. 

ggplot(ma) + 
  geom_sf(color="white") +
  geom_point(data = geo, aes(x=lon, y=lat, size=Location_total), color="#31a354") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'))

# Map looks OK. Create popup for interactive version. 

popup_dcr <- paste0("<b>", geo$DCR_location, 
                   "</b><br />Number of permits: ", "<b>",geo$Location_total,"</b>")

# Create final version using Leaflet.

film_map <- leaflet(geo) %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
  setView(-71.931180, 42.385453, zoom = 8) %>% 
  addCircleMarkers(~lon, ~lat, popup = ~popup_dcr, weight = 3, radius = ~Location_total/2, 
             color="#c51b7d", stroke = FALSE, fillOpacity = 0.7)

film_map
