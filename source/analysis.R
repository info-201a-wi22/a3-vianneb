# load packages
library("tidyverse")
library("plotly")
install.packages("rjson")
library("rjson")

# load data
county_level_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", colClasses = c(fips='character'))

#jurisdiction_level_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")

# line chart

# filter data to select prison admissions counts for each race in Washington
line_data <- county_level_data %>%
  select(year, state, county_name, aapi_prison_adm, black_prison_adm,
         latinx_prison_adm, native_prison_adm, white_prison_adm) %>%
  filter(state=="WA") %>%
  group_by(year) %>%
  summarize(wa_aapi_prison_adm=sum(aapi_prison_adm, na.rm=TRUE), 
            wa_black_prison_adm=sum(black_prison_adm, na.rm=TRUE),
            wa_latinx_prison_adm=sum(latinx_prison_adm, na.rm=TRUE),
            wa_native_prison_adm=sum(native_prison_adm, na.rm=TRUE), 
            wa_white_prison_adm=sum(white_prison_adm, na.rm=TRUE))

# plot line chart
line_chart <- ggplot(data=line_data) +
  geom_line(mapping = aes(x=year, y=wa_aapi_prison_adm, color="AAPI")) +
  geom_line(mapping = aes(x=year, y=wa_black_prison_adm, color="Black")) +
  geom_line(mapping = aes(x=year, y=wa_latinx_prison_adm, color="Latinx")) +
  geom_line(mapping = aes(x=year, y=wa_native_prison_adm, color="Native")) +
  geom_line(mapping = aes(x=year, y=wa_white_prison_adm, color="White")) +
  labs(x="Year",y="Prison Admissions Count",color="Race",
    title = "Prison Admissions Over Time by Race in Washington") + 
  scale_color_manual(name = "Race",
    values = c("AAPI" = "#D43F3A", "Black" = "#EEA236", "Latinx" = "#5CB85C", 
               "Native" = "#46B8DA", "White" = "#9632B8"))

# scatter plot

# filter data to select male_juvenile_jail_pop and male_prison_adm

scatter_data <- county_level_data %>%
  select(year, state, county_name, male_juvenile_jail_pop, male_prison_adm)

# drop NA values
scatter_data <- drop_na(scatter_data)

# convert male_juvenile_jail_pop column from unit=0.01 to unit=1 to match
# male_prison_adm
scatter_data$male_juvenile_jail_pop <- scatter_data$male_juvenile_jail_pop * 100

# plot scatter plot 
scatter_plot <- ggplot(data=scatter_data) +
  geom_point(mapping = aes(y=male_prison_adm, x=male_juvenile_jail_pop)) +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(labels=scales::comma) +
  labs(x="Male Juvenile Jail Population Count",
       y="Male Prison Admissions Count",
       title="Male Juvenile Count versus Male Prison Admissions Count")

# map

# filter data to find proportion of black jailed people in each county in 2018
map_data <- county_level_data %>%
  filter(year==2018) %>%
  mutate(black_jail_pop_prop = black_jail_pop/total_jail_pop,
         na.rm=TRUE) %>%
  select(fips, state, county_name, black_jail_pop, total_jail_pop, black_jail_pop_prop)

# add 0 in front of certain state fips so it works with the geospatial data
map_data <- transform(map_data, fips = ifelse(state %in% c("CA", "AZ", "CO", "AR", "AL", "AK", "HI", "CT", "RI", "VT"),
                                              paste0("0", fips), fips))

# plot map 
url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_ly()

fig <- fig %>% add_trace(
  type="choropleth",
  geojson=counties,
  locations=map_data$fips,
  z=map_data$black_jail_pop_prop,
  colorscale= "Viridis",
  zmin=0,
  zmax=1,
  marker=list(line=list(
    width=0))
)

fig <- fig %>% colorbar(title = "Proportion of black inmates")
fig <- fig %>% layout(
  title = "2018 Proportion of Black Jail Inmates by US County"
)

fig <- fig %>% layout(
  geo=g
)

fig
