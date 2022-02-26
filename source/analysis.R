# load packages
library("tidyverse")
library("plotly")
library("rjson")

# load data
county_level_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", colClasses = c(fips='character'))

# summary: variables examined are black_prison_adm, latin_prison_adm, and
# black_jail_pop proportion

summary_info <- list()

# 1. What is the highest value of black_prison_adm in Washington?
# 2. What is the highest value of latinx_prison_adm in Washington?

# filter by year and select relevant columns
table1 <- county_level_data %>%
  filter(state=="WA") %>%
  select(year, state, county_name, black_prison_adm, latinx_prison_adm)

# drop NA values
table1 <- drop_na(table1)

# group by year 
table1 <- table1 %>% 
  group_by(year) %>%
  summarize(wa_black_prison_adm=sum(black_prison_adm),
            wa_latinx_prison_adm=sum(latinx_prison_adm))
  
summary_info$highest_black_prison_adm_wa <- table1 %>% 
  filter(wa_black_prison_adm==max(wa_black_prison_adm)) %>% 
  pull(wa_black_prison_adm)

summary_info$highest_latinx_prison_adm_wa <- table1 %>% 
  filter(wa_latinx_prison_adm==max(wa_latinx_prison_adm)) %>% 
  pull(wa_latinx_prison_adm)

# 3. What is the average value of the proportion of black_jail_pop to
# total_jail_pop in all counties in 2018

# select relevant columns
table2 <- county_level_data %>%
  select(year, state, county_name, black_jail_pop, total_jail_pop)

# calculate proportion 
table2 <- table2 %>% 
  mutate(black_jail_pop_prop=black_jail_pop/total_jail_pop) %>%
  group_by(year) %>%
  summarize(avg_black_jail_pop_prop=mean(black_jail_pop_prop, na.rm=TRUE)) %>%
  filter(year==2018)

summary_info$avg_black_jail_pop_prop_2018 <- table2 %>%
  pull(avg_black_jail_pop_prop)

# 4. Which location was proportion of black_jail_pop to total_jail_pop
# highest in 2018?

# select relevant columns
table3 <- county_level_data %>%
  select(year, state, county_name, black_jail_pop, total_jail_pop)

# calculation proportion
table3 <- table3 %>% 
  mutate(black_jail_pop_prop=black_jail_pop/total_jail_pop) %>%
  mutate(location=paste0(county_name, ", ", state)) %>%
  filter(year==2018)

max_loc <- table3 %>% 
  filter(black_jail_pop_prop==max(black_jail_pop_prop,na.rm=TRUE)) %>%
  pull(location)

summary_info$highest_black_jail_pop_prop_location_2018 <- max_loc

# 5. Which location was proportion of black_jail_pop to total_jail_pop
# lowest in 2018?
min_loc <- table3 %>% 
  filter(black_jail_pop_prop==min(black_jail_pop_prop,na.rm=TRUE))

min_loc <- head(min_loc, 1) %>%
  pull(location)

summary_info$lowest_black_jail_pop_prop_location_2018 <- min_loc

# line chart: how has prison admissions counts for each race changed over time 
# in Washington?

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

# scatter plot: does the number of Black prison admissions
# affect the number of Latinx prison admissions?

# select relevant columns

scatter_data <- county_level_data %>%
  select(year, state, county_name, black_prison_adm, latinx_prison_adm)

# drop NA values
scatter_data <- drop_na(scatter_data)


# plot scatter plot 
scatter_plot <- ggplot(data=scatter_data) +
  geom_point(mapping = aes(y=latinx_prison_adm, x=black_prison_adm)) +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(labels=scales::comma) +
  labs(x="Black Prison Admissions Count",
       y="Latinx Prison Admissions Count",
       title="Black Prison Admissions Count versus Latinx Prison Admissions Count")

# map: view proportion of proportion of black_jail_pop to total_jail_pop
# by counties in 2018

# calculate proportion  
map_data <- county_level_data %>%
  filter(year==2018) %>%
  mutate(black_jail_pop_prop = black_jail_pop/total_jail_pop, 
         na.rm=TRUE) %>%
  select(fips, state, county_name, black_jail_pop, total_jail_pop,
         black_jail_pop_prop)

# add 0 in front of certain state fips so it works with the geospatial data
map_data <- transform(map_data, fips = ifelse(state %in% c("CA", "AZ", "CO",
                                                           "AR", "AL", "AK",
                                                           "HI", "CT", "RI",
                                                           "VT"),
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

fig <- fig %>% colorbar(title = "Proportion of Black Jail Inmates")
fig <- fig %>% layout(
  title = "2018 Proportion of Black Jail Inmates by US County"
)

fig <- fig %>% layout(
  geo=g
)

fig
