Capital Bikeshare Competition Analysis
================
Joshua Garties
2025-03-26

- [Introduction](#introduction)
- [Background](#background)
- [Load Data](#load-data)
- [Clean and Transform Data](#clean-and-transform-data)
- [Visualize Data](#visualize-data)

## Introduction

The purpose of this case study is to provide recommendations to Capital
Bikeshare on competing with the privately-owned micromobility providers
operating in the region. I use publicly available data from [Capital
Bikeshare](https://capitalbikeshare.com/system-data) for its system and
[Ride Report](https://public.ridereport.com/regions/washington) for
micromobility providers.

The [slide deck](cabi-competition_case-study-deck_jgarties.pdf) in this repository presents my findings and
recommendations. This readme and the [R Markdown file](cabi_competition_2025-03-26.Rmd) walk through the
steps I took to perform my analysis.

## Background

**Capital Bikeshare** is a bikesharing service that operates across the
Washington, DC region. It is owned by the local governments in its
service area, who contract its operations out to a private company.
Riders typically unlock and return bikes at docking stations installed
throughout the region, although riders may end e-bike rides without
returning them to a dock for an additional fee. 

*Key dates:*
* 2010: Capital Bikeshare opens.
* 2018: Capital Bikeshare begins introducing e-bikes.
* 2023: Capital Bikeshare introduces new e-bikes with greater performance and reliability.

Privately-owned **micromobility** companies operate dockless e-bike and
scooter service in DC, Arlington, and the University of Maryland. Riders
can start and end dockless rideables anywhere, with some limitations on
where they can park. For simplicity, I use the term “micromobility” in
this case study to refer to these companies’ services. 

*Key dates:* 
* 2017: DC begins permitting micromobility services.
* 2020: DC raises its cap on micromobility rideables to 5,000 e-bikes and 10,000 scooters.
* 2023: DC raises its cap on micromobility rideables to 10,000 e-bikes and 20,000 scooters.

## Load Data

First, install the packages and load the libraries we will be using.

``` r
# Install packages and load libraries
install.packages("tidyverse",repos = "http://cran.us.r-project.org")
```

    ## package 'tidyverse' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\joshg\AppData\Local\Temp\RtmpyCEfJj\downloaded_packages

``` r
library("tidyverse")
library(scales)
library(readxl)
```

### Capital Bikeshare Data

Capital Bikeshare provides records for each ride taken. Download CSV
files from [Capital Bikeshare’s open data
site](https://capitalbikeshare.com/system-data) for each of the months
in scope and extract them into a single directory. For this case study,
I used data from July 2020 to December 2024.

Use this code to combine the CSVs into the dataframe ‘cabi_data_raw’.

``` r
# Set the path to your directory
path <- "./cabi_data"

# List all CSV files in the directory
file_list <- list.files(path = path, pattern = "*.csv", full.names = TRUE)

# Read and combine all CSV files, selecting only desired columns
cabi_data_raw <- file_list %>%
  lapply(function(file) {
    df <- read_csv(file)
    df <- select(df, rideable_type, started_at, ended_at)
    return(df)
  }) %>%
  bind_rows()

# Remove file_list and temporary dataframe to save memory
rm(file_list)

head(cabi_data_raw)
```

    ## # A tibble: 6 × 3
    ##   rideable_type started_at          ended_at           
    ##   <chr>         <dttm>              <dttm>             
    ## 1 classic_bike  2024-04-29 17:03:15 2024-04-29 17:10:14
    ## 2 classic_bike  2024-04-19 17:36:14 2024-04-19 17:41:31
    ## 3 classic_bike  2024-04-06 10:18:39 2024-04-06 10:39:37
    ## 4 classic_bike  2024-04-01 13:24:00 2024-04-01 15:42:10
    ## 5 classic_bike  2024-04-01 13:24:14 2024-04-01 15:42:02
    ## 6 classic_bike  2024-04-01 13:24:16 2024-04-01 15:42:05

``` r
# OPTIONAL: Write the combined data frame to a new CSV file to avoid having to perform these steps again
#write_csv(cabi_data_raw, "cabi_202007_202412.csv", col_names = TRUE)
```

### Micromobility Data

Download CSV files from [Ride Report’s DC Region
page](https://public.ridereport.com/regions/washington). Save a separate
CSV for each of the three regions and for each rideable type (e-bikes
and scooters) for a total of six CSVs. Save them in another directory.

Since these files have relatively few rows, I chose to combine them in
Excel. I added the following columns: \* ‘region’ populated with “DC”,
“Arlington”, and “UMD” according to their source. \* ‘rideable_type’
populated with “electric_bike” and “scooter” according to their source.

## Clean and Transform Data

### Capital Bikeshare

First we will check for blank/NA values in the data.

``` r
# Check for NA values
colSums(is.na(cabi_data_raw))
```

    ## rideable_type    started_at      ended_at 
    ##             0             0             0

Seeing none, we will create new columns to help us manipulate the data.

The new date columns will help us summarize by year, quarter, and month.
We need quarter because the micromobility dataset is summarized by
quarter.

The ‘duration_mins’ column uses the ‘started_at’ and ‘ended_at’
datetimes to show how long the ride was.

``` r
# create new columns needed for analysis
cabi_data <- cabi_data_raw %>% 
  mutate(year = year(started_at)) %>% 
  mutate(year_month = ym(format(started_at, "%Y-%m"))) %>% 
  mutate(quarter_start = make_date(year(year_month),(quarter(year_month) - 1) * 3 + 1, 1)) %>% 
  mutate(duration_mins = as.numeric(difftime(ended_at, started_at, unit = "mins"))) %>% 
  select(year, quarter_start, year_month, started_at, rideable_type, duration_mins)

head(cabi_data)
```

    ## # A tibble: 6 × 6
    ##    year quarter_start year_month started_at          rideable_type duration_mins
    ##   <dbl> <date>        <date>     <dttm>              <chr>                 <dbl>
    ## 1  2024 2024-04-01    2024-04-01 2024-04-29 17:03:15 classic_bike           6.98
    ## 2  2024 2024-04-01    2024-04-01 2024-04-19 17:36:14 classic_bike           5.28
    ## 3  2024 2024-04-01    2024-04-01 2024-04-06 10:18:39 classic_bike          21.0 
    ## 4  2024 2024-04-01    2024-04-01 2024-04-01 13:24:00 classic_bike         138.  
    ## 5  2024 2024-04-01    2024-04-01 2024-04-01 13:24:14 classic_bike         138.  
    ## 6  2024 2024-04-01    2024-04-01 2024-04-01 13:24:16 classic_bike         138.

Now any rows that still have a negative ‘duration_mins’ value would
represent rides where the start_time is after the end_time. These are
presumably errors, which we will remove.

``` r
# Check the number of rows with negative duration
num_negative_duration <- sum(cabi_data$duration_mins < 0)
num_negative_duration
```

    ## [1] 7402

``` r
# Remove affected rows
cabi_data <- cabi_data %>% filter(duration_mins > 0)
```

Next, inspect the dataframe for outliers.

``` r
# Calculate summary statistics for the dataframe
summary(cabi_data$duration_mins)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##      0.00      6.27     10.98     22.05     19.33 240415.25

``` r
# View the highest and lowest duration_mins values
tail(sort(cabi_data$duration_mins))
```

    ## [1] 225089.0 234080.9 235293.5 235902.2 237550.6 240415.2

``` r
head(sort(cabi_data$duration_mins))
```

    ## [1] 0.0006833355 0.0008666674 0.0010499994 0.0011666695 0.0012166699
    ## [6] 0.0017500003

This shows that there are extremely high ‘duration_mins’ values, with
the maximum thousands of minutes. There are also very low values, under
one second.

Extremely high values likely represent bikes that riders lost or did not
properly dock, while low values may represent riders accidentally or
deliberately ending their ride immediately after they started.

Since there is no natural cutoff for the high values (maybe some people
really do decide to rent a bike for several days?), we can remove values
that are three standard deviations above and below the mean.

``` r
# Remove values three standard deviations above and below the mean
cabi_data <- cabi_data %>% 
  filter(duration_mins < (mean(duration_mins) + (3 * sd(duration_mins))) & duration_mins > (mean(duration_mins) - (3 * sd(duration_mins))))
```

However, this does not address the low values. We can remove values
under one minute, assuming these do not represent actual rides.

``` r
# Remove rides < 1 min
cabi_data <- cabi_data %>% 
  filter(duration_mins > 1)
```

Looking again at the summary statistics, we see that the maximum value
is still high. We will be using the mean for this analysis, which will
help control for the high outliers.

``` r
# Calculate summary statistics for the dataframe
summary(cabi_data$duration_mins)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##    1.000    6.483   11.167   18.064   19.500 1143.567

Next, we want to make sure that the ‘rideable_type’ coding is
consistent, since we will be analyzing trends in the use of e-bikes and
classic bikes.

``` r
# Show the different values for rideable_type
unique(cabi_data$rideable_type)
```

    ## [1] "classic_bike"  "electric_bike" "docked_bike"

This reveals three unique values: “docked_bike”, “classic_bike”, and
“electric_bike” when we are only expecting two. I suspect that
“docked_bike” and “classic_bike” refer to the same type of bike.
“classic_bike” appeared in head(cabi_data_raw) associated with a
relatively recent ride, so perhaps “docked_bike” is an old code that was
phased out.

The two operations below will test this theory.

``` r
# Show when rideable_type codes were first and last used.
cabi_data %>% 
  group_by(rideable_type) %>% 
  summarize(first = min(started_at), last = max(started_at))
```

    ## # A tibble: 3 × 3
    ##   rideable_type first               last               
    ##   <chr>         <dttm>              <dttm>             
    ## 1 classic_bike  2020-12-08 11:40:04 2024-12-31 23:48:38
    ## 2 docked_bike   2020-07-01 00:00:11 2023-09-13 11:27:51
    ## 3 electric_bike 2020-07-01 09:58:13 2024-12-31 23:54:46

``` r
# Create a bar plot to show how usage changed over time.
ggplot(cabi_data) +
  geom_bar(mapping=aes(x=year_month, fill=rideable_type))
```

![](cabi_readme_mar25_files/figure-gfm/examine%20rideable_type%20values-1.png)<!-- -->

The results support the theory that ‘docked_bike’ was phased out in
favor of ‘classic_bike’. To keep these codes consistent, recode
‘docked_bike’ in the dataframe.

``` r
# Replace docked_bike with classic_bike to keep variable values consistent
cabi_data$rideable_type[cabi_data$rideable_type == "docked_bike"] <- "classic_bike"
```

Finally, we will create several summary dataframes that we will use for
our visualizations.

``` r
# Create a dataframe with total rides per year, by rideable_type
cabi_yearly_rideable <- cabi_data %>% 
  group_by(year = year(year_month), rideable_type) %>% 
  summarize(num_rides = n())

# Create a dataframe with total rides per quarter
cabi_quarterly_total <- cabi_data %>% 
  group_by(quarter_start) %>% 
  summarize(num_rides = n())

# Create a dataframe with total rides per quarter, by rideable_type
cabi_quarterly_rideable <- cabi_data %>% 
  group_by(quarter_start, rideable_type) %>% 
  summarize(num_rides = n())
```

### Micromobility

The micromobility data is already summarized at a high level. Manually
reviewing it does not reveal any obvious issues, so we will accept Ride
Report’s cleaning of the underlying data as sufficient.

First, save the manually prepared Excel file as a dataframen using
.name_repair to address spaces in the column headers.

``` r
# Read micromobility data from Excel
mm_data_raw <- read_xlsx("dmv_micromobility.xlsx", .name_repair = "universal")
head(mm_data_raw)
```

    ## # A tibble: 6 × 10
    ##   Time.Period jurisdiction rideable_type Median.Trip.Distance..miles.
    ##   <chr>       <chr>        <chr>                                <dbl>
    ## 1 2019-Q1     DC           scooter                               1.2 
    ## 2 2019-Q2     DC           scooter                               1.21
    ## 3 2019-Q3     DC           scooter                               0.98
    ## 4 2019-Q4     DC           scooter                               0.83
    ## 5 2020-Q1     DC           scooter                               0.76
    ## 6 2020-Q2     DC           scooter                               1.07
    ## # ℹ 6 more variables: Median.Trip.Duration..minutes. <dbl>,
    ## #   Average.Trip.Distance..miles. <dbl>, Average.Trip.Duration..minutes. <dbl>,
    ## #   Average.Trips.per.Day <dbl>, Total.Distance..miles. <dbl>,
    ## #   Total.MDS.Trips <dbl>

Then, mutate the columns to match the date columns created for the
Capital Bikeshare data.

``` r
# Create new date columns
mm_data <- mm_data_raw %>%
  mutate(
    # Extract the year and convert to integer
    year = as.integer(str_extract(Time.Period, "^\\d{4}")),
    # Extract the quarter number (the digit following "Q")
    quarter = as.integer(str_extract(Time.Period, "(?<=Q)\\d")),
    # Calculate the starting month: Q1 -> 1, Q2 -> 4, Q3 -> 7, Q4 -> 10
    month = (quarter - 1) * 3 + 1,
    # Create a Date object for the first day of that month
    quarter_start = make_date(year, month, 1)
  )

head(mm_data)
```

    ## # A tibble: 6 × 14
    ##   Time.Period jurisdiction rideable_type Median.Trip.Distance..miles.
    ##   <chr>       <chr>        <chr>                                <dbl>
    ## 1 2019-Q1     DC           scooter                               1.2 
    ## 2 2019-Q2     DC           scooter                               1.21
    ## 3 2019-Q3     DC           scooter                               0.98
    ## 4 2019-Q4     DC           scooter                               0.83
    ## 5 2020-Q1     DC           scooter                               0.76
    ## 6 2020-Q2     DC           scooter                               1.07
    ## # ℹ 10 more variables: Median.Trip.Duration..minutes. <dbl>,
    ## #   Average.Trip.Distance..miles. <dbl>, Average.Trip.Duration..minutes. <dbl>,
    ## #   Average.Trips.per.Day <dbl>, Total.Distance..miles. <dbl>,
    ## #   Total.MDS.Trips <dbl>, year <int>, quarter <int>, month <dbl>,
    ## #   quarter_start <date>

Finally, create summary dataframes similar to the dataframes we created
for Capital Bikeshare.

``` r
# Create a dataframe with total rides per year, by rideable_type
mm_yearly_rideable <- mm_data %>% 
  group_by(year, rideable_type) %>% 
  summarize(num_rides = sum(Total.MDS.Trips))

# Create a dataframe with total rides per quarter
mm_quarterly_total <- mm_data %>% 
  group_by(quarter_start) %>% 
  summarize(num_rides = sum(Total.MDS.Trips))

# Create a dataframe with total rides per quarter, by rideable_type
mm_quarterly_rideable <- mm_data %>% 
  group_by(year, quarter_start, rideable_type) %>% 
  summarize(num_rides = sum(Total.MDS.Trips))
```

## Visualize Data

### Capital Bikeshare

The first visual will be a stacked area chart that shows Capital
Bikeshare ridership by quarter. While data is available by month, the
Ride Report dataset is summarized by quarter, so we will need to
summarize Capital Bikeshare by quarter to compare the two datasets.

The chart shows Capital Bikeshare’s total quarterly ridership, with the
filled regions representing how much ridership came from classic bikes
and e-bikes.

Adding a vertical line at March 20, 2023 shows when Capital Bikeshare
began introducing the new e-bikes, allowing the reader to see how the
introduction affected ridership.

*Takeaway:* Ridership has grown steadily over the past four years.
E-bikes appear to account for a larger share of ridership since the new
e-bikes were introduced.

``` r
# Create stacked area chart
ggplot(data = cabi_quarterly_rideable) +
  geom_area(mapping = aes(x = quarter_start, y = num_rides, fill = rideable_type)) +
    scale_fill_manual(
    name = NULL,
    values = c("classic_bike" = "#F8766D", "electric_bike" = "#00BFC4"),
    labels = c("classic_bike" = "Classic Bike", "electric_bike" = "Electric Bike")
  ) +
  theme(axis.text.y = element_text(angle = 45), legend.position = c(.125, .825), plot.subtitle = element_text(face = "italic")) +
  scale_y_continuous(
    labels = comma) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "3 months",
    date_labels = "%Y"
  ) +
  labs(title = "Capital Bikeshare Ridership",
       subtitle = "Quarterly",
       caption = "Data from Capital Bikeshare",
       x=NULL,
       y = "Rides") +

# Add a vertical line
  geom_vline(xintercept = as_date("2023-03-20"), linetype = "dashed", color = "#619CFF", size = 1.5) +
  annotate(
    "text", 
    label = "New e-bikes introduced", 
    x = as_date("2022-07-01"), 
    y = 1600000, 
    color = "#619CFF",
    fontface = "bold"
    )
```

To more clearly show that e-bikes account for a larger proportion of
ridership over time, we can plot a stacked bar chart. Each bar
represents 100% of the ridership for the year, which shows the change in
proportions regardless of total ridership.

*Takeaway:* E-bikes have accounted for an increasing proportion of
ridership since 2022, and accounted for the majority of ridership in
2024.

``` r
# Create 100% stacked bar chart
ggplot(data = cabi_yearly_rideable) +
  geom_col(
    mapping = aes(x = year, y = num_rides, fill = rideable_type), 
    position = "fill"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("classic_bike" = "#F8766D", "electric_bike" = "#00BFC4"),
    labels = c("classic_bike" = "Classic Bike", "electric_bike" = "Electric Bike")
  ) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  labs(title = "Percent of Rideable Type",
       caption = "Data from Capital Bikeshare",
       x = NULL,
       y = NULL
  )
```

### Micromobility

Plot a stacked are chart similar to the chart for Capital Bikeshare,
breaking down ridership by e-bikes and scooters.

*Takeaway:* Ridership has grown steadily over the past four years. Most
of the growth is from e-bikes.

``` r
# Create stacked area chart
ggplot(data = mm_quarterly_rideable) +
  geom_area(mapping = aes(x = quarter_start, y = num_rides, fill = rideable_type)) +
  scale_fill_manual(
    name = NULL,
    values = c("scooter" = "#00BA38", "electric_bike" = "#00BFC4"),
    labels = c("scooter" = "Scooter", "electric_bike" = "Electric Bike")
  ) +
  theme(axis.text.y = element_text(angle = 45), legend.position = c(.125, .825), plot.subtitle = element_text(face = "italic")) +
  coord_cartesian(xlim = as.Date(c("2020-01-01", "2024-10-31"))) +
  scale_y_continuous(labels = comma) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "3 months",
    date_labels = "%Y"
  ) +
  labs(title = "Micromobility Ridership",
       subtitle = "Quarterly",
       caption = "Data from Ride Report for DC, Arlington, and University of Maryland (public.ridereport.com)",
       x=NULL,
       y = "Rides")
```

Plot a 100% stacked bar chart similar to Capital Bikeshare chart,
breaking down ridership by e-bikes and scooters.

*Takeaway:* Scooters dominate ridershp, but the proportion of e-bikes is
growing.

``` r
# Create 100% stacked bar chart
ggplot(data = mm_yearly_rideable) +
  geom_col(
    mapping = aes(x = factor(year), y = num_rides, fill = rideable_type), 
    position = "fill"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("scooter" = "#00BA38", "electric_bike" = "#00BFC4"),
    labels = c("scooter" = "Scooter", "electric_bike" = "Electric Bike")
  ) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  labs(title = "Percent of Rideable Type",
       caption = "Data from Ride Report for DC, Arlington, and University of Maryland (public.ridereport.com)",
       x = NULL,
       y = NULL
  )
```

### Combined

To compare total Capital Bikeshare and micromobility ridership, plot a
chart with one line for each service.

*Takeaway:* Micromobility ridership has been consistently higher than
Capital Bikeshare ridership since 2021.

``` r
# Create line chart
ggplot() +
  geom_line(data = cabi_quarterly_total, aes(x = quarter_start, y = num_rides), color = "#F8766D", size = 1.5) +
  geom_line(data = mm_quarterly_total, aes(x = quarter_start, y = num_rides), color = "#C77CFF", size = 1.5) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.y = element_text(angle = 45), plot.subtitle = element_text(face = "italic")) +
  coord_cartesian(xlim = as.Date(c("2021-01-01", "2024-12-31"))) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  annotate(
    "text", 
    x = c(as_date("2024-10-01"), as_date("2024-09-01")), 
    y = c(1450000, 2400000), 
    label = c("Capital Bikeshare", "Micromobility"),
    color = c("#F8766D", "#C77CFF"),
    fontface = "bold"
    ) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "3 months",
    date_labels = "%Y"
  ) +
  labs(title = "Ridership Comparison",
       subtitle = "Quarterly",
       caption = "Data from Capital Bikeshare and Ride Report for DC, Arlington, and University of Maryland (public.ridereport.com)",
       x=NULL,
       y = "Rides")
```

Next, plot a similar chart showing only e-bike ridership for both
services. This requires creating two new dataframes containing only
e-bike data before plotting.

As we did above, we’ll add a vertical line at March 20, 2023 showing
when Capital Bikeshare began introducing the new e-bikes

*Takeaway:* Micromobility e-bikes overtook Capital Bikeshare e-bikes in
2022, but Capital Bikeshare closed the gap after introducing the new
e-bikes.

``` r
# Create new dataframes for only e-bikes.
cabi_quarterly_ebike <- cabi_quarterly_rideable %>% 
  filter(rideable_type == "electric_bike")

mm_quarterly_ebike <- mm_quarterly_rideable %>% 
  filter(rideable_type == "electric_bike")

# Create line chart
ggplot() +
  geom_line(data = cabi_quarterly_ebike, aes(x = quarter_start, y = num_rides), color = "#F8766D", size = 1.5) +
  geom_line(data = mm_quarterly_ebike, aes(x = quarter_start, y = num_rides), color = "#C77CFF", size = 1.5) +
  scale_y_continuous(
    labels = comma
    ) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "3 months",
    date_labels = "%Y"
  ) +
  theme(axis.text.y = element_text(angle = 45), plot.subtitle = element_text(face = "italic")) +
  coord_cartesian(xlim = as.Date(c("2021-01-01", "2024-11-01"))) +
  annotate(
    "text", 
    x = c(as_date("2021-04-01"), as_date("2021-04-01")), 
    y = c(50000, 250000), 
    label = c("Capital Bikeshare", "Micromobility"),
    color = c("#F8766D", "#C77CFF"),
    fontface = "bold"
  ) +
  labs(title = "E-Bike Ridership Comparison",
       subtitle = "Quarterly",
       caption = "Data from Capital Bikeshare and Ride Report for DC, Arlington, and University of Maryland (public.ridereport.com)",
       x=NULL,
       y = "Rides") +
# Add a vertical line
  geom_vline(xintercept = as_date("2023-03-20"), linetype = "dashed", color = "#619CFF", size = 1.5) +
    annotate(
      "text", 
      label = "New e-bikes introduced", 
      x = as_date("2022-08-01"), 
      y = 850000, 
      color = "#619CFF",
      fontface = "bold"
  )
```

Plot median trip duration across all Capital Bikeshare and micromobility
rideables to assess whether riders select their service and rideable
depending on the trip they are taking. We use Q3 2024 data because
ridership peaks in Q3 every year, and 2024 is the most recent year
avaialable.

This requires creating two new dataframes for the two services showing
their median trip duration in each quarter by rideable type, then
joining the two dataframes.

*Takeaway:* Median trip duration is similar across all categories,
suggesting that riders do not select service or rideable based on their
expected trip time.

``` r
# Create dataframe for Capital Bikeshare median trip duration
cabi_quarterly_median <- cabi_data %>% 
  group_by(quarter_start, rideable_type) %>% 
  summarize(median_duration = median(duration_mins)) %>% 
  filter(quarter_start == as_date("2024-07-01")) %>% 
  mutate(operator = "cabi") %>% 
  mutate(rideable_type = if_else(rideable_type == "electric_bike", "cabi_ebike", rideable_type))  

# Create dataframe for micromobility median trip duration
mm_quarterly_median <- mm_data %>% 
  group_by(quarter_start, rideable_type) %>% 
  filter(quarter_start == as_date("2024-07-01") & jurisdiction == "DC") %>%
  mutate(median_duration = Median.Trip.Duration..minutes.) %>% 
  mutate(operator = "mm") %>% 
  mutate(rideable_type = if_else(rideable_type == "electric_bike", "mm_ebike", rideable_type)) %>% 
  select(quarter_start, operator, rideable_type, median_duration)

# Join the two dataframes
combined_quarterly_median <- bind_rows(cabi_quarterly_median, mm_quarterly_median)

# Create bar chart
ggplot(data = combined_quarterly_median) +
  geom_col(
    mapping = aes(
      x = reorder(rideable_type, median_duration), 
      y = median_duration
      ),
    fill = "#CD9600",
    width = 0.5
    ) +
  theme(plot.subtitle = element_text(face = "italic")) +
  labs(title = "Median Trip Duration",
       subtitle = "Q3 2024",
       caption = "Data from Capital Bikeshare and Ride Report for DC (public.ridereport.com)",
       x="Rideable Type",
       y = "Median Trip Duration (minutes)") +
  scale_y_continuous(
    breaks = c(0, 2, 4, 6, 8, 10)
  ) +
  scale_x_discrete(
    labels = c("Micromobility Scooter", "Micromobility E-Bike", "Capital Bikeshare E-Bike", "Capital Bikeshare Classic Bike")
  )
```
END
