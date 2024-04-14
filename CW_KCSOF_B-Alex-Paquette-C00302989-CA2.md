Enhancing Scientific Inquiry with R for Data Manipulation and
Visualization
================
Alexandre Paquette
2024-04-14

# Introduction

Data visualization is a fundamental tool across a multitude of
disciplines, serving as a conduit for conveying complex information in a
visually accessible and palatable manner. In scientific research, where
the exploration and interpretation of data are paramount, effective
visualization techniques play a pivotal role in interpreting patterns,
trends, and relationships within datasets. By transforming raw data into
intuitive visual representations, researchers can gain deeper insights
into their findings and communicate them with clarity and precision. In
this report, we walk through the process of producing data
visualizations for scientific research, exploring the methodologies and
strategies that underline the creation of impactful visual narratives.

## Understanding Scientific Research

Scientific research constitutes a meticulous and systematic pursuit of
knowledge, aiming to present phenomena, develop theories, and solve
practical problems through rigorous methodologies via experimentation,
observation, analysis, and interpretation. Central to this endeavor is
the collection and processing of data, which serves as the cornerstone
for empirical investigation. However, the mere accumulation of data is
insufficient without an effective way to interpret and communicate. Data
visualization emerges as a vital tool in this process, enabling
researchers to distill complex datasets into intuitive visual
representations that facilitate hypothesis confirmation or refutation.
By harnessing the power of visualization, researchers can discern
patterns, trends, and relationships within data, thereby advancing our
understanding of the natural world and informing evidence-based
decision-making.

## A Real-world Case Study

The real-world case study at the heart of our visualization endeavor
revolves around an investigation into the potential benefits of
utilizing a bio-priming agent comprising *Bacillus cereus* and
*Pseudomonas alcaligenes* during the germination stage of plant
development. The overarching objective of this study is to assess
whether the application of this bio-priming agent contributes to the
overall health and vigor of plants upon completion of the germination
process.

The study aims to demonstrate the efficacy of the biopriming agent in
enhancing plant growth, resilience, and productivity by leveraging the
natural symbiotic relationship between *Bacillus cereus* and
*Pseudomonas alcaligenes*. Through a series of controlled experiments
and rigorous data collection protocols, researchers seek to evaluate
various physiological parameters, including seed germination rates,
seedling vigor, root development, and resistance to environmental
stressors.

By employing a multidisciplinary approach that integrates microbiology,
plant biology, and agronomy, the study endeavors to shed light on the
potential applications of bio-priming techniques in sustainable
agriculture practices. The findings from this research hold promise for
informing agricultural strategies aimed at optimizing plant health and
productivity while minimizing reliance on synthetic inputs and
agrochemicals.

This real-world case study serves as the foundation for our exploration
of data visualization techniques, providing a concrete context for the
application of visual analytics in scientific research and innovation
within the agricultural domain.

# Research Question

The focal point of our investigation revolves around a central research
question: How do we effectively transform real-world data from
scientific studies into meaningful visual representations that
facilitate insight and understanding? This inquiry stems from the
imperative to distill scientific data into actionable insights,
particularly within the context of scientific research.

## Rationale Behind the Study

Our study aims to shed light on the process of taking real-world data
from a scientific study, manipulating it to extract meaningful insights,
and visualizing these insights for enhanced comprehension. Specifically,
our interest lies in exploring the relationship between the application
of a PGPB (Plant Growth Promoting Bacteria) consortium as a biopriming
agent on the seeds of various plant species and the resulting health and
vigor of plants during the germination growth stage of the plant life.

Data visualization serves as a powerful tool in this endeavor, enabling
us to visually compare growth parameters and trends between experimental
bioprimed seeds and control non-exposed seeds. Through visual
representations, we seek to uncover patterns, trends, and correlations
within the growth data, providing valuable insights into the efficacy of
biopriming with PGPB as a plant growth promoter.

## Methodology

Our methodology revolves around leveraging programming tools,
specifically R and relevant libraries, to manipulate and visualize the
data obtained from the scientific study. By harnessing the capabilities
of R programming and associated libraries, we aim to streamline the data
processing and visualization workflows, facilitating efficient analysis
and interpretation of the dataset.

The specific libraries and techniques employed will be determined based
on the nature of the data and the visualization requirements. However,
our methodology will prioritize clarity, accuracy, and reproducibility,
ensuring that the visualizations produced effectively communicate the
insights derived from the data.

This approach allows us to not only explore the intricacies of data
manipulation and visualization within the context of scientific research
but also lays the foundation for future research endeavors in
data-driven inquiry.

# Data Manipulation

These are the libraries we’ll be using

``` r
# for data manipulation:
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)

# for visualization
library(ggplot2)
```

Load the data from the .csv and save it in a variable

``` r
bioData <- read.csv("rawData.csv")
```

## Data Cleanup

Replace all NA values with 0

``` r
bioData[is.na(bioData)] <- 0
```

Setting values to be all lower case for consistency (except for Group
column)

``` r
bioData$Plant.Type <- tolower(bioData$Plant.Type)
bioData$Control.Experimental <- tolower(bioData$Control.Experimental)
bioData$Common.Name <- tolower(bioData$Common.Name)
bioData$Scientific.Name <- tolower(bioData$Scientific.Name)
```

Converting the Group, Plant Type, and Control/Experimental columns to a
categorical data type (factor)

``` r
bioData$Group <- factor(bioData$Group)
bioData$Plant.Type <- factor(bioData$Plant.Type)
bioData$Control.Experimental <- factor(bioData$Control.Experimental)
```

We will be renaming the columns to fix spelling mistake and apply a
consistent format to our dataset.

``` r
bioData <- bioData %>%
  rename(Common_Name = Common.Name,
         Scientific_Name = Scientific.Name,
         Plant_Type = Plant.Type,
         Controler_Experimental = Control.Experimental,
         Root_Count = number.of.roots.over.all,
         Sprout_Count = number.of.sprouts.over.all,
         Sprout_Week_1_Change_cm = Change.in.week.1..sprout.,
         Sprout_Week_2_Change_cm = change.in.week.2..sprout.,
         Sprout_Week_3_Change_cm = change.in.week.3..sprout.,
         Sprout_Week_4_Change_cm = change.in.week.4..sprout.,
         Seed_Amount = seed.amount,
         Seed_Amount_Variation = seed.amount.variation..,
         Root_Week_1_Change_cm = Change.in.week.1..root.,
         Root_Week_2_Change_cm = change.in.week.2..root.,
         Root_Week_3_Change_cm = change.in.week.3..root.,
         Root_Week_4_Change_cm = change.in.week.4..root.,
         Root_Lengths_cm = Indavidual.lengrths..roots.,
         Sprout_Lengths_cm = Indavidual.langth..sprouts.
         )
```

We want to remove entries that didn’t show any growth for both sprout
and roots. This means checking for rows that have empty columns for both
root and sprout growth data.

``` r
bioData <- bioData %>% filter(Root_Lengths_cm != "" | Sprout_Lengths_cm != "")
```

Now we want to combine matching plant names by control/experimental to
simplify our data further.

``` r
# remove leading and trailing commas
remove_commas <- function(x) {
  gsub("^,|,$", "", x)
}
```

``` r
bioData <- bioData %>%
  group_by(Scientific_Name, Controler_Experimental) %>%
  summarize(
    Common_Name = first(Common_Name),
    Root_Lengths_cm = remove_commas(paste(Root_Lengths_cm, collapse = ", ")),
    Plant_Type = first(Plant_Type),
    Root_Count = sum(Root_Count),
    Sprout_Count = sum(Sprout_Count),
    Sprout_Week_1_Change_cm = mean(Sprout_Week_1_Change_cm),
    Sprout_Week_2_Change_cm = mean(Sprout_Week_2_Change_cm),
    Sprout_Week_3_Change_cm = mean(Sprout_Week_3_Change_cm),
    Sprout_Week_4_Change_cm = mean(Sprout_Week_4_Change_cm),
    Seed_Amount = sum(Seed_Amount),
    Seed_Amount_Variation = first(Seed_Amount_Variation),
    Root_Week_1_Change_cm = mean(Root_Week_1_Change_cm),
    Root_Week_2_Change_cm = mean(Root_Week_2_Change_cm),
    Root_Week_3_Change_cm = mean(Root_Week_3_Change_cm),
    Root_Week_4_Change_cm = mean(Root_Week_4_Change_cm),
    Sprout_Lengths_cm = remove_commas(paste(Sprout_Lengths_cm, collapse = ", "))
  ) %>%
  ungroup()
```

    ## `summarise()` has grouped output by 'Scientific_Name'. You can override using
    ## the `.groups` argument.

We will generate IDs for each entry so we can identify them uniquely.

``` r
bioData <- bioData %>% mutate(id = row_number())
bioData <- bioData %>% select(id, everything())
```

We are extracting the multiple values from the columns so that they can
be read as individual numbers

``` r
rootLengthsData <- bioData %>% separate_rows(Root_Lengths_cm, sep = ",") %>% select(id, Root_Length_cm = Root_Lengths_cm)
```

Next we want to convert our Root Length to numeric

``` r
rootLengthsData <- rootLengthsData %>% mutate(Root_Length_cm = as.numeric(Root_Length_cm))
```

We’ve now extracted all the individual Root Length for each plant. This
will allow us to visualize this data more effectively if necessary.
Regardless, this data needs to exist in this raw form for further
investigation.

Now we want to repeat this step, but for the sprout length column. Now
that we know the process, we can complete this in a single step.

``` r
sproutLengthsData <- bioData %>%
  separate_rows(Sprout_Lengths_cm, sep = ",") %>%
  select(id, Sprout_Length_cm = Sprout_Lengths_cm) %>%
  mutate(Sprout_Length_cm = as.numeric(Sprout_Length_cm))

sproutLengthsData[is.na(sproutLengthsData)] <- 0
```

Finally, we can remove the old columns from the dataset

``` r
bioData <- bioData %>% select(-Root_Lengths_cm, -Sprout_Lengths_cm)
```

## Derive New Data

Next we need to extract insights from our data. We will be adding
columns to our main dataset to calculate the following: - Average and
median root/stem lengths - Average growth over time for root and sprout

### Calculate average and median

This method will be used to perform calculations on a given dataset and
append the results to the main dataset. In this case, we’ll be passing
`bioData` as our main dataset, and then `rootLengthsData` or
`sproutLengthsData` depending on the calculation we want to make.

``` r
calculate_summary <- function(main_data, summary_data, id_col, summary_col, summary_name, summary_method){
  summary_values <- summary_data %>%
    group_by({{id_col}}) %>%
    summarize({{summary_name}} := summary_method({{summary_col}}))
  result <- left_join(main_data, summary_values)
  return(result)
}
```

``` r
bioData <- calculate_summary(bioData, rootLengthsData, id, Root_Length_cm, avg_root_length_cm, mean) # average root length
```

    ## Joining with `by = join_by(id)`

``` r
bioData <- calculate_summary(bioData, sproutLengthsData, id, Sprout_Length_cm, avg_sprout_length_cm, mean) # average sprout length
```

    ## Joining with `by = join_by(id)`

``` r
bioData <- calculate_summary(bioData, rootLengthsData, id, Root_Length_cm, median_root_length_cm, median) # Median sprout length
```

    ## Joining with `by = join_by(id)`

``` r
bioData <- calculate_summary(bioData, sproutLengthsData, id, Sprout_Length_cm, median_sprout_length_cm, median) # Median sprout length
```

    ## Joining with `by = join_by(id)`

### Calculate average growth over time

``` r
bioData <- cbind(bioData, 
                 avg_root_growth_per_week_cm =
                   rowSums(bioData %>% 
                             select(Root_Week_1_Change_cm, Root_Week_2_Change_cm, Root_Week_3_Change_cm, Root_Week_4_Change_cm)))
```

``` r
bioData <- cbind(bioData, 
                 avg_sprout_growth_per_week_cm =
                   rowSums(bioData %>% 
                             select(Sprout_Week_1_Change_cm, Sprout_Week_2_Change_cm, Sprout_Week_3_Change_cm, Sprout_Week_4_Change_cm)))
```

# Data Visualization

``` r
create_bar_plot <- function(data, x_val, y_val, fill_val, titleName, subtitleName, x_title, y_title){
  colors <- c("experimental" = "#7cb5ec", "control" = "#f7a35c")
  chart <- ggplot(data, aes_string(x = x_val, y = y_val, fill = fill_val)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_fill_manual(values = colors, name = "") +
  labs(title = titleName,
       subtitle = subtitleName,
       x = x_title,
       y = y_title) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),             # Increase subtitle font size
        plot.margin = margin(30, 30, 30, 30, "pt"),
        plot.background = element_rect(fill = "white"),      # Set plot background color
        panel.background = element_rect(fill = "white"))  # Rotate x-axis labels for better readability
  fileName <- paste("charts/",titleName,".png")
  suppressMessages(ggsave(fileName, chart))
  return(chart)
}
```

What do we need to show? - growth differences for roots and sprouts
between control/experimental

## Root Growth Average comparison

``` r
create_bar_plot(data = bioData,
                x_val = "Scientific_Name",
                y_val = "avg_root_growth_per_week_cm",
                fill_val = "Controler_Experimental",
                titleName = "Average Root Growth",
                subtitleName = "per week over 4 weeks",
                x_title = "Plant Species",
                y_title = "Weekly Overall Growth Avg (cm)")
```

![](CW_KCSOF_B-Alex-Paquette-C00302989-CA2_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

This chart illustrates the average root growth for monocot plants
comparing control and experimental.

``` r
create_bar_plot(data = subset(bioData, Plant_Type == "monocot"),
                x_val = "Scientific_Name",
                y_val = "avg_root_growth_per_week_cm",
                fill_val = "Controler_Experimental",
                titleName = "Average Root Growth for monocot plants",
                subtitleName = "per week over 4 weeks",
                x_title = "Monocot Plant Species",
                y_title = "Weekly Overall Growth Avg (cm)")
```

![](CW_KCSOF_B-Alex-Paquette-C00302989-CA2_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

This chart illustrates the average root growth for dicot plants
comparing control and experimental.

``` r
create_bar_plot(data = subset(bioData, Plant_Type == "dicot"),
                x_val = "Scientific_Name",
                y_val = "avg_root_growth_per_week_cm",
                fill_val = "Controler_Experimental",
                titleName = "Average Root Growth for dicot plants",
                subtitleName = "per week over 4 weeks",
                x_title = "Dicot Plant Species",
                y_title = "Weekly Overall Growth Avg (cm)")
```

![](CW_KCSOF_B-Alex-Paquette-C00302989-CA2_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

This generates a chart for each individual plant that compares the
control and experimental sample for root growth.

``` r
unique_plant_names <- unique(bioData$Scientific_Name)
# Iterate through each unique plant name
for(plant_name in unique_plant_names){
  # Generate a chart for the current plant
  create_bar_plot(data = filter(bioData, Scientific_Name == plant_name),
                  x_val = "Scientific_Name",
                  y_val = "avg_root_growth_per_week_cm",
                  fill_val = "Controler_Experimental",
                  titleName = paste("Average Root Growth for",plant_name),
                  subtitleName = "per week over 4 weeks",
                  x_title = "Plant Species",
                  y_title = "Weekly Overall Growth Avg (cm)")
}
```

This generates a chart for the average root growth for all dicot plants
comparing the control and experimental samples

``` r
create_bar_plot(data = subset(bioData, Plant_Type == "dicot"),
                x_val = "Scientific_Name",
                y_val = "avg_root_growth_per_week_cm",
                fill_val = "Controler_Experimental",
                titleName = "Average Root Growth for dicot plants",
                subtitleName = "per week over 4 weeks",
                x_title = "Dicot Plant Species",
                y_title = "Weekly Overall Growth Avg (cm)")
```

![](CW_KCSOF_B-Alex-Paquette-C00302989-CA2_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

## Sprout Growth Average comparison

``` r
create_bar_plot(data = bioData,
                x_val = "Scientific_Name",
                y_val = "avg_sprout_growth_per_week_cm",
                fill_val = "Controler_Experimental",
                titleName = "Average Sprout Growth",
                subtitleName = "per week over 4 weeks",
                x_title = "Plant Species",
                y_title = "Weekly Overall Growth Avg (cm)")
```

![](CW_KCSOF_B-Alex-Paquette-C00302989-CA2_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

This generates a chart for each individual plant that compares the
control and experimental sample for sprout growth.

``` r
# Iterate through each unique plant name
for(plant_name in unique_plant_names){
  # Generate a chart for the current plant
  create_bar_plot(data = filter(bioData, Scientific_Name == plant_name),
                  x_val = "Scientific_Name",
                  y_val = "avg_sprout_growth_per_week_cm",
                  fill_val = "Controler_Experimental",
                  titleName = paste("Average Sprout Growth for",plant_name),
                  subtitleName = "per week over 4 weeks",
                  x_title = "Plant Species",
                  y_title = "Weekly Overall Growth Avg (cm)")
}
```

``` r
create_bar_plot(data = subset(bioData, Plant_Type == "monocot"),
                x_val = "Scientific_Name",
                y_val = "avg_sprout_growth_per_week_cm",
                fill_val = "Controler_Experimental",
                titleName = "Average Sprout Growth for monocot plants",
                subtitleName = "per week over 4 weeks",
                x_title = "Monocot Plant Species",
                y_title = "Weekly Overall Growth Avg (cm)")
```

![](CW_KCSOF_B-Alex-Paquette-C00302989-CA2_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
create_bar_plot(data = subset(bioData, Plant_Type == "dicot"),
                x_val = "Scientific_Name",
                y_val = "avg_sprout_growth_per_week_cm",
                fill_val = "Controler_Experimental",
                titleName = "Average Sprout Growth for dicot plants",
                subtitleName = "per week over 4 weeks",
                x_title = "Dicot Plant Species",
                y_title = "Weekly Overall Growth Avg (cm)")
```

![](CW_KCSOF_B-Alex-Paquette-C00302989-CA2_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

- growth over time chart

# Conclusion

# Glossary of Terms

Biopriming  
The process of coating the seed with a plant-growth promoting bacteria
consortium comprised of Basillus ceres and pusdomonas

Monocot plant  
The seeds of these plants typically contain a single embryonic leaf

Dicot plant  
A plant whose germinating seed contain two embryonic leaves

Embryonic leaf  
The plant embryo, also known as cotyledon
