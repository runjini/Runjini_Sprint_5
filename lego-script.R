# Import all files in Lego data set
library(readr)
colors <- read_csv("Desktop/GitHub/Runjini_Sprint_5/colors.csv")
inventories <- read_csv("Desktop/GitHub/Runjini_Sprint_5/inventories.csv")
inventory_parts <- read_csv("Desktop/GitHub/Runjini_Sprint_5/inventory_parts.csv")
inventory_sets <- read_csv("Desktop/GitHub/Runjini_Sprint_5/inventory_sets.csv")
part_categories <- read_csv("Desktop/GitHub/Runjini_Sprint_5/part_categories.csv")
parts <- read_csv("Desktop/GitHub/Runjini_Sprint_5/parts.csv")
sets <- read_csv("Desktop/GitHub/Runjini_Sprint_5/sets.csv")
themes <- read_csv("Desktop/GitHub/Runjini_Sprint_5/themes.csv")

# Install tidyverse package
install.packages("tidyverse")
library(tidyverse)

# Call ggplot library
library(ggplot2)

# Plot the number of parts by year, colored by theme.  Colors not showing...
ggplot(sets, aes(year, num_parts, colors = theme_id)) 
  +     geom_point()


# Viewing data frams or schema shows several label the primary key/ID of each data frame as merely "id". Rename these columns to perform joins.
# library("dplyr")
# rename(themes, themes_id = id)

# This didn't work, so went back to renaming columns and removal via subset.
themes$theme_id <- themes$id
themes = subset(themes, select = -c(id) )

part_categories$part_cat_id <- part_categories$id
part_categories = subset(part_categories, select = -c(id) )

colors$color_id <- colors$id
colors = subset(colors, select = -c(id) )

inventories$inventory_id <- inventories$id
inventories = subset(inventories, select = -c(id) )

# Also rename "name" fields, since this will affect joins.
themes$theme_name <- themes$name
themes = subset(themes, select = -c(name) )

sets$sets_name <- sets$name
sets = subset(sets, select = -c(name) )

part_categories$part_cat_name <- part_categories$name
part_categories = subset(part_categories, select = -c(name) )

parts$part_name <- parts$name
parts = subset(parts, select = -c(name) )

colors$color_name <- colors$name
colors = subset(colors, select = -c(name) )

# Perform a join to start compiling data together.  Sets contains year, which should be helpful for visualization.  Below is a join for sets and themes using a merge function.
master_join <- merge(sets, inventories, by = "set_num")
master_join <- merge(master_join, inventory_parts, by = "inventory_id")
master_join <- merge(master_join, themes, by = "theme_id")
View(master_join)


# Subset out a portion of the master join.
parent_id_table <- subset(master_join, select = c("parent_id", "theme_id", "theme_name","year", "color_id"))

# Perform some plots to visualize.  First, one that looks at number of parts over time:
ggplot(master_join, aes(year, num_parts))  +
         geom_point()

# Next, plot the themes (via its id) against number of parts:
ggplot(master_join, aes(theme_id, num_parts)) +
         geom_point()

#Then, plot the inventories (via id) against number of parts:
ggplot(master_join, aes(inventory_id, num_parts)) +
        geom_point()

# How many parent IDs are in the themes set?  Use plyr library to count, and then assign the results as a data frame to visualize.
library(plyr)
visual_parent_id <- count(themes, "parent_id")
plot(visual_parent_id)

# Aggregate by subgroup and plot.  This is asking for a sum of the number of parts for each theme ID beneath a parent ID.
num_parts_by_theme <- aggregate(num_parts ~ parent_id + theme_id, data = parent_id_table, sum)
ggplot(num_parts_by_theme, aes(theme_id, num_parts, colors = parent_id)) +
    geom_point()

# This removes subtotaling by theme ID.
num_parts_by_parent_id <- aggregate(num_parts ~ parent_id, data = parent_id_table, sum)
plot(num_parts_by_parent_id) #This graph resembles the ggplot from above ggplot(num_parts_by_theme, aes(theme_id, num_parts, colors = parent_id)) +
geom_point()

# Which parent ID has the most number of parts?
max(num_parts_by_parent_id$num_parts) #This just gives the value.