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

# Plot the number of parts by year, colored by theme.  Colors not showing...  However, it looks like the number of parts across sets is growing over time.  Would be interesting to plot this against mean, because it's possible given the size of this data set, the increaseing slope we see might just be outliers.
ggplot(sets, aes(x=year, y=num_parts, color = theme_id)) +
      geom_point()

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

# And rename quantity fields as well.
inventory_sets$inven_set_quantity <- inventory_sets$quantity
inventory_sets = subset(inventory_sets, select = -c(quantity) )

inventory_parts$inven_parts_quantity <- inventory_parts$quantity
inventory_parts = subset(inventory_parts, select = -c(quantity) )


# Perform a join to start compiling data together.  Sets contains year, which should be helpful for visualization.  Below is a join for sets and themes using a merge function.
master_join <- merge(sets, inventories, by = "set_num")
master_join <- merge(master_join, inventory_parts, by = "inventory_id")
master_join <- merge(master_join, themes, by = "theme_id")
master_join <- merge(master_join, colors, by = "color_id")
View(master_join)

 
# Subset out a portion of the master join.
parent_id_table <- subset(master_join, select = c("parent_id", "theme_id", "theme_name","year", "color_id"))

# Perform some plots to visualize.  First, one that looks at number of parts over time:
ggplot(master_join, aes(year, inven_parts_quantity))  +
         geom_point()

# Plot mean for the number of parts by year.  "11" below in the function refers to the column in the master join column that contains the country of parts.
mean_num_parts <- aggregate(master_join[, 11], list(master_join$year), mean)
ggplot(mean_num_parts, aes(Group.1, x)) +
  geom_point()

# Next, plot the themes (via its id) against number of parts.  It seems like there might be a semi-regular cadence where there are spikes in the number of parts, but hard to tell if there's anything meaningful here.
ggplot(master_join, aes(theme_id, inven_parts_quantity)) +
         geom_point()

#Then, plot the inventories (via id) against number of parts. There is no clear pattern emerging.
ggplot(master_join, aes(inventory_id, inven_parts_quantity)) +
        geom_point()

# How many parent IDs are in the themes set?  Use plyr library to count, and then assign the results as a data frame to visualize.
library(plyr)
visual_parent_id <- count(themes, "parent_id")
plot(visual_parent_id)

# Aggregate by subgroup and plot.  This is asking for a sum of the number of parts for each theme ID beneath a parent ID.  There is nothing really jumping out here; might be interesting to see which theme had the greatest number of parts.
num_parts_by_theme <- aggregate(inven_parts_quantity ~ parent_id + theme_id, data = master_join, sum)
ggplot(num_parts_by_theme, aes(theme_id, inven_parts_quantity, color = parent_id)) +
    geom_point()

# What is the maximum number of parts?  Next question is, what is the theme?
max(num_parts_by_theme$inven_parts_quantity)
# 69085
subset(num_parts_by_theme, inven_parts_quantity == max(num_parts_by_theme$inven_parts_quantity))
# parent_id theme_id inven_parts_quantity
#    22       37                69085
subset(themes, themes$theme_id == 37)
# A tibble: 1 x 3
# parent_id theme_id theme_name
# <int>    <int>      <chr>
#   22       37  Basic Set


# This removes subtotaling by theme ID.
num_parts_by_parent_id <- aggregate(num_parts ~ parent_id, data = parent_id_table, sum)
plot(num_parts_by_parent_id) #This graph resembles the ggplot from above ggplot(num_parts_by_theme, aes(theme_id, num_parts, colors = parent_id)) + geom_point()

# Which parent ID has the most number of parts?
max(num_parts_by_parent_id$num_parts) #This just gives the value.
subset(num_parts_by_parent_id, num_parts == max(num_parts_by_parent_id$num_parts))
# parent_id num_parts
# 17       158  22279042  [Note: There is no name for parent ID, so we don't really know if there's a categorization tied to this ID.]

# What color shows up with highest frequency?
visual_colors <- count(master_join, "color_name")
ggplot(visual_colors, aes(color_name, freq)) +
  geom_point()
subset(visual_colors, freq == max(visual_colors$freq))
#   color_name   freq
#  3      Black 115176

# What color shows up with lowest frequency?
subset(visual_colors, freq == min(visual_colors$freq))
#      color_name freq
# 105 Trans Light Royal Blue    1


# Isolate one part to show how frequently it shows up in other sets.