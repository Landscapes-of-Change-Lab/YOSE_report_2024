## Load libraries ##

library(tidyverse)
library(here)
library(librarian)

## Joan's code chunk part 1 ##
# setting a theme? #
theme_set(
  theme_bw(base_size = 17)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, readxl, viridis, patchwork)


## Michelle set working directory ##
setwd("/Users/michellemohr/Desktop/YOSE_report_2024")
files <- list.files()
outdir <- "/Users/michellemohr/Desktop/YOSE_report_2024"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#PILA data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## reading in PILA data
folders <- list.dirs(outdir)[-c(1,4)]

pila_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "PILAdata")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>%
      dplyr::select(plotID, plot_type, treeNum, DBH_cm, slope, pitchTubes, exitHoles,
                    activeBranchCanker, inactiveBranchCanker,
                    activeBoleCanker, inactiveBoleCanker,
                    notes, plot_elevation_ft, trans_length, width,
                    percentLive, ribes_50m, ribes_100m, ribes_150m, ribes_200m)
    pila_list <- rbind(pila_list, xlsfile)
  }
}

### NOTE: picking up 1 NA row (will drop later in code - so only 3 plots have NAs as placeholders
### (NO PILA in these plots 7, 8, and 29)).
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#ASSOC data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## reading in ASSOC TREE data
folders <- list.dirs(outdir)[-c(1,4)]

assoc_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "YPE_Treedata")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>%
      dplyr::select(plot, treeNum, DBH_cm, height_m, species, notes, percentLive)
    assoc_list <- rbind(assoc_list, xlsfile)
  }
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#VEG data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## reading in VEG data
folders <- list.dirs(outdir)[-c(1,4)]

veg_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "YPE_Understory")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>%
      dplyr::select(plotID, species1, species2, species3, species4,
                    species5, species6, species7, species8, species9,
                    species10,
                    assoc1, assoc2, assoc3, assoc4, assoc5,
                    assoc6, assoc7, assoc8, assoc9, assoc10)
    veg_list <- rbind(veg_list, xlsfile)
  }
}

## NOTE: Jenny provided code to add in the rest of the veg data columns.

#species11, species12, species13, species14,
#species15, species16, species17, species18, species19,
#species20, species21, species22, species23, species24,
#species25, species26, species27, species28, species29,

#assoc11, assoc12,assoc13, assoc14, assoc15, assoc16, assoc17, assoc18,
#assoc19, assoc20, assoc21, assoc22, assoc23, assoc24,
#assoc25, assoc26, assoc27, assoc28,
#assoc29, assoc30, assoc31, assoc32, assoc33


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#FIRE data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

firedata <- read_csv(here("FIREdata.csv"))

firedataCompare <-read_csv(here("fireCompare.csv"))

fire_severity <- firedataCompare %>%
  select(plotID, Fire_Severity_plotAverge)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#CLIMATE data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

climatedata <- read_csv(here("updatedPRISMdata.csv"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#Changing plot numbers to have 1-59
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# create plot_num column which will change the plots to numerical order#

pila_list <- pila_list %>%
  mutate(plotNum = dense_rank(plotID))

assoc_list <- assoc_list %>%
  mutate(plotNum = dense_rank(plot))

veg_list <- veg_list %>%
  mutate(plotNum = dense_rank(plotID))

climatedata <- climatedata %>%
  mutate(plotNum = dense_rank(PlotID))

fire_severity <- fire_severity %>%
  mutate(plotNum = dense_rank(plotID))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#DROPPING CERTAIN ROWS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Drop rows where plotID column has NA values
pila_list <- pila_list[!is.na(pila_list$plotID), ]

# Drop specific row for plotNum 48 treeNum 11
pila_list <- pila_list %>%
  filter(!(plotNum == 48 & treeNum == 11))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#ALIVE STATUS using pila_list
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#add new column to pila_list dataframe called "alive_status"
pila_list <- pila_list %>%
  mutate(alive_status = ifelse(percentLive == 0, "Dead", "Alive"))

#create new dataframe called 'pila_counts'
pila_counts <- pila_list %>%
  group_by(alive_status) %>%
  summarize(count = n())

# There are 1730 pila trees (+3 NAs)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#ALIVE STATUS using assoc_list
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#add new column to pila_list dataframe called "alive_status"
assoc_list <- assoc_list %>%
  mutate(alive_status = ifelse(percentLive == 0, "Dead", "Alive"))

#create new dataframe called 'assoc_counts'
assoc_counts <- assoc_list %>%
  group_by(alive_status) %>%
  summarize(count = n())

# There are 3310 assoc trees (+1 NA)


######## IMPORTANT STATS #############
# Looks like we have three plots without PILA 7, 8, and 25 (old YPE 29)
# Total trees says 1734 but really is probably 1730 ( 4 NAs are placeholders for plots 7, 8, and 25 without PILA data. 1 row is NAs being picked up)

# Figure out how many trees are alive vs dead for each plot
# Create new dataframe pilaplots_list with selected columns from pila_list
pilaplots_list <- pila_list[, c("plotNum", "treeNum", "alive_status")]

# Create new columns Alive and Dead based on alive_status
pilaplots_list <- pilaplots_list %>%
  mutate(Alive = ifelse(alive_status == "Alive", 1, 0),
         Dead = ifelse(alive_status == "Alive", 0, 1)) %>%
  select(-alive_status)  # Remove the original alive_status column if needed

# Create new column plot_alive with the sum of alive trees per plotNum
pilaplots_list <- pilaplots_list %>%
  group_by(plotNum) %>%
  mutate(plot_alive = sum(Alive))

# Create new column plot_dead with the sum of dead trees per plotNum
pilaplots_list <- pilaplots_list %>%
  group_by(plotNum) %>%
  mutate(plot_dead = sum(Dead))

####
pilaplots <- pilaplots_list %>%
  distinct(plotNum, plot_alive, plot_dead)


# Create a bar plot of alive vs dead individuals per plot
pilaalive_plot <- ggplot(pilaplots_list, aes(x = as.factor(plotNum))) +
  geom_bar(aes(y = plot_alive, fill = "Alive"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = plot_dead, fill = "Dead"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Alive" = "blue", "Dead" = "red")) +
  labs(x = "Plot Number", y = "Number of Individuals", fill = "Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Display the plot
print(pilaalive_plot)


### IMPORTANT ########
# 3 plots no PILA
#(plotNum 7, 8, and 25)
# 4 plots with only DEAD PILA
#(plotNum 5: 1 dead tree, 32: 1 dead tree, and 44: 6 dead trees)
### Question: why is this plot showing that I have more than 4 plots with only dead PILA??
#### Answer: redo the plot to show dead and alive side by side instead of stacked

# Assume pilaplots_list is already defined as per your previous steps
# Calculate the number of alive and dead trees per plot
plot_summary <- pilaplots_list %>%
  group_by(plotNum) %>%
  summarise(plot_alive = sum(Alive),
            plot_dead = sum(Dead)) %>%
  pivot_longer(cols = c(plot_alive, plot_dead), names_to = "status", values_to = "count")

# Create a bar plot of alive vs dead individuals per plot
pilaalive_plot <- ggplot(plot_summary, aes(x = as.factor(plotNum), y = count, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("plot_alive" = "blue", "plot_dead" = "red"), labels = c("Alive", "Dead")) +
  labs(x = "Plot Number", y = "Number of Individuals", fill = "Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Print the plot
print(pilaalive_plot)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
###########       WPBR PILA infection     #################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# Create a new dataframe with selected columns from pila_list
pilaWPBR <- pila_list[, c("plotNum", "treeNum", "plot_type", "DBH_cm", "activeBoleCanker", "activeBranchCanker", "inactiveBoleCanker", "inactiveBranchCanker", "alive_status")]


# Drop rows with any NA values in the selected columns
pilaWPBR <- na.omit(pilaWPBR)

# Create the new column WPBRinfected
pilaWPBR$WPBRinfected <- as.integer(rowSums(pilaWPBR[, c("activeBoleCanker", "activeBranchCanker", "inactiveBoleCanker", "inactiveBranchCanker")]) > 0)

# Group by plotNum and calculate the sum of WPBRinfected per plot
pilaWPBR$plotWPBRinfected <- ave(pilaWPBR$WPBRinfected, pilaWPBR$plotNum, FUN = sum)

# Create new column with the number of unique treeNum per plot
pilaWPBR <- pilaWPBR %>%
  group_by(plotNum) %>%
  mutate(trees_plot = n_distinct(treeNum))

# Create new column with the number of trees in the whole study
pilaWPBR <- pilaWPBR %>%
  mutate(total_trees = nrow(.))

#### work with new pilaWPBRclean
# Define the plots to exclude
plots_to_exclude <- c(5, 7, 8, 25, 32, 44)

# Create a new dataframe excluding the specified plots
pilaWPBRclean <- pilaWPBR %>%
  filter(!plotNum %in% plots_to_exclude)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############# INCIDENCE ####### EXTENT ######### SEVERITY ############ INFECTION RATE ######################
#Incidence (Percent) =
  ### number of individuals in the plot with infections/number of trees in the plot * 100
#Extent =
  ### number of plots with at least one infection/total number of plots surveyed
#Severity =
  ### FORMULA ### cs = (25 - DBH) / 5
#Infection Rate =
  ### number of trees in plot with WPBR/number of PILA in the plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##### Incidence Percent ######

# Create new column called incidencepercent using the number of indiv. infected within a plot (plotWPBRinfected)
# divided by the total number of PILA in the plot (trees_plot)
# and multiply it by 100 to get percent

# Create new column incidencepercent
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(incidencepercent = (plotWPBRinfected / trees_plot) * 100)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############# Extent BEGIN ###################
# ### number of plots with at least one infection/total number of plots surveyed
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Create the WPBRone column indicating if a tree has at least one infection
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(WPBRone = ifelse(activeBoleCanker > 0 |
                            activeBranchCanker > 0 |
                            inactiveBoleCanker > 0 |
                            inactiveBranchCanker > 0,
                          1, 0))

# Now summarize to determine if any tree in the plot is infected
plotWPBRone <- pilaWPBRclean %>%
  group_by(plotNum) %>%
  summarize(plotWPBRone = as.integer(any(WPBRone == 1)), .groups = "drop")

# View the resulting summary
print(plotWPBRone)

# Sum the values in the plotWPBRone column
total_WPBRone <- sum(plotWPBRone$plotWPBRone, na.rm = TRUE)

# Print the result
print(total_WPBRone)

# Count the number of unique plots in the plotNum column
num_unique_plots <- n_distinct(plotWPBRone$plotNum)

# Print the result
print(num_unique_plots)


###### IMPORTANT EXTENT NOTES ########
# 27 plots have at least one infection.
# 26 plots do not have any infections.
# There are a total of 53 plots
# (original plot total of 59, but plots 5, 7, 8, 25, 32, and 44 either have no PILA or only dead PILA)

# EXTENT =
#### 27/53 plots with infection = 50.94%

# Step 1: Summarize by plotNum to calculate if each plot has any WPBR infection (1 = infected, 0 = not infected)
plot_extent_summary <- plotWPBRone %>%
  group_by(plotNum) %>%
  summarise(WPBR_infection = ifelse(sum(plotWPBRone) > 0, 1, 0))

# Step 2: Count the number of infected vs. non-infected plots
plot_extent_counts <- plot_extent_summary %>%
  group_by(WPBR_infection) %>%
  summarise(count = n())

# Step 3: Create the bar plot showing WPBR infection extent

#visual 1
ggplot(plot_extent_counts, aes(x = factor(WPBR_infection, labels = c("Not Infected", "Infected")), y = count, fill = factor(WPBR_infection))) +
  geom_bar(stat = "identity") +
  labs(x = "WPBR Infection Status", y = "Number of Plots", title = "Extent of WPBR Infection Across Plots", fill = "WPBR Infection") +
  scale_fill_manual(values = c("Not Infected" = "lightblue", "Infected" = "red")) +
  theme_minimal()

#visual 2
ggplot(plot_extent_counts, aes(x = factor(WPBR_infection, labels = c("Not Infected", "Infected")), y = count, fill = factor(WPBR_infection))) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +  # Add black border, adjust bar width
  labs(x = "WPBR Infection Status", y = "Number of Plots", title = "Extent of WPBR Infection Across Plots", fill = "WPBR Infection") +
  scale_fill_manual(values = c("Not Infected" = "lightblue", "Infected" = "red")) +
  scale_y_continuous(breaks = seq(0, max(plot_extent_counts$count), by = 5)) +  # Add more y-axis ticks
  theme_minimal()

#visual 3
ggplot(plot_extent_counts, aes(x = factor(WPBR_infection, labels = c("Not Infected", "Infected")), y = count)) +
  geom_bar(stat = "identity", aes(fill = factor(WPBR_infection)), color = "black", width = 0.7) +  # Use aes for fill inside geom_bar
  labs(x = "WPBR Infection Status", y = "Number of Plots", title = "Extent of WPBR Infection Across Plots", fill = "WPBR Infection") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"), labels = c("Not Infected", "Infected")) +  # Custom colors for bars
  scale_y_continuous(breaks = seq(0, max(plot_extent_counts$count), by = 5)) +  # More y-axis ticks
  theme_minimal()
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############## Extent END ####################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############## Severity BEGIN ###############   #### SEVERITY NEEDS WORK TALK TO JOAN ABOUT FORMULA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ### severity = cs = (25 - DBH) / 5
## cs =
# 0 -> 0 branch cankers
# 1 -> 1 -3 branch cankers
# 2 -> 4 - 9 branch cankers
# 3 -> 10 - 25 branch cankers
# 4 -> 25 or more branch cankers
# 5 -> bole canker

# NOTE: ask Joan about formula -->
# * if tree diameter is greater than 25 inches, then DBH = 25 *
# * unless cs = 0, then S = 0 *
### note: I am going to have to change the DBH column from cm -> inches

#### NEED TO DO THIS WITH ONLY LIVE TREES ####

###
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(
    # Calculate total branch cankers
    total_branch_cankers = activeBranchCanker + inactiveBranchCanker,

    # Assign canker severity based on the new criteria
    cankerseverity = case_when(
      activeBoleCanker == TRUE | inactiveBoleCanker == TRUE ~ 5,  # Presence of bole canker takes priority
      total_branch_cankers == 1 ~ 1,  # 1 branch canker = severity 1
      total_branch_cankers == 2 ~ 2,  # 2 branch cankers = severity 2
      total_branch_cankers >= 3 & total_branch_cankers <= 4 ~ 3,  # 3-4 branch cankers = severity 3
      total_branch_cankers >= 5 & total_branch_cankers <= 7 ~ 4,  # 5-7 branch cankers = severity 4
      TRUE ~ 0  # No branch cankers or other cases = severity 0
    )
  )



#### Look at the distribution of cankers #####

# Create a histogram to show the distribution of total_branch_cankers
# Create a histogram to show the distribution of total_branch_cankers
ggplot(pilaWPBRclean, aes(x = total_branch_cankers)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total Branch Cankers",
       x = "Total Branch Cankers",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )

# Create a histogram excluding total_branch_cankers == 0
ggplot(pilaWPBRclean %>% filter(total_branch_cankers > 0), aes(x = total_branch_cankers)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total Branch Cankers (Excluding 0)",
       x = "Total Branch Cankers",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )


### Look at the distribution of canker severity ###
# Create a bar plot to show the distribution of canker severity
ggplot(pilaWPBRclean, aes(x = factor(cankerseverity))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Canker Severity",
    x = "Canker Severity",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# Create a bar plot to show the distribution of canker severity, excluding 0
ggplot(pilaWPBRclean %>% filter(cankerseverity != 0), aes(x = factor(cankerseverity))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Canker Severity (Excluding 0)",
    x = "Canker Severity",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )


### Clean DBH for severity formula
### note: I am going to have to change the DBH column from cm -> inches

pilaWPBRclean <- pilaWPBRclean %>%
  mutate(DBH_in = DBH_cm * 0.393701)

## note: I'm going to need to make a new DBH column that takes the original DBH_in column and makes anything that is above 50 = 50

# Create a new DBH column where any value greater than 50 is set to 50
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(DBH_sevformula = ifelse(DBH_in > 50, 50, DBH_in))

#### Severity Formula
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(
    severity = if_else(cankerseverity == 0, 0, (cankerseverity + (50 - DBH_sevformula)) / 5)
  )
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################ Severity END ################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
########## Infection Rate BEGIN ##############
# ### number of trees in plot with WPBR/number of PILA in the plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### NEED TO DO THIS WITH ONLY LIVE TREES ####

pilaWPBRclean <- pilaWPBRclean %>%
  mutate(infectionrate = (plotWPBRinfected / trees_plot))

# Step 1: Filter for only alive trees
alive_trees_data <- pilaWPBRclean %>%
  filter(alive_status == "Alive")  # Assuming "alive_status" column is present

# Step 2: Group by plotNum and calculate the infection rate for each plot
alive_infection_rate <- alive_trees_data %>%
  group_by(plotNum) %>%
  summarise(
    total_alive_trees = n(),  # Count of alive trees in each plot
    infected_alive_trees = sum(WPBRinfected == 1, na.rm = TRUE),  # Count of infected alive trees
    infection_rate_alive = infected_alive_trees / total_alive_trees  # Infection rate
  )

# Step 3: Inspect the result
alive_infection_rate

ggplot(alive_infection_rate, aes(x = factor(plotNum), y = infected_alive_trees, fill = infected_alive_trees)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  labs(
    title = "Number of Infected Alive Trees per Plot",
    x = "Plot Number",
    y = "Number of Infected Trees"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

# Calculate total number of infected live trees
total_infected_alive_trees <- sum(alive_infection_rate$infected_alive_trees, na.rm = TRUE)

# Print the result
total_infected_alive_trees

# Calculate the total number of alive trees
total_alive_trees <- pilaWPBRclean %>%
  filter(alive_status == "Alive") %>%   # Filter for alive trees
  nrow()                                # Count the number of rows

# Print the total number of alive trees
total_alive_trees


#### IMPORTANT STATS: 57/1400 alive trees are infected = 4.07% infection ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############ Infection Rate END #############
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

###### Let's separate random and high severity plots ##########
###### to see how infection rates change ########

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#$$$$$$$$$$$$$$$$$$$$$$#
      # EXTENT #
#$$$$$$$$$$$$$$$$$$$$$$#
# Step 1: Summarize the number of plots with WPBR infections by plot type
plottype_summary <- pilaWPBRclean %>%
  group_by(plotNum, plot_type) %>%
  summarize(wpbr_present = sum(WPBRinfected > 0)) %>%  # 1 if WPBR is present, 0 otherwise
  ungroup() %>%
  group_by(plot_type) %>%
  summarize(
    total_plots = n_distinct(plotNum),      # Count total plots
    plots_with_wpbr = sum(wpbr_present > 0) # Count plots with WPBR presence
  )

# View the summary table
print(plottype_summary)

#### IMPORTANT STATS:

# 52 total plots??? why is one missing

# random -> 36 plots total (22 of the random plots have infections)
## random 22/36 = 61.1%

# high severity -> 16 plots total (5 of the high severity plots have infections)
## high severity 5/16 = 31.25%

### both random and high severity = 27/52 = 51.9%

#$$$$$$$$$$$$$$$$$$$$$$#
   # INFECTION RATE #
#$$$$$$$$$$$$$$$$$$$$$$#
# Step 1: Group by plotNum and plot_type, calculate infection rate for each plot
plottypeWPBRtreesummary <- pilaWPBRclean %>%
  group_by(plotNum, plot_type) %>%
  summarize(
    trees_plot = n(),                             # Total number of trees in the plot
    plotWPBRinfected = sum(WPBRinfected > 0)      # Number of infected trees in the plot
  ) %>%
  mutate(
    infectionrate = plotWPBRinfected / trees_plot  # Calculate infection rate
  ) %>%
  ungroup()

# View the result
print(plottypeWPBRtreesummary)

# OR #

# Step 2: Calculate average infection rate per plot type
infectionrate_summary <- pilaWPBRclean %>%
  group_by(plot_type) %>%
  summarize(
    total_plots = n_distinct(plotNum),            # Total number of plots in each plot type
    avg_infectionrate = mean(infectionrate, na.rm = TRUE)  # Average infection rate per plot type
  )

# View the infection rate summary
print(infectionrate_summary)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
###########         PLOT CHARACTERISTICS        ############
# objective: create a table that has the following
### DBH
### height
### presence/absence of Ribes
### number of standing dead trees
### number of trees with signs of DEPO/DEVA/weevils
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#RIBES pila_list
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### work with new pila_list_clean
# Define the plots to exclude
plots_to_exclude <- c(5, 7, 8, 25, 32, 44)

# Create a new dataframe excluding the specified plots
pila_list_clean <- pila_list %>%
  filter(!plotNum %in% plots_to_exclude)

# create a ribes A/P (0/1) column #
pila_list_clean <- pila_list_clean %>%
  mutate(ribes = ifelse(
    !is.na(ribes_50m) & !(ribes_50m %in% c("N", "None", "NONE")) |
      !is.na(ribes_100m) & !(ribes_100m %in% c("N", "None", "NONE")) |
      !is.na(ribes_150m) & !(ribes_150m %in% c("N", "None", "NONE")) |
      !is.na(ribes_200m) & !(ribes_200m %in% c("N", "None", "NONE")),
    1, 0))


#create new dataframe called 'ribespila_counts'
ribespila_counts <- pila_list_clean %>%
  group_by(plotNum) %>%
  summarize(ribes_plot = ifelse(any(ribes == 1), 1, 0))

# Find the sum of ribes_plot
ribes_plot_sum <- sum(ribespila_counts$ribes_plot)

############# Important Stats ################
### 36 out of 53 plots have ribes = 67.92% ###

#### Create visuals #####
# Create a summary dataframe for plotting
ribes_plot_summary <- ribespila_counts %>%
  group_by(ribes_plot) %>%
  summarize(count = n(), .groups = "drop")  # Count the number of plots for each presence/absence category

# Create the bar plot
ggplot(ribes_plot_summary, aes(x = factor(ribes_plot, labels = c("Absent", "Present")), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Plots with Ribes", x = "Ribes Presence", y = "Number of Plots") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14, face = "bold"))

###### VISUAL OF RIBES AND WPBR #######
# Assuming `pila_list_clean` includes a column for WPBR status (e.g., wpbr_present)
# Modify this based on your actual WPBR column name

# Create wpbr_present column in pila_list_clean
pila_list_clean <- pila_list_clean %>%
  mutate(wpbr_present = ifelse(
    activeBoleCanker > 0 |
      activeBranchCanker > 0 |
      inactiveBoleCanker > 0 |
      inactiveBranchCanker > 0,
    1, 0
  ))

# Create a new dataframe to combine Ribes and WPBR information
ribes_wpbr_combined <- pila_list_clean %>%
  mutate(ribes_presence = ifelse(ribes == 1, "Present", "Absent"),
         wpbr_presence = ifelse(wpbr_present == TRUE, "Present", "Absent")) %>%
  group_by(plotNum) %>%
  summarize(ribes_status = first(ribes_presence),
            wpbr_status = first(wpbr_presence), .groups = "drop")

# Create a summary table for plotting
plot_summary <- ribes_wpbr_combined %>%
  count(ribes_status, wpbr_status) %>%
  mutate(status = paste(ribes_status, wpbr_status))

### Visual 1 ###
# Create the visual using ggplot2
ggplot(plot_summary, aes(x = status, y = n, fill = status)) +
  geom_bar(stat = "identity") +
  labs(title = "Plots with Ribes and WPBR Status", x = "Ribes and WPBR Status", y = "Number of Plots") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14, face = "bold"))

##### NOTE: Not sure this is correctly counting how many plots have both ribes and wpbr
####### SIDE NOTE: it would be cool to make a
######### tmap showing which plots have ribes and which plots have wpbr
########## to show where infections are occuring spatially and the interaction with ribes

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#BEETLES pila_list
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# pila_list_clean <- pila_list_clean %>%
#   mutate(
#     # Create a new column 'beetles_present'
#     beetles_treespresent = ifelse(
#       (!is.na(pitchTubes) & !(pitchTubes %in% c("N", "NONE"))) |  # Check pitchTubes
#         (!is.na(exitHoles) & !(exitHoles %in% c("N", "NONE"))),     # Check exitHoles
#       1,  # Assign 1 if either column contains anything other than 'N' or 'NONE'
#       0   # Assign 0 if both columns contain 'N', 'NONE', or are missing
#     )
#   )

pila_list_clean <- pila_list_clean %>%
  mutate(
    # Create a new column 'beetles_treespresent' for DEPO presence
    beetles_treespresent = ifelse(
      (!is.na(pitchTubes) & pitchTubes == "DEPO") |  # Check if pitchTubes contains 'DEPO'
        (!is.na(exitHoles) & exitHoles == "DEPO"),     # Check if exitHoles contains 'DEPO'
      1,  # Assign 1 if either column contains 'DEPO'
      0   # Assign 0 otherwise
    )
  )


# View the updated dataframe
print(pila_list_clean)

# Create a new column 'beetles_present' at the plot level
pila_list_clean <- pila_list_clean %>%
  group_by(plotNum) %>%
  mutate(
    # Create a new column 'beetles_present' to indicate presence in the plot
    beetles_present = ifelse(
      any((!is.na(pitchTubes) & !(pitchTubes %in% c("N", "NONE"))) |  # Check pitchTubes
            (!is.na(exitHoles) & !(exitHoles %in% c("N", "NONE")))),  # Check exitHoles
      1,  # Assign 1 if either column contains anything other than 'N' or 'NONE' in any tree
      0   # Assign 0 if both columns contain 'N', 'NONE', or are missing in all trees
    )
  ) %>%
  ungroup()  # Remove grouping after calculation



## VISUALS ##
# Summarize data by plotNum and beetles_present
beetles_summary <- pila_list_clean %>%
  group_by(plotNum) %>%
  summarize(beetles_present_plot = ifelse(any(beetles_present == 1), 1, 0))  # 1 if beetles present in the plot

# Count how many plots have beetles and how many don't
beetles_plot_count <- beetles_summary %>%
  group_by(beetles_present_plot) %>%
  summarize(n = n())

# Create a bar plot showing the number of plots with and without beetles
ggplot(beetles_plot_count, aes(x = factor(beetles_present_plot), y = n, fill = factor(beetles_present_plot))) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Plots with and without Beetles", x = "Beetles Present (1 = Yes, 0 = No)", y = "Number of Plots") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "orange"), name = "Beetles Present") +
  theme_minimal()


#### SEPARATE ALIVE VS DEAD IN EACH PLOT #####

pila_list_clean <- pila_list_clean %>%
  mutate(
    alive_trees = ifelse(alive_status == "Alive", 1, 0),  # Assign 1 if tree is alive, otherwise 0
    dead_trees = ifelse(alive_status == "Dead", 1, 0)     # Assign 1 if tree is dead, otherwise 0
  )


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#FIRE pila_list
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

fire_severity <- fire_severity %>%
  mutate(fire_present = ifelse(Fire_Severity_plotAverge %in% 1:3, 1, 0))

pilalist_climate_wpbr <- pilalist_climate_wpbr %>%
  left_join(fire_severity %>% select(plotNum, fire_present, Fire_Severity_plotAverge),
            by = "plotNum")


############## PILA PLOT CHARACTERISTICS DATAFRAME #######################
## Need to create a table
### ribes_plot (present in plot?)
### wpbr_plot (present in plot?)
### beetles_plot (present in plot?)
### alive_plot (how many trees are alive in plot)
### dead_plot (how many trees are dead in plot)

pila_plotcharacteristics <- pila_list_clean %>%
  select(plotNum, treeNum, DBH_cm, ribes, wpbr_present, beetles_present, alive_trees, dead_trees)


install.packages("gt")
library(gt)

# Create a table using gt
pila_plotcharacteristics %>%
  gt() %>%
  tab_header(
    title = "Plot Characteristics Table",
    subtitle = "Ribes, WPBR, Beetles, and Tree Status by Plot"
  ) %>%
  cols_label(
    plotNum = "Plot Number",
    ribes = "Ribes Present",
    wpbr_present = "WPBR Present",
    beetles_present = "Beetles Present",
    alive_trees = "Alive Trees",
    dead_trees = "Dead Trees"
  ) %>%
  fmt_number(columns = vars(alive_trees, dead_trees), decimals = 0) %>%
  tab_options(
    table.width = pct(80),
    table.font.size = 12
  )


##### VISUALIZE PLOT CHARACTERISTICS ######
# Create a summary dataframe to count the presence of Ribes, WPBR, and Beetles in each plot
plotcharacteristics_summary <- pila_plotcharacteristics %>%
  group_by(plotNum) %>%
  summarize(
    ribes_present = max(ribes, na.rm = TRUE),  # Assuming ribes is a binary column (0 or 1)
    wpbr_present = max(wpbr_present, na.rm = TRUE),  # Same for wpbr_present
    beetles_present = max(beetles_present, na.rm = TRUE)  # Same for beetles_present
  ) %>%
  pivot_longer(cols = c(ribes_present, wpbr_present, beetles_present),
               names_to = "Characteristic",
               values_to = "Present") %>%
  filter(Present == 1)  # Only keep rows where the characteristic is present

# Create the plot
ggplot(plotcharacteristics_summary, aes(x = Characteristic, fill = Characteristic)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Presence of Ribes, WPBR, and Beetles in Plots",
       x = "Characteristic",
       y = "Number of Plots") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        title = element_text(size = 14, face = "bold"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# AREA CODE ############
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# Load required libraries
library(sf)
library(ggplot2)
library(sp)
library(readr)
library(dplyr)
library(tmap)  # For interactive mapping

# Step 1: Load Data and Ensure Valid Format
waypoints <- read_csv("pila_datum.csv") %>%
  mutate(
    plot_beg_UTM_E = as.numeric(plot_beg_UTM_E),
    plot_beg_UTM_N = as.numeric(plot_beg_UTM_N),
    zone = as.numeric(zone)
  ) %>%
  drop_na(plot_beg_UTM_E, plot_beg_UTM_N, zone)  # Remove rows with NA values

# Step 2: Convert UTM to Lat/Lon
convert_utm_to_latlon <- function(easting, northing, zone) {
  proj_string <- paste0("+proj=utm +zone=", zone, " +datum=WGS84")
  coords <- SpatialPoints(cbind(easting, northing), proj4string = CRS(proj_string))
  latlon <- spTransform(coords, CRS("+proj=longlat +datum=WGS84"))
  return(data.frame(lat = latlon@coords[, 2], lon = latlon@coords[, 1]))
}

latlon_coords <- do.call(rbind, mapply(function(easting, northing, zone) {
  tryCatch({
    convert_utm_to_latlon(easting, northing, zone)
  }, error = function(e) {
    return(data.frame(lat = NA, lon = NA))  # Handle conversion errors
  })
}, waypoints$plot_beg_UTM_E, waypoints$plot_beg_UTM_N, waypoints$zone, SIMPLIFY = FALSE))

waypoints <- cbind(waypoints, latlon_coords) %>%
  drop_na(lat, lon)  # Remove rows with invalid lat/lon

# Step 3: Create sf Object and Reproject CRS
waypoints_sf <- st_as_sf(waypoints, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 32611)  # Change CRS to your specific UTM zone

# Step 4: Calculate Convex Hull and Area for Each Plot
plot_areas <- waypoints_sf %>%
  group_by(plotID) %>%
  summarise(
    convex_hull = st_convex_hull(st_union(geometry)),
    area_sq_m = st_area(convex_hull)
  ) %>%
  mutate(area_sq_km = as.numeric(area_sq_m) / 1e6) %>%  # Convert to square kilometers
  st_set_geometry(NULL)  # Remove geometry column for merging

# Step 5: Join Areas Back to Original Data
waypoints_with_area <- waypoints %>%
  left_join(plot_areas, by = "plotID")

# Step 6: Create Interactive Map with tmap
waypoints_sf_with_area <- st_as_sf(waypoints_with_area, coords = c("lon", "lat"), crs = 4326)

tmap_mode("view")  # Set tmap to interactive mode

# Create a map showing all waypoints
tm_shape(waypoints_sf_with_area) +
  tm_dots(col = "plotID", palette = "Set1", title = "Plot ID") +
  tm_layout(legend.position = c("left", "bottom"))

# Step 7: Visualize Results with ggplot
ggplot() +
  geom_sf(data = waypoints_sf_with_area, aes(color = factor(plotID))) +
  geom_sf(data = st_as_sf(plot_areas, crs = 4326), aes(fill = area_sq_km), color = "blue", alpha = 0.2) +
  ggtitle("Waypoints with Convex Hull by PlotID") +
  theme(legend.position = "right")

# Optional: Save the Updated Data
write_csv(waypoints_with_area, "pila_datum_with_area.csv")

######### MORE ################
#issue with calculating areas and projections of plots in wrong spots

# Read the CSV file containing the waypoints data
waypoints <- read.csv("pila_datum.csv")

# Convert the UTM columns to numeric (this will introduce NAs for any non-numeric values)
waypoints$PILA_UTM_E <- as.numeric(waypoints$PILA_UTM_E)
waypoints$PILA_UTM_N <- as.numeric(waypoints$PILA_UTM_N)

# Remove rows with missing or invalid UTM coordinates
waypoints_clean <- waypoints %>%
  filter(!is.na(PILA_UTM_E) & !is.na(PILA_UTM_N))

# Convert the cleaned waypoints to an sf object (assuming UTM zone 11N, EPSG: 32611)
waypoints_sf <- st_as_sf(waypoints_clean, coords = c("PILA_UTM_E", "PILA_UTM_N"), crs = 32611)

# Group by plotID, calculate convex hull, and compute area in square meters
plot_areas <- waypoints_sf %>%
  group_by(plotID) %>%
  summarise(
    convex_hull = st_convex_hull(st_union(geometry)),  # Calculate the convex hull of the plot
    area_sq_m = st_area(convex_hull)  # Calculate the area of the convex hull in square meters
  )

# Print the area for each plot
print(plot_areas)

# Optional: Visualize the plot and convex hull
ggplot() +
  geom_sf(data = waypoints_sf, aes(color = factor(plotID))) +  # Plot the tree waypoints
  geom_sf(data = plot_areas, aes(fill = factor(plotID)), alpha = 0.2) +  # Plot the convex hulls
  ggtitle("Plot Convex Hulls and Tree Waypoints") +
  theme_minimal()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#LENGTH WIDTH CODE #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


########################################
# Load required libraries
library(sf)
library(dplyr)

# Ensure waypoints_clean is an sf object with the correct CRS
waypoints_clean_sf <- st_as_sf(waypoints_clean, coords = c("PILA_UTM_E", "PILA_UTM_N"), crs = 32611)

# Step 1: Define the starting waypoint (waypoint_beg) for each plot
waypoints_clean_sf <- waypoints_clean_sf %>%
  group_by(plotID) %>%
  mutate(
    waypoint_beg = st_sfc(st_point(c(first(plot_beg_UTM_E), first(plot_beg_UTM_N))), crs = 32611)
  ) %>%
  ungroup()

# Step 2: Function to calculate the maximum distance within a plotID
calculate_distances_within_plot <- function(geometry, waypoint_beg) {
  # Calculate distances from the starting waypoint to all other waypoints in the same plot
  dist_from_beg <- st_distance(waypoint_beg, geometry)  # Distance from the starting waypoint
  furthest_distance <- max(dist_from_beg)  # Maximum distance from waypoint_beg within the same plot
  return(furthest_distance)
}

# Step 3: Group by plotID and calculate length, width, and area for each plot
plot_areas <- waypoints_clean_sf %>%
  group_by(plotID) %>%
  summarise(
    waypoint_beg = first(waypoint_beg),  # Store the starting waypoint
    length_m = calculate_distances_within_plot(geometry, first(waypoint_beg)),  # Maximum distance for length within plot
    # Calculate the maximum width between the two furthest waypoints within the same plot
    width_m = max(as.numeric(st_distance(geometry)), na.rm = TRUE),  # Maximum distance between waypoints within plot
    area_sq_m = length_m * width_m,  # Compute area as length * width
    area_sq_km = area_sq_m / 1e6  # Convert to square kilometers
  ) %>%
  ungroup()  # Ungroup after summarization

# View results
print(plot_areas)


#### Manually change some widths based on what the notes say (dSide)

# plot 9 width should be 100
# plot 31 width should be 100
# plot 37 width (only one tree and the notes say it is too old to assess, did we eliminate this plot???)
# plot 68 width should be 105.2


# Convert width_m to numeric if it's not
plot_areas$width_m <- as.numeric(plot_areas$width_m)

# Then run the mutate
plot_areas <- plot_areas %>%
  mutate(width_m = case_when(
    plotID == 9 ~ 100,
    plotID == 31 ~ 100,
    plotID == 68 ~ 105.2,
    TRUE ~ width_m
  ))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# CUSTOM CODE FOR PLOT 48 USING ONLY TREE WAYPOINTS (EXCLUDING treeNum 1) #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Filter tree waypoints for plotID 48, excluding treeNum 1
plot_48_tree_waypoints <- waypoints_clean_sf %>%
  filter(plotID == 48 & !is.na(PILA_waypoint) & treeNum != 1)  # Exclude treeNum 1

# Recalculate length and width for plotID 48 using only tree waypoints (excluding treeNum 1)
plot_48_custom <- plot_48_tree_waypoints %>%
  summarise(
    plotID = first(plotID),  # Keep plotID
    length_m = calculate_distances_within_plot(geometry, first(geometry)),  # Recalculate length using only tree waypoints
    width_m = max(as.numeric(st_distance(geometry)), na.rm = TRUE),  # Recalculate width using only tree waypoints
    area_sq_m = length_m * width_m,  # Compute new area as length * width
    area_sq_km = area_sq_m / 1e6  # Convert to square kilometers
  )


#####$$$$$$$$$$$$$$$$$$$$######################
# Now I need to bring this data back in with the rest of the pila data
# plotID is the old plotID so match them that way

# Join length_m and width_m from plot_areas to pila_list_clean based on plotID
pila_list_clean <- pila_list_clean %>%
  left_join(plot_areas %>% select(plotID, length_m, width_m), by = "plotID")




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
##### Climate Data Analysis #########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


pilalist_climate <- left_join(pila_list_clean, climatedata, by = c("plotID" = "PlotID"))

# Drop specific columns from the dataframe
pilalist_climate <- pilalist_climate %>%
  select(-plotNum.y)

pilalist_climate <- pilalist_climate %>%
  rename(plotNum = plotNum.x)

####
# Aggregate pilaWPBR data by plotNum, taking the mean of incidencepercent
pilaWPBR_aggregated <- pilaWPBRclean %>%
  group_by(plotNum) %>%
  summarise(incidencepercent = mean(incidencepercent, na.rm = TRUE))

# Perform left join
pilalist_climate <- left_join(pilalist_climate, pilaWPBR_aggregated, by = "plotNum")


VPD <- ggplot(pilalist_climate, aes(x = vpdmax, y = incidencepercent)) +
  geom_point()+
  geom_smooth(method = "lm") +
  labs(x = "VPD Max", y = "Incidence %") +  # Labels for axes
  ggtitle("VPD Max and Incidence") +
  theme_minimal()
VPD

TEMP <- ggplot(pilalist_climate, aes(x = tmean, y = incidencepercent)) +
  geom_point()+
  geom_smooth(method = "lm") +
  labs(x = "Mean", y = "Incidence %") +  # Labels for axes
  ggtitle("Mean Temperature and Incidence") +
  theme_minimal()
TEMP

PRECIP <- ggplot(pilalist_climate, aes(x = ppt, y = incidencepercent)) +
  geom_point()+
  geom_smooth(method = "lm") +
  labs(x = "Precipitation", y = "Incidence %") +  # Labels for axes
  ggtitle("Precipitation and Incidence") +
  theme_minimal()
PRECIP

ELEV <- ggplot(pilalist_climate, aes(x = plot_elevation_ft, y = incidencepercent)) +
  geom_point()+
  geom_smooth(method = "lm") +
  labs(x = "Elevation", y = "Incidence %") +  # Labels for axes
  ggtitle("Elevation and Incidence") +
  theme_minimal()
ELEV

### The combined graph is not showing
install.packages("cowplot")
library(cowplot)

combinedclimategraphs <- VPD + TEMP + PRECIP + ELEV
combinedclimategraphs

# Calculate linear regression model
lm_modelclimate <- lm(incidencepercent ~ vpdmax, data = pilalist_climate)


install.packages("ggpubr")
library(ggpubr)
## Create VPD plot ##
VPDscatter_plot <- ggscatter(pilalist_climate, x = "vpdmax", y = "incidencepercent",
                             color = "black", shape = 20, size = 3,
                             add = "reg.line",
                             add.params = list(color = "blue", fill = "lightgray"),
                             conf.int = TRUE,
                             cor.coef = TRUE,
                             cor.coeff.args = list(method = "pearson", label.x = 18, label.sep = "\n")) +
  labs(x = "VPD Max", y = "WPBR Incidence (% infected)") +
  coord_cartesian(xlim = c(12, 20))

VPDscatter_plot

TEMPscatter_plot <- ggscatter(pilalist_climate, x = "tmean", y = "incidencepercent",
                              color = "black", shape = 20, size = 3,
                              add = "reg.line",
                              add.params = list(color = "blue", fill = "lightgray"),
                              conf.int = TRUE,
                              cor.coef = TRUE,
                              cor.coeff.args = list(method = "pearson", label.x = 13, label.sep = "\n")) +
  labs(x = "Mean Temp", y = "WPBR Incidence (% infected)") +
  coord_cartesian(xlim = c(5, 15))

TEMPscatter_plot

PRECIPscatter_plot <- ggscatter(pilalist_climate, x = "ppt", y = "incidencepercent",
                                color = "black", shape = 20, size = 3,
                                add = "reg.line",
                                add.params = list(color = "blue", fill = "lightgray"),
                                conf.int = TRUE,
                                cor.coef = TRUE,
                                cor.coeff.args = list(method = "pearson", label.x = 1300, label.sep = "\n")) +
  labs(x = "Precipitation", y = "WPBR Incidence (% infected)") +
  coord_cartesian(xlim = c(950, 1450))

PRECIPscatter_plot

ELEVscatter_plot <- ggscatter(pilalist_climate, x = "plot_elevation_ft", y = "incidencepercent",
                              color = "black", shape = 20, size = 3,
                              add = "reg.line",
                              add.params = list(color = "blue", fill = "lightgray"),
                              conf.int = TRUE,
                              cor.coef = TRUE,
                              cor.coeff.args = list(method = "pearson", label.x = 5500, label.sep = "\n")) +
  labs(x = "Elevation", y = "WPBR Incidence (% infected)") +
  coord_cartesian(xlim = c(3200, 8000))

ELEVscatter_plot

combinedscatteredclimategraphs <- VPDscatter_plot + TEMPscatter_plot + PRECIPscatter_plot + ELEVscatter_plot
combinedscatteredclimategraphs

###### Dataframe for the model #####
# Select only the columns you want from pilaWPBRclean
pilaWPBR_subset <- pilaWPBRclean %>%
  select(plotNum, treeNum, severity, infectionrate)

# Perform the join using both columns
pilalist_climate_wpbr <- pilalist_climate %>%
  left_join(pilaWPBR_subset, by = c("plotNum", "treeNum"))

### Bring in the rest of the pila data
### pilalist_climate_wpbr and plot_areas


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# MODELING  #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

library("lme4")
install.packages("lme4", type = "source")

librarian::shelf(lme4, tidyverse, terra, sjPlot, Matrix)



mod1 <- lmer(infectionrate ~ vpdmax + ribes + beetles_present + (1 | plotNum),
             data = pilalist_climate_wpbr)

tab_model(mod1)

# rescale

pilalist_climate_wpbr$vpdmax_scaled <- scale(pilalist_climate_wpbr$vpdmax)

mod1 <- lmer(incidencepercent ~ vpdmax_scaled + ribes + beetles_present + (1 | plotNum),
             data = pilalist_climate_wpbr)

tab_model(mod1)


pila_climate_wpbr_model <- pilalist_climate_wpbr %>%
  select(plotNum, treeNum, incidencepercent, vpdmax_scaled, ribes, beetles_present)

pila_climate_wpbr_model$ribes = as.factor(pila_climate_wpbr_model$ribes)
pila_climate_wpbr_model$beetles_present = as.factor(pila_climate_wpbr_model$beetles_present)

pila_climate_wpbr_model = scale(pila_climate_wpbr_model)
glimpse(pila_climate_wpbr_model)

mod1 <- lmer(incidencepercent ~ vpdmax_scaled + ribes + beetles_present + (1 | plotNum),
             data = pila_climate_wpbr_model)

mod1 <- lmer(incidencepercent ~ ribes + beetles_present + (1 | plotNum),
             data = pilalist_climate_wpbr)


########## Correlation Matrix ###########
# Calculate the correlation matrix for slope and vpdmax
correlation_matrix <- cor(pilalist_climate_wpbr$slope, pilalist_climate_wpbr$vpdmax, use = "complete.obs")

# Display the correlation matrix
correlation_matrix

