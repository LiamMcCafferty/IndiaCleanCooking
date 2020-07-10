# Empties Global Environment cache
rm(list=ls())

library(tidyverse) # For data management


#Importing our data. We should have 2 sets of useful data: ego data, alter data.
load("women_ego_alter_data.rda")
load("alter_data.rda")

sample_data[sample_data$snaw_redcap_id %in% c(42, 73),]$prop_lpg <- c(0.7132857, .333333)

levels(sample_data$household_group) <- c("No LPG", "LPG", NA)

sample_data[sample_data$snaw_redcap_id == 52,"snaw_g10_name3name10"] <- NA


sample_data %>%
	ggplot(aes(x = prop_lpg)) +
	geom_density(aes(color = household_group, fill = household_group,
									 linetype = household_group), alpha = 0.5) +
	scale_color_manual(values = c("red", "blue")) +
	scale_fill_manual(values = c("red", "blue")) +
	theme_bw() +
	labs(color = "Household Group", fill = "Household Group",
			 linetype = "Household Group",
			 x = "Proportion of Alters having LPG") ->
	output_plot1

ggsave("LPG Density by LPG overlapping.pdf", output_plot1,device = "pdf")

sample_data %>%
	ggplot(aes(x = prop_lpg)) +
	geom_density(aes(color = household_group, fill = household_group,
									 linetype = household_group), alpha = 0.5) +
	scale_color_manual(values = c("red", "blue")) +
	scale_fill_manual(values = c("red", "blue")) +
	theme_bw() +
	labs(color = "Household Group", fill = "Household Group",
			 linetype = "Household Group", x = "Proportion of Alters having LPG") +
	facet_wrap(~household_group) ->
	output_plot2

ggsave("LPG Density by LPG split.pdf", output_plot2,
			 device = "pdf", height = 3.72, width = 8)

sample_data %>%
	ggplot(aes(x = prop_lpg)) +
	geom_density(aes(color = household_group), color = "green", fill = "green", alpha = 0.5) +
	theme_bw() +
	labs(x = "Proportion of Alters having LPG") ->
	output_plot3

ggsave("LPG Density total.pdf", output_plot3,
			 device = "pdf")

##### Representative Networks #####


