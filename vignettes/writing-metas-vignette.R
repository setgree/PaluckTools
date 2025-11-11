## ----summarise_lm demonstration-----------------------------------------------
library(PaluckMetaSOP)
sv_data |> summarise_lm()

# now in different subsets
library(purrr)
sv_data |> split(~study_design) |> map(summarise_lm)

## ----summarise_table_demonstration--------------------------------------------
library(dplyr)
# let's say you want to see the table of `behavior_type` in `sv_data
# you can do
table(sv_data$behavior_type)

# the summarise_table version
sv_data |> summarise_table(behavior_type)

# where this really becomes useful is something like:
PaluckMetaSOP::sv_data |> split(~study_design) |>
map(~ summarise_table(., behavior_type)) |> bind_rows(.id = "study_design")

# Note: summarise_table() returns a table object. For more complex manipulations,
# you can convert to a data frame:
# as.data.frame(summarise_table(sv_data, behavior_type))

## ----study_count_demonstration------------------------------------------------
sv_data |> study_count()
sv_data |> split(~study_design) |> map(study_count) |> 
  bind_rows(.id = "study_design")

## ----bib_stuff, eval=FALSE----------------------------------------------------
# library(dplyr)
# 
# # Load the MAP reduction dataset
# data(map_reduction_data)
# 
# # Preview the data structure
# map_reduction_data |>
#   select(author, year, title, doi) |>
#   head(5)
# 
# # Filter for entries with valid DOIs (start with "10.")
# map_with_dois <- map_reduction_data |>
#   filter(grepl("^10\\.", doi)) |>
#   distinct(doi, .keep_all = TRUE)
# 
# cat("Found", nrow(map_with_dois), "unique DOIs\n")
# 
# # Generate bibliography
# make_bib(
#   data = map_with_dois,
#   doi_column = "doi",
#   bib_file = "./references.bib",
#   verbose = TRUE
# )
# 
# # The function will:
# # 1. Extract unique DOIs from the specified column
# # 2. Fetch bibliographic information from DOI.org
# # 3. Write each entry to the .bib file
# # 4. Print progress messages
# 
# # You can also manually add individual DOIs:
# # Create a simple data frame with a DOI
# extra_doi <- data.frame(doi = "10.1016/j.appet.2025.108233")
# make_bib(extra_doi, bib_file = "./references.bib", overwrite = FALSE)

## ----Forest plot--------------------------------------------------------------
library(ggplot2)
library(ggtext)
model <- contact_data |> map_robust()

bold_labels <- function(x) {
  ifelse(x == "RE Estimate", "<b>RE Estimate</b>", x)
}

plot_dat <- as.data.frame(contact_data) |> 
  mutate(lower_bound = d - (1.96 * se_d),
         upper_bound = d + (1.96 * se_d)) |>
  select(name_short, d, se_d, lower_bound, upper_bound, 
         target_spelled_out) |> 
  add_row(name_short = "RE Estimate", d = model$Delta, 
          lower_bound = model$Delta - (1.96 * model$se),
          upper_bound = model$Delta + (1.96 * model$se), 
          target_spelled_out = NA)

# Get unique study names excluding "RE Estimate"
unique_studies <- unique(plot_dat$name_short[plot_dat$name_short != "RE Estimate"])

# Append "RE Estimate" to the end of the list
ordered_levels <- c(unique_studies, "RE Estimate")

# Set this order to name_short
plot_dat$name_short <- reorder(factor(plot_dat$name_short, levels = ordered_levels), desc(plot_dat$se_d))

plot_dat |> ggplot(aes(x = d, y = name_short)) +
  geom_point(data = subset(plot_dat, name_short == "RE Estimate"), 
             shape = 18) + # shape = 5 for a transparent diamond 
  geom_point(data = subset(plot_dat, name_short != "RE Estimate"), 
             aes(color = target_spelled_out), shape = 18) +
  geom_errorbar(data = subset(plot_dat, name_short != "RE Estimate"),
                aes(xmin = lower_bound, xmax = upper_bound, color = target_spelled_out),
                height = .1, orientation = "y") +
  geom_vline(xintercept = 0, color = "black", alpha = .5) +
  geom_vline(xintercept = model$Delta, 
             color = 'black', lty = 'dashed') +
  theme_minimal() +
  theme(axis.text.y = element_markdown()) +  # Apply HTML formatting to y-axis text
  scale_y_discrete(labels = bold_labels) +    # Use custom function for y-axis labels
  scale_x_continuous(name = expression(paste("Glass's", " ", Delta))) +
  labs(color = "Target of Prejudice") +
  ylab("Study") +
  ggtitle("Contact hypothesis forest plot") +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold"),
        axis.line = element_line(colour = "black")) 

## ----se_d_plot, warning=FALSE, message=FALSE----------------------------------
library(ggrepel)
shape_orders <- c(19, 17, 15, 18)
target_labels <- c("Age", "Disability", "Foreigners", "Gender", 
                   "LGBT",  "Race", "Religion" )

  ggplot(contact_data, 
         aes(x = se_d, y = d, 
             label = name_short)) +
  geom_hline(yintercept = 0, lty = "dashed", colour = "grey") + 
  geom_point(aes(shape = factor(pop),
                 color = target_spelled_out), size = 3, alpha = NA) +
  scale_shape_manual(values = shape_orders, name = "Population", 
                     labels = c("Children grades 4-8","High school students",
                                "College-aged subjects", "Adults over 25")) +
  stat_smooth(aes(fill = NULL), lty = "dashed", fullrange = TRUE, 
              method = "lm",
              show.legend = FALSE, alpha = .1) +  
    geom_label_repel(size = 3) +
  xlab("Standard Errors") + 
  ylab("Effect Sizes")  +
  ggtitle("Contact hypothesis publication bias plot") +
  labs(color = "Target of prejudice") +
  theme_bw()


## ----sv_data_descriptive_figs, warning=FALSE, message=FALSE, fig.show='hold'----
library(forcats)
library(ggplot2)
library(ggtext)
library(ggthemes)
library(patchwork)
library(stringr)
library(tidyr)

country_fig = sv_data |>
  distinct(unique_study_id, country) |> 
  mutate(country = ifelse(country == "USA", "USA", "Rest of the world")) |> 
  count(country, sort = T) |> 
  mutate(country = fct_rev(fct_reorder(factor(country), n)),
         perc = n / sum(n)) |>
  ggplot(aes(country, n, label = n, fill = country)) +
  geom_col() +
  theme_bw() +
  theme(axis.text = element_markdown(size = 12), 
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_markdown()) +
  scale_y_continuous(breaks = seq(0, 300, by = 100), limits = c(0, 300)) +
  scale_fill_manual(values = solarized_pal()(2)) +
  guides(fill = "none") +
  labs(title = "**a)** Number of Studies by Country",
       x = NULL, 
       y = "# of studies", 
       col = NULL)

setting_fig = sv_data |>
    distinct(unique_study_id, setting, setting2) |> 
    pivot_longer(cols = c("setting", "setting2"), values_to = "setting") |> 
    drop_na(setting) |> 
    count(setting, sort = T) |> 
    mutate(setting = fct_rev(fct_reorder(setting, n)), 
           perc = n / sum(n)) |>
    ggplot(aes(setting, n, fill = setting)) +
    geom_col() +
    scale_fill_manual(values = ggthemes::tableau_color_pal('Tableau 10')(6)) +
    labs(title = "**b)** Number of Studies by Setting",
         x = NULL, 
         y = "# of studies", 
         col = NULL) +
    theme_bw() +
    guides(fill = "none") +
    theme(axis.text = element_markdown(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          plot.title = element_markdown())

dat_participant_sex <- sv_data |> # participant sex lines
  mutate(participant_sex = 
           if_else(participant_sex %in% 
                     c("male and female in mixed groups", 
                       "male and female in separate groups and also in combined groups",
                       "male and female in separate groups",
                       "male and female with group composition not specified",
                       "general population"), 
                   "mixed gender groups", 
                   participant_sex)) |>
  distinct(year, unique_study_id, participant_sex) |>
  filter(participant_sex %in% 
           c("mixed gender groups", "only male", "only female")) |>
  count(year, participant_sex) |>
  add_row(year = 1985, participant_sex = "mixed gender groups") |>
  complete(year, participant_sex, fill = list(n = 0)) |>
  mutate(participant_sex = str_to_title(participant_sex)) |>
  mutate(participant_sex = factor(participant_sex,
                                  levels = c("Mixed Gender Groups",
                                             "Only Male", 
                                             "Only Female")))
tt_gender_fig = dat_participant_sex |>
    ggplot(aes(year, n, col = participant_sex)) +
    geom_line(linewidth = 2) +
    scale_x_continuous(
      limits = c(1985, 2018),
      breaks = c(1985, seq(1985, 2020, by = 5), 2018)
    ) +
    scale_color_manual(values = ggthemes::palette_pander(3)) +
    theme_bw() +
    theme(
      axis.text = element_markdown(size = 12), 
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      plot.title = element_markdown(),
      legend.position = "bottom"
    ) +
    labs(
      title = "**c)** Number of Studies Over Time by Group Gender (1985-2018)",
      x = NULL,
      y = "# of studies",
      col = NULL
    )

# The patchwork library enables this arrangement:
country_fig / setting_fig / tt_gender_fig 

## ----attitude_behavioral_scatterplot------------------------------------------

sv_data |> filter(has_both == 'both') |>
  group_by(author, year, study_design, unique_study_id, scale_type) |>
  filter(delay == min(delay)) |>
  summarise(mean_d = mean(d),
         mean_var_d = mean(var_d),
         mean_se_d = mean(se_d)) |>
  pivot_wider(id_cols = c(author, year, unique_study_id, study_design),
    names_from = c(scale_type),
    values_from = c(mean_d, mean_var_d, mean_se_d)) |>
  ggplot(aes(x = mean_d_ideas,
             y = mean_d_behavior)) +
  geom_point(aes(color = study_design),
             size = 3) +
  geom_abline(slope = 1,
              lty = 'dashed') +
  geom_smooth(lty = "dashed",
              method = "lm",
              se = FALSE,
              color = 'grey') +
  labs(title = "Correlation between ideas and behavioral change",
       x = "Effect size (ideas)",
       y = "Effect size (behaviors)",
       color = "Study Design") +
 guides(color = guide_legend(title = "")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "bottom",
        legend.box = "vertical")

## ----dockerfile, eval=FALSE---------------------------------------------------
# # After loading all packages you used in your analysis, run:
# library(PaluckTools)
# library(dplyr)
# library(ggplot2)
# # ... other packages you used
# 
# # Create a Dockerfile with all currently loaded packages
# write_dockerfile()
# 
# # Or include ALL installed packages (not just loaded ones)
# write_dockerfile(all_packages = TRUE)
# 
# # Preview the Dockerfile content without writing to file
# write_dockerfile(write_file = FALSE)

