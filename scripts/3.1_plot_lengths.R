# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, March 9th, 2021

# Script to produce length plots based on distilled length data.tables from "3_distill_lengths.R"
# -------------------------------------------------------- #

# Three plots:
# 1. Summary figure: mean length (all periods and max lengths) for each site
# 2. Summary lengths by abandonment threshold (like 1, but with different thresholds)
# 3. Histograms (all periods and max lengths)

# -------------------------------------------------------- #
# load libraries
cluster_packages <- c("data.table", "tictoc", "raster",
                      "landscapemetrics", "landscapetools", "sp",
                      "tidyverse", "rgdal", "dtraster", "pryr")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths and source functions:
source("/Users/christophercrawford/Google Drive/_Projects/abandonment_trajectories/scripts/util/_util_master.R")

# download from the cluster with the following bash command:
# scp clc6@della:/scratch/network/clc6/abandonment_trajectories/data_derived/input_rasters/length_distill_df_2021-03-05.csv /Users/christophercrawford/Google\ Drive/_Projects/abandonment_trajectories/data_derived/_2021-03-05/


# set up parameters:
# data.frame of all sites contains information about sites
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))

run_label <- "_2021_03_13" #"_2021-03-05"
# _2021_03_13


cat(fill = TRUE, "Run label (time stamp):", run_label)

# 0. Load distilled length data
length_distill_df <- read_csv(file = paste0(p_dat_derived, run_label, "/", 
                                            "length_distill_df", run_label, ".csv"))
# length_distill_df %>% filter(site == "shaanxi") %>% select(length_type) %>% unique()

mean_length_df <- lapply(c(1, 3, 5), function(x) {
  length_distill_df %>% as_tibble() %>% 
    mutate(product = length*freq) %>% 
    filter(length >= x) %>% # to filter by length # this is important for max, since some pixels have max length of 0
    group_by(site, length_type) %>% 
    summarise(mean_duration = sum(product)/sum(freq),
              median_duration = median(rep(length, freq)),
              sd_duration = sd(rep(length, freq))
              ) %>% 
    mutate(abn_threshold = x) 
}) %>% bind_rows()

# save mean_length_df
write_csv(mean_length_df, file = paste0(p_dat_derived, run_label, "/", "mean_length_df", run_label, ".csv"))
cat(fill = TRUE, "Saved mean_length_df to:", paste0(p_dat_derived, run_label, "/", "mean_length_df", run_label, ".csv"))

# mean_length_df <- read_csv(file = paste0(p_dat_derived, run_label, "/", "mean_length_df", run_label, ".csv"))


# mean mean length
mean_mean_df <- mean_length_df %>% 
  group_by(abn_threshold, length_type) %>%
  summarise(mean_mean = mean(mean_duration, na.rm = TRUE),
            mean_median = mean(median_duration, na.rm = TRUE))



# create directory for plot outputs
if(!dir.exists(paste0(p_output, "plots/", run_label))) {
  dir.create(paste0(p_output, "plots/", run_label))
}

# -------------------------------------------------------- #
# 1. Plot mean length of time abandoned (>= 5) ----

gg_mean_length <-
  ggplot(data = mean_length_df %>% 
           filter(abn_threshold == "5"),
         mapping = aes(x = fct_reorder2(site, abn_threshold, desc(mean_duration)),
                       y = mean_duration, 
                       color = length_type)) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 330, vjust = 1, hjust = 0),
        plot.caption = element_text(face = "italic")) +
  labs(#title = "Mean length of abandonment",
       #caption = "Counting only recultivation of 2 or more continuous years",
       linetype = NULL,
       color = "Mean of:", 
       y = "Mean abandonment duration (years)", 
       x = "Site") +
  geom_point(size = 2) + 
  geom_hline(data = mean_length_df %>% 
               group_by(abn_threshold, length_type) %>%
               summarise(overall_mean = mean(mean_duration, na.rm = TRUE)) %>% 
               filter(abn_threshold == "5"), 
             mapping = aes(yintercept = overall_mean, 
                           col = length_type, 
                           linetype = "Overall \nMean")) + 
  scale_linetype_manual(values = c("Overall \nMean" = "dashed")) + 
  scale_colour_discrete(labels = c(all = "All periods", 
                                   max = "Max. length \nper pixel")) +
  guides(color = guide_legend(order = 1))
    


# ----------------- save ---------------- #
png(filename = paste0(p_output, "plots/", run_label, "/mean_lengths", run_label, ".png"), 
    # width = 6, height = 5, 
    width = 5, height = 4, 
    units = "in", res = 400)
print(gg_mean_length)
dev.off()



# 2. Plot mean length across multiple abandonment thresholds ------

gg_mean_length_by_threshold <- 
  ggplot(data = mean_length_df,
         mapping = aes(x = fct_reorder(site, mean_duration), y = mean_duration, fill = fct_reorder(site, mean_duration))) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 320, vjust = 1, hjust = 0),
        plot.caption = element_text(face = "italic")) +
  labs(title = "Mean length of abandonment",
       #caption = "Counting only recultivation of 2 or more continuous years",
       linetype = NULL,
       fill = "Site", 
       y = "Mean length of time abandoned (years)", 
       x = "Site") +
  geom_col(width = 0.75) + 
  facet_grid(length_type ~ abn_threshold,
             labeller = labeller(length_type = c(all = "all lengths", #old = "new",
                                                 max = "max length per pixel"),
                                 abn_threshold = c("1" = "abn threshold = 1",
                                                   "3" = "abn threshold = 3",
                                                   "5" = "abn threshold = 5"))) +
  geom_hline(data = mean_length_df %>% 
               group_by(abn_threshold, length_type) %>%
               summarise(overall_mean = mean(mean_duration, na.rm = TRUE)), 
             mapping = aes(yintercept = overall_mean, linetype = "mean"),
             show.legend = TRUE,
             color = "black", size = 0.75) + 
  scale_linetype_manual(values = c("mean" = "dotted")) + 
  guides(fill = guide_legend(override.aes = list(linetype = 0)))



# ----------------- save ---------------- #
png(filename = paste0(p_output, "plots/", run_label, "/mean_lengths_by_threshold", run_label, ".png"), 
    width = 8.5, height = 5, units = "in", res = 400)
print(gg_mean_length_by_threshold)
dev.off()




# 3. Save Histograms ----

# -------------------------------------------------------------------- #
# ----------------- plot histograms ---------------- #
# -------------------------------------------------------------------- #
for (i in 1:11) {
  gg_hist <-
    ggplot(data = filter(length_distill_df, site == site_df$site[i], length > 0)) + 
    theme_classic() +
    labs(linetype = "", x = "Time abandoned (years)", 
         y = expression("Count  (10"^{6}*" pixels)")
    ) +
    geom_col(mapping = aes(x = length, y = freq/(10^6)), fill = "gray70") +
    facet_grid(rows = vars(length_type), scales = "free",
               labeller = labeller(length_type = c(all = "all abn. periods", #old = "new",
                                                   max = "max. length per pixel"))) +
    geom_vline(data = filter(mean_length_df,
                             site == site_df$site[i],
                             abn_threshold == 1), 
               aes(xintercept = mean_duration, linetype = "mean"),
               show.legend = TRUE, size = 1) + 
    scale_linetype_manual(values = c("mean" = "dashed")) +
    theme(legend.position = c(0.85, 0.95), plot.caption = element_text(face = "italic")) #+ scale_x_log10()
  

  # save
  png(filename = paste0(p_output, "plots/", run_label, "/length_hist", run_label, site_df$label[i], ".png"), 
      width = 7, height = 5, units = "in", res = 400)
  print(gg_hist)
  dev.off()
}





