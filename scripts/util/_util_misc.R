# --------------------------------------------------------------- #
#
# Miscellaneous functions
# 
# --------------------------------------------------------------- #


# ------------------------- #
# Update packages
# ------------------------- #

#update.packages()


# ------------------------- #
# calculate the sizes of items in the environment
# ------------------------- #
env_size <- function(workspace = ls()) {
  size = 0
  for (x in workspace){
    thisSize = object_size(get(x))
    size = size + thisSize
    message(x, " = ", appendLF = F); print(thisSize, units='auto')
  }
  message("total workspace is ", appendLF = F); print(size, units='auto')
}

# get_env_size <- function() {
#   my_workspace <- ls()
#   temp <- tibble(
#     things = my_workspace[1:10]) %>%
#     mutate(size = sapply(1:10, #seq_along(my_workspace), 
#                          function(i) {
#                            object_size(get(my_workspace[i]))
#                          }
#     ),
#     size_mb = size / 1e3)
#   
#   temp
# }
# 
# object_size(age_2017_grid)
# 
# get_env_size()

# ------------------------- #
# calculate a dummy data.table
# ------------------------- #
cc_create_dt <- function(numrow = 15, numcol = 15, seed = 34L) {
  set.seed(seed)
  dt <- matrix(round(runif(numrow*numcol)), nrow = numrow, ncol = numcol)
  dt <- as.data.frame(dt)
  setDT(dt)
}

# ------------------------- #
#
# ------------------------- #
cc_create_bin <- function(numrow = 15, numcol = 15, seed = 34L) {
  dt <- cc_create_dt(numrow = numrow, numcol = numcol, seed = seed)
  dt[3] <- 1
  dt[13] <- 1
  dt[12] <- 0
  dt[4, 1:4] <- 1
  dt[14, 1] <- 1
  dt[1, 9] <- 1
  dt[3, 1:2] <- 0
  dt
}


# ------------------------- #
# capitalize name
# ------------------------- #
capwords <- function(s, strict = FALSE) {
  cap <- function(s) {
    paste(
      toupper(substring(s, 1, 1)), 
      {s <- substring(s, 2); if(strict) tolower(s) else s},
      sep = "", collapse = " "
    )
  }
  sapply(strsplit(s, split = " "), 
         cap, 
         USE.NAMES = !is.null(names(s))
  )
}


# ------------------------ #
# capitalize site labels
# ------------------------ #
cap_labels <- function(string) capwords(gsub("_", " ", string))
# for use in facet_wrap(labeller = as_labeller(cap_labels))


# ------------------------ #
# capitalize site labels
# ------------------------ #
cap_update_labels <- function(string) {
  capwords(ifelse(grepl("herze", string, ignore.case = TRUE),
                  gsub("_", " & ", string),
                  ifelse(grepl("bel", string, ignore.case = TRUE),
                         gsub("belarus", "Belarus/Smolensk", string),
                         ifelse(grepl("goias", string, ignore.case = TRUE),
                                gsub("goias", "Goiás", string),
                                ifelse(grepl("shaanxi", string, ignore.case = TRUE),
                                       gsub("shaanxi", "Shaanxi/Shanxi", string),
                                       ifelse(grepl("nebraska", string, ignore.case = TRUE),
                                              gsub("nebraska", "Nebraska/Wyoming", string),
                                              ifelse(grepl("orenburg", string, ignore.case = TRUE),
                                                     gsub("orenburg", "Orenburg/Uralsk", string),
                                                     gsub("_", " ", string))))))))
}
# for use in facet_wrap(labeller = as_labeller(cap_update_labels))



# ------------------------ #
# scale a vector by mean and sd
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}







# --------------------------------------------------------------- #
# ----------------------- plotting colors ------------------------ 
# --------------------------------------------------------------- #

# from: https://medialab.github.io/iwanthue/
cbf_col <- c("#6972d7", "#9fac3a", "#583586", "#69a150", "#b853a2",
              "#45c097", "#ba4b7d", "#c1893c", "#628ed6", "#b85136",
              "#bf81d7", "#ba4758")

# general ggplot color palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
# show_col(gg_color_hue(3))

# to define a color palette for plotting, first make a character vector with the colors, then assign names to match the factor levels.
# use this as scale_fill_manual(values = your_custom_palette)

# define color palette for plotting:
# 1. Non-veg
# 2. Woody veg
# 3. Crop
# 4. Grassland
plot_cols1 <- data.frame(
  color = c("gray80", # gray, 1. Non-veg
            terrain.colors(9)[1], # "#00A600" # dark green, 2. Woody veg
            terrain.colors(9)[5], # "#E8C32E" # gold, 3. Crop
            terrain.colors(9)[3]  # "#8BD000" # light green, 4. Grassland
  ),
  name = c("1. Non-veg", "2. Woody veg", "3. Crop", "4. Grassland"),
  breaks = c(1, 2, 3, 4))

# show_col(brewer_pal(palette = "PiYG")(11))
plot_cols <- c("gray80", 
               brewer_pal(palette = "PiYG")(11)[11], # dark green,
               terrain.colors(9)[5], # gold
               brewer_pal(palette = "PiYG")(11)[9], # light green
               brewer_pal(palette = "PiYG")(11)[2], # dark pink
               brewer_pal(palette = "PiYG")(11)[4] # light pink
)

names(plot_cols) <- c("Non-veg.", "Woody veg.", "Cropland", "Grassland", 
                      "Abandoned (>=5)", "Abandoned (>1)")
as_factor(plot_cols)
plot_cols <- plot_cols[order(names(plot_cols), decreasing = FALSE)]
# fct_relevel(plot_cols, order(names(plot_cols), decreasing = FALSE))


# visualize them
# show_col(plot_cols$color) # (topleft = 1, topright = 2, bottomleft = 3, bottomright = 4)
# gray,         1. Non-veg
# dark green,   2. Woody veg
# gold,         3. Crop
# light green,  4. Grassland


# colors for recoded land cover classes, specifically the plots of area
# in each land cover class over time. (Figures S4, S27-S37)
plot_cols_new <- c("gray80", # gray, 1. Non-veg
                   terrain.colors(9)[5], # "#E8C32E" # gold, 2 (formerly 3) Crop
                   terrain.colors(9)[3], # "#8BD000" # light green, 3 (formerly 4) Grassland
                   terrain.colors(9)[1]  # "#00A600" # dark green, 4 (formerly 2) Woody veg
)


