# --------------------------------------------------------------- #
# PREDICTS Database demonstration code
# Author: Tim Newbold 
# Shared with Chris Crawford: January 9th, 2020
# --------------------------------------------------------------- #

library(devtools)

# Install my package for simple manipulation of PREDICTS data
install_github(repo = "timnewbold/predicts-demo",subdir = "predictsFunctions")

library(predictsFunctions)

# Set the path to your local copy of the database
# predicts.path <- "C:/Users/tim_n/Dropbox/PREDICTSDemonstration/database.rds"
predicts.path <- "/Users/christophercrawford/Google_Drive/_Projects/data/Bd/PREDICTS/database.rds"
predicts.path <- paste0(p_dat, "Bd/PREDICTS/database.rds")


# Read in the PREDICTS data
predicts <- ReadPREDICTS(predicts.path)

nrow(predicts)

# Let's look at the column names - there are a lot
names(predicts)

# This powerpoint explains the different types of column
system(command = "powerpnt /S DatabaseVariables.ppsx") # cc- note doesn't work

# This powerpoint gives a bit more detail on the land-use classification
system(command = "powerpnt /S LanduseClassification.ppsx") # cc- note doesn't work

# Correct effort-sensitive abundance measures (assumes linear relationship between effort and recorded abundance)
predicts <- CorrectSamplingEffort(diversity = predicts)

# Merge sites that have the same coordinates (e.g. multiple traps on a single transect)
nrow(predicts) # 3.25 M rows before, 2.9 M after
predicts <- MergeSites(diversity = predicts)
# Dropping 343410 measurements that have been merged
# Dropping unused factor levels
## (cc- note: this matches with the before and after row count)

names(predicts)
predicts %>% select(Source_ID) %>% unique() %>% unlist(use.names = FALSE) %>% length() # 480
predicts %>% select(Site_name) %>% unique() %>% unlist(use.names = FALSE) %>% length() # 16,496

# Calculate site metrics of diversity - currently only species richness and total abundance
sites2 <- SiteMetrics(diversity = predicts,
                     extra.cols = c("Predominant_land_use",
                                    "SSB","SSBS", 
                                    # cc additions
                                    "Country", "Biome", "Ecoregion", "Years_since_fragmentation_or_conversion"))
nrow(sites) # 22678
nrow(sites2) # 22678
names(sites)
names(sites2)

names(predicts)
sites %>% select(Source_ID, 
                 Diversity_metric_type,
                 Predominant_land_use, Use_intensity,
                 Total_abundance, Species_richness) %>% summary()

sites2 %>% select(Source_ID, 
                 Diversity_metric_type,
                 Predominant_land_use, Use_intensity,
                 Biome, Country, Ecoregion, Years_since_fragmentation_or_conversion,
                 Total_abundance, Species_richness) %>% summary()

sites$Predominant_land_use %>% levels()

# Now install my package for building statistical models (including mixed-effects models)
install_github("timnewbold/StatisticalModels")
library(StatisticalModels)

# Let's first try a model of site-level species richness

# First, we will rearrange the land-use classification a bit
sites$LandUse <- paste(sites$Predominant_land_use)
# Drop classification where land use could not be identified
sites$LandUse[(sites$LandUse == "Cannot decide")] <- NA
# Drop classification where the stage of recovery of secondary vegetation is unknown
sites$LandUse[(sites$LandUse == "Secondary vegetation (indeterminate age)")] <- NA
# Now make the variable a factor, and set the reference level to primary vegetation
sites$LandUse <- factor(sites$LandUse)
sites$LandUse <- relevel(sites$LandUse, ref="Primary vegetation")

table(sites$LandUse)


# We will compare some random-effects structures
# We must always consider study identity, because of major differences in sampling
r1 <- GLMER(modelData = sites, responseVar = "Species_richness",
            fitFamily = "poisson", fixedStruct = "LandUse",
            randomStruct = "(1|SS)", REML = FALSE)
r1$model

# Try adding spatial block to account for spatial differences among sites within studies
r2 <- GLMER(modelData = sites, responseVar = "Species_richness",
            fitFamily = "poisson", fixedStruct = "LandUse",
            randomStruct = "(1|SS)+(1|SSB)", REML = FALSE)
r2$model

# Is the second random-effects structure better?
AIC(r1$model, r2$model)
glmer() # from lme4
lmer() # from lme4


# Species richness models are often overdispersed - is this the case?
GLMEROverdispersion(model = r2$model)

# We can control for this by fitting an observation-level random effect
r3 <- GLMER(modelData = sites, responseVar = "Species_richness",
            fitFamily = "poisson", fixedStruct = "LandUse",
            randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)", REML = FALSE)

# Is this better than the previous random-effects structure?
AIC(r2$model,r3$model)

# Has it removed the overdispersion?
GLMEROverdispersion(model = r3$model)

# Now we will run backward stepwise selection to see if land use has a significant effect
sr1 <- GLMERSelect(modelData = sites, responseVar = "Species_richness",
                   fitFamily = "poisson", fixedFactors = "LandUse",
                   randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)")

# A statistics table is produced
sr1$stats

# And the model itself is shown by:
sr1$model

# Now try plotting this model
PlotGLMERFactor(model = sr1$model, data = sr1$data,
                responseVar = "Species richness", seMultiplier = 1.96,
                logLink = "e", catEffects = "LandUse")

# We can rotate the x-axis labels by adding xtext.srt=45
PlotGLMERFactor(model = sr1$model,data = sr1$data,
                responseVar = "Species richness",seMultiplier = 1.96,
                logLink = "e",catEffects = "LandUse",xtext.srt=45)

# Increase the bottom and right margins using 
# params=list(mar = c(1.2, 3.5, 0.2, 1.2))
PlotGLMERFactor(model = sr1$model,data = sr1$data,
                responseVar = "Species richness",seMultiplier = 1.96,
                logLink = "e",catEffects = "LandUse",xtext.srt=45,
                params=list(mar = c(2.2, 3.5, 0.2, 1.2)))

# Reorder the land uses into a logical order, using
# order=c(1,4,3,8,6,2,5,7)
sites$LandUse %>% levels() # to be reordered.
class(sites$LandUse)
sites <- sites %>% mutate(LandUse = fct_relevel(LandUse, levels(sites$LandUse)[c(1, 4, 3, 8, 6, 2, 5, 7)]))

PlotGLMERFactor(model = sr1$model,data = sr1$data,
                responseVar = "Species richness",seMultiplier = 1.96,
                logLink = "e",catEffects = "LandUse",xtext.srt=45,
                params=list(mar = c(2.2, 3.5, 0.2, 1.2)),
                order=c(1,4,3,8,6,2,5,7))

# Make some predictions from the model
nd <- data.frame(
  LandUse=factor(x = levels(sr1$data$LandUse),
                 levels = levels(sr1$data$LandUse)), # with added reordering vector
  Species_richness=0)

preds <- PredictGLMER(model = sr1$model, data = nd, se.fit = TRUE,
                      seMultiplier = 1.96, randEffs = FALSE)
preds <- exp(preds)

errbar(x = levels(sr1$data$LandUse), y = preds$y,
       yplus = preds$yplus, yminus = preds$yminus)
abline(v = preds$y[1])

# We can also build models at the species level using the raw database

# First, we will rearrange the land uses as before
predicts$LandUse <- paste(predicts$Predominant_land_use)
# Drop classification where land use could not be identified
predicts$LandUse[(predicts$LandUse=="Cannot decide")] <- NA
# For these models we will group all secondary vegetation together
predicts$LandUse[(predicts$LandUse=="Secondary vegetation (indeterminate age)")] <- "Secondary vegetation"
predicts$LandUse[(predicts$LandUse=="Mature secondary vegetation")] <- "Secondary vegetation"
predicts$LandUse[(predicts$LandUse=="Intermediate secondary vegetation")] <- "Secondary vegetation"
predicts$LandUse[(predicts$LandUse=="Young secondary vegetation")] <- "Secondary vegetation"
# We will also group cropland and pasture together into an 'agriculture' class
predicts$LandUse[(predicts$LandUse=="Cropland")] <- "Agriculture"
predicts$LandUse[(predicts$LandUse=="Pasture")] <- "Agriculture"
# Now make the variable a factor, and set the reference level to primary vegetation
predicts$LandUse <- factor(predicts$LandUse)
predicts$LandUse <- relevel(predicts$LandUse,ref="Primary vegetation")

# Now create a column for species presence or absence
predicts$Occurrence <- ifelse(predicts$Measurement > 0, 1, 0)

# Because these models take a long time to run, we will focus on reptiles
nrow(predicts)
reptiles <- predicts[(predicts$Class=="Reptilia"),]
nrow(reptiles)

# Now we will try a model of species presence or absence as a function of land use
# We will add additional random effects of species identity and site
# to control for additional likely random variation
occ1 <- GLMERSelect(modelData = reptiles, responseVar = "Occurrence",
                    fitFamily = "binomial", fixedFactors = "LandUse",
                    randomStruct = "(1|SS)+(1|SSBS)+(1|Taxon_name_entered)")

# Stats as before
occ1$stats
names(occ1$model)
names(occ1$stats)

# Now we will plot this model using the same parameters as above
PlotGLMERFactor(model = occ1$model, data = occ1$data,
                responseVar = "Occurrence", seMultiplier = 1.96,
                logLink = "b", catEffects = "LandUse", xtext.srt = 45,
                params = list(mar = c(2.2, 3.5, 0.2, 1.2)),
                order = c(1, 4, 3, 2))
