library(nlme)
library(MASS)
tmins = read.table("tmins.csv", header=T, sep=",")
model_depth <- gls(data = tmins, Tsoil_min ~ Latitude * Elevation_m * depthN)
model_depth_lme <- lme(data = tmins, Tsoil_min ~ Latitude * Elevation_m * depthN, random=~1|Network)
AIC(model_depth, model_depth_lme)

model_depth_2 <- gls(data = tmins, Tsoil_min ~ Latitude * Elevation_m * depthN, correlation = corSpher(form =~ Longitude + Latitude|Elevation_m/depthN, nugget = TRUE))
model_depth_3 <- gls(data = tmins, Tsoil_min ~ Latitude * Elevation_m * depthN, correlation = corLin(form =~ Longitude + Latitude|Elevation_m/depthN, nugget = TRUE))
model_depth_4 <- gls(data = tmins, Tsoil_min ~ Latitude * Elevation_m * depthN, correlation = corRatio(form =~ Longitude + Latitude|Elevation_m/depthN, nugget = TRUE))
model_depth_5 <- gls(data = tmins, Tsoil_min ~ Latitude * Elevation_m * depthN, correlation = corGaus(form =~ Longitude + Latitude|Elevation_m/depthN, nugget = TRUE))

AIC(model_depth, model_depth_2, model_depth_3, model_depth_4, model_depth_5)
summary(model_depth_2)

E <- resid(model_depth_2)

#look for patterns in the residuals
plot(E~tmins$depthN)  #a strong pattern appears
plot(E~tmins$Elevation_m)
plot(E~tmins$Latitude)

#try to add depth as a depth^2 factor
tmins$log.depth = log(tmins$depthN)
model_depth_2.1 <- gls(data = tmins, Tsoil_min ~ Latitude * Elevation_m * log.depth, correlation = corSpher(form =~ Longitude + Latitude|Elevation_m/depthN, nugget = TRUE))
AIC(model_depth, model_depth_2, model_depth_2.1, model_depth_3, model_depth_4, model_depth_5)

summary(model_depth_2.1)

E <- resid(model_depth_2.1)
#look for patterns in the residuals
plot(E~tmins$depthN)  #much better now
plot(E~tmins$Elevation_m)
plot(E~tmins$Latitude)

library(gstat)
library(sp)
mydata <- data.frame(E, x = tmins$Longitude, y = tmins$Latitude)
coordinates(mydata) <- ~ x + y
# Black dotes are negative residuals, and grey dots are positive residuals
bubble(mydata, "E",
       col = c("black", "grey"),
       main = "Residuals", xlab = "X-coordinates",
       ylab = "Y-coordinates"
)

Vario1 <- variogram(E ~ 1, mydata)
plot(Vario1)

model_depth_2_ml = gls(data = tmins, Tsoil_min ~ Latitude * Elevation_m * log.depth, correlation = corSpher(form =~ Longitude + Latitude|Elevation_m/depthN, nugget = TRUE), method = "ML")
model3  = stepAIC(model_depth_2_ml)


require(ggplot2)
require(plotly)
# Regrouping data in a 3-columns data.frame
range(tmins$Latitude)
range(tmins$Elevation_m)
range(tmins$depthN)

#look at the distribution of stations
plot(tmins$Elevation_m, tmins$Latitude)

#make predictions
new_data = expand.grid(Latitude = 30:48, Elevation_m = seq(0, 3500, 100), depthN = seq(5, 100, 15))
new_data$log.depth = log(new_data$depthN)
pred_new = predict(model_depth_2.1, new_data)
range(pred_new)
new_data$pred = pred_new
tickvals = seq(-15, 15, 5) # I still need to figure out how to make all the legends have equal scale
# Plotting
p1_data = new_data[new_data$depthN==5,]
P1 <- ggplot(data=p1_data, aes(x=Elevation_m, y=Latitude, fill=pred)) + 
  geom_tile() +
  ggtitle("Depth = 5 cm") + 
  scale_fill_gradientn(colors=colorRampPalette(c("white","royalblue","seagreen","orange","red","brown"))(500),name="Min Temperature\n[°C]") +
  labs(x = "Elevation [m]",y="Latitude (°N)") +
  theme_bw()

p2_data = new_data[new_data$depthN==20,]
P2 <- ggplot(data=p2_data, aes(x=Elevation_m, y=Latitude, fill=pred)) + 
  geom_tile() +
  ggtitle("Depth = 20 cm") + 
  scale_fill_gradientn(colors=colorRampPalette(c("white","royalblue","seagreen","orange","red","brown"))(500),name="Min Temperature\n[°C]") +
  labs(x = "Elevation [m]",y="Latitude (°N)") +
  theme_bw()


p3_data = new_data[new_data$depthN==50,]
P3 <- ggplot(data=p3_data, aes(x=Elevation_m, y=Latitude, fill=pred)) + 
  geom_tile() +
  ggtitle("Depth = 50 cm") + 
  scale_fill_gradientn(colors=colorRampPalette(c("white","royalblue","seagreen","orange","red","brown"))(500),name="Min Temperature\n[°C]") +
  labs(x = "Elevation [m]",y="Latitude (°N)") +
  theme_bw()

p4_data = new_data[new_data$depthN==95,]
P4 <- ggplot(data=p4_data, aes(x=Elevation_m, y=Latitude, fill=pred)) + 
  geom_tile() +
  ggtitle("Depth = 95 cm") + 
  scale_fill_gradientn(colors=colorRampPalette(c("white","royalblue","seagreen","orange","red","brown"))(500),name="Min Temperature\n[°C]") +
  labs(x = "Elevation [m]",y="Latitude (°N)") +
  theme_bw()

library(cowplot)
plot_grid(P1, P2, P3, P4)

