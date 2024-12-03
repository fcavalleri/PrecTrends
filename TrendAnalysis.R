library(raster)
library(rgdal)
library(zoo)
library(RColorBrewer)
library(sp)
library(maps)
library(maptools)
library(trend)


# This script performs an analysis of precipitation trends using reanalysis and observational data.
# It calculates the bias between the reanalysis and observational data, and applies rolling averages to the bias series.
# The script also includes masking of the data with the observational domain and handling of missing values.

# Define reanalysis and observational datasets
reans <- c("ERA5-HRES","ERA5-EDA","CERA20C","ERA20CM","ERA20C")
obss <- c("UniMi/ISAC-CNR","LAPrec1901","NGCD1","NGCD2")

# Define output path for the generated plots
path_out <- "/media/met_mods2/METEO/Rielab/DataForValidation/Prec/Climatologies/Trends/ASL_Fig/"

# Load world map shapefile
world <- map(fill = TRUE, col="transparent", plot=FALSE)
IDs <- sapply(strsplit(world$names, ":"), function(x) x[1])
world <- map2SpatialPolygons(world, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

# Load shapefiles for UniMi and LAPrec domins
shp_unimi <- readOGR("/media/met_mods2/METEO/Rielab/DataForValidation/Obs/UniMi-ISAC-CNR/shape_unimi_remapped_ERA5.shp")
shp_laprec <- readOGR("/media/met_mods2/METEO/Rielab/DataForValidation/Obs/LAPrec1901/shape_laprec_remapped_ERA5.shp")

# Define the domain of interest
domain <- 1

# Select the observational dataset based on the domain
obs <- obss[domain]

# Define ensemble numbers
ens <- 0:9

# Perform analysis based on the selected domain
if (domain == 1){
    # Load reanalysis data for ERA5, ERA5-EDA, CERA20C, and ERA20CM
    rean_raster_list <- list()
    rean_raster_list[[1]] <- stack("/media/met_mods2/METEO/Rielab/DataForValidation/Reanalyses/ERA5/tp/with_GAR_1940-2020/Cumulates/ERA5_tp_yearcumul_new.nc") # nolint
    path_in_era5enda <- "/media/met_mods2/METEO/Rielab/DataForValidation/Reanalyses/ERA5_ENDA/tp/"
    for (i in 1:10){
        filename <- paste0(path_in_era5enda,"ens",ens[i],"/rr_cut_yearcumuls_1940-2020.nc") 
        rean_raster_list[[i+1]] <- stack (filename)
    }
    path_in_cera20c <- "/media/met_mods2/METEO/Rielab/DataForValidation/Reanalyses/CERA20C/tp/"
    for (i in 1:10){
        filename <- paste0(path_in_cera20c,"ens",ens[i],"/rr_cut_yearcumuls_1901-2010.nc") 
        rean_raster_list[[i+11]] <- stack (filename)
    }
    path_in_era20cm <- "/media/met_mods2/METEO/Rielab/DataForValidation/Reanalyses/ERA20CM/tp/"
    for (i in 1:10){
        filename <- paste0(path_in_era20cm,"ens",ens[i],"/rr_cut_yearcumuls_1901-2010.nc") 
        rean_raster_list[[i+21]] <- stack (filename)
    }
    rean_raster_list[[32]]  <- stack("/media/met_mods2/METEO/Rielab/DataForValidation/Reanalyses/ERA20C/tp/era20c_rr_cut_yearcumuls_1901-2010.nc")
    
    # Load observational data for UniMi-ISAC-CNR
    obs_raster <- stack("/media/met_mods2/METEO/Rielab/DataForValidation/Obs/UniMi-ISAC-CNR/tp_mon_upscaled_ERA5/Cumulates/Obs_Unimi_yearcumul_1921-2020.nc")
    start_years_obs <- 1921
    end_years_obs <- 2020
    shp_domain <- readOGR("/media/met_mods2/METEO/Rielab/DataForValidation/Obs/UniMi-ISAC-CNR/shape_unimi_remapped_ERA5.shp")
}

if (domain == 2){
    # Load reanalysis data for ERA5, ERA5-EDA, CERA20C, and ERA20CM
    rean_raster_list <- list()
    rean_raster_list[[1]] <- stack("/media/met_mods2/METEO/Rielab/DataForValidation/Reanalyses/ERA5/tp/with_GAR_1940-2020/Cumulates/ERA5_tp_yearcumul_new.nc") # nolint
    path_in_era5enda <- "/media/met_mods2/METEO/Rielab/DataForValidation/Reanalyses/ERA5_ENDA/tp/"
    for (i in 1:10){
        filename <- paste0(path_in_era5enda,"ens",ens[i],"/rr_cut_yearcumuls_1940-2020.nc") 
        rean_raster_list[[i+1]] <- stack (filename)
    }
    path_in_cera20c <- "/media/met_mods2/METEO/Rielab/DataForValidation/Reanalyses/CERA20C/tp/"
    for (i in 1:10){
        filename <- paste0(path_in_cera20c,"ens",ens[i],"/rr_cut_yearcumuls_1901-2010.nc") 
        rean_raster_list[[i+11]] <- stack (filename)
    }
    path_in_era20cm <- "/media/met_mods2/METEO/Rielab/DataForValidation/Reanalyses/ERA20CM/tp/"
    for (i in 1:10){
        filename <- paste0(path_in_era20cm,"ens",ens[i],"/rr_cut_yearcumuls_1901-2010.nc") 
        rean_raster_list[[i+21]] <- stack (filename)
    }
    rean_raster_list[[32]]  <- stack("/media/met_mods2/METEO/Rielab/DataForValidation/Reanalyses/ERA20C/tp/era20c_rr_cut_yearcumuls_1901-2010.nc")
    
    # Load observational data for LAPrec1901
    obs_raster <- stack("/media/met_mods2/METEO/Rielab/DataForValidation/Obs/LAPrec1901/LAPrec1901_yearcumul_remapped_ERA5.nc")
    shp_domain <- readOGR("/media/met_mods2/METEO/Rielab/DataForValidation/Obs/LAPrec1901/shape_laprec_remapped_ERA5.shp")
}

# Perform masking of the reanalysis data with the observational domain
n_series <- length(rean_raster_list)
for (i in 1:n_series){
    rean_raster_list[[i]] <- mask(rean_raster_list[[i]],shp_domain)
}

# Calculate series of average values for the reanalysis and observational data
list_series <- list()
for (i in 1:n_series){
    list_series[[i]] <- as.numeric(as.character(cellStats(rean_raster_list[[i]],mean)))
}
obs_series <- as.numeric(as.character(cellStats(obs_raster,mean)))

# Complete the series with NA for the whole time period
start <- 1901
end <- 2020
years_all <- seq(start,end,1)

start_years <- c(1940,rep(1940,10),rep(1901,10),rep(1901,10),1901)
end_years <- c(2020,rep(2020,10),rep(2010,10),rep(2010,10),2010)

if (domain==1){
    obs_series <- c(rep(NA,start_years_obs-years_all[1]),obs_series)
    for (i in 1:n_series){
        list_series[[i]] <- c(rep(NA,start_years[i]-years_all[1]),list_series[[i]],rep(NA,rev(years_all)[1]-end_years[i]))
    }
}

if (domain==2){
    for (i in 1:n_series){
        list_series[[i]] <- c(rep(NA,start_years[i]-years_all[1]),list_series[[i]],rep(NA,rev(years_all)[1]-end_years[i]))
    }
}

# Calculate bias for each series
bias_series <- list()
for (i in 1:n_series){
    rean_series <- list_series[[i]]
    bias_series[[i]] <- (rean_series - obs_series)
}

### Average bias for 1940-2010

for (i in 1:n_series){
   writeLines(paste(mean(bias_series[[i]][40:110])))
}

clim_val <- mean(obs_series[61:90])

# Apply rolling averages to the bias series
n <- 3  # width of the window
buff <- (n-1)/2
x_time <- seq(years_all[1]+buff,rev(years_all)[1]-buff,1)
x_ticks <- seq(1910,2020,10)

bias_series_rll <- list()
for (i in 1:n_series){
    bias_series_rll[[i]] <- rollapply(as.numeric(bias_series[[i]]),n,FUN=mean)
}


if (domain==1){y_ticks <- seq(-600,200,100)}
if (domain==2){y_ticks <- seq(-400,300,100)}

# 6. plot with polygons

pdf(file = paste0(path_out,"Figure_",domain+1,"_bis.pdf"), width = 8, height = 7)

plot(c(1900,2020),c(0,0),lty=2,lwd=1,col="black",
xlim=c(1900,max(x_time)),ylim=c(min(y_ticks),max(y_ticks)),
xlab = "", ylab = "Reanalysis - Observations (mm/year)")
grid(nx = NULL,
     ny = NA,
     lty = 1, col = "gray", lwd = 1)
     
lines(c(1900,2020),c(0,0),lty=2,lwd=1,col="gray")
axis(1, at = seq(1910,2010,10))
axis(2, at = y_ticks)
axis(4, at = y_ticks/1000*clim_val, labels = paste0(y_ticks/10, "%"))

vals <- matrix(unlist(bias_series_rll[22:31]),nrow=length(x_time))
up_poly <- apply(vals, 1, max, na.rm=FALSE)
min_poly <- apply(vals, 1, min, na.rm=FALSE)
polygon( c(x_time[!is.na(min_poly)],rev(x_time[!is.na(min_poly)])), 
c(min_poly[!is.na(min_poly)],rev(up_poly[!is.na(min_poly)])),
 lwd=1.5, col="darkolivegreen2", density=0, angle=-45)
lines(x_time,bias_series_rll[[22]],col="darkolivegreen3",lwd=3)

lines(x_time,bias_series_rll[[32]],col="chartreuse4",lwd=3)

vals <- matrix(unlist(bias_series_rll[12:21]),nrow=length(x_time))
up_poly <- apply(vals, 1, max, na.rm=FALSE)
min_poly <- apply(vals, 1, min, na.rm=FALSE)
polygon( c(x_time[!is.na(min_poly)],rev(x_time[!is.na(min_poly)])), 
c(min_poly[!is.na(min_poly)],rev(up_poly[!is.na(min_poly)])), 
lwd=1.5, col="coral1", density=10, angle=45)
lines(x_time,bias_series_rll[[12]],col="coral2",lwd=3)

vals <- matrix(unlist(bias_series_rll[2:11]),nrow=length(x_time))
up_poly <- apply(vals, 1, max, na.rm=FALSE)
min_poly <- apply(vals, 1, min, na.rm=FALSE)
polygon( c(x_time[!is.na(min_poly)],rev(x_time[!is.na(min_poly)])), 
c(min_poly[!is.na(min_poly)],rev(up_poly[!is.na(min_poly)])), 
lwd=1.5, col="cadetblue1", density=20, angle=-60)
lines(x_time,bias_series_rll[[2]],col="cadetblue3",lwd=3)

lines(x_time,bias_series_rll[[1]],type='l',col="dodgerblue4",lwd=3)

if (domain==1){
    legend(1901, 300, legend=reans,
    col=c(), lwd=2, cex=0.8, bg="white")
}

dev.off()



#### SLOPES

library(trend)

periods <- list(40:70,70:95,95:120,40:110)

i<-4
for (j in 1:32){

series <- bias_series[[j]][periods[[i]]]

slope_obj <- sens.slope(series,conf.level=0.95)
slope <- slope_obj$estimates
sigma <- abs(slope_obj$conf.int[2]-slope_obj$conf.int[1])/2
mk_obj <- mk.test(series, alternative = c("two.sided"), continuity = TRUE)
p_value <- mk_obj$p.value

    dec_slope <- slope*10
    dec_sigma <- sigma*10

#str_slope <- paste0(reans[j]," in ",years_all[periods[[i]]][1],"-",rev(years_all[periods[[i]]])[1]," : ",sprintf("%.6f +/- %.6f ",dec_slope,dec_sigma),
#" mm/decade, p-val = ",sprintf("%.6f",p_value))

writeLines(paste(p_value))

}


# Slopes analyses for the selected reanalyses / years

reans <- c("ERA5-HRES","ERA5-EDA","CERA20C","ERA20CM","ERA20C")

my_sen_slope <- function(series, years_all, period) {
    slope_obj <- sens.slope(series, conf.level = 0.95)
    slope <- slope_obj$estimates
    sigma <- abs(slope_obj$conf.int[2] - slope_obj$conf.int[1]) / 2
    mk_obj <- mk.test(series, alternative = c("two.sided"), continuity = TRUE)
    p_value <- mk_obj$p.value

    dec_slope <- slope * 10
    dec_sigma <- sigma * 10

    str_slope <- paste0("Slope: ", sprintf("%.6f +/- %.6f ", dec_slope, dec_sigma),
                        "mm/decade, p-val = ", sprintf("%.6f", p_value))
    return(list(slope = dec_slope, sigma = dec_sigma, p_value = p_value, str_slope = str_slope))
}

# Example usage of the function
for (j in 1:32) {
    series <- bias_series[[j]][periods[[4]]]
    result <- my_sen_slope(series, years_all, periods[[4]])
    writeLines(result$str_slope)
}

for (i in 1:5) {
    series <- bias_series[[indx[i]]][periods[[4]]]
    result <- my_sen_slope(series, years_all, periods[[4]])
    writeLines(result$str_slope)
}
