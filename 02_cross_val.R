######################################################################
# Subtle features in slangbos encroachment, SA
#
# based on script by Urban, M. and Schratz, P.
#
# This script was created by Dr. Marcel Urban (marcel.urban@uni-jena.de),
# Patrick Schratz (p.schratz@lmu.de) and Konstantin Schellenberg (konstantin.schellenberg@posteo.de)
#
# Step 2/4
# Spatial Cross Validation

source("01_import_tuning.R")

######################################################################
#Spatial Cross Validation
######################################################################

# Specify the resampling strategy. Define the outer resampling iterations
outer <- makeResampleDesc("SpRepCV", folds = 5, reps = 100) # add SpRepCV for spatial

# parallization of spatial cross validation (Note: mode="multicore"
# for Unix, mode="socket" for Windows)

if (!file.exists(paste0(rds_path, "resample_rf"))){

    parallelStart(mode = "socket", level = "mlr.resample", cpus = 5)

    # Spatial Cross Validation
    slangbos_spcv <- mlr::resample(wrapper, classif.task, resampling = outer, show.info = TRUE,
                                   measures = list(acc, mmce))

    parallelStop()
    saveRDS(slangbos_spcv, paste0(rds_path, "resample_rf"))

} else {
    slangbos_spcv = readRDS(paste0(rds_path, "resample_rf"))
}


# performance
slangbos_spcv
slangbos_spcv$aggr
slangbos_spcv$measures.test

library(cowplot)

plots = createSpatialResamplingPlots(classif.task, slangbos_spcv, crs = 32735, repetitions = 1)

cowplot::plot_grid(plotlist = plots[["Plots"]], ncol = 2, nrow = 3,
          labels = plots[["Labels"]], label_size = 5)
