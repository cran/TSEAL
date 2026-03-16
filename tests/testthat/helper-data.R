testExperiments <- readRDS("../testExperiments.rds")
MedicalClasification <- c(1, 1, 2, 2)

MWA <- MultiWaveAnalysis(testExperiments, "haar")
MWADiscrim <- StepDiscrim(MWA, MedicalClasification, 2)
MWADiscrim1 <- StepDiscrim(MWA, MedicalClasification, 1)


load(system.file("extdata/ECGExample.rda", package = "TSEAL"))
grps <- c(rep(1, 8), rep(2, 8))

MWADiscrimLargue <-
    generateStepDiscrim(ECGExample, grps, "haar", maxvars = 3)
