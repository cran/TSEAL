## ----include = FALSE----------------------------------------------------------
set.seed(123)
knitr::opts_chunk$set(
  collapse = TRUE,
  cache = FALSE,
  autodep = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(caret)
library(TSEAL)
load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
labels <- c(rep(1,8),rep(2,8))

## ----eval=FALSE---------------------------------------------------------------
# parameters <- testFilters(ECGExample, labels, maxvars = 2, trainSize = 1.0)

## ----echo=TRUE----------------------------------------------------------------
load(system.file("extdata/parameters.rda",package = "TSEAL"))
length(parameters)
filteredParameters <- filterParameters(parameters, 1)
length(filteredParameters)


## ----echo=TRUE----------------------------------------------------------------
filteredParameters <- Filter(function(x) x$Method =="linear",
                             filteredParameters)
min_feature_elements <- filteredParameters[
  (feature_lengths <- vapply(
    filteredParameters,
    function(x) length(x$Features),
    integer(1)
  )) == min(feature_lengths)
]
length(min_feature_elements)
table(sapply(min_feature_elements,`[[`,"Features"))

## -----------------------------------------------------------------------------
sample <- sample(0:length(labels),2)
training <- ECGExample[,,-sample]
labelsTraining <- labels[-sample]
test <- ECGExample[,,sample]
labelsTest <- labels[sample]

## ----MWA, eval=FALSE----------------------------------------------------------
# MWA <- MultiWaveAnalysis(series = training, f = "d6", features = c("IQR","DM"))

## ----echo=TRUE----------------------------------------------------------------
MWA

## ----MWAstepMax, eval=FALSE---------------------------------------------------
# MWAStep <- StepDiscrim(MWA = MWA, labels = labelsTraining, maxvars = 2,
#                        features = c("IQR","DM"))
# 

## ----echo=TRUE----------------------------------------------------------------
MWAStep

## ----MWAstepTresh, eval=FALSE-------------------------------------------------
# MWAStepV <- StepDiscrimV(MWA = MWA, labels = labelsTraining, VStep = 2.1,
#                          features = c("IQR","DM"))

## ----echo=TRUE----------------------------------------------------------------
MWAStepV

## ----LOOCV, eval=FALSE--------------------------------------------------------
# LOOCV <- LOOCV(MWAStep, labels = labelsTraining, method = "linear")

## ----echo=TRUE----------------------------------------------------------------
LOOCV

## ----LOOCV array, eval=FALSE--------------------------------------------------
# LOOCV_raw <- LOOCV(training, labels = labelsTraining, f = "d6",features = c("IQR","DM"),
#       maxvars = 2, method = "linear")

## ----echo=TRUE----------------------------------------------------------------
LOOCV_raw

## ----classifier, echo=TRUE----------------------------------------------------
classifier <- trainModel(MWAStep, labels = labelsTraining, method = "linear")

## ----classifierRaw, eval=FALSE------------------------------------------------
# calssifierRaw <- trainModel(training, labels = labelsTraining,
#                             method = "linear", f = "d6",
#                             features = c("IQR","DM"), maxvars = 2 )

## ----classify, echo=TRUE------------------------------------------------------
classification <- classify(test, model = classifier)
CM <- confusionMatrix(as.factor(classification), as.factor(labelsTest))
CM

