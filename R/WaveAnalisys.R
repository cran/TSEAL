WaveAnalysis <- function(fVar = NA,
                         fCor = NA,
                         fIQR = NA,
                         fDM = NA,
                         fPE = NA,
                         stVar = NA,
                         stCor = NA,
                         stIQR = NA,
                         stDM = NA,
                         stPE = NA,
                         siVar = NA,
                         siCor = NA,
                         siIQR = NA,
                         siDM = NA,
                         siPE = NA,
                         observations,
                         signals,
                         NLevels,
                         filter) {
    x <- list(
        Features = list(
            Var = fVar,
            Cor = fCor,
            IQR = fIQR,
            DM = fDM,
            PE = fPE
        ),
        StepSelection = list(
            Var = stVar,
            Cor = stCor,
            IQR = stIQR,
            DM = stDM,
            PE = stPE
        ),
        SignalSelection = list(
            Var = siVar,
            Cor = siCor,
            IQR = siIQR,
            DM = siDM,
            PE = siPE
        ),
        Observations = observations,
        Signals = signals,
        NLevels = NLevels,
        Filter = filter
    )
    attr(x, "class") <- "MultiWaveAnalysis"
    return(x)
}

#' @export
print.MultiWaveAnalysis <- function(x, ...) {
    summary(x)
}



#' @export
summary.MultiWaveAnalysis <- function(object, ...) {
    MWA <- object
    InitStr <- paste(
        "MultiWave Analysis Object:",
        "\n\tNumber of Observations: ",
        MWA$Observations,
        "\n\tNumber of decomposing levels: ",
        MWA$NLevels,
        "\n\tFilter used: ",
        MWA$Filter,
        sep = ""
    )


    FeaturesStr <- paste("\tStored Features per observation:")
    for (feature in names(MWA$Features)) {
        if (!all(is.na(MWA$Features[[feature]]))) {
            NFeature <- dim(MWA$Features[[feature]])[1]
            FeaturesStr <-
                paste(FeaturesStr,
                      paste("\n\t\t-", feature, ": ", NFeature, sep = ""))
        }
    }

    SelectionStr <- paste("\tVariables selected by the selection process:\n")
    if (all(is.na(MWA$StepSelection))) {
        SelectionStr <-
            paste(
                SelectionStr,
                "\t\t- This MultiWaveAnalysis object has not gone",
                "through the variable selection process."
            )
    } else {
        SelectionStr <-
            paste(SelectionStr,
                  "\t(Note that they refer to the index before being",
                  "filtered):")

        for (feature in names(MWA$StepSelection)) {
            if (!all(is.na(MWA$StepSelection[[feature]]))) {
                selected <- MWA$StepSelection[[feature]]
                SelectionStr <- paste(SelectionStr,
                                      paste(
                                          "\n\t\t-",
                                          feature,
                                          ": ",
                                          paste(selected, collapse = ", "),
                                          sep = ""
                                      ))
            }
        }
    }

    SelectionSignalsStr <- paste("\tSignals chosen by the classifier:")
    if (all(is.na(MWA$SignalSelection))) {
        SelectionSignalsStr <- paste(
            SelectionSignalsStr,
            "\n\t\t- This MultiWaveAnalysis object has not gone",
            "through the variable selection process."
        )
    } else {
        for (feature in names(MWA$SignalSelection)) {
            if (!all(is.na(MWA$SignalSelection[[feature]]))) {
                selected <- MWA$SignalSelection[[feature]]
                SelectionSignalsStr <- paste(
                    SelectionSignalsStr,
                    "\n\t\t-",
                    feature,
                    ":\t",
                    mat_to_string(selected, 4),
                    sep = ""
                )
            }
        }
    }

    message(paste(
        InitStr,
        FeaturesStr,
        SelectionStr,
        SelectionSignalsStr,
        sep = "\n"
    ))
}

values <- function(MWA) {
    stopifnot(is(MWA, "MultiWaveAnalysis"))
    values <- matrix(0, nrow = 0, ncol = MWA$Observations)
    for (feature in MWA$Features) {
        if (!(is.na(feature[1]))) {
            values <- rbind(values, as.matrix(feature))
        }
    }
    return(values)
}

#' Extract observations from a MultiWaveAnalysis
#'
#' This function permits to extract certain observations from a
#'  MultiWaveAnalysis
#'
#' @param MWA MultiWaveAnalysis from which the desired observations will be
#'  extracted
#' @param indices Indices that will indicate which observations will be
#'        extracted
#'
#'
#' @return A list with two elements:
#'  * MWA: The MultiWaveAnalysis provided minus the extracted observations.
#'  * MWAExtracted: A new MultiWaveAnalysis with the extracted observations
#' @export
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' MWA <- MultiWaveAnalysis(ECGExample, "haar", features = "Var")
#' aux <- extractSubset(MWA, c(1, 2, 3))
#' MWATrain <- aux[[1]]
#' MWATest <- aux[[2]]
#' }
#' @md
extractSubset <- function(MWA, indices) {
    if (missing(MWA)) {
        stop("The argument \"MWA\" must be provided.")
    }
    if (missing(indices)) {
        stop("The argument \"indices\" must be provided.")
    }

    n <- length(indices)

    MWA1 <- MWA
    MWA1$Observations <- n

    MWA2 <- MWA
    MWA2$Observations <- MWA2$Observations - n

    for (feature in names(MWA$Features)) {
        if (!(is.na(MWA$Features[[feature]][1]))) {
            MWA1$Features[[feature]] <-
                MWA1$Features[[feature]][, indices, drop = FALSE]

            MWA2$Features[[feature]] <-
                MWA2$Features[[feature]][, -indices, drop = FALSE]
        }
    }
    return(list(MWA1, MWA2))
}
