# Title     : TODO
# Objective : TODO
# Created by: ivan
# Created on: 8/4/21

D3toD2 <- function(i, j, k, nRows, nCols, nPages) {
    if (missing(i) && missing(j)) {
        return(list(k, list(0, nRows * nCols)))
    }

    if (missing(i)) {
        return(k, list)
    }

    if (missing(j)) {
        return(list(k, list((i - 1) * nCols + 1, (i) * nCols)))
    }

    return(list(k, list(i * nCols + j, i * nCols + j)))
}

#' Generate StepDiscrim from raw data
#'
#' This function allows to obtain in a single step the complete
#' MultiWaveAnalysis and the selection of the most discriminating variables of
#' the MultiWaveAnalysis.
#'
#' @param series Sample from the population (dim x length x cases)
#' @param labels Labeled vector that classify the observations
#' @param f Selected filter for the MODWT (to see the available filters use the
#'        function \code{\link{availableFilters}}
#' @param maxvars Maximum number of variables included by the StepDiscrim
#'        algorithm (Note that if you defined this, can not define VStep). Must
#'        be a positive integer
#' @param VStep Minimum value of V above which all other variables are
#'        considered irrelevant and therefore will not be included. (Note that
#'        if you defined this, can not defined maxvars).Must be a positive
#'        number. For more information see StepDiscrim documentation.
#' @param lev Determines the number of decomposition levels for MODWT
#'        (by default the optimum is calculated). Must be a positive integer,
#'        where 0 corresponds to the default behavior.
#' @param features A list of characteristics that will be used for the
#'        classification process. To see the available features see
#'        \code{\link{availableFeatures}}
#' @param nCores Determines the number of processes that will be used in the
#'        function, by default it uses all but one of the system cores.
#'        Must be a positive integer, where 0 corresponds to the default
#'        behavior
#'
#' @return A MultiWaveAnalysis with the most discriminant variables based on the
#'         features indicated.
#'
#' @seealso
#' * \code{\link{MultiWaveAnalysis}}
#' * \code{\link{StepDiscrim}}
#' * \code{\link{StepDiscrimV}}
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' # The dataset has the first 8 elements of class 1
#' # and the last 8 of class 2.
#' labels <- c(rep(1, 8), rep(2, 8))
#' MWADiscrim <- generateStepDiscrim(ECGExample, labels, "haar",
#'   features = c("Var"), maxvars = 5
#' )
#' # or using the VStep option
#' MWADiscrim <- generateStepDiscrim(ECGExample, labels, "haar",
#'  features = c("Var", "Cor"), VStep = 0.7
#' )
#' }
#' @export
generateStepDiscrim <-
    function(series,
             labels,
             f,
             maxvars,
             VStep,
             lev = 0,
             features = c("Var", "Cor", "IQR", "PE", "DM"),
             nCores = 0) {
        if (missing(series)) {
            stop("The argument \"series\" must be provided.")
        }
        if (missing(f)) {
            stop(
                "The argument \"f\" (filter) must be provided.
                       To see available filter use availableFilters()"
            )
        }
        if (missing(labels)) {
            stop("The argument \"labels\" must be provided.")
        }

        if (length(dim(series)) != 3) {
            stop(
                "It seems that a dimension is missing, in case your series
                contains only one case, make sure that you have activated the
                option \"drop = FALSE\" as in the following example
                Series1 = Series2 [,,1, drop = FALSE]."
            )
        }

        if (length(labels) != dim(series)[3]) {
            stop("The number of observations in the data and those provided in
            labels do not match.")
        }

        if (missing(maxvars) && missing(VStep)) {
            stop("maxvars o VStep must be defined")
        }
        if (!missing(maxvars) && !missing(VStep)) {
            stop("only maxvars or VStep can be defined.")
        }

        if (!missing(maxvars)) {
            if (!is.numeric(maxvars) || length(maxvars) != 1 || maxvars <= 0) {
                stop("The argument \"maxvars\" must be an integer greater than
                     0")
            }
        } else {
            if (!is.numeric(VStep) || length(VStep) != 1 || VStep <= 0) {
                stop("The argument \"VStep\" must be a number greater than 0")
            }
        }

        f <- tolower(f)

        MWA <- MultiWaveAnalysis(series, f, lev, features, nCores)
        if (!missing(maxvars)) {
            MWA <- StepDiscrim(MWA, labels, maxvars, features, nCores)
        } else {
            MWA <- StepDiscrimV(MWA, labels, VStep, features, nCores)
        }

        return(MWA)
    }

#' testFilters
#'
#' This function performs a test with a series of filters defined by the user,
#' for the maximum number of variables determined. This function can be used to
#' compare the performance of different filters with a different number of
#' variables to be considered and the differences between a linear and a
#' quadratic discriminant.
#'
#' @param series Samples from the population (dim x length x cases)
#' @param labels Labeled vector that classify the observations.
#' @param maxvars maximum number of variables included by the StepDiscrim
#'        algorithm. Must be grater than 0 and, in normal cases, lesser than 100
#' @param filters Vector indicating the filters to be tested. To see the
#'        available filters use the function \code{\link{availableFilters}}
#' @param features A list of characteristics that will be used for the
#'        classification process. To see the available features see
#'        \code{\link{availableFeatures}}
#' @param lev Wavelet decomposition level, by default is selected using the
#'        "conservative" strategy. See \code{\link{chooseLevel}} function.
#' @param trainSize allows you to select only a subset of the data. Increases
#'        speed at the expense of accuracy of results.
#' @param linear Results are generated for a linear model (lda),
#' @param quadratic Results are generated for a quadratic model (qda)
#'
#' @return A list that each element contains:
#'   * CM: confusion matrix with a particular configuration using LOOCV
#'   * Classification: a vector with the raw classification result. "1" if the
#'                     observation belongs to the population 1 and "2" if
#'                    belongs to the population 2.
#'   * NVars: the total numbers of variables have been taken into account in the
#'            classification process
#'   * Method: type of classifier used.
#'   * Filter: filter used in the MultiWave analysis process
#'   * Features: vector containing the features taken into account
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' # The dataset has the first 8 elements of class 1
#' # and the last 8 of class 2.
#' labels <- c(rep(1, 8), rep(2, 8))
#' result <- testFilters(ECGExample, labels, features=c("var","cor"),
#'           filters= c("haar","d4"), maxvars = 3, trainSize = 1)
#' }
#'
#' @export
#'
#' @seealso
#' * \code{\link{LOOCV}}
#' * \code{\link{MultiWaveAnalysis}}
#' * \code{\link{StepDiscrim}}
#' * \code{\link{availableFilters}}
#' * \code{\link{availableFeatures}}
#'
#' @md
testFilters <- function(series,
                        labels,
                        maxvars,
                        filters = c("haar", "d4", "d6", "d8", "la8"),
                        features = c("Var", "Cor", "IQR", "PE", "DM"),
                        lev = 0,
                        trainSize = 0.2,
                        linear = TRUE,
                        quadratic = TRUE) {
    anyMissing(c(series, labels, maxvars))
    checkmate::assertFlag(linear)
    checkmate::assertFlag(quadratic)
    if (length(filters) == 0) {
        stop(
            "At least one filter must be provided. To see the available filters
            use availableFilters()"
        )
    }

    if (length(features) == 0) {
        stop(
            "At least one feature must be provided. To see the available filters
         use availableFeatures()"
        )
    }

    if (length(dim(series)) != 3) {
        stop(
            "It seems that a dimension is missing, in case your series contains
         only one case, make sure that you have activated the option
         \"drop = FALSE\" as in the following example
         Series1 = Series2 [,,1, drop = FALSE]."
        )
    }

    if (length(labels) != dim(series)[3]) {
        stop("The number of observations in the data and those provided in
        labels do not match.")
    }

    data <- list()
    nFeatures <- length(features)
    nCases <- dim(series)[3]
    if (trainSize < 1) {
        sample <- sample (1:nCases,
                          size = nCases * trainSize,
                          replace = FALSE)
        sampledSeries <- series[, , sample]
        sampledLabels <- labels[sample]
    } else {
        sampledSeries <- series
        sampledLabels <- labels
    }

    for (f in filters) {
        MWA <- MultiWaveAnalysis(sampledSeries, f)
        for (i in seq_len(nFeatures)) {
            comFeatures <- combn(features, i)
            listFeatures <- split(comFeatures, rep(seq_len(ncol(
                comFeatures
            )), each = nrow(comFeatures)))
            for (cFeatures in listFeatures) {
                aux <- StepDiscrimRaw_(MWA, sampledLabels, maxvars, cFeatures)
                Tr <- aux[[1]]
                incl <- aux[[2]]
                maxVar <- min(maxvars, length(incl))
                for (v in seq(2, maxVar)) {
                    filterValues <- Tr[incl[seq_len(v)], ]
                    MWAAux <- list(
                        Features = list(
                            Var = filterValues,
                            Cor = NA,
                            IQR = NA,
                            DM = NA,
                            PE = NA
                        ),
                        StepSelection = list(
                            Var = NA,
                            Cor = NA,
                            IQR = NA,
                            DM = NA,
                            PE = NA
                        ),
                        Observations = MWA$Observations,
                        NLevels = MWA$NLevels,
                        Filter = MWA$Filter
                    )
                    attr(MWAAux, "class") <- "MultiWaveAnalysis"
                    if (linear) {
                        aux <- LOOCV(MWAAux,
                                     sampledLabels,
                                     "linear",
                                     returnClassification = TRUE)
                        data <- append(data, list(
                            list(
                                CM = aux[[1]],
                                classification = aux[[2]],
                                NVars = v,
                                Method = "linear",
                                filter = f,
                                Features = cFeatures
                            )
                        ))
                    }
                    if (quadratic) {
                        aux <- LOOCV(MWAAux,
                                     sampledLabels,
                                     "quadratic",
                                     returnClassification = TRUE)
                        data <- append(data, list(
                            list(
                                CM = aux[[1]],
                                classification = aux[[2]],
                                NVars = v,
                                Method = "quadratic",
                                filter = f,
                                Features = cFeatures
                            )
                        ))
                    }
                }
            }
        }
    }
    return(data)
}

#' filterParameters
#'
#' This function allows to filter the results obtained by the
#' \code{\link{testFilters}} function according to the precision achieved.
#'
#' @param data Collection of parameter tests obtained with the
#'             \code{\link{testFilters}} function.
#' @param accuracy  Determines from which precision the elements will be
#'                  selected. For example, a value of 0.9 will select all
#'                   combinations with a precision equal to or greater than 0.9.
#' @return The parameter combinations that have achieved an accuracy greater
#' than or equal to that indicated in the accuracy parameter.
#'
#' @examples
#' \donttest{
#' load(system.file("extdata/ECGExample.rda",package = "TSEAL"))
#' # The dataset has the first 5 elements of class 1
#' # and the last 5 of class 2.
#' labels <- c(rep(1, 8), rep(2, 8))
#' result <- testFilters(ECGExample, labels, features=c("var","cor"),
#'           filters= c("haar","d4"), maxvars = 3, trainSize = 1)
#' filterResult <- filterParameters(result, 0.9)
#' }
#'
#' @export
#' @seealso
#' * \code{\link{testFilters}}
#'
#' @md

filterParameters <- function(data, accuracy) {
    Filter(function (x) {
        val <- x$CM$overall["Accuracy"]
        r = !is.null(val) && !is.na(val) && val >= accuracy
    }, data)
}

mat_to_string <- function(mat, indent = 0) {
    if (!is.matrix(mat)) {
        mat <- matrix(mat, nrow = 1)
    }

    widths <- apply(mat, 2, function(x)
        max(nchar(as.character(x))))
    mat_formateada <- apply(mat, 2, function(x, w)
        format(x, width = w, justify = "right"), w = widths)

    if (!is.matrix(mat_formateada)) {
        mat_formateada <- matrix(mat_formateada, nrow = 1)
    }

    mat_lines <- apply(mat_formateada, 1, paste, collapse = " ")

    if (length(mat_lines) > 1) {
        indent_str <- paste(rep("\t", indent), collapse = "")
        mat_lines <- c(mat_lines[1], paste0(indent_str, mat_lines[-1]))
    }

    mat_string <- paste(mat_lines, collapse = "\n")

    return(mat_string)
}

mgrinit <- function (cls) {
    # set up so that each worker node will have a global variable myinfo
    # that contains the thread ID and number of threads
    setmyinfo <- function(i, n) {
        assign("myinfo", list(id = i, nwrkrs = n), pos = tmpenv)
    }
    ncls <- length(cls)
    parallel::clusterEvalQ(cls, tmpenv <- new.env())
    parallel::clusterApply(cls, seq_len(ncls), setmyinfo, ncls)
    parallel::clusterEvalQ(cls, myinfo <- get("myinfo", tmpenv))

    parallel::clusterExport(cls, "getidxs", envir = loadNamespace("TSEAL"))
}

mgrmakevar <- function(cls, varname, nr, nc) {
    matrix <- bigmemory::big.matrix(nrow = nr,
                                    ncol = nc,
                                    type = "double")
    assign(varname, matrix, pos = parent.frame())
    parallel::clusterExport(cls, "varname", envir = environment())
    desc <- bigmemory::describe(matrix)
    parallel::clusterExport(cls, "desc", envir = environment())
    parallel::clusterEvalQ(cls, matrix <- bigmemory::attach.big.matrix(desc))
    parallel::clusterEvalQ(cls, assign(varname, matrix))
}

getidxs <- function(m) {
    parallel::splitIndices(m, myinfo$nwrkrs)[[myinfo$id]]
}

#' @importFrom parallel stopCluster
#' @noRd
stoprdsm <- function(cls) {
    stopCluster(cls)
    rm(cls)
}

utils::globalVariables("myinfo")
