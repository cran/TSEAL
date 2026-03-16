test_that("GenerateStepDiscrim", {
    MWADiscrim <- generateStepDiscrim(
        testExperiments,
        MedicalClasification,
        f = "haar",
        maxvars = 1,
        features = c("Var", "Cor"),
        nCores = 2
    )


    m <- read.csv("../Results/StepVar&Cor.csv", header = FALSE)
    expect_equal(sort(values(MWADiscrim)), sort(as.matrix(m)),
                 ignore_attr = TRUE)
})

test_that("testFilters", {
    data <- testFilters(
        ECGExample,
        grps,
        maxvars = 2,
        f = c("haar"),
        features = c("var", "cor"),
        trainSize = 1
    )

    r1 <- LOOCV(
        ECGExample,
        grps,
        f = "haar",
        features = c("var", "cor"),
        method = "linear",
        maxvars = 2,
        returnClassification = TRUE
    )

    expect_equal(r1$classification, data[[5]]$classification)
})


test_that("testFilters Iputs", {
    expect_error(testFilters(grps = MedicalClasification, maxvars = 10))
    expect_error(testFilters(testExperiments, maxvars = 10))
    expect_error(testFilters(testExperiments, MedicalClasification))

    expect_error(testFilters(
        testExperiments,
        MedicalClasification,
        maxvars = 10,
        features = c()
    ))
    expect_error(testFilters(
        testExperiments,
        MedicalClasification,
        maxvars = 10,
        filters = c()
    ))
})

test_that("GenerateStepDiscrim Iputs", {
    grps <- c(1, 1, 2, 2)

    expect_error(generateStepDiscrim(
        grps = MedicalClasification,
        f = "haar",
        maxvars = 10
    ))
    expect_error(generateStepDiscrim(ECGExample, f = "haar", maxvars = 10))
    expect_error(generateStepDiscrim(ECGExample, "haar"))
    expect_error(generateStepDiscrim(
        ECGExample,
        "haar",
        maxvars = 10,
        VStep = 1
    ))
    expect_error(generateStepDiscrim(ECGExample, "haar", maxvars = 0))
    expect_error(generateStepDiscrim(ECGExample, "haar", VStep = 0))
})
