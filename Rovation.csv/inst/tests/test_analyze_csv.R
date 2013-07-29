# Test import_csv

library(testthat)
Sys.setenv(NOAWT=1)
library(Rovation)
Sys.unsetenv("NOAWT")
library(plyr)

context("Analyzing tabular data from collected Measurements")

EpochFixture <- function(ctx, exp=NULL) {
  if(is.null(exp)) {
    project <- ctx$insertProject("unused", "unused", Datetime(2013,7,1))
    exp <- project$insertExperiment("unused", Datetime(2013, 7, 1))
  }
  
  protocol <- ctx$insertProtocol("unused", "unused")
  
  protocol.parameters = list(param.1=1, param.2="abc")
  device.parameters = list(param.1=10, param.2="xyz")
  
  start.time <- Datetime(2013,1,1)
  end.time <- Datetime(2013,1,2)
  
  epoch <- ImportCSV(ctx,
                     "../fixtures/example_fixture.csv",
                     start.time,
                     end.time,
                     protocol$getURI()$toString(),
                     protocol.parameters,
                     Vector2Set(c()),
                     device.parameters,
                     exp$getURI()$toString())
  
  return(epoch)
}


test_that("collects one Epoch", {
  TestWrapper("collects-one-epoch", function(ctx) {
    epoch <- EpochFixture(ctx)
    experiment <- epoch$getExperiment()
    
    expect_that(length(CollectEpochs(experiment)), equals(1))
  })
})


test_that("collects multiple Epochs", {
  TestWrapper("collects-one-epoch", function(ctx) {
    epoch <- EpochFixture(ctx)
    experiment <- epoch$getExperiment()
    epoch2 <- EpochFixture(ctx, experiment)
    
    expect_that(length(CollectEpochs(experiment)), equals(2))
  })
})


test_that("collects homgenous tabular data", {
  TestWrapper("collects-homogenous-tabular-data", function(ctx) {
    epoch <- EpochFixture(ctx)
    experiment <- epoch$getExperiment()
    epoch2 <- EpochFixture(ctx, experiment)
    
    expected.df <- rbind(read.csv("../fixtures/example_fixture.csv"),
                         read.csv("../fixtures/example_fixture.csv"))
    
    actual.df <- CollectMeasurements(CollectEpochs(experiment))
    
    expect_equal(length(expected.df), length(actual.df))
    for(col in colnames(expected.df)) {
      expect_true(col %in% colnames(actual.df))
    }
  })
})


test_that("ignores non-tabular measurement data", {
  TestWrapper("ignores-non-tabular-measurement-data", function(ctx) {
    epoch <- EpochFixture(ctx)
    
    epoch$insertMeasurement("distraction", Vector2Set(c("label.1-id.1")), Vector2Set(c()), NewUrl(paste(getwd(), "/../fixtures/example_fixture2.csv", sep="")), "other/type")
    
    experiment <- epoch$getExperiment()
    
    actual.df <- CollectMeasurements(CollectEpochs(experiment))
    expected.df <- rbind(read.csv("../fixtures/example_fixture.csv"))
    
    expect_equal(length(actual.df), length(expected.df))
    
  })
})


test_that("merges non-homogenous tabular data", {
  TestWrapper("merges-non-homogenous-tabular-data", function(ctx) {
    epoch <- EpochFixture(ctx)
    
    epoch$insertMeasurement("other-measurement", Vector2Set(c("label.1-id.1")), Vector2Set(c()), NewUrl("../fixtures/example_fixture2.csv"), "text/csv")
    
    experiment <- epoch$getExperiment()
    
    actual.df <- CollectMeasurements(CollectEpochs(experiment))
    expected.df <- rbind.fill(read.csv("../fixtures/example_fixture.csv"),
                         read.csv("../fixtures/example_fixture2.csv"))
    
    expect_equal(length(expected.df), length(actual.df))
    for(col in colnames(expected.df)) {
      expect_true(col %in% colnames(actual.df))
    }
    
  })
})