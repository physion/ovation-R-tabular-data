# Test import_csv

library(testthat)
Sys.setenv(NOAWT=1)
library(Rovation)
Sys.unsetenv("NOAWT")
library(plyr)

context("Importing tabular data with ImportCSV")

test_that("inserts one Epoch", {
  TestWrapper("inserts-one-epoch", function(ctx) {
    project <- ctx$insertProject("unused", "unused", Datetime(2013,7,1))
    exp <- project$insertExperiment("unused", Datetime(2013, 7, 1))
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
    
    expect_that(is.null(epoch), is_false())
    expect_that(epoch$getStart()$equals(start.time), is_true())
    expect_that(epoch$getEnd()$equals(end.time), is_true())
    expect_that(epoch$getProtocol()$equals(protocol), is_true())
    for(name in names(protocol.parameters)) {
      expect_that(epoch$getProtocolParameters()$get(name), equals(protocol.parameters[[name]]))
    }
    for(name in names(device.parameters)) {
      expect_that(epoch$getDeviceParameters()$get(name), equals(device.parameters[[name]]))
    }
  })
  
})


test_that("inserts one Measurement per [source labe, source ID] group", {
  TestWrapper("inserts-measurements", function(ctx) {
    project <- ctx$insertProject("unused", "unused", Datetime(2013,7,1))
    exp <- project$insertExperiment("unused", Datetime(2013, 7, 1))
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
    
    measurements <- as.list(epoch$getMeasurements())
    
    df <- read.csv("../fixtures/example_fixture.csv")
    expect_that(length(measurements), equals(nrow(df)))
  })
})


test_that("inserts sources", {
  TestWrapper("inserts-measurements", function(ctx) {
    project <- ctx$insertProject("unused", "unused", Datetime(2013,7,1))
    exp <- project$insertExperiment("unused", Datetime(2013, 7, 1))
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
    
    
    measurements <- as.list(epoch$getMeasurements())
    for(m in measurements) {
      for(source.name in as.list(m$getSourceNames())) {
        expect_that(is.null(epoch$getInputSources()$get(source.name)), is_false())
      }
    }
  })
})

test_that("uses existing sources", {
  TestWrapper("inserts-measurements", function(ctx) {
    project <- ctx$insertProject("unused", "unused", Datetime(2013,7,1))
    exp <- project$insertExperiment("unused", Datetime(2013, 7, 1))
    protocol <- ctx$insertProtocol("unused", "unused")
    
    protocol.parameters = list(param.1=1, param.2="abc")
    device.parameters = list(param.1=10, param.2="xyz")
    
    start.time <- Datetime(2013,1,1)
    end.time <- Datetime(2013,1,2)
    
    src.1 <- ctx$insertSource("label.1", "id.1")
    
    epoch <- ImportCSV(ctx,
                       "../fixtures/example_fixture.csv",
                       start.time,
                       end.time,
                       protocol$getURI()$toString(),
                       protocol.parameters,
                       Vector2Set(c()),
                       device.parameters,
                       exp$getURI()$toString())
    
    
    expect_that(length(as.list(ctx$getSources("label.1", "id.1"))), equals(1))
  })
})


test_that("throws an error for ambiguous (duplicate) sources", {
  TestWrapper("inserts-measurements", function(ctx) {
    project <- ctx$insertProject("unused", "unused", Datetime(2013,7,1))
    exp <- project$insertExperiment("unused", Datetime(2013, 7, 1))
    protocol <- ctx$insertProtocol("unused", "unused")
    
    protocol.parameters = list(param.1=1, param.2="abc")
    device.parameters = list(param.1=10, param.2="xyz")
    
    start.time <- Datetime(2013,1,1)
    end.time <- Datetime(2013,1,2)
    
    ctx$insertSource("label.1", "id.1")
    ctx$insertSource("label.1", "id.1")
    
    expect_error({
      ImportCSV(ctx,
                "../fixtures/example_fixture.csv",
                start.time,
                end.time,
                protocol$getURI()$toString(),
                protocol.parameters,
                Vector2Set(c()),
                device.parameters,
                exp$getURI()$toString())
    })
  })
})

test_that("writes group data to measurement csv", {
  TestWrapper("inserts-measurements", function(ctx) {
    project <- ctx$insertProject("unused", "unused", Datetime(2013,7,1))
    exp <- project$insertExperiment("unused", Datetime(2013, 7, 1))
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
    
    
    df <- read.csv("../fixtures/example_fixture.csv")
    d_ply(df, .variables=c(df$Source.Label, df$Source.ID), .fun=function(r) {
      source.label <- unique(as.character(r$Source.Label))
      source.id <- unique(as.character(r$Source.ID))
      source.name <- sprintf("%s-%s", source.label, source.id)
      
      m <- epoch$getMeasurement(source.name)
      
      m.df <- read.csv(m$getLocalDataPath()$get())
      
      expect_that(colnames(r), is_identical_to(colnames(m.df)))
      expect_that(r$Col1, is_identical_to(m.df$Col1))
      expect_that(r$Col2, is_identical_to(m.df$Col2))
      expect_that(r$Col3, is_identical_to(m.df$Col3))
    })
  })
})
