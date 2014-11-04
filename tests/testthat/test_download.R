library(usfrbcp)

context("Downloading from web service")

cp <- NULL

test_that("download completes",{
  skip_on_cran()
  cp <<- getCommercialPaper()
  expect_equal(length(names(cp)),8)
  expect_equal(sum(names(cp)=="DataSet"),6)
  expect_match(cp[[1]]$Sender$Name,"Federal Reserve Board")
  expect_match(cp[[1]]$Name,"Commercial Paper")
})

test_that("summary prints",{
  skip_on_cran()
  summary(cp)  
})

test_that("rates extract and merge",{
  skip_on_cran()
  
  rates <- getCommercialPaperRates(cp,"none")
  expect_equal(length(names(rates)),25)
  
  rt <- mergeSeries(rates)
  expect_equal(ncol(rt),24)
  expect_match(colnames(rt)[1],"RIFSPPNAAD01_N.B")
})

test_that("rates extract elements",{
  skip_on_cran()
  
  rates <- getCommercialPaperRates(cp)
  expect_match(rates[[2]]$currency,"NA")
  expect_match(rates[[2]]$freq,"9")
  expect_match(rates[[2]]$unit,"Percent")
  expect_match(rates[[2]]$unit_mult,"1")
  expect_match(rates[[2]]$short_desc,"Overnight AA nonfinancial commercial paper rate")
  
  expect_match(rates[[9]]$currency,"NA")
  expect_match(rates[[9]]$freq,"9")
  expect_match(rates[[9]]$unit,"Percent")
  expect_match(rates[[9]]$unit_mult,"1")
  expect_match(rates[[9]]$short_desc,"7-day A2/P2 nonfinancial commercial paper rate")
    
})

test_that("volumes extract elements",{
  skip_on_cran()
  
  volumes <- getCommercialPaperVolumes(cp)
  expect_equal(length(volumes),61)
  
  expect_match(volumes[[2]]$currency,"USD")
  expect_match(volumes[[2]]$freq,"9")
  expect_match(volumes[[2]]$unit,"Currency")
  expect_match(volumes[[2]]$unit_mult,"1000000")
  expect_match(volumes[[2]]$short_desc,"Amount of AA nonfinancial commercial paper maturing in 1-4 days")
  
  expect_match(volumes[[9]]$currency,"NA")
  expect_match(volumes[[9]]$freq,"9")
  expect_match(volumes[[9]]$unit,"Number")
  expect_match(volumes[[9]]$unit_mult,"1")
  expect_match(volumes[[9]]$short_desc,"Number of issues of AA financial commercial paper maturing in 1-4 days")
})

test_that("Outstanding extract elements",{
  skip_on_cran()
  
  outstanding <- getCommercialPaperOutstanding(cp)
  expect_equal(length(outstanding),78)
  
  expect_match(outstanding[[2]]$currency,"USD")
  expect_match(outstanding[[2]]$freq,"19")
  expect_match(outstanding[[2]]$unit,"Currency")
  expect_match(outstanding[[2]]$unit_mult,"1000000")
  expect_match(outstanding[[2]]$short_desc,"Total commercial paper outstanding; seasonally adjusted")
  
  expect_match(outstanding[[9]]$currency,"USD")
  expect_match(outstanding[[9]]$freq,"19")
  expect_match(outstanding[[9]]$unit,"Currency")
  expect_match(outstanding[[9]]$unit_mult,"1000000")
  expect_match(outstanding[[9]]$short_desc,"Financial foreign commercial paper outstanding; seasonally adjusted")
})

test_that("Year end extract elements",{
  skip_on_cran()
  
  yearend <- getCommercialPaperYearend(cp)
  expect_equal(length(yearend),5)
  
  expect_match(yearend[[2]]$currency,"USD")
  expect_match(yearend[[2]]$freq,"19")
  expect_match(yearend[[2]]$unit,"Currency")
  expect_match(yearend[[2]]$unit_mult,"1000000")
  expect_match(yearend[[2]]$short_desc,"Total commercial paper outstanding, maturing after December 31")
  
  expect_match(yearend[[5]]$currency,"USD")
  expect_match(yearend[[5]]$freq,"19")
  expect_match(yearend[[5]]$unit,"Currency")
  expect_match(yearend[[5]]$unit_mult,"1000000")
  expect_match(yearend[[5]]$short_desc,"Asset-backed commercial paper outstanding, maturing after December 31")
})
