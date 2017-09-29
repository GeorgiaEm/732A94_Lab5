# val2014()
context("val2014()")
test_that("Assignment: val2014()", {
  expect_true(exists("val2014"), info = "No object 'val2014' exists.")
  
  expect_is(val2014, class = "function",
            
            info = "Object 'val2014' is not a function.")
  
  
  #   a<-logical()
  # for (i in 1:length(res)){
  #   a[i]<-expect_that(str_sub(res[i], start= -4),equals(".skv"),info="url does not contain .skv files")
  # }
  # 
  #  expect_lte(val_data, length(res), info="Value too big")
  # expect_gt(val_data, 0, info="Value too small")
  #
  # expect_lte(lan_val, length(unique(zz[,3])), info="Value too big")
  # expect_gt(lan_val, 0, info="Value too small")
  #
  # expect_lte(kommun_val, length(unique(yy[,4])), info="Value too big")
  # expect_gt(kommun_val, 0, info="Value too small")
})