latex_1 <- c("l1", "l2 % anki myname", "l3", "l4 % anki myname")

latex_double_1 <- c("l1", "l2 %% anki myname", "l3", "l4 %% anki myname")
latex_double_2 <- c("l1", "l2 %% anki myname_1 %% anki myname_2")

hashtag_1 <- c("l1", "l2 # anki myname", "l3", "l4 # anki myname")
hashtag_2 <- c("l1", "l2 # anki myname_1 # anki myname_2")

latex_1_whitespace <- c("l1", "l2 %  anki myname", "l3", "l4 % anki myname")
latex_2_whitespace <- c("l1", "l2 % anki myname_1 % anki myname_2 ")

test_list_1 <- list(to_anki_list('myname', 2),
                    to_anki_list('myname', 4))

test_list_2 <- list(to_anki_list('myname_1', 2),
                    to_anki_list('myname_2', 2))

context('basic usage with only name - no fields, comments, tags or flags')

test_that("Correctly returns null", {
              expect_null(find_ankis("", "tex", "#"))
})

test_that("simple - latex", {
              expect_equal(find_ankis(latex_1), test_list_1)
              expect_equal(find_ankis(latex_2), test_list_2)
              expect_null(find_ankis(latex_2, "tex", "#"))
})

test_that("simple - hashtag", {
              expect_equal(find_ankis(hashtag_1, comment_string = "#"), test_list_1)
              expect_equal(find_ankis(hashtag_2, comment_string = "#"), test_list_2)
              expect_null(find_ankis(hashtag_2, "tex", "%"))
})


test_that("double %%", {
              expect_equal(find_ankis(latex_double_1, "tex", "%%"), test_list_1)
              expect_equal(find_ankis(latex_double_2, "tex", "%%"), test_list_2)
              expect_null(find_ankis(latex_2, "tex", "##"))
})


test_that("single % also finds double", {
              expect_equal(find_ankis(latex_double_1, "tex", "%"), test_list_1)
              expect_equal(find_ankis(latex_double_2, "tex", "%"), test_list_2)
})



context('basic usage with name and parameters - no fields, comments, tags')

latex_parm_1 <- c("l1", "l2 % anki myname parm", "l3", "l4 % anki myname parm")
latex_parm_2 <- c("l1", "l2 % anki myname parm_1", "l3", "l4 % anki myname parm_2")

latex_parm_mult_1 <- c("l1", "l2 % anki myname parm1 parm2", "l3", "l4 % anki myname parm")
latex_parm_mult_2 <- c("l1", "l2 % anki myname parm_1 parm_2", "l3", "l4 % anki myname parm_2")

test_list_1_param <- list(to_anki_list(c('myname','parm'), 2),
                    to_anki_list(c('myname','parm'), 4))

test_list_2_param <- list(to_anki_list(c('myname','parm_1'), 2),
                    to_anki_list(c('myname','parm_2'), 4))

test_list_1_param_mult <- list(to_anki_list(c('myname','parm1', 'parm2'), 2),
                    to_anki_list(c('myname','parm'), 4))

test_list_2_param_mult <- list(to_anki_list(c('myname','parm_1', 'parm_2'), 2),
                    to_anki_list(c('myname','parm_2'), 4))

test_that("parameters - single - latex", {
              expect_equal(find_ankis(latex_parm_1), test_list_1_param)
              expect_equal(find_ankis(latex_parm_2), test_list_2_param)
})


test_that("parameters - multiple - latex", {
              expect_equal(find_ankis(latex_parm_mult_1), test_list_1_param_mult)
              expect_equal(find_ankis(latex_parm_mult_2), test_list_2_param_mult)
})

test_that("removes addional whitespace", {
              expect_equal(find_ankis(latex_1_whitespace), test_list_1)
              expect_equal(find_ankis(latex_2_whitespace), test_list_2)
})

context("Find_ankis - throw warnings on limitations")

test_that("cannot use whitespace charater in comment_string argument", {
              expect_warning(find_ankis("","", "% "))
              expect_warning(find_ankis("","", "# "))
})

latex_1_found <- find_ankis(c("l1", "l2 % anki myname", "l3", "l4 % anki myname"))
latex_2_found <- find_ankis(c("l1", "l2 % anki myname_1 % anki myname_2",
             "l3 % anki myname_1 % anki myname_2"))

basic_1_extracted <- list(
                          myname = c("l2 % anki myname", "l3")
)

basic_2_extracted <- list(
                          myname_1 = c("l2 % anki myname_1 % anki myname_2"),
                          myname_2 = c("l2 % anki myname_1 % anki myname_2")
)


test_that("Basic extract tests", {
              extract_1  <- extract_ankis(latex_1, latex_1_found)
              extract_2  <- extract_ankis(latex_2, latex_2_found)
              expect_equal(extract_1, basic_1_extracted)
              expect_equal(extract_2, basic_2_extracted)
})


latex_fields_1 <- c("l1", "l2 % anki myname field1", "l3", "l4 % anki mynamefield1")
latex_fields_2 <- c("l1 % anki myname field1", "l2 % anki myname field1",
                    "l3 % anki myname field2", "l4 % anki myname field2")

context("edgecases")

test_that('escape quotation marks', {
              testthat::skip('not yet implemented')
})
# Example of using testing functions
# test_that("floor_date works for different units", {
#  as_time <- function(x) as.POSIXct(x, tz = "UTC")
#  expect_floor_equal <- function(unit, time) {
#    eval(bquote(expect_equal(floor_date(base, .(unit)), as_time(.(time)))))
#  }
#
#  base <- as_time("2009-08-03 12:01:59.23")
#  expect_floor_equal("second", "2009-08-03 12:01:59")
#  expect_floor_equal("minute", "2009-08-03 12:01:00")
#  expect_floor_equal("hour",   "2009-08-03 12:00:00")
#  expect_floor_equal("day",    "2009-08-03 00:00:00")
#  expect_floor_equal("week",   "2009-08-02 00:00:00")
#  expect_floor_equal("month",  "2009-08-01 00:00:00")
#  expect_floor_equal("year",   "2009-01-01 00:00:00")
#})

