latex_1 <- c("l1", "l2 % anki myname", "l3", "l4 % anki myname")
latex_2 <- c("l1", "l2 % anki myname_1 % anki myname_2")

latex_double_1 <- c("l1", "l2 %% anki myname", "l3", "l4 %% anki myname")
latex_double_2 <- c("l1", "l2 %% anki myname_1 %% anki myname_2")

hashtag_1 <- c("l1", "l2 # anki myname", "l3", "l4 # anki myname")
hashtag_2 <- c("l1", "l2 # anki myname_1 # anki myname_2")

latex_1_whitespace <- c("l1", "l2 %  anki myname", "l3", "l4 % anki myname")
latex_2_whitespace <- c("l1", "l2 % anki myname_1 % anki myname_2 ")


test_df_1 <- as.data.frame(do.call(rbind, 
                       list(to_anki_vec('myname', 2),
                            to_anki_vec('myname', 4))))

test_df_2 <- as.data.frame(do.call(rbind, 
                       list(to_anki_vec('myname_1', 2),
                            to_anki_vec('myname_2', 2))))

context('find ankis')

test_that("Correctly returns null", {
              expect_null(find_ankis("", "tex", "#"))
})

test_that("simple - latex", {
              expect_equal(find_ankis(latex_1), test_df_1)
              expect_equal(find_ankis(latex_2), test_df_2)
              expect_null(find_ankis(latex_2, "tex", "#"))
})

test_that("simple - hashtag", {
              expect_equal(find_ankis(hashtag_1, comment_string = "#"), test_df_1)
              expect_equal(find_ankis(hashtag_2, comment_string = "#"), test_df_2)
              expect_null(find_ankis(hashtag_2, "tex", "%"))
})


test_that("double %%", {
              expect_equal(find_ankis(latex_double_1, "tex", "%%"), test_df_1)
              expect_equal(find_ankis(latex_double_2, "tex", "%%"), test_df_2)
              expect_null(find_ankis(latex_2, "tex", "##"))
})


test_that("single % also finds double", {
              expect_equal(find_ankis(latex_double_1, "tex", "%"), test_df_1)
              expect_equal(find_ankis(latex_double_2, "tex", "%"), test_df_2)
})



context('find ankis with parameters')

latex_parm_1 <- c("l1", "l2 % anki myname parm", "l3", "l4 % anki myname parm")
latex_parm_2 <- c("l1", "l2 % anki myname parm_1", "l3", "l4 % anki myname parm_2")

latex_parm_mult_1 <- c("l1", "l2 % anki myname parm1 parm2", "l3", "l4 % anki myname parm")
latex_parm_mult_2 <- c("l1", "l2 % anki myname parm_1 parm_2", "l3", "l4 % anki myname parm_2")

test_df_1_param <- as.data.frame(do.call(rbind, list(to_anki_vec(c('myname','parm'), 2),
                    to_anki_vec(c('myname','parm'), 4))))

test_df_2_param <- as.data.frame(do.call(rbind, list(to_anki_vec(c('myname','parm_1'), 2),
                    to_anki_vec(c('myname','parm_2'), 4))))

test_df_1_param_mult <- as.data.frame(do.call(rbind, list(to_anki_vec(c('myname','parm1', 'parm2'), 2),
                    to_anki_vec(c('myname','parm'), 4))))

test_df_2_param_mult <- as.data.frame(do.call(rbind, list(to_anki_vec(c('myname','parm_1', 'parm_2'), 2),
                    to_anki_vec(c('myname','parm_2'), 4))))

test_that("parameters - single - latex", {
              expect_equal(find_ankis(latex_parm_1), test_df_1_param)
              expect_equal(find_ankis(latex_parm_2), test_df_2_param)
})


test_that("parameters - multiple - latex", {
              expect_equal(find_ankis(latex_parm_mult_1), test_df_1_param_mult)
              expect_equal(find_ankis(latex_parm_mult_2), test_df_2_param_mult)
})

test_that("removes addional whitespace", {
              expect_equal(find_ankis(latex_1_whitespace), test_df_1)
              expect_equal(find_ankis(latex_2_whitespace), test_df_2)
})

context("Find_ankis - throw warnings on limitations")

test_that("cannot use whitespace charater in comment_string argument", {
              expect_warning(find_ankis("","", "% "))
              expect_warning(find_ankis("","", "# "))
})


context('Fields')


missing_fields_single_name <- c("l1 % anki myname", "l2 % anki myname ",
                      "l3 % anki myname", "l4 % anki myname ",
                      "l5 % anki myname", "l6")

missing_fields_two_names <- c("l1 % anki myname1", "l2 % anki myname1",
                      "l3 % anki myname2", "l4 % anki myname2",
                      "l5 % anki myname2", "l6")

missing_fields_two_names_mixed <- c("l1 % anki myname1", "l2 % anki myname2",
                      "l3 % anki myname1", "l4 % anki myname2",
                      "l5 % anki myname2", "l6")


expect_fields_equal <- function(ankis, true_fields) {
    ankis <- find_ankis(ankis)
    test_ankis <- ankis
    test_ankis$field <- true_fields
    eval(bquote(expect_equal(add_fields(ankis), test_ankis)))
}

test_that("Add missing", {
  expect_fields_equal(missing_fields_single_name, 1:5)
  expect_fields_equal(missing_fields_two_names, c(1,2,1,2,3))
  expect_fields_equal(missing_fields_two_names_mixed, c(1,1,2,2,3))
})

latex_fields_1 <- c("l1", "l2 % anki myname f=field1", "l3", "l4 % anki myname f=field1")
latex_fields_2 <- c("l1 % anki myname f=field1", "l2 % anki myname f=field1",
                    "l3 % anki myname f=field2", "l4 % anki myname f=field2")

test_that("Add given fields", {
  expect_fields_equal(latex_fields_1, c('field1', 'field1'))
  expect_fields_equal(latex_fields_2, c('field1', 'field1', 'field2', 'field2'))
})


fields_mix_1 <- c("l1", "l2 % anki myname f=field1", "l3", "l4 % anki myname")
fields_mix_2 <- c("l1 % anki myname", "l2 % anki myname f=field1",
                    "l3 % anki myname ", "l4 % anki myname f=field2")

test_that("Add mix of given and unknown fields", {
  expect_fields_equal(fields_mix_1, c('field1', '1'))
  expect_fields_equal(fields_mix_2, c('1', 'field1', '2', 'field2'))
})

context('extract')

latex_1_found <- find_ankis(latex_1)
latex_2_found <- find_ankis(latex_2)

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


context("edgecases")

test_that('escape quotation marks', {
              testthat::skip('not yet implemented')
})

