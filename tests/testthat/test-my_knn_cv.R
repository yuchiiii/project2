test_that("my_knn_cv works", {
  penguins_df <- my_penguins
  penguins_df <- na.omit(penguins_df)
  penguins_df$species <- as.character(penguins_df$species)
  one_neighbor <- my_knn_cv(penguins_df, penguins_df$species, 1, 5)
  expect_type(one_neighbor$cv_err, "double")
})
