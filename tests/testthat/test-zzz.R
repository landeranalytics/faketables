test_that('cant use onload outside package', {
  .onLoad() |>
    expect_error()
})
