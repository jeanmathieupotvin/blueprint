test_that("$print()",
{
    expect_snapshot_output(Blueprint$new()$print())
})
