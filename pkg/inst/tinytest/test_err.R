expect_equal(
  read_error_msg("Error : error; err_name: name\n"),
  list(err_msg = "error", err_name = "name")
)

expect_error(
  fgc:::error_if(TRUE, "error", "name"), "error; err_name: name"
)
