b <- blueprint::Blueprint$new()


# Test errors related to fields' types.
b_wrong_scalar <- blueprint::Blueprint$new()
b_wrong_scalar$is_blueprint      <- "a character"
b_wrong_scalar$blueprint_version <- 1L


# Test errors related to fields' lengths.
b_wrong_vec <- blueprint::Blueprint$new()
b_wrong_vec$is_blueprint      <- c(TRUE, TRUE)
b_wrong_vec$blueprint_version <- c("0.1.1", "0.1.2")
