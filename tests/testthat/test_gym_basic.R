context("gym_basic")

test_that("test MountainCar works", {
  dqn_mountain_car()
  expect_true(TRUE)
})

test_that("test Cart-Pole with DQN works", {
  dqn_cart_pole()
  expect_true(TRUE)
})

test_that("test Cart-Pole with Simple Policy Gradient works", {
  pg_cart_pole()
  expect_true(TRUE)
})

test_that("test Cart-Pole with actor critic works", {
  a3c_cart_pole()
  expect_true(TRUE)
})
 
