test_that("wrap_tabsets() works", {
  expect <- "# h1\n`<tabset>`{=html}\n\n# h2 \n\n`</tabset>`{=html}\ntest"

  expect_equal(wrap_tabsets("# h1\n# h2 {.tabset}\ntest"), expect)
  expect_equal(wrap_tabsets("# h1\n# h2 { .tabset }\ntest"), expect)
  expect_equal(wrap_tabsets("# h1\n# h2 {.some-class .tabset .other-class}\ntest"), expect)
})
