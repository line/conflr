test_that("wrap_tabsets() works", {
  expect <- "# h1\n`<tabset-start>`{=html}\n\n# h2 \n\n`</tabset-start>`{=html}\ntest"

  expect_equal(wrap_tabsets("# h1\n# h2 {.tabset}\ntest"), expect)
  expect_equal(wrap_tabsets("# h1\n# h2 { .tabset }\ntest"), expect)
  expect_equal(wrap_tabsets("# h1\n# h2 {.some-class .tabset .other-class}\ntest"), expect)
})

test_that("translate_to_confl_macro() can handle tabset", {
  # code chunk
  html_text1 <- commonmark::markdown_html(
    "
<tabset-start>

## t1

</tabset-start>

### t2

content2

### t3

content3

"
  )
  expect_equal(
    translate_to_confl_macro(html_text1, supported_syntax_highlighting_default),
    '<h2>t1</h2>
<ac:structured-macro ac:name="deck">
<ac:parameter ac:name="id">t1</ac:parameter>
<ac:rich-text-body>
<ac:structured-macro ac:name="card">
<ac:parameter ac:name="label">t2</ac:parameter>
<ac:rich-text-body>
<p>content2</p>
</ac:rich-text-body>
</ac:structured-macro>

<ac:structured-macro ac:name="card">
<ac:parameter ac:name="label">t3</ac:parameter>
<ac:rich-text-body>
<p>content3</p>
</ac:rich-text-body>
</ac:structured-macro>

</ac:rich-text-body>
</ac:structured-macro>')


  # code chunk
  html_text2 <- commonmark::markdown_html(
    "
## t0

<tabset-start>

## t1

</tabset-start>

### t2

content2

### t3

content3

## t4

conent4

")

  expect_equal(
    translate_to_confl_macro(html_text2, supported_syntax_highlighting_default),
    '<h2>t0</h2>
<h2>t1</h2>
<ac:structured-macro ac:name="deck">
<ac:parameter ac:name="id">t1</ac:parameter>
<ac:rich-text-body>
<ac:structured-macro ac:name="card">
<ac:parameter ac:name="label">t2</ac:parameter>
<ac:rich-text-body>
<p>content2</p>
</ac:rich-text-body>
</ac:structured-macro>

<ac:structured-macro ac:name="card">
<ac:parameter ac:name="label">t3</ac:parameter>
<ac:rich-text-body>
<p>content3</p>
</ac:rich-text-body>
</ac:structured-macro>

</ac:rich-text-body>
</ac:structured-macro>
<h2>t4</h2>
<p>conent4</p>')
})
