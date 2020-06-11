test_that("wrap_tabsets() works", {
  expect <- "# h1\n# h2 \n\n`<tabset-start/>`{=html}\ntest"

  expect_equal(wrap_tabsets("# h1\n# h2 {.tabset}\ntest"), expect)
  expect_equal(wrap_tabsets("# h1\n# h2 { .tabset }\ntest"), expect)
  expect_equal(wrap_tabsets("# h1\n# h2 {.some-class .tabset .other-class}\ntest"), expect)
})

html_doc <- function(x) {
  x <- commonmark::markdown_html(x)
  xml2::read_xml(
    paste0("<body>", x, "</body>"),
    options = c("RECOVER", "NOERROR", "NOBLANKS")
  )
}

test_that("determine_tabset_level() works", {
  # return NULL for non-tabset document
  expect_equal(determine_tabset_level(html_doc("# 1\n## 2")), NULL)

  # code chunk
  html_doc1 <- html_doc(
    "
## t1

<tabset-start/>

### t2

content2

### t3

content3

"
  )
  expect_equal(determine_tabset_level(html_doc1), 2)

  # invalid case
  html_doc2 <- html_doc(
    "
## t1

<tabset-start/>

### t2

content2

### t3

content3

### invalid

<tabset-start/>

"
  )
  expect_warning(
    expect_equal(determine_tabset_level(html_doc2), 2)
  )

  # complex case
  html_doc3 <- html_doc(
    "
## t1

# t2

### t3

<tabset-start/>

#### t4

content4
"
  )

  expect_equal(determine_tabset_level(html_doc3), 3)
})

test_that("mark_tabsets() works", {
  # Do nothing on the document without tabsets
  expect_null(mark_tabsets(html_doc("## t1\n##t2")))

  html_doc1 <- html_doc(
    "
## t1

<tabset-start/>

### t2

content2

### t3

content3

")

  mark_tabsets(html_doc1)

  expect_equal(
    as.character(html_doc1),
    '<?xml version="1.0" encoding="UTF-8"?>
<body>
  <h2>t1</h2>
  <tabset-start/>
  <tabset-tab-first>t2</tabset-tab-first>
  <p>content2</p>
  <tabset-tab>t3</tabset-tab>
  <p>content3</p>
  <tabset-end/>
</body>
')

  html_doc2 <- html_doc(
    "
## t1

<tabset-start/>

### t2

content2

## t3

<tabset-start/>

### t4

content4

### t5

## t1

")

  mark_tabsets(html_doc2)

  expect_equal(
    as.character(html_doc2),
    '<?xml version="1.0" encoding="UTF-8"?>
<body>
  <h2>t1</h2>
  <tabset-start/>
  <tabset-tab-first>t2</tabset-tab-first>
  <p>content2</p>
  <tabset-end/>
  <h2>t3</h2>
  <tabset-start/>
  <tabset-tab-first>t4</tabset-tab-first>
  <p>content4</p>
  <tabset-tab>t5</tabset-tab>
  <tabset-end/>
  <h2>t1</h2>
</body>
')
})

test_that("translate_to_confl_macro() can handle tabset", {
  # code chunk
  html_text1 <- commonmark::markdown_html(
    "
## t1

<tabset-start/>

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

## t1

<tabset-start/>

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
