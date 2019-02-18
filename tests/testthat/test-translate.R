context("translate")

test_that("unescape_html() works", {
  expect_equal(unescape_html("evony &amp; ivory"), "evony & ivory")
  expect_equal(unescape_html("x &lt;&gt; &#39;foo&#39;"), "x <> 'foo'")
  expect_equal(unescape_html("&quot;foo&quot;"), '"foo"')
})

test_that("restore_cdata() works", {
  # if there's no match, return the input as is
  expect_equal(restore_cdata('"x &lt; 1"'), '"x &lt; 1"')

  expect_equal(
    restore_cdata('"x &lt; 1" is <ac:plain-text-body><![CDATA[x &lt; 1]]></ac:plain-text-body>'),
    '"x &lt; 1" is <ac:plain-text-body><![CDATA[x < 1]]></ac:plain-text-body>'
  )
  # multiline
  expect_equal(
    restore_cdata('"x &lt; 1\nx &lt; 1" is <ac:plain-text-body><![CDATA[x &lt; 1\nx &lt; 1]]></ac:plain-text-body>'),
    '"x &lt; 1\nx &lt; 1" is <ac:plain-text-body><![CDATA[x < 1\nx < 1]]></ac:plain-text-body>'
  )
  # multiple matches
  expect_equal(
    restore_cdata('<ac:plain-text-body><![CDATA[x &lt; 1\nx &lt; 1]]></ac:plain-text-body>\n<ac:plain-text-body><![CDATA[x &lt; 1\nx &lt; 1]]></ac:plain-text-body>'),
    '<ac:plain-text-body><![CDATA[x < 1\nx < 1]]></ac:plain-text-body>\n<ac:plain-text-body><![CDATA[x < 1\nx < 1]]></ac:plain-text-body>'
  )
})

test_that("replace_code_chunk() works", {
  expect_equal(
    replace_code_chunk("<pre><code>print(1)</code></pre>"),
'<ac:structured-macro ac:name="code">
  <ac:plain-text-body><![CDATA[print(1)]]></ac:plain-text-body>
</ac:structured-macro>'
)
  # w/ spaces
  expect_equal(
    replace_code_chunk("<pre>\n<code>print(1)</code>  </pre>"),
'<ac:structured-macro ac:name="code">
  <ac:plain-text-body><![CDATA[print(1)]]></ac:plain-text-body>
</ac:structured-macro>'
)
  # w/ attributes
  expect_equal(
    replace_code_chunk("<pre><code language='r'>print(1)</code></pre>"),
'<ac:structured-macro ac:name="code">
  <ac:plain-text-body><![CDATA[print(1)]]></ac:plain-text-body>
</ac:structured-macro>'
  )
})

test_that("replace_inline_math() works", {
  x <- mark_inline_math("$\\frac{1}{3}$")
  expect_equal(x, "%1%D%O%L%L%A%R%\\frac{1}{3}%1%D%O%L%L%A%R%")
  expect_equal(
    replace_inline_math(x),
"<ac:structured-macro ac:name=\"mathinline\">
  <ac:parameter ac:name=\"body\">\\frac{1}{3}</ac:parameter>
</ac:structured-macro>"
  )
  x <- mark_inline_math("$\\frac{1}{3}$ is $0.3333\\cdots$")
  expect_equal(x, "%1%D%O%L%L%A%R%\\frac{1}{3}%1%D%O%L%L%A%R% is %1%D%O%L%L%A%R%0.3333\\cdots%1%D%O%L%L%A%R%")
  expect_equal(
    replace_inline_math(x),
"<ac:structured-macro ac:name=\"mathinline\">
  <ac:parameter ac:name=\"body\">\\frac{1}{3}</ac:parameter>
</ac:structured-macro> is <ac:structured-macro ac:name=\"mathinline\">
  <ac:parameter ac:name=\"body\">0.3333\\cdots</ac:parameter>
</ac:structured-macro>"
  )

  # unpaired
  x <- mark_inline_math("$100")
  expect_equal(x, "%1%D%O%L%L%A%R%100")
  expect_equal(replace_inline_math(x), "$100")
})

test_that("Unrelated $s are left as is", {
  expect_equal(mark_inline_math("$\\frac{\\$}{3}$"), "%1%D%O%L%L%A%R%\\frac{\\$}{3}%1%D%O%L%L%A%R%")
  expect_equal(mark_inline_math("$ is $\\frac{1}{3}$"), "$ is %1%D%O%L%L%A%R%\\frac{1}{3}%1%D%O%L%L%A%R%")
  expect_equal(mark_inline_math("\\$ is $\\frac{1}{3}$"), "\\$ is %1%D%O%L%L%A%R%\\frac{1}{3}%1%D%O%L%L%A%R%")
  expect_equal(mark_inline_math("\\$\\frac{1}{3}$"), "\\$\\frac{1}{3}%1%D%O%L%L%A%R%")
  expect_equal(mark_inline_math("$\\frac{1}{3}$ is $"), "%1%D%O%L%L%A%R%\\frac{1}{3}%1%D%O%L%L%A%R% is $")
  expect_equal(mark_inline_math("$\\frac{1}{3}$ is \\$"), "%1%D%O%L%L%A%R%\\frac{1}{3}%1%D%O%L%L%A%R% is \\$")
})

test_that("replace_math() works", {
  x <- mark_math(
"$$
\\frac{1}{3}
$$")
  expect_equal(x, "%2%D%O%L%L%A%R%\n\\frac{1}{3}\n%2%D%O%L%L%A%R%")
  expect_equal(
    replace_math(x),
'<ac:structured-macro ac:name="mathblock">
  <ac:plain-text-body><![CDATA[\n\\frac{1}{3}\n]]></ac:plain-text-body>
</ac:structured-macro>'
  )
})

test_that("replace_image() works", {
  expect_equal(
    replace_image('<img src="/path/to/img.png" alt="foo"/>'),
    '<ac:image ac:height="400"><ri:attachment ri:filename="img.png" /></ac:image>'
  )

  expect_equal(
    replace_image('<img src="/path/to/img.png" />'),
    '<ac:image ac:height="400"><ri:attachment ri:filename="img.png" /></ac:image>'
  )

  # use width
  expect_equal(
    replace_image('<img src="/path/to/img.png" height="300" />'),
    '<ac:image ac:height="300"><ri:attachment ri:filename="img.png" /></ac:image>'
  )

  # use width and height
  expect_equal(
    replace_image('<img src="/path/to/img.png" height="300" width="300" />'),
    '<ac:image ac:height="300" ac:width="300"><ri:attachment ri:filename="img.png" /></ac:image>'
  )
})

test_that("translate_to_confl_macro() works", {
  # code chunk
  html_text <- commonmark::markdown_html(
"code:
``` r
x < 1 & y > 1
```
")
  expect_equal(
    translate_to_confl_macro(html_text),
"<p>code:</p>
<ac:structured-macro ac:name=\"code\">
  <ac:plain-text-body><![CDATA[x < 1 & y > 1\n]]></ac:plain-text-body>
</ac:structured-macro>"
  )

  # code chunk with $ and $$
  html_text <- commonmark::markdown_html(
"code:
``` r
iris$Species
'$$'
```
")
  expect_equal(
    translate_to_confl_macro(html_text),
"<p>code:</p>
<ac:structured-macro ac:name=\"code\">
  <ac:plain-text-body><![CDATA[iris$Species\n'$$'\n]]></ac:plain-text-body>
</ac:structured-macro>"
  )

  # image
  html_text <- commonmark::markdown_html("![foo](/path/to/foo.png)")
  expect_equal(
    translate_to_confl_macro(html_text),
    '<p>\n  <ac:image ac:height="400"><ri:attachment ri:filename="foo.png" /></ac:image>\n</p>'
  )
  # inline math
  html_text <- commonmark::markdown_html("$ is $\\frac{1}{3}$")
  expect_equal(
    translate_to_confl_macro(html_text),
'<p>$ is <ac:structured-macro ac:name="mathinline">
  <ac:parameter ac:name="body">\\frac{1}{3}</ac:parameter>
</ac:structured-macro></p>'
  )

  # math
  html_text <- commonmark::markdown_html(
"This $$ is not
$$
\\frac{1}{3}
$$")
  expect_equal(
    translate_to_confl_macro(html_text),
'<p>This $$ is not
<ac:structured-macro ac:name="mathblock">
  <ac:plain-text-body><![CDATA[\n\\frac{1}{3}\n]]></ac:plain-text-body>
</ac:structured-macro></p>'
  )

  html_text <- commonmark::markdown_html(
    "<p> $$\\frac{1}{3}$$ </p>")
  expect_equal(
    translate_to_confl_macro(html_text),
    '<p><ac:structured-macro ac:name="mathblock">
  <ac:plain-text-body><![CDATA[\\frac{1}{3}]]></ac:plain-text-body>
</ac:structured-macro></p>'
  )
})
