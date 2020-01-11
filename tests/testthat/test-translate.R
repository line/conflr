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
    restore_cdata("<ac:plain-text-body><![CDATA[x &lt; 1\nx &lt; 1]]></ac:plain-text-body>\n<ac:plain-text-body><![CDATA[x &lt; 1\nx &lt; 1]]></ac:plain-text-body>"),
    "<ac:plain-text-body><![CDATA[x < 1\nx < 1]]></ac:plain-text-body>\n<ac:plain-text-body><![CDATA[x < 1\nx < 1]]></ac:plain-text-body>"
  )
})

test_that("get_corresponding_lang() works", {
  # supported
  expect_equal(get_corresponding_lang("sql", supported_syntax_highlighting_default), "sql")
  expect_equal(get_corresponding_lang("cpp", supported_syntax_highlighting_default), "cpp")
  expect_equal(get_corresponding_lang("python", supported_syntax_highlighting_default), "py")
  expect_equal(get_corresponding_lang("html", supported_syntax_highlighting_default), "html/xml")
  expect_equal(get_corresponding_lang("css", supported_syntax_highlighting_default), "css")
  expect_equal(get_corresponding_lang("bash", supported_syntax_highlighting_default), "bash")
  expect_equal(get_corresponding_lang("yaml", supported_syntax_highlighting_default), "yaml")
  # unsupported
  expect_equal(get_corresponding_lang("r", supported_syntax_highlighting_default), "none")
  expect_equal(get_corresponding_lang(NA, supported_syntax_highlighting_default), "none")
  # custom
  expect_equal(get_corresponding_lang("r", c(r = "r")), "r")
})

test_that("replace_code_chunk() works", {
  do_test_code_chunk <- function(code_tag_attr, expected_language, supported_syntax_highlighting = supported_syntax_highlighting_default) {
    given <- as.character(glue::glue("<pre><code {code_tag_attr}>print(1)</code></pre>"))

    expected <- as.character(glue::glue(
      '<ac:structured-macro ac:name="code">\n',
      '  <ac:parameter ac:name="language">{expected_language}</ac:parameter>\n',
      '  <ac:plain-text-body><![CDATA[print(1)]]></ac:plain-text-body>\n',
      '</ac:structured-macro>'
    ))

    expect_equal(replace_code_chunk(given, supported_syntax_highlighting), expected)
  }

  # by default Python and SQL are supported
  do_test_code_chunk('class="language-python"', "py")
  do_test_code_chunk('class="language-sql"', "sql")
  # by default R is not supported
  do_test_code_chunk('class="language-r"', "none")
  # no atrribute
  do_test_code_chunk('', "none")

  # with options, r is supported
  do_test_code_chunk('class="language-r"', "r", c(r = "r"))
  do_test_code_chunk('language="r"', "r", c(r = "r"))

  # w/ spaces
  expect_equal(
    replace_code_chunk('<pre>\n<code class="language-r">print(1)</code>  </pre>', c(r = "r")),
    '<ac:structured-macro ac:name="code">
  <ac:parameter ac:name="language">r</ac:parameter>
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
$$"
  )
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
    '<ac:image ac:width="600"><ri:attachment ri:filename="img.png" /></ac:image>'
  )

  expect_equal(
    replace_image('<img src="/path/to/img.png" />'),
    '<ac:image ac:width="600"><ri:attachment ri:filename="img.png" /></ac:image>'
  )

  # use width
  expect_equal(
    replace_image('<img src="/path/to/img.png" height="300" />'),
    '<ac:image ac:width="600" ac:height="300"><ri:attachment ri:filename="img.png" /></ac:image>'
  )

  # use width and height
  expect_equal(
    replace_image('<img src="/path/to/img.png" height="300" width="300" />'),
    '<ac:image ac:width="300" ac:height="300"><ri:attachment ri:filename="img.png" /></ac:image>'
  )

  # use the other default size
  expect_equal(
    replace_image('<img src="/path/to/img.png" />', image_size_default = 333),
    '<ac:image ac:width="333"><ri:attachment ri:filename="img.png" /></ac:image>'
  )

  # the default size is ignored when the image has its width
  expect_equal(
    replace_image('<img src="/path/to/img.png" width="450" />', image_size_default = 333),
    '<ac:image ac:width="450"><ri:attachment ri:filename="img.png" /></ac:image>'
  )

  # use the original size
  expect_equal(
    replace_image('<img src="/path/to/img.png" />', image_size_default = NULL),
    '<ac:image ><ri:attachment ri:filename="img.png" /></ac:image>'
  )

  # external images are not converted
  expect_equal(
    replace_image('<img src="https://example.com/img.png" />', image_size_default = NULL),
    '<img src="https://example.com/img.png" />'
  )

  # minor case
  expect_equal(
    replace_image('<img src="http.png" width="450" />', image_size_default = 333),
    '<ac:image ac:width="450"><ri:attachment ri:filename="http.png" /></ac:image>'
  )
})

test_that("translate_to_confl_macro() works", {
  # code chunk
  html_text <- commonmark::markdown_html(
    "code:
``` r
x < 1 & y > 1
```
"
  )
  expect_equal(
    translate_to_confl_macro(html_text, supported_syntax_highlighting_default),
    "<p>code:</p>
<ac:structured-macro ac:name=\"code\">
  <ac:parameter ac:name=\"language\">none</ac:parameter>
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
"
  )
  expect_equal(
    translate_to_confl_macro(html_text, supported_syntax_highlighting_default),
    "<p>code:</p>
<ac:structured-macro ac:name=\"code\">
  <ac:parameter ac:name=\"language\">none</ac:parameter>
  <ac:plain-text-body><![CDATA[iris$Species\n'$$'\n]]></ac:plain-text-body>
</ac:structured-macro>"
  )

  # image
  html_text <- commonmark::markdown_html("![foo](/path/to/foo.png)")
  expect_equal(
    translate_to_confl_macro(html_text),
    '<p>\n  <ac:image ac:width="600"><ri:attachment ri:filename="foo.png" /></ac:image>\n</p>'
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
$$"
  )
  expect_equal(
    translate_to_confl_macro(html_text),
    '<p>This $$ is not
<ac:structured-macro ac:name="mathblock">
  <ac:plain-text-body><![CDATA[\n\\frac{1}{3}\n]]></ac:plain-text-body>
</ac:structured-macro></p>'
  )

  html_text <- commonmark::markdown_html(
    "<p> $$\\frac{1}{3}$$ </p>"
  )
  expect_equal(
    translate_to_confl_macro(html_text),
    '<p><ac:structured-macro ac:name="mathblock">
  <ac:plain-text-body><![CDATA[\\frac{1}{3}]]></ac:plain-text-body>
</ac:structured-macro></p>'
  )
})
