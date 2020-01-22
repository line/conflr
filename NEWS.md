# conflr 0.0.5.9000

* Added a `NEWS.md` file to track changes to the package.

* conflr now works outside RStudio (e.g. Emacs/Vim) thanks to the power of
  askpass package (#10).

* conflr addin now has "Use original image sizes" option to control whether to resize
  the image (default) or not (#21).

* `confl_create_post_from_Rmd()` gets `interactive` argument. When it's `FALSE`
  it doesn't show Shiny popups, which is suitable for console use (#32, @ndiquattro).

* `confl_create_post_from_Rmd()` gets `params` argument for parameterized R
  Markdown (#37, @ellisvalentiner).

* External images are now converted properly (#39).

* Add an option `conflr_addin_clear_password_after_success` not to cache the
  password as envvars. (#41 and #48, @Curycu).

" conflr now accepts `confluence_settings` front-matter, which is useful for
  automation (#44, @kazutan).

* A new function `confl_contentbody_convert()` converts the Confluence-related
  formats by using the Confluence REST API (#58).

* `confl_create_post_from_Rmd()` gets `toc` and `toc_depth` argument. When
  `toc = TRUE`, TOC is added at the top of the document (#64). The depth of the
  TOC can be specified via `toc_depth` argument (#67).

# conflr 0.0.5

* Initial release on GitHub
