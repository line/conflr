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

* Add an option `conflr_dont_cache_envvar` not to cache the URL, username and
  password as envvars (#43).

" conflr now accepts `confluence_settings` front-matter, which is useful for
  automation (#44, @kazutan).

# conflr 0.0.5

* Initial release on GitHub
