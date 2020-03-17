# conflr (development version)

# conflr 0.1.0

* First CRAN release.

## Major changes

* conflr now works outside RStudio (e.g. Emacs/Vim) (#10).

* conflr now fits batch/console uses; you can either
    1. run `confl_create_post_from_Rmd()` with `interactive = FALSE` so that it
       doesn't show Shiny popups (#32, @ndiquattro).
    2. set `output: conflr::confluence_document` in the YAML front matter of the
       R Markdown file, and run `rmarkdown::render()` (#44, @kazutan / #80).
    
    ``` md
    ---
    title: "I love Confluence"
    output:
      conflr::confluence_document:
        space_key: "space1"
    ---
    ```

* conflr provides several new options:
    * "Use original image sizes" option controls whether to resize the image
      (default) or not (#21).
    * "TOC" option adds a table of contents and "TOC depth" option changes the
      max level of headers to include (#67).
    * "Fold code blocks" option controls whether to fold codes (default) or not
      (#81).

### Minor changes

* `confl_create_post_from_Rmd()` gets `params` argument for parameterized R
  Markdown (#37, @ellisvalentiner).

* External images are now converted properly (#39).

* Add an option `conflr_addin_clear_password_after_success` not to cache the
  password as envvars. (#41 and #48, @Curycu).

* A new function `confl_contentbody_convert()` converts the Confluence-related
  formats by using the Confluence REST API (#58).

* `confl_post_page()` and `confl_update_page()` no longer translate the
  Confluence macros automatically. Accordingly, they lose `image_size_default`
  and `supported_syntax_highlighting` arguments (#76).

* `confl_create_post_from_Rmd()` now handles documents containing Confluence
  macro tags (i.e. `<ac:...>` or `<ri:...>`) properly (#76).

# conflr 0.0.5

* Initial release on GitHub
