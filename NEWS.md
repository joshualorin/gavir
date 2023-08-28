# gavir 0.1.0

## Major changes 

* gavir now has a full-fledged website
* Added a new helper function, `add_iso3()`. When no iso3 code is present but a country name is, this loads the synonyms file and matches on country.  
* Added new get functions, `get_synonyms()` and `get_support()`
* Added new color palettes for the `scale_gavi_*()` functions: "standard", "map", "map blues". Now default is "standard", a discrete palette that has pretty and distinctive color defaults.  
* Updated `get_ihme()` to follow updated folder structure, and added option to load in ihme/admin subnational file

## Bug fixes 

* Resolved issue with `theme_gavi_table()` where function threw error when officer package was not loaded
* Resolved issue with `get_ihme()` to no longer throw error when vaccine parameter was empty

# gavir 0.0.0.9000

## Major changes 

* Initial release with `get_*` functions, allowing easy retrieval of common datasets from the shared drive. 
* Two new theme functions, `theme_gavi()` and `theme_gavi_table()` 
* Functions for custom gavi color palettes (`scale_color_gavi()` and `scale_fill_gavi()`)
* Some miscellaneous helper functions, notably `set_root()`
