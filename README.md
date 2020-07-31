
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggkeyboard

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

ggkeyboard lets you plot a keyboard and change the colours on it. This
package is very experimental, so expect breaking changes as I work
through making all the customization options as friendly as possible.

You can install ggkeyboard from github:

``` r
# install.packages("devtools")
devtools::install_github("sharlagelfand/ggkeyboard", ref = "main")
```

# Usage

Plot a keyboard using `ggkeyboard()`. The default is very cute:

``` r
library(ggkeyboard)

ggkeyboard()
```

<img src="man/figures/README-pastel-1.png" width="100%" />

## Colour palettes

You can change the colours, font, sizes, etc. There are a few built in
colour palettes, available via `keyboard_palette()`:

``` r
library(scales)

show_col(keyboard_palette("serika"))
```

<img src="man/figures/README-palette-1.png" width="100%" />

This keyboard is inspired by the [Drop + Zambumon MT3 Serika Custom
Keycap
Set](https://drop.com/buy/drop-zambumon-mt3-serika-custom-keycap-set):

``` r
ggkeyboard(palette = keyboard_palette("serika"))
```

<img src="man/figures/README-serika-1.png" width="100%" />

This one is inspired by the [Melgeek MG Wahtsy ABS Doubleshot Keycap
Set](https://drop.com/buy/melgeek-mg-wahtsy-abs-doubleshot-keycap-set):

``` r
ggkeyboard(palette = keyboard_palette("wahtsy"))
```

<img src="man/figures/README-wahtsy-1.png" width="100%" />

and this one by the [Domikey ABS Doubleshot SA Cyberpunk Pumper Keycap
Set](https://drop.com/buy/domikey-abs-doubleshot-sa-cyberpunk-pumper-keycap-set):

``` r
ggkeyboard(palette = keyboard_palette("cyberpunk"))
```

<img src="man/figures/README-cyberpunk-1.png" width="100%" />

## Layouts

`ggkeyboard()` defaults to using a tenkeyless keyboard, available in
`tkl`:

``` r
head(tkl)
#> # A tibble: 6 x 8
#>   key       key_label key_type   row number width height layout
#>   <chr>     <chr>     <chr>    <dbl>  <dbl> <dbl>  <dbl> <chr> 
#> 1 Ctrl Left Ctrl      modifier     1      1  1.25      1 60%   
#> 2 Win Left  <NA>      modifier     1      2  1.25      1 60%   
#> 3 Alt Left  Alt       modifier     1      3  1.25      1 60%   
#> 4 Spacebar  <NA>      accent       1      4  6.25      1 60%   
#> 5 Alt Right Alt       modifier     1      5  1.25      1 60%   
#> 6 Win Right <NA>      modifier     1      6  1.25      1 60%
```

There is also a full keyboard layout available via `full`. This palette
is based off the [Varmilo VA108 Fullsize
Keyboard](https://drop.com/buy/varmilo-108-keyboard)

``` r
ggkeyboard(full, palette = keyboard_palette("varmilo"), font_size = 2.75)
```

<img src="man/figures/README-full-1.png" width="100%" />

a 60% layout available via `sixty_percent` (colour palette based off the
[Drop + T0mb3ry SA Yuri Custom Keycap
Set](https://drop.com/buy/drop-t0mb3ry-sa-yuri-custom-keycap-set)):

``` r
ggkeyboard(sixty_percent, palette = keyboard_palette("t0mb3ry"), adjust_text_colour = FALSE)
```

<img src="man/figures/README-sixty-1.png" width="100%" />

and a mac layout, based off the Apple magic keyboard:

``` r
ggkeyboard(mac, palette = keyboard_palette("magic"))
```

<img src="man/figures/README-mac-1.png" width="100%" />

You can use an ISO layout by changing `layout` to “iso”, for any of the
given keyboards:

``` r
ggkeyboard(tkl, layout = "iso")
```

<img src="man/figures/README-iso-1.png" width="100%" />

## Highlighting

You can use `highlight_keys()` to emphasize any keys - for example, to
highlight what keys make up the shortcut for the pipe (%\>%):

``` r
ggkeyboard(tkl) %>%
  highlight_keys(c("Alt Left", "Shift Left", "M"))
```

<img src="man/figures/README-highlight-1.png" width="100%" />
