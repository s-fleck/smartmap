
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smartmap

**smartmap** is an R package for previewing spatial data as
[leaflet](https://rstudio.github.io/leaflet/) maps. It is designed for
interactive use and produces nice maps with minimal typing required. In
contrast to other similar packages, smartmap does not require an `sf` or
`sp` object as input, but *just works* with normal `data.frames` or
`matrices` that have longitude and latitude columns. See [the package
homepage](https://s-fleck.github.io/smartmap/) for more infos.

## Installation

You can install smartmap from CRAN:

``` r
install.packages("smartmap")
```

or get the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("s-fleck/smartmap")
```

## Development status

smartmap is in maintenance mode since I don’t much work with spatial
data anymore, so new features are unlikely to be added.

## Example

``` r
library(smartmap)

# coordinate pairs without names are interpreted as longitude/latitude
smap(c(16.422524, 48.185686))

# if you supply column names, smap uses those to identify the correct columns
smap(c(LAT = 48.185686, LoNgItuDE = 16.422524))

# when supplied with an url or file system path, smap tries its best to discover
# supported spatial datasets
# Note: the naturalearthdata url doesn't work anymore and i didn't find another good new example
# smap("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip")

# You can call smap() on existing leaflet objects to add background tiles and
# a ruler for measuring distance
library(leaflet)
lf <- leaflet::leaflet() %>% 
  leaflet::addCircleMarkers(lng = 16.422524, lat = 48.185686)

lf
smap(lf)
```
