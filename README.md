# MAPLEemo <img src="man/figures/logo.png" align="right" />
An R package for analyzing and visualizing the Emotional Piano Project data. This package includes a number of specialized visualization tools as well as statistical analysis techniques developed for the Emotional Piano Project.

## Installation
This package is not available through CRAN. Instead, you can install the package through `remotes`, `devtools`, or `pak`:
```{r}
# remotes/devtools.
remotes::install_github( # or devtools::install_github()
    "MAPLE-Lab/MAPLEemo",
    build_vignettes = TRUE # Vignettes not built by default.
)

# remotes through git repo link.
remotes::install_git(
    "https://github.com/MAPLE-Lab/MAPLEemo.git",
    build_vignettes = TRUE  
)

# pak.
pak::pak("MAPLE-Lab/MAPLEemo")
```

## Documentation
All objects in this package are documented internally: as with any R package, you can access this by prefacing any function with `?`:
```{r}
?plot_circumplex
```
In addition, there are a number of vignettes that describe generic applications of this package:
```{r}
# Get a list of all vignettes in the package.
browseVignettes("MAPLEemo")

# Read a specific vignette.
vignette("MAPLEemo", package = "MAPLEemo")
```

## Contributing
While this package is maintained by the MAPLE Lab, we welcome users to report issues and suggest improvements. If you encounter a bug or have a feature request, please open a clear and detailed issue. Pull requests should follow the tidyverse style guide, and include relevant documentation and tests where applicable: a more detailed guide is forthcoming. We expect all contributors to engage respectfully and constructively in discussions.
