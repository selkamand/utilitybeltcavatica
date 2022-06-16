# utilitybeltcavatica

A bunch of helper functions for working with the sbg cavatica platform

## Installation

```{r}
    install.packages("remotes") # Can skip if you already have 'remotes' package installed
    remotes::install_github("selkamand/utilitybeltcavatica")
```

## Usage

This package extends functionality of the `sevenbridges` package on bioconductor.

All functions described bellow are vectorised

### Find File By FilePath

```{r}
     # Get File Object
    find_file_using_filepath(sevenbridges_project, filepath)

    # Get File Identifier
    find_fileid_using_filepath(sevenbridges_project, filepath)
```
