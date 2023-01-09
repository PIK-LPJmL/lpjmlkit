## Contribute

If you want to contribute to further develop `lpjmlkit`, by modifying or adding new functions, please follow the guidelines here below.

**Required libraries**

```R
# For generating automated documentation
library(roxygen2)
# For building and installing the package
library(devtools)
# For testing the functions and catching errors
library(testthat)
# For complying with the coding style standards
library(lintr)
```

**Note on the structure of the lpjmlkit repository**

The `lpjmlkit` repository include the following branches:

- `master`: this is the clean version of the package. Note: **this branch is protected**, direct push is not allowed, changes to `master` are possible only via 'Merge Requests'.
- `your_temporary_branch`:  The branch you create whenever you want to make a new development. **Please, delete it when you are done.**
- `development`: this is a lose collection of un-cleaned functions. **This branch should NOT be merged with the master!** Here you can find / add useful code that can serve as an inspiration to develop new  functions, or that can be useful to make accessible to other colleagues.

**How to add a new function**

Note: To be done for every new development that should go to master.

1. Create `your_temporary_branch` branch from `master`
1. Manually copy the functions you want to clean from the `development` to `your_temporary_branch` branch, or write a new function from scratch
1. Clean the function and coding style (`lintr::lint("R/my_function.R")`)
1. Add documentation and examples. In Rstudio: `Code -> Insert roxygen skeleton` or by `Ctrl + Alt + Shift + R`
1. Add tests in the `tests/testthat` folder. Small sample data can be added in `tests/testdata`, if needed
1. Test if the new functions compile well with the rest of the package. Make use of the functionalities provided by [`devtools`](https://rawgit.com/rstudio/cheatsheets/main/package-development.pdf).
1. If you are ready to merge your changes to the `master`, see below how to make a merge request

**How to Create a Merge Request to the `master`**

1. Increase version number in `DESCRIPTION` file (follow `MajorVersionNumber.MinorVersionNumber.PatchNumber`)
1. Run roxygen2 and update the descriptions `roxygen2::roxygenise()` or `CTRL + Shift + D`
1. Make a merge request
1. Apply the 4-eyes principles
1. Delete `your_temporary_branch` branch
1. Ask for feedback and help if you have any questions!
