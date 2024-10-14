**Guide for This Package**

**1. Description**

  Thank you for using this package. This package is designed for Quick Non-Uniform Space-Filling (QNUSF) designs, authored by Xiankui Yang, Lu Lu, and Christine M. Anderson-Cook. Since
  [shinyapps.io](https://xiankuiyangstatistics.shinyapps.io/QNUSF/) limits
the memory usage to 1 GB. It is hard to use the online app to generate designs with large candidate data. Therefore, we uploaded our app to GitHub for users to download and let people use the app in local R. 
The following section will show you how to use it in R.

**2. How to use our app in R**

  Please read the following codes to access the local Shiny App.

# Install ‘devtools’ package and access this package.
install.packages("devtools")
library(devtools)

# Install local Shiny app through GitHub
install_github("XiankuiYang/QNUSF")

#  Access app
QNUSF::app()

   
