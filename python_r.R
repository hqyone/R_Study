# This file is a example to use python and bash in R
library(reticulate)

py_install("seaborn")
use_virtualenv("r-reticulate")

sns <- import("seaborn")
fmri <- sns$load_dataset('fmri')
mpl <- import("matplotlib")
print(dim(fmri))

sns$lmplot("time")

py_run_file("python_test.py")