# Build against ICU static libraries.
if(!file.exists("../windows/xapian-core-1.2.21/include/xapian.h")){
  setInternet2()
  download.file("https://github.com/rwinlib/xapian-core/archive/v1.2.21.zip", "lib.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}
