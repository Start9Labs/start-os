local jsonpath = require("jsonpath")
jsonpath.init("/etc/jsonpath/libjsonpath_lib.so")
ngx.log(ngx.INFO, "loaded libjsonpath_lib.so")