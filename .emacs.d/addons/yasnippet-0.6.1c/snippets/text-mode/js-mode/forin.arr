# -*- mode: snippet -*-
#contributor : Sebastian Wallin
#name : for in - array
# --
for(var ${1:i} = 0; $1 < ${2:array}.length; $1++) {
  var item = $2[$1];
  $0
}
