INCLUDE := true
INIT_LOAD := false

* = $280
.include "load8000.s"
.include "unlz4.s"
