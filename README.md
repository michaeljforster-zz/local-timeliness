# local-timeliness

local-timeliness is a utility library for the Common Lisp local-time
library.

local-timeliness depends
on [local-time](https://common-lisp.net/project/local-time/).

local-timeliness is being developed
with [SBCL](http://sbcl.org/), [CCL](http://ccl.clozure.com/),
and [LispWorks](http://www.lispworks.com/) on OS X.  local-timeliness is
being deployed with SBCL on FreeBSD/AMD64 and Linux/AMD64.


### Installation

```lisp
(ql:quickload "local-timeliness")
```

### Example

```lisp

(defparameter *now* (local-time:now))

(defparameter *my-timestamp-format*
  (append local-time:+iso-8601-date-format+
          (list #\space) '((:hour12 2) #\: (:min 2) #\Space :ampm #\Space :timezone)))

(format t "~%~A~%~/local-timeliness:format-timestamp/~%" *now* *now*)

2017-06-02T16:07:55.368758-05:00
2017-06-02T16:07:55.368758-05:00
=> NIL

(let ((local-timeliness:*print-timestamp-format* *my-timestamp-format*))
  (format t "~%~A~%~/local-timeliness:format-timestamp/~%" *now* *now*))

2017-06-02T16:07:55.368758-05:00
2017-06-02 04:07 pm CDT
=> NIL

(let ((local-timeliness:*print-timestamp-format* *my-timestamp-format*)
      (local-time:*default-timezone* (local-time:find-timezone-by-location-name "America/Toronto")))
  (format t "~%~A~%~/local-timeliness:format-timestamp/~%" *now* *now*))

2017-06-02T17:07:55.368758-04:00
2017-06-02 05:07 pm EDT
=> NIL

```

### License

local-timeliness is distributed under the MIT license. See LICENSE.
