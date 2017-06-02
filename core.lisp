;;;; core.lisp

;;; The MIT License (MIT)
;;;
;;; Copyright (c) 2017 Michael J. Forster
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(defpackage "LOCAL-TIMELINESS/CORE"
  (:nicknames "LOCAL-TIMELINESS")
  (:use "CL")
  (:import-from "LOCAL-TIME")
  (:export "*PRINT-TIMESTAMP-FORMAT*"
           "FORMAT-TIMESTAMP"))

(in-package "LOCAL-TIMELINESS/CORE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (local-time:reread-timezone-repository))

(defparameter *print-timestamp-format*
  local-time:+iso-8601-format+
  "The printer control variable that determines the format of
LOCAL-TIME timestamps printed using FORMAT-TIMESTAMP.

The initial value of *PRINT-TIMESTAMP-FORMAT* is the value of
LOCAL-TIME:ISO-8601-FORMAT+.")

(defun format-timestamp (stream timestamp &optional colon-p at-sign-p)
  "Write the formatted representation of TIMESTAMP to STREAM and return NIL.

Ignore COLON-P and AT-SIGN-P. Bind *PRINT-PRETTY* to NIL. Obey
*PRINT-READABLY*, LOCAL-TIME:*DEFAULT-TIMEZONE*, and
*PRINT-TIMESTAMP-FORMAT*.

Signal a correctable error of type TYPE-ERROR if the value of
TIMESTAMP is not a LOCAL-TIME:TIMESTAMP.

FORMAT-TIMESTAMP is intended for use with SET-PPRINT-DISPATCH or the
~// format directive."
  (declare (ignore colon-p at-sign-p))
  (check-type timestamp local-time:timestamp)
  (let ((*print-pretty* nil)) ; avoid endless recursion!
    (if *print-readably*
        (write timestamp :stream stream)
        (local-time:format-timestring stream timestamp :format *print-timestamp-format*)))
  nil)
