;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;---------------------------------------------------------------------------
;;; Copyright (c) 2022 Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; All rights reserved.
;;;
;;; This code is made available under the terms of the Mozilla Public License,
;;; distributed with the system as the file LICENSE.html
;;;
;;;---------------------------------------------------------------------------

(defpackage :temporal-shop2-asd
    (:use :common-lisp :asdf)
    )
(in-package :temporal-shop2-asd)

(defsystem :temporal-shop2
    :serial t
    :depends-on (:shop2)
    :components ((:file "temporal-shop2")
		 )
    )
