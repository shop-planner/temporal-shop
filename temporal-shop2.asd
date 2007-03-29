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
