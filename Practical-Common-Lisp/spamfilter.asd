(defpackage :ch.codehome.spamfilter-system (:use :asdf :cl))
(in-package :ch.codehome.spamfilter-system)

(defsystem spamfilter
  :name "spamfilter"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Spam filter"
  :long-description ""
  :components
  ((:file "packages")
   (:file "spamfilter" :depends-on ("packages")))
  :depends-on (:cl-ppcre :pathnames))
