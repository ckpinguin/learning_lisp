(defpackage :ch.codehome.practical-system (:use :asdf :cl))
(in-package :ch.codehome.practical-system)

(defsystem practical
  :name "practical"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Utilities for writing macros and other useful stuff"
  :long-description ""
  :components
  ((:file "packages")
   (:file "practical" :depends-on ("packages")))
  :depends-on ())
