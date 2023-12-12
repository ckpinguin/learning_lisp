(defpackage :ch.codehome.pathnames-system (:use :asdf :cl))
(in-package :ch.codehome.pathnames-system)

(defsystem pathnames
  :name "pathnames"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Portable pathname manipulation functions."
  :long-description ""
  :components
  ((:file "packages")
   (:file "pathnames" :depends-on ("packages"))))
