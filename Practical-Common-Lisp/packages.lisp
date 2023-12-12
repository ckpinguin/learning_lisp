(in-package :cl-user)

(defpackage :ch.codehome.practical
  (:use :common-lisp)
  (:export 
   :shuffle-vector
   :nschuffle-vector
   :start-of-file
   :with-gensyms
   :once-only
   :spliceable
   :ppme))
           
(defpackage :ch.codehome.pathnames
  (:use :common-lisp)
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))

(defpackage :ch.codehome.spam
  (:use :common-lisp :ch.codehome.pathnames))

(defpackage :ch.codehome.binary-data
  (:use :common-lisp :ch.codehome.practical)
  (:export :define-binary-class
           :define-tagged-binary-class
           :define-binary-type
           :read-value
           :write-value
           :*in-progress-objects*
           :parent-of-type
           :current-binary-object
           :+null+))

(defpackage :ch.codehome.id3v2
  (:use :common-lisp
        :ch.codehome.binary-data
        :ch.codehome.pathnames)
  (:export
   :read-id3
   :mp3-p
   :id3-p
   :count-versions
   :show-tag-header
   :show-tag-headers
   :frame-types
   :frame-types-in-dir
   :album
   :composer
   :genre
   :encoding-program
   :artist
   :part-of-set
   :track
   :song
   :year
   :size
   :translated-genre))
