;; Copyright 2019 Sony Computer Science Laboratories Paris
;;                Remi van Trijp (www.remivantrijp.eu)

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :common-lisp-user)

(defpackage :dev-tools
  (:use :cl-user
	:common-lisp
	:test-framework
	:fcg :nlp-tools
        #+:hunchentoot-available-on-this-platform :web-interface
        :utils
        :monitors
        :capi))

(export '(*dev-tools-path*))
(defparameter *dev-tools-path* (make-pathname :directory (pathname-directory (or *load-truename*
                                                                                 *compile-file-truename*))))
