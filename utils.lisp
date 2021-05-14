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

(in-package :dev-tools)

(export '(dev-basic-visualizations dev-load-source))

(defun dev-basic-visualizations (inventory)
  "Do not use dot for visualizing networks or dependencies."
  (set-configuration (visualization-configuration inventory) :show-constructional-dependencies nil)
  (set-configuration inventory :draw-meaning-as-network nil)
  inventory)

(defun dev-load-source (package-name)
  "Creates a call to asdf:operate."
  (handler-case (asdf/operate:operate 'asdf/lisp-action:load-source-op
                                      (read-from-string (format nil "\:~a" package-name)))
    (undefined-function (error)
      (let ((r (find-restart 'continue error)))
        (when r (invoke-restart r))))))
