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


;;;;; |===========================================================================================|
;;;;; |                                                                                           |
;;;;; | The construction bank builder is a tool for building a corpus of analyzed sentences       |
;;;;; | in a semi-automatic fashion. It requires:                                                 |
;;;;; | - LispWorks on MacOSX                                                                     |
;;;;; | - A hashed-fcg-construction-set in which words/expressions are hashed using their string. |
;;;;; | - Recommended: a hybrid FCG grammar for construction extraction.                          |
;;;;; |                (see /sharing/fcg-hybrids/)                                                |
;;;;; ---------------------------------------------------------------------------------------------

;;;;; With the construction bank builder, you take the role of construction supplier and you
;;;;; manage the search space. Once you reach a good and final solution, you can store the final
;;;;; cip-node in a savefile. These savefiles are meant to be used to train future construction-
;;;;; suppliers and priming-networks.
;;;;;
;;;;; NOTE: If the web interface pane is not working, you probably already have the web interface
;;;;;       opened in another window (a web browser or another dev-tool). Close all those windows
;;;;;       and restart the construction bank builder.

; (ql:quickload :dev-tools)

(in-package :dev-tools)

(export '(dev-construction-bank-builder
          dev-perform-lexical-lookup))

(defun expand-selected-node-with-selected-cxn (cxn-name interface)
  (let* ((cxn-inventory (get-data interface :interface-inventory))
         (the-cxn (fcg-find-cxn cxn-name cxn-inventory))
         (the-node (first (get-data interface :cip-nodes)))
         (nodes-to-queue nil)
         (failed-nodes nil))
    ;; Create the possible expansions.
    (multiple-value-bind (succeeded-cars failed-cars)
        (fcg-apply (fcg::safe-cxn (get-processing-cxn the-cxn) (applied-constructions the-node))
                   (car-resulting-cfs (cipn-car the-node))
                   (direction (cip the-node))
                   :notify nil
                   :configuration (configuration cxn-inventory)
                   :cxn-inventory cxn-inventory)
      (declare (ignore failed-cars))
      (loop for car in succeeded-cars
            do (push (fcg::cip-add-child the-node car) nodes-to-queue))
      (loop for child in nodes-to-queue
            ;; Node tests.
            when (loop for mode in (get-configuration (cip the-node) :node-tests)
                       always (cip-node-test child mode))
            do (cip-enqueue child (cip the-node) (get-configuration (cip the-node) :queue-mode)))
      (setf (graph-pane-roots (search-tree interface)) (last (get-data interface :cip-nodes)))
      (display-node-in-web-pane the-node interface))))

(defun display-node-in-web-pane (cip-node self) ;; Self is the capi interface.
  "Display the current node in the web interface pane."
  (with-slots (cxn-view-pane) self
    (objc:invoke (objc:invoke (capi:cocoa-view-pane-view cxn-view-pane)
                              "mainFrame")
                 "loadRequest:"
                 (objc:invoke "NSURLRequest"
                              "requestWithURL:"
                              (objc:invoke "NSURL"
                                           "URLWithString:"
                                           "http://localhost:8000/")))
    (clear-page)
    (add-element (make-html cip-node))))

(defun dev-perform-lexical-lookup (data interface &optional (direction '<-))
  "Apply only hashed construction labels."
  (declare (ignore data))
  (set-data interface :cip-nodes nil)
  (let* ((utterance (text-input-pane-text (sentence-input-pane interface)))
         (cxn-inventory (get-data interface :interface-inventory))
         (config-copy (copy-object (configuration cxn-inventory))))
    ;; Preparing the cxn-inventory.
    (dolist (inventory (list cxn-inventory (processing-cxn-inventory cxn-inventory)))
      (set-configuration inventory :parse-order (get-configuration inventory :hashed-labels))
      (set-configuration inventory :goal-tests (list :no-applicable-cxns)))
    ;; Doing lexical lookup:
    (multiple-value-bind (meaning initial-node)
        (comprehend utterance :cxn-inventory cxn-inventory :silent t)
      (declare (ignore meaning))
      (setf (all-parents initial-node) nil ;; We treat the node as the initial one.
            (fully-expanded? initial-node) nil ;; No longer consider it as a final node.
            (statuses initial-node) (remove-if #'(lambda(x)
                                                   (member x '(fcg::goal-test-failed fcg::goal-test-succeeded?)))
                                               (statuses initial-node))) ;; Do not keep goal-test status            
      (set-data interface :cip-nodes (list initial-node))
      ;; Now display the node in the web-pane.
      (display-node-in-web-pane initial-node interface)
      ;; Update the search tree pane.
      (setf (graph-pane-roots (search-tree interface)) (list initial-node))
      ;; Reset the configuration
      (setf (configuration cxn-inventory) config-copy
            (configuration (processing-cxn-inventory cxn-inventory)) config-copy)
      initial-node)))

(defun save-selected-node (data interface)
  (declare (ignore data))
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (get-universal-time))
    (declare (ignore day daylight-p zone))
    (let* ((selected-node (first (get-data interface :cip-nodes)))
           (subdirectory (text-input-pane-text (pathname-pane interface)))
           (name (format nil "cip-node-~a_~a_~a-~a~a~a.lisp" date month year hour minute second))
           (pathname (babel-pathname :directory (list subdirectory)
                                     :name name
                                     :type "lisp")))
      (cl-store::store selected-node pathname))))

(define-interface construction-bank-builder (interface blackboard)
  (delegate)
  (:panes
   ;; ROW 1: SENTENCE INPUT
   ;; -----------------------------------------------------------------
   ;; For typing in a sentence
   (sentence-input-pane text-input-pane
                        :title "Sentence:"
                        :reader sentence-input-pane
                        :initial-constraints '(:visible-min-height 10
                                               :visible-min-width 200)
                        :text "the green mouse ate our cheese") ;; "Type you sentence here.")
   ;; For starting the comprehension process.
   (lexical-lookup-button push-button
                          :callback 'dev-perform-lexical-lookup
                          :data "Start with Lexical Lookup")
   (initialize-button push-button
                      :data "Start without Lexical Lookup"
                      :callback #'(lambda(data interface)
                                       (declare (ignore data interface))
                                       (display-message "This function is not implemented yet.")))
   ;; ROW 2: PARAMETERS
   ;; -----------------------------------------------------------------
   (refresh-button push-button
                   :text "Refresh Constructions")
   (pathname-pane text-input-pane
                  :title "Store in Babel2 subfolder (folder must exist):"
                  :reader pathname-pane
                  :initial-constraints '(:visible-min-height 10)
                  :visible-max-width 400
                  :text "grammars/English/cxn-bank/")
   (save-node-button push-button
                     :data "Save selected node"
                     :callback #'save-selected-node)
  ;; ROW 3: CONSTRUCTION SELECTION AND WEB INTERFACE
  ;; -----------------------------------------------------------------
  (cxn-names list-panel
             :reader cxn-names)
  (cxn-view-pane capi:cocoa-view-pane
              :view-class "WebView"
              :initial-constraints '(:visible-min-width 1000
                                     :visible-min-height 400)
              :visible-min-width 1
              :horizontal-scroll t
              :vertical-scroll t
              :init-function 'init-construction-browser-html-pane)
  ;; ROW 4: SEARCH TREE
  ;; -----------------------------------------------------------------
  (search-tree simple-network-pane
               :reader search-tree
               :roots nil
               :layout-function :left-right
               :y-gap 10
               :children-function 'children
               :callback 'display-node-in-web-pane
               :action-callback #'(lambda(cip-node self)
                                    (display-node-in-web-pane cip-node self)
                                    (set-data self :cip-nodes (cons cip-node (all-parents cip-node))))
               :initeraction :single-selection
               :initial-constraints '(:visible-min-width 1000
                                      :visible-min-height 400)))
  (:layouts
   (parameters-row row-layout '(refresh-button pathname-pane save-node-button))
   (sentence-input row-layout '(sentence-input-pane lexical-lookup-button initialize-button))
   (selection-and-cip row-layout '(cxn-names :divider cxn-view-pane))
   (main-layout column-layout '(sentence-input parameters-row selection-and-cip search-tree)))
  (:default-initargs
   :layout 'main-layout
   :visible-min-width 1000
   :title "Construction Bank Builder"))
;; (display (make-instance 'construction-bank-builder))

(defmethod initialize-instance :after ((self construction-bank-builder) &key)
  (fli:register-module "/System/Library/Frameworks/WebKit.framework/WebKit"
                       :connection-style :immediate))

(defun dev-construction-bank-builder (&key (cxn-inventory *fcg-constructions*))
  (let ((interface (make-instance 'construction-bank-builder))
        (cxn-names (fcg-all-construction-names-from-hashed-construction-set cxn-inventory)))
    (set-data interface :interface-inventory cxn-inventory)
    (set-cxn-list-panel interface cxn-names :callback-fn nil
                        :action-callback-fn 'expand-selected-node-with-selected-cxn)
    (setf (slot-value (slot-value interface 'refresh-button) 'capi::selection-callback)
          #'(lambda(data interface)
              (declare (ignore data))
              (setf (collection-items (slot-value interface 'cxn-names))
                    (fcg-all-construction-names-from-hashed-construction-set cxn-inventory))))
    (display interface)
    interface))
;; (dev-construction-bank-builder)
