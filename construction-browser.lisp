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
;;;;;
;;;;; To use the construction browser, perform the following steps:
;;;;; ---------------------------------------------------------------------------------
;;;;; 1- Define/load a grammar inventory
;;;;; 2- Evaluate the following:
;;;;;    (ql:quickload :dev-tools)
;;;;; 3- (dev-construction-browser t :cxn-inventory *fcg-constructions*) ;; Or another inventory.
;;;;;    or using the web interface:
;;;;;    (dev-construction-browser :graphical :cxn-inventory *fcg-constructions*)
;;;;;    or using a mix:
;;;;;    (dev-construction-browser :mixed :cxn-inventory *fcg-constructions*)
;;;;;
;;;;; Note: the construction browser will only find constructions that were defined
;;;;;       using the def-fcg-cxn macro. If you want it to find other constructions,
;;;;;       you need to add its pathname to the construction inventory in the
;;;;;       following way:
;;;;; (setf (gethash 'construction-name (cxn-pathnames *fcg-constructions*)) "REPLACE-WITH-PATHNAME")
;;;;;
;;;;; Depending on the size of your inventory, it may take a second or two before the construction
;;;;; browser loads.
;;;;;
;;;;; NOTE: If the web interface pane is not working, you probably already have the web interface
;;;;;       opened in another window (a web browser or another dev-tool). Close all those windows
;;;;;       and restart the construction browser.

; (ql:quickload :dev-tools)

(in-package :dev-tools)

(export '(dev-construction-browser))

;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;; Interface 1: Construction selection menu + Lisp editor.
;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; Utility functions.
;; ----------------------------------------------------------------
(defun find-string-in-buffer (buffer string)
  "Looks for a string in a buffer and returns its offset."
  (editor::with-buffer-locked (buffer :for-modification nil)
    (editor::with-point ((point (editor::buffer-%start buffer)))
      (when (editor::find-string string point nil)
        (editor::i-find-point-offset buffer point)))))
;; (find-string-in-buffer (editor:current-buffer) "find-string-in-buffer")

(defun goto-line-of-string-in-buffer (buffer string)
  "Go to the line of the first instance of the provided string."
  (let* ((position (editor::find-place-in-buffer (find-string-in-buffer buffer string)
                                                 buffer))
         (line (1+ (editor::count-lines (editor::buffers-start buffer) position))))
    (editor:goto-line buffer line)))
;; (goto-line-of-string-in-buffer (editor:current-buffer) "find-STRING-in-buffer")

;; Callback functions
;; ----------------------------------------------------------------------
;; Opening the correct file and going to the relevant line in the buffer.
(defun construction-choice (cxn-name interface)
  "Backtrace function when selecting or double clicking a construction name."
  (when (editor::bufferp (editor-pane-buffer (viewer-pane interface)))
    (editor::kill-buffer-no-confirm (editor-pane-buffer (viewer-pane interface))))
  (let* ((cxn-inventory (get-data interface :interface-inventory))
         (file (gethash cxn-name (cxn-pathnames cxn-inventory))))
    ;; Now we try to open the file in the editor pane in the right line position.
    (if (null file)
      (display-message (format nil "Sorry, I could not find the construction ~a" cxn-name))
      (progn
        (setf (titled-object-title (viewer-pane interface)) (format nil "Filename: ~a" file)
              (editor-pane-buffer (viewer-pane interface)) (editor:find-file-buffer file)
              (editor-pane-text (viewer-pane interface)) ;; Syncing the text of the pane.
              (with-open-file (stream file)
                (let ((buffer
                       (make-array 1024
                                   :element-type
                                   (stream-element-type stream)
                                   :adjustable t
                                   :fill-pointer 0)))
                  (do ((char (read-char stream nil nil)
                             (read-char stream nil nil)))
                      ((null char))
                    (vector-push-extend char buffer))
                  (subseq buffer 0))))
        (let ((pos (find-string-in-buffer (editor-pane-buffer (viewer-pane interface))
                                                             (symbol-name cxn-name))))
          (when pos (editor::find-place-in-buffer pos
                                                  (editor-pane-buffer (viewer-pane interface)))))))))

;; For changing the editor fontsize.
(defun change-editor-fontsize (data interface)
  (let ((size (parse-integer data)))
    (setf (simple-pane-font (viewer-pane interface))
          (gp:make-font-description :family "Monaco"
                                    :size size))))

;; For retrieving the actual construction based on its name.
(defun get-selected-cxn-name (interface)
  (let* ((list-panel (slot-value interface 'cxn-names))
         (focus-item (slot-value list-panel 'capi::focus-item))
         (names (slot-value list-panel 'capi-internals:items-representation)))
    (when (and focus-item names)
      (svref names focus-item))))

;; For applying the construction in the web interface.
(defun apply-selected-cxn (interface direction)
  (with-activated-monitor trace-fcg
    (let ((cxn-name (get-selected-cxn-name interface)))
      (if (null cxn-name)
        (display-message "Please select a construction first.")
        (let ((the-cxn (fcg-find-cxn cxn-name
                                     (find-data interface :interface-inventory))))
          (cond
           ((null *saved-cfs*)
            (display-message
             "Please save a transient structure first using the web interface."))
           ((null the-cxn)
            (display-message (format nil "Sorry, I could not find construction ~a."
                                     cxn-name)))
           (t
            (fcg::ajax-apply-fcg-light-construction-aux the-cxn direction))))))))

;; Register bmp-files for the toolbar.
;; --------------------------------------------------
(gp:register-image-translation
 'send-to-web-interface-image
 #.(gp:read-external-image (merge-pathnames (make-pathname :directory '(:relative "imgs")
                                            :name "web-interface"
                                            :type "bmp")
                                            *dev-tools-path*)))

(gp:register-image-translation
 'apply-in-formulation-image
 #.(gp:read-external-image (merge-pathnames (make-pathname :directory '(:relative "imgs")
                                            :name "formulate"
                                            :type "bmp")
                                            *dev-tools-path*)))

(gp:register-image-translation
 'apply-in-comprehension-image
 #.(gp:read-external-image (merge-pathnames (make-pathname :directory '(:relative "imgs")
                                            :name "comprehension"
                                            :type "bmp")
                                            *dev-tools-path*)))

;; The interface
;; -----------------------------------------------------------------------------------------
(define-interface construction-browser (interface blackboard)
  ()
  (:panes
   (toolbar toolbar
            :items (list show-cxn-button formulation-button comprehension-button)
            :initial-constraints '(:visible-min-width 100
                                   :visible-min-height 10)
            :visible-min-width 1
            :visible-max-width nil)
   (show-cxn-button toolbar-button
                    :image 'send-to-web-interface-image
                    :tooltip "Send the selected construction to the web interface"
                    :selection-callback
                    #'(lambda(data interface)
                        (declare (ignore data))
                        (with-activated-monitor trace-fcg
                          (let ((cxn-name (get-selected-cxn-name interface)))
                            (if (null cxn-name)
                              (display-message "Please select a construction first.")
                              (let ((the-cxn (fcg-find-cxn cxn-name (find-data interface :interface-inventory))))
                                (if the-cxn
                                  (add-element (make-html the-cxn))
                                  (display-message (format nil "Sorry, I could not find construction ~a."
                                                           cxn-name)))))))))
   (formulation-button toolbar-button
                       :image 'apply-in-formulation-image
                       :tooltip "Apply the selected construction in formulation to *saved-cfs*."
                       :selection-callback
                       #'(lambda(data interface)
                           (declare (ignore data))
                           (apply-selected-cxn interface "T")))
   (comprehension-button toolbar-button
                         :image 'apply-in-comprehension-image
                       :tooltip "Apply the selected construction in comprehension to *saved-cfs*."
                       :selection-callback
                       #'(lambda(data interface)
                           (declare (ignore data))
                           (apply-selected-cxn interface "NIL")))
   (cxn-names list-panel)
   (cxn-definition editor-pane
                   :title "Definition"
                   :text "No construction selected."
                   :buffer-modes '("Lisp")
                   :font (gp:make-font-description :family "Monaco" :size 14)
                   :echo-area t
                   :vertical-scroll t
                   :visible-min-height '(:character 30)
                   :visible-min-width '(:character 120)
                   :reader viewer-pane))
  (:layouts
   (main-layout column-layout
                '(toolbar row-of-panes))
   (row-of-panes row-layout
                 '(cxn-names :divider cxn-definition)))
  (:menus
   (load-grammar "Load Grammar" (("Open"))
                 :selection-callback 'file-choice)
   (editor-fontsize "Change Fontsize" (("10" :selection-callback #'(lambda (data interface)
                                                                     (declare (ignore data))
                                                                     (change-editor-fontsize "10" interface)))
                                       ("12" :selection-callback #'(lambda (data interface)
                                                                     (declare (ignore data))
                                                                     (change-editor-fontsize "12" interface)))
                                       ("14" :selection-callback #'(lambda (data interface)
                                                                     (declare (ignore data))
                                                                     (change-editor-fontsize "14" interface)))
                                       ("16" :selection-callback #'(lambda (data interface)
                                                                     (declare (ignore data))
                                                                     (change-editor-fontsize "16" interface)))
                                       ("18" :selection-callback #'(lambda (data interface)
                                                                     (declare (ignore data))
                                                                     (change-editor-fontsize "18" interface)))
                                       ("20" :selection-callback #'(lambda (data interface)
                                                                     (declare (ignore data))
                                                                     (change-editor-fontsize "20" interface))))))
  (:menu-bar load-grammar editor-fontsize)
  (:default-initargs :title "Construction Browser"))
;; (dev-construction-browser t)

(defun file-choice (data interface)
  (declare (ignore data))
  (multiple-value-bind (filename res)
      (prompt-for-file "Load grammar image")
    (when res
      (set-data interface :interface-inventory (cl-store:restore filename))
      (setf (collection-items (slot-value interface 'cxn-names))
            (fcg-all-construction-names-from-hashed-construction-set
             (get-data interface :interface-inventory))))))


;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;; Interface 2: Construction selection menu + Web Interface
;;;; This reuses code from the LispWorks examples objc/webkit.lisp
;;;; (see LispWorks examples folder) 
;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(define-interface graphical-construction-browser (interface blackboard)
  (delegate)
  (:panes
   (cxn-names list-panel)
   (title title-pane
          :text "Construction:")
   (cxn-view-pane capi:cocoa-view-pane
              :view-class "WebView"
              :initial-constraints '(:visible-min-width 800)
              :visible-min-width 1
              :init-function 'init-construction-browser-html-pane))
  (:layouts
   (column column-layout '(title cxn-view-pane))
   (row-of-panes row-layout
                 '(cxn-names :divider column)))
  (:default-initargs
   :layout 'row-of-panes
   :best-width 1200
   :best-height 500
   :message-area t
   :title "Graphical Construction Browser"))

(defmethod initialize-instance :after ((self graphical-construction-browser) &key)
  (fli:register-module "/System/Library/Frameworks/WebKit.framework/WebKit"
                       :connection-style :immediate))

(defun init-construction-browser-html-pane (self view)
  (let ((view (objc:invoke view "init"))
        (delegate (make-instance 'web-kit-test-delegate
                                 :web-view-pane self)))
    (setf (slot-value (capi:top-level-interface self) 'delegate) delegate)
    (objc:invoke view "setFrameLoadDelegate:"
                 (objc:objc-object-pointer delegate))    
    view))

(defun show-graphical-cxn (data self)
  (declare (ignore data))
  ;;(clear-page)
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
    (let* ((cxn-name (get-selected-cxn-name self))
           (the-cxn (fcg-find-cxn cxn-name
                                  (find-data self :interface-inventory))))
      (add-element (make-html the-cxn)))))

(objc:define-objc-class web-kit-test-delegate ()
  ((web-view-pane :initarg :web-view-pane
                  :reader web-kit-test-delegate-pane))
  (:objc-class-name "WebKitTestDelegate"))

(objc:define-objc-method ("webView:didReceiveTitle:forFrame:" :void)
    ((self web-kit-test-delegate)
     (sender objc:objc-object-pointer) ; WebView *
     (title objc:objc-object-pointer)  ; NSString *
     (frame objc:objc-object-pointer)  ; WebFrame *
     )
  (when (fli:pointer-eq frame (objc:invoke sender "mainFrame"))
    (setf (capi:titled-object-title
           (capi:top-level-interface
            (web-kit-test-delegate-pane self)))
          (concatenate 'string
                       "Graphical Construction Browser with the "
                       (objc:ns-string-to-string title)))))

(objc:define-objc-method ("webView:didStartProvisionalLoadForFrame:" :void)
    ((self web-kit-test-delegate)))

(defun fcg-all-construction-names-from-hashed-construction-set (cxn-inventory)
  (sort (with-hash-table-iterator (pathname-iterator (cxn-pathnames cxn-inventory))
                           (let ((cxn-names nil))
                             (loop (multiple-value-bind (entry-p key value)
                                       (pathname-iterator)
                                     (declare (ignore value))
                                     (if entry-p
                                       (push key cxn-names)
                                       (return cxn-names))))))
        #'string<))

;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;; Interface 3: Construction selection menu + Lisp editor + Web interface
;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(define-interface mixed-construction-browser (interface blackboard)
  (delegate)
  (:panes
   (cxn-names list-panel)
   (title title-pane
          :text "Construction:")
   (cxn-view-pane capi:cocoa-view-pane
              :view-class "WebView"
              :initial-constraints '(:visible-min-width 800)
              :visible-min-width 1
              :init-function 'init-construction-browser-html-pane)
   (cxn-definition editor-pane
                   :title "Definition"
                   :text "No construction selected."
                   :buffer-modes '("Lisp")
                   :font (gp:make-font-description :family "Monaco" :size 14)
                   :echo-area t
                   :vertical-scroll t
                   :visible-min-height '(:character 30)
                   :visible-min-width '(:character 120)
                   :reader viewer-pane))
  (:menus
   (editor-fontsize "Change Fontsize" (("10" :selection-callback #'(lambda (data interface)
                                                                     (declare (ignore data))
                                                                     (change-editor-fontsize "10" interface)))
                                       ("12" :selection-callback #'(lambda (data interface)
                                                                     (declare (ignore data))
                                                                     (change-editor-fontsize "12" interface)))
                                       ("14" :selection-callback #'(lambda (data interface)
                                                                     (declare (ignore data))
                                                                     (change-editor-fontsize "14" interface)))
                                       ("16" :selection-callback #'(lambda (data interface)
                                                                     (declare (ignore data))
                                                                     (change-editor-fontsize "16" interface)))
                                       ("18" :selection-callback #'(lambda (data interface)
                                                                     (declare (ignore data))
                                                                     (change-editor-fontsize "18" interface)))
                                       ("20" :selection-callback #'(lambda (data interface)
                                                                     (declare (ignore data))
                                                                     (change-editor-fontsize "20" interface))))))
  (:menu-bar editor-fontsize)
  (:layouts
   (main-layout column-layout '(row-of-panes cxn-definition))
   (column column-layout '(title cxn-view-pane))
   (row-of-panes row-layout
                 '(cxn-names :divider column)))
  (:default-initargs
   :layout 'main-layout
   :best-width 1200
   :best-height 500
   :message-area t
   :title "Graphical Construction Browser"))

(defmethod initialize-instance :after ((self mixed-construction-browser) &key)
  (fli:register-module "/System/Library/Frameworks/WebKit.framework/WebKit"
                       :connection-style :immediate))

;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;; Function dev-construction-browser
;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defgeneric dev-construction-browser (mode &key cxn-inventory))

(defun set-cxn-list-panel (interface cxn-names &key (callback-fn 'construction-choice)
                                     action-callback-fn)
  (setf (slot-value interface 'cxn-names)
        (make-instance 'list-panel
                       :title "Construction Names"
                       :vertical-scroll t
                       :interaction :single-selection
                       :initial-constraints '(:visible-min-height 300)
                       :filter t
                       :keep-selection-p t
                       :selection-callback callback-fn
                       :action-callback (or action-callback-fn callback-fn)
                       :items cxn-names)))

(defmethod dev-construction-browser ((mode (eql :mixed))
                                     &key (cxn-inventory *fcg-constructions*))
  (declare (ignore mode))
  (let ((interface (make-instance 'mixed-construction-browser))
        (cxn-names (fcg-all-construction-names-from-hashed-construction-set cxn-inventory)))
    (set-data interface :interface-inventory cxn-inventory)
    (set-cxn-list-panel interface cxn-names :callback-fn #'(lambda(data interface)
                                                             (show-graphical-cxn data interface)
                                                             (construction-choice data interface)))
    (display interface)
    interface))
;; (dev-construction-browser :mixed)

(defmethod dev-construction-browser ((mode (eql :graphical))
                                     &key (cxn-inventory *fcg-constructions*))
  (declare (ignore mode))
  (let ((interface (make-instance 'graphical-construction-browser))
        (cxn-names (fcg-all-construction-names-from-hashed-construction-set cxn-inventory)))
    (set-data interface :interface-inventory cxn-inventory)
    (set-cxn-list-panel interface cxn-names :callback-fn 'show-graphical-cxn)
    (display interface)
    interface))
;; (dev-construction-browser :graphical)

(defmethod dev-construction-browser ((mode t)
                                     &key (cxn-inventory *fcg-constructions*))
  (declare (ignore mode))
  (let ((interface (make-instance 'construction-browser))
        (cxn-names (fcg-all-construction-names-from-hashed-construction-set cxn-inventory)))
    (set-data interface :interface-inventory cxn-inventory)
    (set-cxn-list-panel interface cxn-names)
    (setf (slot-value interface 'capi::plist)
          (list 'capi::meta-key :Alt))
    (display interface)
    interface))
;; (dev-construction-browser t)
