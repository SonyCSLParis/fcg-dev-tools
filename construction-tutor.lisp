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
;;;;; | The construction tutor is a tool for the semi-automatic definitions of constructions.     |
;;;;; | It requires:                                                                              |
;;;;; | - LispWorks on MacOSX                                                                     |
;;;;; | - A hashed-fcg-construction-set in which words/expressions are hashed using their string. |
;;;;; | - Recommended for faster/targeted development: a hybrid FCG grammar.                      |
;;;;; |                (see /sharing/fcg-hybrids/)                                                |
;;;;; ---------------------------------------------------------------------------------------------
;;;;;
;;;;; The construction tutor is a GUI that helps you select units and features from a transient 
;;;;; structure for building a new construction. When you are happy with the result, you can write
;;;;; it to a file where you can continue to edit it.
;;;;;
;;;;; Check out the Babeldocs folder "video-tutorials" for a demonstration on how to use the
;;;;; construction tutor.

; (ql:quickload :dev-tools)

(in-package :dev-tools)

(export '(dev-construction-tutor))

;;;;; Helper Functions
;;;;; ---------------------------------------------------------------------------------------------
(defun retain-symbols-alphabetically (units)
  "Extract all symbols found in units."
  (sort (remove-duplicates (remove-if #'numberp (flatten units)))
                              #'string< :key #'(lambda(x) (if (symbolp x) (symbol-name x) x))))

(defun retain-only-variables (symbols)
  "Retain only variables from a list of symbols."
  (remove-if-not #'variable-p symbols))

(defun tutor-get-roots (units)
  "Retain the units that are considered as roots for the tree-view-panes."
  (loop for unit in units
        unless (string= "ROOT" (symbol-name (unit-name unit)))
        collect (list (unit-name unit) (unit-body unit))))

(defun update-choice-items (new old1 old2 array)
  "Rename symbols in an array."
  (loop for i from 1 to (array-total-size array)
        do (setf (aref array (1- i))
                 (subst new old1 (if old2 (subst new old2 (aref array (1- i)))
                                   (aref array (1- i)))))))

(defun dev-simple-feature-p (feature-name feature-types)
  "If a feature type has been defined, it is considered as simple."
  (assoc feature-name feature-types :key #'symbol-name :test #'string=))

(defun tutor-children-fn (x-with-value feature-types)
  "Calculates the children of the selection menu items."
  (let ((feature (feature-name x-with-value))
        (value (second x-with-value)))
    (unless (or (listp feature) (null value))
      (if (or (symbolp value)
              (dev-simple-feature-p feature feature-types))
        (list (list value nil))
        (loop for f in value
              collect (list (first f) (second f)))))))

(defun find-children (item other-items feature-types n)
  (let ((children (tutor-children-fn item feature-types)))
    (loop for child in children
          collect (+ n (position child other-items :test #'equal)))))

(defun compute-map-for-roots-and-children (items feature-types &optional (n 0) results)
  (if (null items) (reverse results)
    (compute-map-for-roots-and-children (rest items) feature-types (1+ n)
                                        (cons (list n (find-children (first items) (rest items)
                                                                     feature-types (1+ n)))
                                              results))))

(defun handle-unselected-value (value feature-types)
  (cond ((null value) nil)
        ((or (and (symbolp (first value)) (symbolp (second value)))
             (dev-simple-feature-p (first value) feature-types))
         value)
        (t
         (let ((feature (first value))
               (subfeatures (second value)))
           (cons feature (loop for subfeature in subfeatures
                               collect (handle-unselected-value subfeature feature-types)))))))

(defun format-feature (feature feature-types &key (where nil))
  "For formatting features in the definition pane."
  (if (symbolp feature)
    (format where "  ~(~a~)" feature)
    (let ((feature-name (feature-name feature)))
      (if (or (dev-simple-feature-p feature-name feature-types)
              (symbolp (second feature)))
        (format where "  ~(~a~)" feature)
        (format where "  (~(~a~)~&     ~{~a~^~&     ~})"
                feature-name (rest feature))))))

(defun format-contributing-units (units feature-types &key (where nil))
  "For formatting units in the definition pane."
  (if (symbolp units) (format where "~a" units)
    (format where "~{~a~^~%   ~}"
            (loop for unit in units
                  collect (format nil "(~(~a~)~&  ~{~(~a~)~^~&  ~})" (unit-name unit)
                                  (loop for feature in (unit-body unit)
                                        collect (format-feature feature feature-types)))))))

;;;;; Interface Class
;;;;; ---------------------------------------------------------------------------------------------
(define-interface construction-tutor (interface blackboard)
  ()
  (:panes
   ;; Options for the construction:
   (name-pane text-input-pane
              :title "Name your construction:"
              :reader cxn-name
              :text (format nil "construction-~a" (gensym)))
   (save-pane push-button
              :text "Save construction"
              :callback 'tutor-save-cxn)
   (disable-footprints-choice radio-button-panel)
   (hash-form extended-selection-tree-view
              :title "Form:"
              :reader hash-form
              :roots nil
              :interaction :multiple-selection
              :children-function 'symbolp)
   (hash-meaning extended-selection-tree-view
                 :title "Meaning:"
                 :reader hash-meaning
                 :roots nil
                 :interaction :multiple-selection
                 :children-function 'symbolp)
   (hash-destination option-pane
                     :reader hash-destination)
   (hash-save push-button
              :text "Hash selection"
              :callback 'hash-selection)
   (hash-clear push-button
               :text "Clear hash"
               :callback 'clear-hash)
   (attributes editor-pane
               :title "Attributes:"
               :text "Write the attributes here."
               :buffer-modes '("Lisp")
               :font (gp:make-font-description :family "Monaco" :size 14)
               :vertical-scroll t
               :visible-min-height 30
               :visible-max-height 40
               :reader attributes-pane)
   ;; Panes that allow you to rename symbols and equalize variables:
   (original-variable-pane option-pane)
   (original-variable-pane2 option-pane)
   (var-renaming text-input-pane :title "as:"
                 :reader var-renaming)
   (equalizer-button push-button
                     :text "Equalize!"
                     :callback 'tutor-equalize-variables)
   (symbol-name-pane option-pane)
   (symbol-renaming text-input-pane
                    :title "as:"
                    :reader symbol-renaming)
   (renaming-button push-button
                    :text "Rename!"
                    :callback 'tutor-rename-symbols)
   ;; Selection of the units and their features.
   (dev-contributing-units extended-selection-tree-view)
   (dev-formulation-lock extended-selection-tree-view)
   (dev-comprehension-lock extended-selection-tree-view)
   ;; Resulting construction:
   (def-fcg-cxn-pane editor-pane
                     :title "Construction definition:"
                     :text "You construction definition will appear here."
                     :buffer-modes '("Lisp")
                     :font (gp:make-font-description :family "Monaco" :size 14)
                     :vertical-scroll t
                     :visible-min-height 500
                     :visible-min-width '(:character 120)
                     :reader def-fcg-cxn-pane))
  (:layouts
   (main-layout column-layout '(name-save-row
                                :separator choices-row
                                :separator renamings-column selection-row def-fcg-cxn-pane))
   (name-save-row row-layout '(name-pane save-pane))
   (choices-row row-layout '(disable-footprints-choice attributes hash-column))
   (hash-features row-layout '(hash-meaning hash-form))
   (hash-execution row-layout '(hash-destination hash-save hash-clear))
   (hash-column column-layout '(hash-features hash-execution))
   (renamings-column column-layout '(variable-renamings-row symbol-renamings-row))
   (symbol-renamings-row row-layout '(symbol-name-pane symbol-renaming renaming-button))
   (variable-renamings-row row-layout '(original-variable-pane original-variable-pane2
                                                               var-renaming equalizer-button))
   (selection-row row-layout '(dev-contributing-units dev-formulation-lock dev-comprehension-lock)))
  (:default-initargs
   :layout 'main-layout
   :visible-min-height 900
   :visible-min-width 1200
   :title "Construction Tutor"))
;; (display (make-instance 'construction-tutor))

;;;;; Interface Support Functions
;;;;; ---------------------------------------------------------------------------------------------
(defun hash-selection (data interface)
  "Callback function that sends the hashed features to their destined units."
  (declare (ignore data))
  (let* ((destination-unit (svref (collection-items (hash-destination interface))
                                  (choice-selection (hash-destination interface))))
         (selected-meaning (loop for i in (choice-selection (hash-meaning interface))
                                 collect (aref (collection-items (hash-meaning interface)) i)))
         (selected-form (loop for i in (choice-selection (hash-form interface))
                              collect (aref (collection-items (hash-form interface)) i)))
         (old-selection (loop for unit in (get-data interface :hashed-features)
                              unless (eql (unit-name unit) destination-unit)
                              collect unit)))
    (set-data interface :hashed-features (cons `(,destination-unit
                                                 ,(if selected-meaning
                                                    `((HASH meaning ,selected-meaning))
                                                    NIL)
                                                 ,(if selected-form
                                                    `((HASH form ,selected-form))
                                                    NIL))
                                               old-selection))
    (update-cxn-definition interface :package (get-data interface :package))))

(defun clear-hash (data interface)
  (declare (ignore data))
  (set-data interface :hashed-features nil)
  (update-cxn-definition interface :package (get-data interface :package)))

(defun tutor-save-cxn (data interface)
  (declare (ignore data))
  (multiple-value-bind (filename res)
      (capi:prompt-for-file (format nil "Save ~a" (text-input-pane-text (cxn-name interface)))
                            :pathname (format nil "~a" (text-input-pane-text (cxn-name interface)))
                            :filter "*.lisp"
                            :operation :save)
    (when res (editor:write-file-command t filename (editor-pane-buffer (def-fcg-cxn-pane interface))))))

(defun dev-contributing-units (interface)
  (slot-value interface 'dev-contributing-units))

(defun dev-formulation-lock (interface)
  (slot-value interface 'dev-formulation-lock))

(defun dev-comprehension-lock (interface)
  (slot-value interface 'dev-comprehension-lock))

(defun unit-selection-panes (interface)
  (list (dev-contributing-units interface)
        (dev-formulation-lock interface)
        (dev-comprehension-lock interface)))

(defun collapse-all-units (pane)
  (loop for i from 0 to (1- (array-total-size (collection-items pane)))
        do (setf (tree-view-expanded-p pane (aref (collection-items pane) i)) nil)))

(defun collapse-unit-selection-panes (interface)
  (loop for selection-pane in (unit-selection-panes interface)
        do (collapse-all-units selection-pane)))

(defun subst-two-symbols (new-value symb-1 symb-2 lst)
  (subst new-value symb-1 (if symb-2
                            (subst new-value symb-2 lst) lst)))

(defun tutor-perform-renaming (interface new-value symb-1 &optional symb-2)
  "The user wants to rename some symbols."
  ;; First we rename the units...
  (set-data interface :units (subst-two-symbols new-value symb-1 symb-2 (get-data interface :units)))
  ;; Then we rename the symbols in the unit selection panes.
  (loop for the-pane in (unit-selection-panes interface)
        do (let* (;; We need to remember which items were expanded.
                  (expanded-items (loop for i from 0 to (1- (array-total-size (collection-items the-pane)))
                                        when (tree-view-expanded-p the-pane (aref (collection-items the-pane) i))
                                        collect i))
                  ;; We need to remember which items were selected.
                  (selection (copy-list (choice-selection the-pane))))
             ;; We update the roots with the renamed units information. The rest of the tree follows automatically.
             (setf (tree-view-roots the-pane) (tutor-get-roots (get-data interface :units)))
             ;; We re-expand the previously expanded items.
             (collapse-all-units the-pane)
             (loop for item-number in expanded-items
                   do (setf (tree-view-expanded-p the-pane (aref (collection-items the-pane) item-number)) t))
             ;; We re-selected the previously selected units and features.
             (setf (choice-selection the-pane) selection)))
  ;; Then we rename all the hash-related panes and slots.
  (setf (collection-items (hash-destination interface)) (loop for unit in (get-data interface :units)
                                                              unless (string= "ROOT" (symbol-name (unit-name unit)))
                                                              collect (unit-name unit)))
  (setf (tree-view-roots (hash-meaning interface)) (subst-two-symbols new-value symb-1 symb-2
                                                                      (tree-view-roots (hash-meaning interface)))
        (tree-view-roots (hash-form interface)) (subst-two-symbols new-value symb-1 symb-2
                                                                   (tree-view-roots (hash-form interface))))
  (set-data interface :hashed-features (subst-two-symbols new-value symb-1 symb-2 (get-data interface :hashed-features)))
  ;; Then we rename all the option panes.
  (let* ((all-symbols (retain-symbols-alphabetically (get-data interface :units)))
         (all-variables (retain-only-variables all-symbols)))
    (setf (collection-items (slot-value interface 'original-variable-pane)) all-variables
          (collection-items (slot-value interface 'original-variable-pane2)) all-variables
          (collection-items (slot-value interface 'symbol-name-pane)) all-symbols)
    ;; Then we update everything:
    (update-cxn-definition interface :package (get-data interface :package))))

(defun tutor-rename-symbols (data interface)
  "Callback function for the symbol renaming button."
  (declare (ignore data))
  (let* ((symbol-pane (slot-value interface 'symbol-name-pane))
         (old-symbol (svref (collection-items symbol-pane)
                            (choice-selection symbol-pane)))
         (new-symbol (make-symbol (text-input-pane-text (symbol-renaming interface)))))
    (tutor-perform-renaming interface new-symbol old-symbol)))

(defun tutor-equalize-variables (data interface)
  "Callback function for the equalize button."
  (declare (ignore data))
  (let* (;; Get the panes.
         (variable-pane-1 (slot-value interface 'original-variable-pane))
         (variable-pane-2 (slot-value interface 'original-variable-pane2))
         ;; Get their selections...
         (selection-1 (choice-selection variable-pane-1))
         (selection-2 (choice-selection variable-pane-2)))
    (unless (= selection-1 selection-2)
      (let* ((var-1 (svref (slot-value variable-pane-1 'capi-internals:items-representation) selection-1))
             (var-2 (svref (slot-value variable-pane-2 'capi-internals:items-representation) selection-2))
             (my-renaming (make-symbol (text-input-pane-text (var-renaming interface)))))
        (tutor-perform-renaming interface my-renaming var-1 var-2)))))
          
(defun handle-selected-units (interface &key feature-types (which-pane 'dev-contributing-units))
  "Obtain a printable version of the selected units and features."
  (let* ((pane (slot-value interface which-pane))
         (the-selection-keys (choice-selection pane)))
    (when the-selection-keys
      (let* ((already-handled nil)
             (items (collection-items pane))
             (map (compute-map-for-roots-and-children (array->list items) feature-types)))
        (labels ((expand-selection-keys (selection-keys)
                   (cond ((null selection-keys) nil)
                         ((find (first selection-keys) already-handled)
                          (expand-selection-keys (rest selection-keys)))
                         (t
                          (let* ((selection-expansion (first (aref items (first selection-keys)))) ;; retrieve the item
                                 (selection-expansion-value (second (aref items (first selection-keys))))
                                 (selection-children (second (assoc (first selection-keys)
                                                                    map)))
                                 (selected-selection-children
                                  (sort (intersection selection-children the-selection-keys) #'<)))
                            (push (first selection-keys) already-handled)
                            (cond ((null selection-expansion) nil)
                                  ((or (symbolp selection-expansion-value)
                                       (dev-simple-feature-p selection-expansion feature-types))
                                   (append (list (aref items (first selection-keys)))
                                           (expand-selection-keys (rest selection-keys))))
                                  ((null selected-selection-children) ;; We didn't select subfeatures
                                   (append (list (cons selection-expansion
                                                       (loop for a-value in selection-expansion-value
                                                             collect (handle-unselected-value a-value
                                                                                              feature-types))))
                                           (expand-selection-keys (rest selection-keys))))
                                  ((null selection-children)
                                   selection-expansion-value) ;; We reached an end-point.
                                  (t
                                   (append (list (cons selection-expansion
                                                       (expand-selection-keys selected-selection-children)))
                                           (expand-selection-keys (loop for key in (rest selection-keys)
                                                                        unless (member key selected-selection-children)
                                                                        collect key))))))))))
          (expand-selection-keys the-selection-keys))))))

(defun handle-locks (interface &key feature-types)
  "Obtain a printable version of the selected formulation and comprehension locks."
  (let ((formulation-units (handle-selected-units interface :feature-types feature-types
                                                  :which-pane 'dev-formulation-lock))
        (comprehension-units (handle-selected-units interface :feature-types feature-types
                                                    :which-pane 'dev-comprehension-lock))
        (hashed-features (find-data interface :hashed-features))
        (results nil))
    (dolist (formulation-unit formulation-units)
      (let ((corresponding-unit (assoc (unit-name formulation-unit) comprehension-units))
            (corresponding-hash (assoc (unit-name formulation-unit) hashed-features)))
        (setf results (cons `(,(unit-name formulation-unit)
                              ,@(second corresponding-hash)
                              ,@(rest formulation-unit)
                              --
                              ,@(third corresponding-hash)
                              ,@(rest corresponding-unit))
                            results))))
    (dolist (comprehension-unit comprehension-units)
      (unless (assoc (unit-name comprehension-unit) results)
        (let ((corresponding-hash (assoc (unit-name comprehension-unit) hashed-features)))
          (setf results (cons `(,(unit-name comprehension-unit)
                                ,@(second corresponding-hash)
                                --
                                ,@(third corresponding-hash)
                                ,@(rest comprehension-unit))
                              results)))))
    (dolist (hashed-unit hashed-features)
      (unless (assoc (unit-name hashed-unit) results)
        (setf results (cons `(,(unit-name hashed-unit)
                              ,@(second hashed-unit)
                              --
                              ,@(third hashed-unit))
                            results))))
    (reverse results)))

(defun update-cxn-definition (interface &key (package :fcg))
  "Function that dynamically updates the information in the interface."
  (let ((feature-types (feature-types
                        (get-data interface :interface-inventory))))
    (setf (editor-pane-text (def-fcg-cxn-pane interface))
          (format nil "(in-package :~(~a~))
                 ~%(def-fcg-cxn ~a~a~a~a~a~a)"
                  package ;; in which package?
                  (text-input-pane-text (cxn-name interface)) ;; cxn-name
                  (format nil "~%  (~(~a~)"
                          (format-contributing-units
                           (handle-selected-units interface :which-pane 'dev-contributing-units
                                                  :feature-types feature-types)
                           feature-types))
                  (format nil "~%   <-~&   ")
                  (format nil (format nil "~a)" (format-contributing-units
                                                 (handle-locks interface :feature-types feature-types)
                                                 feature-types)))
                  (format nil "~% :disable-automatic-footprints ~(~a~)"
                          (if (string= (choice-selected-item (slot-value interface 'disable-footprints-choice)) "Yes")
                            "t" "nil"))
                  (format nil "~% :attributes ~a" (editor-pane-text (attributes-pane interface)))))
    (editor::redisplay-all)))

(defun dev-construction-tutor (&key attributes
                                    (construction-inventory *fcg-constructions*)
                                    (cxn-set 'unhashed)
                                    (disable-automatic-footprints t)
                                    (package :fcg)
                                    (transient-structure *saved-cfs*))
  "Function to start and initialize the construction tutor."
  (if (null transient-structure)
    (display-message
     "Please provide a transient structure, or save a transient structure in the web interface to *saved-cfs*.")
    (let* ((original-units (fcg-get-transient-unit-structure transient-structure))
           (unit-renamings (loop for unit in original-units
                                 unless (string= "ROOT" (symbol-name (unit-name unit)))
                                 collect (cons (unit-name unit) (make-var (unit-name unit)))))
           (units (sublis unit-renamings original-units))
           (root-unit (find-if #'(lambda(unit)
                                   (string= "ROOT" (symbol-name (unit-name unit))))
                               units))
           (all-symbols (retain-symbols-alphabetically units))
           (all-variables (remove-if-not #'variable-p all-symbols))
           (feature-types (feature-types construction-inventory))
           (roots (tutor-get-roots units))
           (interface (make-instance 'construction-tutor))
           (selection-pane-names '(dev-contributing-units dev-formulation-lock dev-comprehension-lock))
           (selection-panes (progn (set-data interface :interface-inventory construction-inventory)
                              (set-data interface :temp-selection nil)
                              (loop for title in (list "Contributing part:" "Formulation lock:" "Comprehension lock:")
                                    collect (make-instance 'extended-selection-tree-view
                                                           :roots roots
                                                           :retain-expanded-nodes t
                                                           :keep-selection-p t
                                                           :expandp-function 'second
                                                           :interaction :multiple-selection
                                                           :selection-callback #'(lambda(selection interface)
                                                                                   (declare (ignore selection))
                                                                                   (update-cxn-definition interface
                                                                                                          :package package))
                                                           :retract-callback #'(lambda(selection interface)
                                                                                   (declare (ignore selection))
                                                                                   (update-cxn-definition interface
                                                                                                          :package package))
                                                           :visible-min-height 100
                                                           :horizontal-scroll t
                                                           :vertical-scroll t
                                                           :print-function #'(lambda(x)
                                                                               (format nil "~a" (first x)))
                                                           :children-function #'(lambda(x)
                                                                                  (tutor-children-fn x feature-types))
                                                           :title title))))
           (update-fn #'(lambda(&rest args)
                          (declare (ignore args))
                          (update-cxn-definition interface :package package))))
      (setf ;; Name of the construction
            (slot-value (cxn-name interface) 'capi-internals:callback) update-fn
            ;; Set the attributes:
            (editor-pane-text (attributes-pane interface)) (cond (attributes (format nil "~a" attributes))
                                                                 (cxn-set (format nil "(:label ~a)" cxn-set))
                                                                 (t nil))
            (slot-value (attributes-pane interface) 'capi::after-input-callback) update-fn
            ;; Symbol option pane:
            (slot-value interface 'symbol-name-pane) (make-instance 'option-pane
                                                                   :items all-symbols
                                                                   :title "Rename the symbol:")
            ;; Hash panes:
            (tree-view-roots (hash-form interface)) (unit-feature-value root-unit 'form)
            (tree-view-roots (hash-meaning interface)) (unit-feature-value root-unit 'meaning)
            (collection-items (hash-destination interface)) (mapcar #'rest unit-renamings)
            ;; Footprints:
            (slot-value interface 'disable-footprints-choice)
            (make-instance 'radio-button-panel
                           :title "Disable automatic footprints?"
                           :items '("Yes" "No")
                           :selection-callback update-fn
                           :interaction :single-selection
                           :initial-focus-item (if disable-automatic-footprints 0 1)))
      ;; Variable panes:
      (loop for variable-pane in '(original-variable-pane original-variable-pane2)
            for title in '("Equalize variable:" "with:")
            for selection-fn in (list nil #'(lambda(data interface)
                                              (setf (text-input-pane-text (var-renaming interface))
                                                    (format nil "~(~a~)" data))))
            do (setf (slot-value interface variable-pane)
                     (make-instance 'option-pane :items all-variables
                                    :title title
                                    :selection-callback selection-fn)))
      ;; Choice-panes:
      (loop for choice in selection-pane-names
            for selection-pane in selection-panes            
            do (setf (slot-value interface choice) selection-pane))
      (setf (text-input-pane-text (var-renaming interface)) (format nil "~(~a~)" (first all-variables)))
      (update-cxn-definition interface :package package)
      (set-data interface :all-symbols all-symbols)
      (set-data interface :all-variables all-variables)
      (set-data interface :package package)
      (set-data interface :units units)
      (set-data interface :hashed-features nil)
      (display interface)
      (collapse-unit-selection-panes interface)
      interface)))
;; (dev-construction-tutor :package :english-grammar)
