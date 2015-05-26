;;;; Copyright (C) 2013 Paulo Madeira
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Interface

(cl:in-package #:cl-user)


(defpackage #:editor-color-theme
  (:use #:cl)
  (:export #:*current-colors*
           #:all-color-themes
           #:color-theme-args
           #:color-theme
           #:define-color-theme
           #:remove-color-theme
           ))

(in-package #:editor-color-theme)


;;; Configuration

;; Default foreground and background colors
(defconstant +default-foreground-color+ :black)
(defconstant +default-background-color+ :white)

(defvar *current-colors* (make-hash-table))

;;; Implementation

(defvar *all-color-themes* (make-hash-table :test 'string=))

(defvar *editor-face-names*
  '(:region
    :show-point-face
    :interactive-input-face
    :highlight
    :non-focus-complete-face
    :font-lock-function-name-face
    :font-lock-comment-face
    :font-lock-type-face
    :font-lock-variable-name-face
    :font-lock-string-face
    :font-lock-keyword-face
    :font-lock-builtin-face
    :compiler-note-highlight
    :compiler-warning-highlight
    :compiler-error-highlight
    :incremental-search-face
    :incremental-search-other-matches-face
    ))



(defclass editor-panes-theme ()
  ((editor-panes :initform nil :accessor editor-panes)
   (editor-background :initform +default-background-color+ :accessor bg) 
   (editor-foreground :initform +default-foreground-color+ :accessor fg)))

(defclass listener-panes-theme ()
  ((listener-panes :initform nil :accessor listener-panes)
   (listener-foreground :initform +default-foreground-color+ :accessor bg)
   (listener-background :initform +default-background-color+ :accessor fg)))


(defclass general-panes-theme ()
  ((output-panes :initform nil :accessor output-panes)
   (output-foreground :initform +default-foreground-color+ :accessor output-fg)
   (output-background :initform +default-background-color+ :accessor output-bg)))

(defvar *editor-tool* (make-instance 'editor-panes-theme))
(defvar *listener-tool* (make-instance 'listener-panes-theme))
(defvar *all-tools* (make-instance 'general-panes-theme))

;; (eq (capi:capi-object-name b) 'lw-tools::buffers-list)


(defun all-color-themes ()
  (maphash #'(lambda (key value)
               (declare (ignore value))
               key)
           *all-color-themes*))

(defun color-theme-data (theme-name)
  (multiple-value-bind (data found?)
      (gethash theme-name *all-color-themes*)
    (if found?
        data
        (error "No color theme named ~s found." theme-name))))

(defun color-theme-super-theme-names (theme-name)
  (first (color-theme-data theme-name)))

(defun color-theme-args (theme-name)
  (rest (color-theme-data theme-name)))

(defun update-pane-colors (pane foreground background)
  (setf (capi:simple-pane-foreground pane) foreground)
  (setf (capi:simple-pane-background pane) background)
  
  (let ((recolorize-p (editor::buffer-font-lock-mode-p (capi:editor-pane-buffer pane))))
    (when recolorize-p
      (gp:invalidate-rectangle pane))))


(defgeneric clear-colors (tool)
  (:documentation "Clear colors for tool keeping other data untouched"))

(defgeneric update (tool)
  (:documentation "Update tool's colors"))


(defmethod clear-colors ((self editor-panes-theme))
  (with-slots (editor-background editor-foreground) self
    (setf editor-background +default-background-color+)
    (setf editor-foreground +default-foreground-color+)))

(defmethod clear-colors ((self listener-panes-theme))
  (with-slots (listener-background listener-foreground) self
    (setf listener-background +default-background-color+)
    (setf listener-foreground +default-foreground-color+)))

(defmethod clear-colors ((self general-panes-theme))
  (with-slots (output-background output-foreground) self
    (setf output-background +default-background-color+)
    (setf output-foreground +default-foreground-color+)))


(defmethod update ((self editor-panes-theme))
  (mapcar #'(lambda (pane)
              (update-pane-colors pane (fg self) (bg self)))
          (editor-panes self)))

(defmethod update ((self listener-panes-theme))
  (mapcar #'(lambda (pane)
              (update-pane-colors pane (fg self) (bg self)))
          (listener-panes self)))


(defmethod update ((self general-panes-theme))
  (mapcar #'(lambda (pane)
              (update-pane-colors pane (output-fg self) (output-bg self)))
          (output-panes self)))


(defun set-color-theme (theme-name)
  (destructuring-bind (&rest color-theme-args
                             &key foreground background
                             listener-foreground
                             listener-background
                             output-foreground
                             output-background
                             &allow-other-keys)
      (color-theme-args theme-name)

    ;; new instances of tools wrappers
    (clear-colors *editor-tool*)
    (clear-colors *listener-tool*)
    (clear-colors *all-tools*)
    
    ;; editor foreground and background
    (when foreground
      (setf (fg *editor-tool*) foreground))
    (when background
      (setf (bg *editor-tool*) background))
    ;; listener foreground and background, uses
    ;; the :background and :foreground if not specified
    (setf (fg *listener-tool*)
          (or listener-foreground
              (fg *editor-tool*))
          (bg *listener-tool*)
          (or listener-background
              (bg *editor-tool*)))

    ;; output foreground and background, uses :background and
    ;; :foreground if not specified
    (setf (output-fg *all-tools*)
          (or output-foreground
              (fg *editor-tool*))
          (output-bg *all-tools*)
          (or output-background
              (bg *editor-tool*)))
                                 
    (dolist (name *editor-face-names*)
      (let* ((color-theme-args-for-face (getf color-theme-args name))
             (face-name (intern (string name) '#:editor))
             (face (editor:make-face face-name :if-exists t)))
        (apply 'editor:make-face face-name :if-exists :overwrite
               :documentation (or (getf color-theme-args-for-face :documentation)
                                  (slot-value face 'documentation))
               color-theme-args-for-face))))
  
  theme-name)

(defun color-theme (theme-name)
  (mapc 'set-color-theme (color-theme-super-theme-names theme-name))
  (set-color-theme theme-name)
  
  (update *editor-tool*)
  (update *listener-tool*)
  (update *all-tools*)
  
  theme-name)

(defun define-color-theme (theme-name super-theme-names
                           &rest color-theme-args &key &allow-other-keys)
  (unless super-theme-names
    (setf super-theme-names '("default")))
  (dolist (super-theme-name super-theme-names)
    (multiple-value-bind (color-theme-data found?)
        (gethash super-theme-name *all-color-themes*)
      (declare (ignore color-theme-data))
      (unless found?
        (warn "Inherited color theme ~s not defined." super-theme-name))))
  
  (setf (gethash theme-name *all-color-themes*) (list* super-theme-names color-theme-args))
  
  theme-name)

(defun remove-color-theme (theme-name)
  (remhash theme-name *all-color-themes*))

(defun set-editor-pane-colors (pane)
  (typecase pane
    (capi:editor-pane
     (progn
       (pushnew pane (editor-panes *editor-tool*))
       (let ((bg-color (bg *editor-tool*))
             (fg-color (fg *editor-tool*)))
         (setf (capi:simple-pane-foreground pane) fg-color)
         (setf (capi:simple-pane-background pane) bg-color))))))


(defun set-listener-pane-colors (pane)
  (typecase pane
    (capi:editor-pane
     (progn
       (pushnew pane (listener-panes *listener-tool*))
       (let ((bg-color (bg *listener-tool*))
             (fg-color (fg *listener-tool*)))
         (setf (capi:simple-pane-foreground pane) fg-color)
         (setf (capi:simple-pane-background pane) bg-color))))))


(defun set-collector-pane-colors (pane)
  ;;(when (typep (capi:top-level-interface pane) 'lw-tools:listener)
  (pushnew pane (output-panes *all-tools*))
  (let ((bg-color (output-bg *all-tools*))
        (fg-color (output-fg *all-tools*)))
    (setf (capi:simple-pane-foreground pane) fg-color)
    (setf (capi:simple-pane-background pane) bg-color)))
   

(lispworks:defadvice ((method capi:interface-display :before (lw-tools:editor))
                      change-editor-colors
                      :before
                      :documentation "Change editor colors.")
    (interface)
  (capi:map-pane-descendant-children interface 'set-editor-pane-colors))


;; we don't have defined capi:interface-display for lw-tools::listener,
;; so nothing to advice. Instead we need to define our own
(sys::without-warning-on-redefinition
  (defmethod capi:interface-display :before ((self lw-tools::listener))
    (capi:map-pane-descendant-children
     self 'set-listener-pane-colors)))

;; capi:collector-pane does'nt have interface-display method called,
;; so we adding the :after constuctor instead
(sys::without-warning-on-redefinition
  (defmethod initialize-instance :after ((self capi:collector-pane) &rest
                                         clos::initargs &key &allow-other-keys)
    (set-collector-pane-colors self)))


;; This makes it "work" after the podium is launched
(defun is-editor-pane-p (obj)
  (and (typep obj 'capi:editor-pane)
       (not (eq obj (hcl:class-prototype (class-of obj))))))

(defun cache-existing-pane (pane)
  (pushnew pane (editor-panes *editor-tool*)))

(defun cache-if-pane (obj)
  (when (is-editor-pane-p obj)
    (cache-existing-pane obj)))

#+:lispworks-personal-edition
(hcl:sweep-all-objects #'cache-if-pane)


;;; Initial color themes

(define-color-theme "default" ()
  :foreground nil
  :background nil
  :region '(:foreground :color_highlighttext
            :background :color_highlight)
  :show-point-face '(:background :green)
  :interactive-input-face '(:foreground :red3)
  :highlight '(:bold-p t)
  :non-focus-complete-face '(:background :tweak_background)
  :font-lock-function-name-face '(:foreground :blue)
  :font-lock-comment-face '(:foreground :firebrick)
  :font-lock-type-face '(:foreground :forestgreen)
  :font-lock-variable-name-face '(:foreground :darkgoldenrod)
  :font-lock-string-face '(:foreground :rosybrown)
  :font-lock-keyword-face '(:foreground :purple)
  :font-lock-builtin-face '(:foreground :orchid)
  :compiler-note-highlight '(:foreground :magenta)
  :compiler-warning-highlight '(:foreground :orange3)
  :compiler-error-highlight '(:foreground :red)
  :incremental-search-face '(:background :tweak_background)
  :incremental-search-other-matches-face '(:underline-p t))

(define-color-theme "plain" ()
  :foreground nil :background nil
  :region '(:foreground :color_highlighttext
            :background :color_highlight)
  :show-point-face '()
  :interactive-input-face '()
  :highlight '(:bold-p t)
  :non-focus-complete-face '(:background :tweak_background)
  :font-lock-function-name-face '()
  :font-lock-comment-face '()
  :font-lock-type-face '()
  :font-lock-variable-name-face '()
  :font-lock-string-face '()
  :font-lock-keyword-face '()
  :font-lock-builtin-face '()
  :compiler-note-highlight '()
  :compiler-warning-highlight '()
  :compiler-error-highlight '()
  :incremental-search-face '(:background :tweak_background)
  :incremental-search-other-matches-face '(:underline-p t))


(define-color-theme "emacs" ()
  ;; :foreground nil :background nil
  :region '(:foreground :color_highlighttext
            :background :color_highlight)
  :show-point-face '(:background :green)
  :interactive-input-face '(:foreground :red3)
  :highlight '(:bold-p t)
  :non-focus-complete-face '(:background :tweak_background)
  :font-lock-function-name-face '(:foreground :blue)
  :font-lock-comment-face '(:foreground :gray40)
  :font-lock-type-face '(:foreground :forestgreen)
  :font-lock-variable-name-face '(:foreground :darkgoldenrod)
  :font-lock-string-face '(:foreground :rosybrown)
  :font-lock-keyword-face '(:foreground :purple)
  :font-lock-builtin-face '(:foreground :orchid)
  :compiler-note-highlight '(:foreground :magenta)
  :compiler-warning-highlight '(:foreground :orange3)
  :compiler-error-highlight '(:foreground :red)
  :incremental-search-face '(:background :tweak_background)
  :incremental-search-other-matches-face '(:underline-p t))


(define-color-theme "torte" ()
  :foreground (color:make-rgb 0.8s0 0.8s0 0.8s0)
  :background (color:make-rgb 0.0s0 0.0s0 0.0s0)
  :region '(:foreground :color_highlighttext
            :background :color_highlight)
  :show-point-face `(:background ,(color:make-rgb 0.6275s0 0.1255s0 0.9412s0))
  :interactive-input-face '(:foreground :pink)
  :highlight '(:bold-p t)
  :non-focus-complete-face '(:background :tweak_background)
  :font-lock-function-name-face `(:foreground ,(color:make-rgb 0.0s0 1.0s0 1.0s0))
  :font-lock-comment-face `(:foreground ,(color:make-rgb 0.5s0 0.6275s0 1.0s0))
  :font-lock-type-face `(:foreground ,(color:make-rgb 0.5s0 1.0s0 0.5s0))
  :font-lock-variable-name-face `(:foreground ,(color:make-rgb 1.0s0 1.0s0 1.0s0))
  :font-lock-string-face `(:foreground ,(color:make-rgb 1.0s0 0.6275s0 0.6275s0))
  :font-lock-keyword-face `(:foreground ,(color:make-rgb 1.0s0 1.0s0 0.0s0))
  :font-lock-builtin-face `(:foreground ,(color:make-rgb 1.0s0 1.0s0 0.0s0))
  :compiler-note-highlight '(:foreground :magenta)
  :compiler-warning-highlight '(:foreground :orange)
  :compiler-error-highlight '(:foreground :red)
  :incremental-search-face '(:background :tweak_background)
  :incremental-search-other-matches-face '(:underline-p t))



;;; Show presence when loaded
(pushnew :editor-color-theme *features*)
