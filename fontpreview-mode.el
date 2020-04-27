;;;  fontpreview-mode.el --- Emacs Major mode for  preview of installed fonts.

;;; Copyright (C) 2020 Sebastian Meisel <sebastian.meisel@gmail.com>

;;; Author: Sebastian Meisel <sebastian.meisel@gmail.com>
;;; Created:  April  24, 2020
;;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is fontpreview-mode.el a model to simply search and preview the
;; fonts installed on your system.
;; It a elisp reimplementation of fontpreview by Siddharth Dushantha

;; (defgroup fontpreview
;;   "Major mode for  preview of installed fonts."
;;   :prefix "fontpreview-"
;;   :group 'wp
;;   )

(defcustom  fontpreview-search-promt
  "❯"
  "Search promt symbol"
  :group 'fontpreview
  )					;

(defcustom  fontpreview-preview-size
  "532x365"
  "Preview image size."
  :group 'fontpreview
  )

(defcustom  fontpreview-font-size
  "38"
  "Preview font size."
  :group 'fontpreview
  )

(defcustom  fontpreview-background-color
  "#ffffff"
  "Preview font background color."
  :group 'fontpreview
  )

(defcustom  fontpreview-foregound-color
  "#000000"
  "Preview font foreground color."
  :group 'fontpreview
  )

(defcustom  fontpreview-preview-text
  "ABCDEFGHIJKLM\nNOPQRSTUVWXYZ\nabcdefghijklm\nnopqrstuvwxyz\n1234567890\n!@$(){\
}[]"
  "The text used for fontpreview"
  :group 'fontpreview
  )

;;;;;;;;;;;;;;;;;;;;;;;;; Create temporary file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar fontpreview-preview-file
  (make-temp-file "fontpreview" nil ".jpg")
  "Temporary fontpreview file."
  )

;;(defvar font "Gentium")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fontpreview-generate-preview (font)
  "Generate preview image."
  (start-process-shell-command
   "convert"
   "*fontpreview-output*"
   (concat
    "convert "
    "-size " fontpreview-preview-size " xc:"  "'" fontpreview-background-color  "'"
    " -gravity center "
    "-pointsize " fontpreview-font-size
    " -font '" font "'"
    " -fill '" fontpreview-foregound-color "'"
    " -annotate +0+0  '" font "\n\n" fontpreview-preview-text "'" 
    " -flatten "
    "'" fontpreview-preview-file "'"
    ))
  (let ((preview (car (last (split-string fontpreview-preview-file "/" )))))
    ( when (get-buffer  preview)   (kill-buffer preview))
    (find-file fontpreview-preview-file)
    (image-mode)
    (fontpreview-mode)
    ) )
   
(defvar fontpreview-font-list
  '((name . "Installed Fonts")
    (candidates .  (lambda ()
		     (split-string
		      (shell-command-to-string "convert -list font | awk -F: '/^[ ]*Font: /{print substr($NF,2)}'")
		      )))
    (action  . (lambda (candidate)
		 (message "%s" candidate)
		 ))
    )
  "List of fonts installed on the system, provided by convert.")

(defun fontpreview ()
  "Preview installed fonts"
  (interactive)
  (let ((font (helm
	       :sources '(fontpreview-font-list))
	      ))
    (progn
      (fontpreview-generate-preview font)  
      )))

;;;;;;;;;;;;;;;;;;;;;;;;  Mode ;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode fontpreview-mode
  "Minor mode for acting on preview created by fontpreview"
  :lighter " fontpreview"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c n") 'fontpreview)
            map)
  )



(provide 'fontpreview-mode)
