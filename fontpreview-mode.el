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



(defcustom  fontpreview-preview-size
  "960x365" 
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

(defvar fontpreview-current-font "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fontpreview-generate-preview (font)
  "Generate preview image."
  (shell-command
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
    )
   "*fontpreview-output*"
   "*fontpreview-error*")
  (with-current-buffer (get-buffer-create "*fontpreview*")
      (fundamental-mode)
      (erase-buffer)
      (insert-file-literally fontpreview-preview-file)
      (image-mode)
      (fontpreview-mode)
      (switch-to-buffer "*fontpreview*")
      ))

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

(defvar fontpreview-window-config
  nil
  "Temporary variable to save window configuration to restore it."
  )

(defun fontpreview ()
  "Preview installed fonts"
  (interactive)
  (setq fontpreview-current-font (helm
				   :sources '(fontpreview-font-list))
				  )
  (progn
    (setq fontpreview-window-config (current-window-configuration))
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (split-window-below 20)
    (fontpreview-generate-preview fontpreview-current-font)  
    ))

(defun fontpreview-copy-font-name ()
  "Copy name of selected font to kill-ring"
  (interactive)
  (kill-new fontpreview-current-font)
  )

(defun fontpreview-next-font ()
  "Choose another font for preview"
   (interactive)
   (kill-buffer)
   (fontpreview))

(defun fontpreview-quit ()
  "Clean up fontpreview"
   (interactive)
  (kill-buffer "*fontpreview*")
  (set-window-configuration fontpreview-window-config)
  (delete-file  fontpreview-preview-file)
  )

;;;;;;;;;;;;;;;;;;;;;;;;  Mode ;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode fontpreview-mode
  "Minor mode for acting on preview created by fontpreview"
  :lighter " fontpreview"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "n")  'fontpreview-next-font) ;; select next font
	    (define-key map (kbd "c") 'fontpreview-copy-font-name) ;; copy font name
	    (define-key map (kbd "q") 'fontpreview-quit) ;; copy font name
            map)
  )

(provide 'fontpreview-mode)
