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

;;;;;;;;;;;;;;;;;;;;;;;;; Help text ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar fontpreview-help-text
  "
n      Choose different font.
c      Copy font name.
p     Copy path to font file.
s     Set frame font.
q     Quit fontpreview"
  "Help text to be displayed in *fontpreview-info* buffer")

;;;;;;;;;;;;;;;;;;;;;;;;; Create temporary file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar fontpreview-preview-file
  (make-temp-file "fontpreview" nil ".jpg")
  "Temporary fontpreview file."
  )

(defvar fontpreview-current-font "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fontpreview-generate-preview (font &optional preview-text foreground-color background-color font-size)
  "Generate preview image from FONT.

If PREVIEW-TEXT, FOREGROUND-COLOR, BACKGROUND-COLOR or FONT-SIZE are non-nil the value(s) are use for the preview.
"
  (shell-command
   (concat
    "convert "
    "-size "  fontpreview-preview-size  " xc:"  "'" (or background-color fontpreview-background-color)  "'"
    " -gravity center "
    "-pointsize " (or font-size fontpreview-font-size)
    " -font '" font "'"
    " -fill '" (or foreground-color  fontpreview-foregound-color) "'" 
    " -annotate +0+0  '" (or preview-text fontpreview-preview-text) "'" 
    " -flatten "
    "'" fontpreview-preview-file "'"
    )
   "*fontpreview-output*"
   "*fontpreview-error*")
  ;; Create / refresh preview window
  (with-current-buffer (get-buffer-create "*fontpreview*")
      (fundamental-mode)
      (erase-buffer)
      (insert-file-literally fontpreview-preview-file)
      (image-mode)
      (fontpreview-mode)
      (switch-to-buffer "*fontpreview*")
      ;; Create / refresh info window
      (other-window 1)
      (with-current-buffer (get-buffer-create "*fontpreview-info*")
	(fundamental-mode)
	(erase-buffer)
	(insert
	 (replace-regexp-in-string "[()]" ""
	 (replace-regexp-in-string  "   " "\n" ( format  "%s" (assoc font fontpreview-font-assoc)))))
	(newline 2)
	(insert fontpreview-help-text)
	(switch-to-buffer "*fontpreview-info*")
	)
      ;; switch back to  preview window
      (other-window -1)
      ))

(defvar fontpreview-font-assoc ""
  "List of installed font with attributes and create an assoction list used for the *fontpreview-info* buffer.")

;; (defun endcons (a v)
;;    (if (null v) (cons a nil) (cons (car v) (endcons a (cdr v)))))

(defun fontpreview-get-font-list ()
  "Return list of installed font and create font info hash"
  (let ((fonts nil))
  (dolist   (element (split-string  (shell-command-to-string "convert -list font |  sed -ne '/Font/,/-1/p' ") "Font:" t " ") )
    (let ((elt (split-string   element "[\n]" t " ")))
      (setq fontpreview-font-assoc   (cons elt fontpreview-font-assoc))
      ))
))


(defvar fontpreview-query
  '((name . "Installed Fonts")
    (candidates .  (lambda ()
		     (split-string
		      (shell-command-to-string "convert -list font | awk -F: '/^[ ]*Font: /{print substr($NF,2)}'")
		      ))
		)
    (action  . (lambda (candidate)
		 (message "%s" candidate)
		 ))
    )
  "Helm query for fonts installed on the system, provided by convert.")

;; save window configuration
(defvar fontpreview-window-config
  nil
  "Temporary variable to save window configuration to restore it."
  )

(defvar fontpreview-called-from-preview
  nil
  "Temporary flag to mark if fontpreview is called from preview, to prevent saving the window configuration."
  )



(defun fontpreview (&optional preview-text foreground-color background-color font-size)
"Preview installed fonts.

If PREVIEW-TEXT, FOREGROUND-COLOR, BACKGROUND-COLOR or FONT-SIZE are non-nil the value(s) are use for the preview."
(interactive)
(fontpreview-get-font-list)
(setq fontpreview-current-font (helm
				:sources  fontpreview-query)
      )
(progn
  (if fontpreview-called-from-preview
      (setq fontpreview-called-from-preview nil)
    (setq fontpreview-window-config (current-window-configuration)))
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (split-window-below 20)
  (fontpreview-generate-preview fontpreview-current-font  preview-text foreground-color background-color font-size)  
  ))

(defun fontpreview-copy-font-name ()
  "Copy name of selected font to kill-ring"
  (interactive)
  (kill-new (replace-regexp-in-string "-" " " fontpreview-current-font))
  )

(defun fontpreview-copy-font-path ()
  "Copy path of selected font to kill-ring"
  (interactive)
  (kill-new
   (replace-regexp-in-string "[()]" "" (car (last
					     (split-string (car (last
								 (assoc  fontpreview-current-font fontpreview-font-assoc))))))
			     )))

(defun fontpreview-set-frame-font ()
  "Set current font (temporarily) as  frame font"
  (interactive)
  (set-frame-font (replace-regexp-in-string "-" " "   fontpreview-current-font) nil t)
  )

(defun fontpreview-next-font ()
  "Choose another font for preview"
  (interactive)
  (setq fontpreview-called-from-preview t)
  (kill-buffer)
  (fontpreview))

(defun fontpreview-quit ()
  "Clean up fontpreview"
   (interactive)
  (kill-buffer "*fontpreview*")
  (kill-buffer "*fontpreview-info*")
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
	    (define-key map (kbd "p") 'fontpreview-copy-font-name) ;; copy font name
	    (define-key map (kbd "s") 'fontpreview-set-frame-font) ;; copy font name
	    (define-key map (kbd "q") 'fontpreview-quit) ;; copy font name
            map)
  )

(provide 'fontpreview-mode)
