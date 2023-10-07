;;;  fontpreview-mode.el  ---  fontpreview is  a package for  previewing of installed fonts  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2005-2023 Sebastian Meisel
;;
;; Author: Sebastian Meisel <sebastian.meisel@gmail.com>
;; Version: 0.8
;; Homepage: http://github.com/SebastianMeisel/fontpreview-mode
;; Keywords: faces
;; Package-Requires: ((helm  "32") (emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)q
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is fontpreview-mode.el a model to simply search and preview the
;; fonts installed on your system.
;; It a elisp reimplementation of fontpreview by Siddharth Dushantha

;;; Code:

(defcustom  fontpreview-preview-size
  "960x365"
  "Preview image size."
  :group 'fontpreview
  :type 'string )

(defcustom  fontpreview-font-size
  "38"
  "Preview font size."
  :group 'fontpreview
  :type 'string )

(defcustom  fontpreview-background-color
  "#ffffff"
  "Preview font background color."
  :group 'fontpreview
  :type 'string  )

(defcustom  fontpreview-foreground-color
  "#000000"
  "Preview font foreground color."
  :group 'fontpreview
  :type 'string  )

(defcustom  fontpreview-preview-text
  "ABCDEFGHIJKLM\nNOPQRSTUVWXYZ\nabcdefghijklm\nnopqrstuvwxyz\n1234567890\n!@$(){\
}[]"
  "The text used for fontpreview."
  :group 'fontpreview
  :type 'string  )

(defvar  fontpreview-current-font-size
  nil
  "Preview font size."    )

(defvar  fontpreview-current-background-color
  nil
    "Preview font background color."    )

(defvar  fontpreview-current-foreground-color
  nil
    "Preview font foreground color."    )

(defvar  fontpreview-current-preview-text
  nil
    "The text used for fontpreview."    )

(defvar fontpreview-font-assoc ""
  "List of installed font with attributes and create an assoction list used for the *fontpreview-info* buffer.")

(defvar fontpreview-current-font "")


(defvar fontpreview-help-text
  "! Set frame font only works with TTF or OTF fonts !

n     Choose a different font.   t    Change preview text.
c n   Copy font name.   f    Change foreground-color.
c p   Copy path to font file.   b    Change background-color.
s     Set frame font.  +    Change font-size.
q     Quit fontpreview"
  "Help text to be displayed in *fontpreview-info* buffer.")

(defvar fontpreview-preview-file
  (make-temp-file "fontpreview" nil ".jpg")
  "Temporary fontpreview file.")


(defun fontpreview-generate-preview (font &optional preview-text foreground-color background-color font-size)
  "Generate preview image from FONT.

If PREVIEW-TEXT, FOREGROUND-COLOR, BACKGROUND-COLOR or FONT-SIZE are non-nil the value(s) are use for the preview."
  (shell-command
   (concat
    "convert "
    "-size "  fontpreview-preview-size  " xc:"  "'" (or background-color fontpreview-current-background-color)  "'"
    " -gravity center "
    "-pointsize " (or font-size fontpreview-current-font-size)
    " -font '" font "'"
    " -fill '" (or foreground-color  fontpreview-current-foreground-color) "'"
    " -annotate +0+0  '" (or preview-text fontpreview-current-preview-text) "'"
    " -flatten "
    "'" fontpreview-preview-file "'" )
   "*fontpreview-output*"
   "*fontpreview-error*")
  ;; Create / refresh preview window
  (with-current-buffer (get-buffer-create "*fontpreview*")
      (fundamental-mode)
      (erase-buffer)
      (insert-file-contents-literally fontpreview-preview-file)
      (image-mode)
      (fontpreview-mode)
      (when (boundp 'preview-text) (setq fontpreview-current-preview-text preview-text))
      (when (boundp 'foreground-color) (setq fontpreview-current-foreground-color foreground-color))
      (when (boundp 'background-color) (setq fontpreview-current-background-color background-color))
      (when (boundp 'font-size) (setq fontpreview-current-font-size font-size))
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
	(let ((start (point)))
	  (insert fontpreview-help-text)
	  (align-regexp start (point) "\\(\s\s+\\)" 1  3  t))
	(newline 2)
	(let ((cur-font-path
	  (replace-regexp-in-string "[()]" "" (car (last
						    (split-string (car (last
									(assoc  fontpreview-current-font fontpreview-font-assoc)))))))))
	(when (string-match "[ot]tf$" cur-font-path)
	  (insert "OTF-features:")
	  (newline 2)
	  (insert (shell-command-to-string (concat "otfinfo -f " cur-font-path)))
	  (goto-char (point-min))
	  ))
	(switch-to-buffer "*fontpreview-info*"))
      ;; switch back to  preview window
      (other-window -1) ))

(defun fontpreview-get-font-list ()
  "Return list of installed font and create font info hash."
  (dolist   (element (split-string  (shell-command-to-string "convert -list font |  sed -ne '/Font/,/-1/p' ") "Font:" t " ") )
    (let ((elt (split-string   element "[\n]" t " ")))
      (setq fontpreview-font-assoc   (cons elt fontpreview-font-assoc)) )))


(defvar fontpreview-query
  '((name . "Installed Fonts")
    (candidates .  (lambda ()
		     (split-string
		      (shell-command-to-string "convert -list font | awk -F: '/^[ ]*Font: /{print substr($NF,2)}'") ))	)
    (action  . (lambda (candidate)
		 (message "%s" candidate) )) )
  "Helm query for fonts installed on the system, provided by convert.")

;; save window configuration
(defvar fontpreview-window-config
  nil
  "Temporary variable to save window configuration to restore it." )

(defvar fontpreview-called-from-preview
  nil
  "Temporary flag to mark if fontpreview is called from preview, to prevent saving the window configuration."  )



(defun fontpreview (&optional preview-text foreground-color background-color font-size)
"Preview installed fonts.

If PREVIEW-TEXT, FOREGROUND-COLOR, BACKGROUND-COLOR or FONT-SIZE are non-nil the value(s) are use for the preview."
(interactive)
(fontpreview-get-font-list)
(setq fontpreview-current-font (helm
				:sources  fontpreview-query)  )
(progn
  (if fontpreview-called-from-preview
      (progn
	(setq fontpreview-called-from-preview nil)
	(kill-buffer "*fontpreview*"))
    (setq fontpreview-window-config (current-window-configuration)))
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (split-window-below 20)
  (setq fontpreview-current-preview-text (or preview-text fontpreview-preview-text))
  (setq fontpreview-current-foreground-color (or  foreground-color fontpreview-foreground-color))
  (setq fontpreview-current-background-color (or background-color fontpreview-background-color ))
  (setq fontpreview-current-font-size (or font-size fontpreview-font-size))
  (fontpreview-generate-preview fontpreview-current-font
				fontpreview-current-preview-text
				fontpreview-current-foreground-color
				fontpreview-current-background-color
				fontpreview-current-font-size) ))

(defun fontpreview-copy-font-name ()
  "Copy name of selected font to `kill-ring'."
  (interactive)
  (kill-new (replace-regexp-in-string "-" " " fontpreview-current-font)) )

(defun fontpreview-copy-font-path ()
  "Copy path of selected font to `kill-ring'."
  (interactive)
  (kill-new
   (replace-regexp-in-string "[()]" "" (car (last
					     (split-string (car (last
								 (assoc  fontpreview-current-font fontpreview-font-assoc)))))) )))

(defun fontpreview-set-frame-font ()
  "Set current font (temporarily) as  frame font."
  (interactive)
  (set-frame-font (replace-regexp-in-string "-" " "   fontpreview-current-font) nil t)  )

(defun fontpreview-next-font ()
  "Choose another font for preview."
  (interactive)
  (setq fontpreview-called-from-preview t)
  (fontpreview
   fontpreview-current-preview-text
   fontpreview-current-foreground-color
   fontpreview-current-background-color
   fontpreview-current-font-size) )
	       
(defun fontpreview-quit ()
  "Clean up fontpreview."
   (interactive)
  (kill-buffer "*fontpreview*")
  (kill-buffer "*fontpreview-info*")
  (set-window-configuration fontpreview-window-config)
  (delete-file  fontpreview-preview-file) )

(defun fontpreview-change-preview-text ()
  "Change the preview-text in fontpreview."
  (interactive)
  (setq fontpreview-current-preview-text (read-string "Preview-text: "))
  (fontpreview-generate-preview
   fontpreview-current-font
   fontpreview-current-preview-text
   fontpreview-current-foreground-color
   fontpreview-current-background-color
   fontpreview-current-font-size) )

(defun fontpreview-change-foreground-color ()
  "Change the foreground-color of preview in fontpreview."
  (interactive)
  (setq fontpreview-current-foreground-color (helm-colors))
  (fontpreview-generate-preview
   fontpreview-current-font
   fontpreview-current-preview-text
   fontpreview-current-foreground-color
   fontpreview-current-background-color
   fontpreview-current-font-size) )

(defun fontpreview-change-background-color ()
  "Change the background-color of preview in fontpreview."
  (interactive)
  (setq fontpreview-current-background-color (helm-colors))
  (fontpreview-generate-preview
   fontpreview-current-font
   fontpreview-current-preview-text
   fontpreview-current-foreground-color
   fontpreview-current-background-color
   fontpreview-current-font-size) )

(defun fontpreview-change-font-size ()
  "Change the font-size of preview in fontpreview."
  (interactive)
  (setq fontpreview-current-font-size (read-string "pt-size: "))
  (fontpreview-generate-preview
   fontpreview-current-font
   fontpreview-current-preview-text
   fontpreview-current-foreground-color
   fontpreview-current-background-color
   fontpreview-current-font-size) )


;;;###autoload
(define-minor-mode fontpreview-mode
  "Minor mode for acting on preview created by fontpreview."
  :lighter " fontpreview"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "n")  'fontpreview-next-font)
	    (define-key map (kbd "c n") 'fontpreview-copy-font-name)
	    (define-key map (kbd "c p") 'fontpreview-copy-font-path)
	    (define-key map (kbd "s") 'fontpreview-set-frame-font)
	    (define-key map (kbd "t") 'fontpreview-change-preview-text)
	    (define-key map (kbd "f") 'fontpreview-change-foreground-color)
	    (define-key map (kbd "b") 'fontpreview-change-background-color)
	    (define-key map (kbd "+") 'fontpreview-change-font-size)
	    (define-key map (kbd "q") 'fontpreview-quit)
            map) )

(provide 'fontpreview-mode)

;;; fontpreview-mode.el ends here

;; Local Variables:
;; jinx-languages: "en"
;; End:
