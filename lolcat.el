;;; lolcat.el --- Rainbows and unicorns!  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/lolcat.el
;; Package-Requires: ((emacs "24.3"))
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs Port of the Python version at https://github.com/tehmaze/lolcat

;;; Code:

(eval-when-compile
  (require 'cl-lib))                    ; Emacs 24.3

(defun lolcat--color (freq i)
  (format "#%02x%02x%02x"
          (+ (* (sin (+ (* freq i) (/ (* 0 float-pi) 3))) 127) 128)
          (+ (* (sin (+ (* freq i) (/ (* 2 float-pi) 3))) 127) 128)
          (+ (* (sin (+ (* freq i) (/ (* 4 float-pi) 3))) 127) 128)))

(defun lolcat (s &optional seed freq spread)
  "Colorize S.

If SEED is nil, a random seed will be used.
If FREQ is nil, 0.1 will be used.
If SPREAD is nil, 3.0 will be used."
  (setq seed (or seed (random 256))
        freq (or freq 0.1)
        spread (or spread 3.0))
  (cl-loop for line in (split-string s "\n")
           for os from seed
           collect
           (cl-loop for char across line
                    for i from 0
                    for color = (lolcat--color freq (+ os (/ i spread)))
                    concat (propertize (string char)
                                       (if font-lock-mode 'font-lock-face 'face)
                                       `(:foreground ,color)))
           into lines
           finally return (mapconcat #'identity lines "\n")))

;;;###autoload
(defun lolcat-this-buffer (&optional buffer)
  "Colorize BUFFER (defaults to the current buffer)."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (insert (lolcat (delete-and-extract-region (point-min) (point-max))))))

;;;###autoload
(defun lolcat-view-file (filename)
  "View FILENAME with color."
  (interactive "fFile: ")
  (with-current-buffer (get-buffer-create
                        (format "*Lolcat %s*" filename))
    (display-buffer (current-buffer))
    (erase-buffer)
    (insert
     (with-temp-buffer
       (insert-file-contents filename)
       (lolcat (buffer-string))))
    (view-mode)))

;;;###autoload
(defun lolcat-view-buffer (buffer)
  "View BUFFER with color."
  (interactive "bBuffer: ")
  (with-current-buffer (get-buffer-create
                        (format "*Lolcat %s*"
                                (buffer-name (get-buffer buffer))))
    (display-buffer (current-buffer))
    (erase-buffer)
    (insert
     (lolcat
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))))
    (view-mode)))

;;;###autoload
(defun lolcat-message (format-string &rest args)
  "Like `message' but with color.

FORMAT-STRING and ARGS are used in the same way as `message'."
  (interactive (list "%s" (read-string "Message: ")))
  (message "%s" (lolcat (apply #'format format-string args))))

;;;###autoload
(defun eshell/lolcat (filename)
  "Display contents of FILENAME with color."
  (with-temp-buffer
    (insert-file-contents filename)
    (lolcat (buffer-string))))

(provide 'lolcat)
;;; lolcat.el ends here
