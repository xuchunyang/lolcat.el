;;; lolcat.el --- Rainbows and unicorns!  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/lolcat.el
;; Keywords:

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
  (require 'cl-lib))

(defun lolcat--color (freq i)
  (format "#%02x%02x%02x"
          (+ (* (sin (+ (* freq i) (/ (* 0 float-pi) 3))) 127) 128)
          (+ (* (sin (+ (* freq i) (/ (* 2 float-pi) 3))) 127) 128)
          (+ (* (sin (+ (* freq i) (/ (* 4 float-pi) 3))) 127) 128)))

(defun lolcat (s &optional seed freq spread)
  "Colorize S."
  (setq seed (or seed (random 256))
        freq (or freq 0.1)
        spread (or spread 3.0))
  (cl-loop for line in (split-string s "\n")
           for os from seed
           collect
           (cl-loop for char across line
                    for i from 0
                    for color = (lolcat--color freq (+ os (/ i spread)))
                    concat (propertize (string char) 'face `(:foreground ,color)))
           into lines
           finally return (mapconcat #'identity lines "\n")))

;;;###autoload
(defun lolcat-view-file (filename)
  (interactive "fLOLcat View File: ")
  (with-current-buffer (get-buffer-create "*Lolcat*")
    (display-buffer (current-buffer))
    (erase-buffer)
    (insert
     (with-temp-buffer
       (insert-file-contents filename)
       (lolcat (buffer-string))))))

;;;###autoload
(defun lolcat-message (format-string &rest args)
  (interactive (list "%s" (read-string "LOLcat message: ")))
  (message "%s" (lolcat (apply #'format format-string args))))

;;;###autoload
(defun eshell/lolcat (filename)
  "Display contents of FILENAME in color."
  (with-temp-buffer
    (insert-file-contents filename)
    (lolcat (buffer-string))))

(provide 'lolcat)
;;; lolcat.el ends here
