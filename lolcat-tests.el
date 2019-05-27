;;; lolcat-tests.el --- Tests for lolcat.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>

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

;;

;;; Code:

(require 'lolcat)

(ert-deftest lolcat ()
  (should (string= "hello world" (lolcat "hello world")))
  (should
   ;; `equal-including-properties' won't work here because it uses `eq' to
   ;; compare text properties.
   ;;
   ;; In `font-lock-mode', `font-lock-face' is alias of `face'
   (equal (get-text-property 0 'face (lolcat "a" 5))
          (get-text-property 0 'face (lolcat "a" 5)))))

(provide 'lolcat-tests)
;;; lolcat-tests.el ends here
