;;; system-const --- System constants -*- lexical-binding: t -*-

;; Author: Jonas Avrin
;; URL: https://www.github.com/jawabiscuit
;; Package-Requires: (`')

;; This file is not part of GNU Emacs
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:
;;
;; Information about the platform emacs is running on. See seagle0128's init-custom.el

;;; Code:

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Emacs is running on a WinTel system.")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Emacs is running on a GNU/Linux system.")

(defconst sys/macp
  (eq system-type 'darwin)
  "Emacs is running on a Mac system.")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Emacs is running under X on a Mac system.")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Emacs is running with Cocoa on a Mac system.")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Emacs is running under X on a GNU/Linux system.")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Emacs is running on a Cygwin system")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Using Emacs as a root user.")

(defconst sys/wslp
  (and sys/linuxp
       (integerp (with-temp-buffer
                   (insert-file-contents "/proc/sys/kernel/osrelease")
                   (insert-file-contents "/proc/version")
                   (string-match-p (rx (or "Microsoft" "WSL"))
                                   (buffer-string)))))
  "Emacs is running inside WSL.")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=25.3p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 3)))
  "Emacs is 25.3 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")

(defconst sys/bss-hostname-p
  (or (equal system-name "x3395e")
      (equal system-name "southbridge"))
  "This is a bss machine.")

(defconst sys/home-hostname-p
  (equal system-name "vrexton")
  "This is a machine at home.")

(provide 'system-const)
;;; system-const.el ends here
