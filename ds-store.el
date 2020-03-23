;;; ds-store.el --- Reading the macOS Finder's .DS_Store files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/ds-store.el
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools
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

;; This package is a library for reading .DS_Store files
;; https://en.wikipedia.org/wiki/.DS_Store

;;; Code:

(require 'cl-lib)

(cl-defstruct (ds-store-ds
               (:constructor nil)
               (:constructor ds-store-ds-make (path id type data))
               (:copier nil))
  path id type data)

(cl-defstruct (ds-store-iloc
               (:constructor nil)
               (:constructor ds-store-iloc-make (x y))
               (:copier nil))
  x y)

(cl-defstruct (ds-store-fwind
               (:constructor nil)
               (:constructor ds-store-fwind-make (top l b r mode sideview-p))
               (:copier nil))
  top l b r mode sideview-p)

(defconst ds-store-HEAD 4)

(defun ds-store-bytes-to-string (bytes)
  "Convert UTF-16 BYTES to string."
  (decode-coding-string bytes 'utf-16))

(define-error 'ds-store-error "Ds-Store error" 'error)
(define-error 'ds-store-end-of-file "End of file while parsing Ds-Store"
  '(end-of-file ds-store-error))

(defun ds-store-read-byte ()
  "Read one byte."
  (if (eobp)
      (signal 'bencode-end-of-file nil)
    (prog1 (following-char)
      (forward-char 1))))

(defun ds-store-read-bytes (amt)
  "Read AMT bytes, return a unibyte string."
  (cl-assert (>= amt 0))
  (if (> amt (- (point-max) (point)))
      (signal 'ds-store-end-of-file nil)
    (let ((op (point)))
      (forward-char amt)
      (buffer-substring-no-properties op (point)))))

(defun ds-store-check-bytes (string)
  "Signal an error if bytes at point is not unibyte STRING."
  (cl-assert (not (multibyte-string-p string)))
  (pcase (ds-store-read-bytes (length string))
    ((pred (string= string)))
    (got (signal 'ds-store-error
                 (list "Unexpected bytes" string got)))))

(defun ds-store-bytes-to-unsigned (bytes)
  "Convert BYTES to an unsigned integer."
  (cl-loop for i from 0
           for n across (nreverse bytes)
           sum (* n (expt 256 i))))

(defun ds-store-read-int ()
  "Read a 4-byte (32-bit) big-endian integer."
  (ds-store-bytes-to-unsigned (ds-store-read-bytes 4)))

(defun ds-store-read-short ()
  "Read a 2-byte (16-bit) big-endian integer."
  (ds-store-bytes-to-unsigned (ds-store-read-bytes 2)))

(defun ds-store-read-utf-16 (len)
  "Read 2*LEN bytes in UTF-16 encoding, return a string."
  (ds-store-bytes-to-string (ds-store-read-bytes (* 2 len))))

(defun ds-store-read-four ()
  "Read a FourCharCode and return a symbol."
  (intern (ds-store-read-bytes 4)))

(defun ds-store-seek (pos)
  "Goto 0-based position POS."
  (let ((new-point (1+ pos)))
    (goto-char new-point)
    (cl-assert (= (point) new-point))))

(defun ds-store-read-file (file &optional verbose)
  "Read .DS_Store FILE and return a list of records.
Optional argument VERBOSE indicates that we should print debug
message."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (cl-flet ((addr-offset (o) (- o (logand o #x1f)))
              (addr-size (o) (expt 2 (logand o #x1f))))
      (let (bookkeeping-offset
            bookkeeping-size
            count
            block-addresses
            directory-count
            dirs
            free-lists
            header-block
            header-addr
            root-block-number
            more-root-data)
        (ds-store-check-bytes "\0\0\0\1")
        (ds-store-check-bytes "Bud1")

        (setq bookkeeping-offset (ds-store-read-int)
              bookkeeping-size   (ds-store-read-int))

        (when verbose
          (message "Bookkeeping at %s, size %s"
                   bookkeeping-offset
                   bookkeeping-size))
        
        (ds-store-seek (+ bookkeeping-offset ds-store-HEAD))
        (setq count (ds-store-read-int))
        (when verbose
          (message "File has %s blocks" count))
        (ignore (ds-store-read-bytes 4))
        (setq block-addresses
              (cl-loop repeat count
                       collect (ds-store-read-int)))
        (when verbose
          (message "Block addresses:")
          (cl-loop for i from 0
                   for a in block-addresses
                   do (message "%s: %s = %s"
                               i
                               (addr-offset a)
                               (addr-size a))))
        (ignore (ds-store-read-bytes (* 4 (- 256 count))))
        (setq directory-count (ds-store-read-int))
        (setq dirs (cl-loop repeat directory-count
                            collect (cons
                                     (ds-store-read-bytes (ds-store-read-byte))
                                     (ds-store-read-int))))
        (setq free-lists (cl-loop repeat 32
                                  for c = (ds-store-read-int)
                                  collect (cl-loop repeat c
                                                   collect (ds-store-read-int))))
        (when verbose
          (message "Free list:")
          (cl-loop for i below 32
                   for l in free-lists
                   do (message "%s: %s" (expt 2 i) l)))

        (setq header-block (assoc-default "DSDB" dirs))

        (setq header-addr (elt block-addresses header-block))

        (when verbose
          (message "Header block is %s at %s (size %s)"
                   header-block
                   (addr-offset header-addr)
                   (addr-size header-addr)))

        (ds-store-seek (+ (addr-offset header-addr) ds-store-HEAD))

        (setq root-block-number (ds-store-read-int)) ; root node
        (setq more-root-data
              (list
               (ds-store-read-int)      ; levels
               (ds-store-read-int)      ; records
               (ds-store-read-int)))    ; nodes
        (pcase (ds-store-read-int)
          (#x1000)
          (n (signal 'ds-store-error (list "not #x1000" n))))

        (when verbose
          (message "Root block is %d %s" root-block-number more-root-data))

        (cl-labels ((show-tree
                     (n accum)
                     (let ((addr (elt block-addresses n))
                           P count)
                       (ds-store-seek (+ (addr-offset addr) ds-store-HEAD))
                       (setq P (ds-store-read-int))
                       (setq count (ds-store-read-int))
                       (when verbose
                         (message "block %s %s" P count))
                       (pcase P
                         (0 (cl-loop repeat count
                                     do (setq accum (show-record accum))
                                     finally return accum))
                         (_
                          (let ((a3 (cl-loop repeat count
                                             for bn = (ds-store-read-int)
                                             for a2 = (save-excursion
                                                        (show-tree bn accum))
                                             do (setq accum (show-record a2)))))
                            (show-tree P a3))))))
                    (show-record
                     (accum)
                     (let* ((len (ds-store-read-int))
                            (name (ds-store-read-utf-16 len))
                            (id (ds-store-read-four))
                            (type (ds-store-read-four))
                            (data (pcase type
                                    ((or 'long 'shor) (ds-store-read-int))
                                    ('bool (> (ds-store-read-byte) 0))
                                    ('blob
                                     (let ((len (ds-store-read-int)))
                                       (pcase id
                                         ('fwi0
                                          (prog1 (ds-store-fwind-make
                                                  (ds-store-read-short)
                                                  (ds-store-read-short)
                                                  (ds-store-read-short)
                                                  (ds-store-read-short)
                                                  (ds-store-read-four)
                                                  (progn
                                                    (ds-store-read-byte)
                                                    (not (zerop (ds-store-read-byte)))))
                                            (ds-store-read-bytes (- len 14))))
                                         ('Iloc
                                          (prog1 (ds-store-iloc-make
                                                  (ds-store-read-int)
                                                  (ds-store-read-int))
                                            (ds-store-read-bytes (- len 8))))
                                         (_
                                          (ds-store-read-bytes len)))))
                                    ('type (ds-store-read-four))
                                    ('ustr (ds-store-read-utf-16 (ds-store-read-int))))))
                       (when verbose
                         (message "%S '%s' '%s':\n  %S" name id type data))
                       (cons (ds-store-ds-make name
                                               id
                                               type
                                               data)
                             accum))))
          (nreverse (show-tree root-block-number nil)))))))

(provide 'ds-store)
;;; ds-store.el ends here
