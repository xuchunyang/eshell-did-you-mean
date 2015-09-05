;;; eshell-did-you-mean.el --- command not found ("did you mean…" feature) in Eshell  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/eshell-did-you-mean
;; Package-Requires: ((emacs "24.1"))
;; Keywords: eshell

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Setup:
;;   (add-to-list 'eshell-preoutput-filter-functions
;;                #'eshell-did-you-mean-output-filter)

;;; Code:

(require 'cl-lib)
(require 'eshell)
(require 'pcomplete)

(defun eshell-did-you-mean--edit-distance (s1 s2)
  "Return the edit (levenshtein) distance between strings S1 S2.

Adapted from `org-babel-edit-distance'."
  (let* ((l1 (length s1))
         (l2 (length s2))
         (dist (vconcat (mapcar (lambda (_) (make-vector (1+ l2) nil))
                                (number-sequence 1 (1+ l1)))))
         (in (lambda (i j) (aref (aref dist i) j))))
    (setf (aref (aref dist 0) 0) 0)
    (dolist (j (number-sequence 1 l2))
      (setf (aref (aref dist 0) j) j))
    (dolist (i (number-sequence 1 l1))
      (setf (aref (aref dist i) 0) i)
      (dolist (j (number-sequence 1 l2))
        (setf (aref (aref dist i) j)
              (min
               (1+ (funcall in (1- i) j))
               (1+ (funcall in i (1- j)))
               (+ (if (equal (aref s1 (1- i)) (aref s2 (1- j))) 0 1)
                  (funcall in (1- i) (1- j)))))))
    (funcall in l1 l2)))

(defun eshell-did-you-mean--edit-distances (string strings &optional threshold)
  "Calculate edit distance of STRING to each element of STRINGS.
Return a alist of result, the associated value is the edit distance.

If THRESHOLD is non-nil, use is as the maximum edit distance."
  (let ((res (cl-sort
              (mapcar
               (lambda (elt)
                 (cons elt (eshell-did-you-mean--edit-distance string elt)))
               strings)
              '< :key 'cdr)))
    (if threshold
        (cl-subseq res 0 (cl-position threshold res :key 'cdr :test '<))
      res)))

(defun eshell-did-you-mean--get-all-commands ()
  "Get candidates for eshell completion using `pcomplete'.

Adapted from `helm-esh-get-candidates'."
  (catch 'pcompleted
    (let* ((pcomplete-stub)
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (table (pcomplete-completions))
           (entry (or (try-completion "" (pcomplete-entries))
                      "")))
      (cl-loop ;; expand entry too to be able to compare it with file-cand.
       with exp-entry = (and (stringp entry)
                             (not (string= entry ""))
                             (file-name-as-directory
                              (expand-file-name entry default-directory)))
       for i in (all-completions pcomplete-stub table)
       ;; Transform the related names to abs names.
       for file-cand = (and exp-entry
                            (if (file-remote-p i) i
                              (expand-file-name
                               i (file-name-directory entry))))
       ;; Compare them to avoid dups.
       for file-entry-p = (and (stringp exp-entry)
                               (stringp file-cand)
                               ;; Fix :/tmp/foo/ $ cd foo
                               (not (file-directory-p file-cand))
                               (file-equal-p exp-entry file-cand))
       if (and file-cand (or (file-remote-p file-cand)
                             (file-exists-p file-cand))
               (not file-entry-p))
       collect file-cand into ls
       else
       ;; Avoid adding entry here.
       unless file-entry-p collect i into ls
       finally return
       (if (and exp-entry
                (file-directory-p exp-entry)
                ;; If the car of completion list is
                ;; an executable, probably we are in
                ;; command completion, so don't add a
                ;; possible file related entry here.
                (and ls (not (executable-find (car ls))))
                ;; Don't add entry if already in prompt.
                (not (file-equal-p exp-entry pcomplete-stub)))
           (append (list exp-entry)
                   ;; Entry should not be here now but double check.
                   (remove entry ls))
         ls)))))

(defvar eshell-did-you-mean--all-commands nil
  "Cache of the return of `eshell-did-you-mean--get-all-commands'.
Since computing it is a bit slow.")

;;;###autoload
(defun eshell-did-you-mean-output-filter (output)
  "\"Did you mean\" filter for eshell OUTPUT.
Should be added to `eshell-preoutput-filter-functions'."
  (if (and (not (= eshell-last-command-status 0))
           (string-prefix-p (format "%s: command not found"
                                    eshell-last-command-name)
                            output))
      (progn (unless eshell-did-you-mean--all-commands
               (setq eshell-did-you-mean--all-commands
                     (eshell-did-you-mean--get-all-commands)))
             (let ((guesses (eshell-did-you-mean--edit-distances
                             eshell-last-command-name
                             eshell-did-you-mean--all-commands
                             2)))
               (if guesses
                   (concat
                    output
                    "\n\n"
                    (if (= (length guesses) 1)
                        "Did you mean this?"
                      "Did you mean one of these?") "\n"
                      (mapconcat (lambda (elt) (format "\t%s\n" (car elt)))
                                 guesses ""))
                 output)))
    output))

(provide 'eshell-did-you-mean)
;;; eshell-did-you-mean.el ends here
