;;;; Genifer/user-interface.lisp
;;;;
;;;; Copyright (C) 2009 Genint
;;;; All Rights Reserved
;;;;
;;;; Written by YKY
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License v3 as
;;;; published by the Free Software Foundation and including the exceptions
;;;; at http://opencog.org/wiki/Licenses
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program; if not, write to:
;;;; Free Software Foundation, Inc.,
;;;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;;; ==========================================================
;;;; Let user ask a query.  Return if it's true or false.

(defvar incoming-fact)

(defvar GUI-off t)
;(if (or (equal (first ext:*args*) '"GUI")
;    (equal (second ext:*args*) '"GUI"))
;  (progn
;    (format t "connecting GUI...~%")
;    (setq GUI-socket (socket:socket-connect 8221))
;    (write-line "Testing" GUI-socket)
;    (setf GUI-off nil)))

(defvar studio-off t)
;(if (or (equal (first ext:*args*) '"studio")
;    (equal (second ext:*args*) '"studio"))
;  (progn
;    (format t "connecting Visual Studio...~%")
;    (setq studio-socket (socket:socket-connect 8223))
;    (write-line "Testing" studio-socket)
;    (setf studio-off nil)))

(defvar prolog-off t)
; (if (or (null prolog-off)
;         (equal (first  ext:*args*) '"prolog")
;         (equal (second ext:*args*) '"prolog"))
;   (progn
;     (format t "creating socket for SWI prolog...~%")
;     (setq prolog-server (socket:socket-server 8224 :interface "127.0.0.1" :backlog 5))
;     (setq prolog-socket (socket:socket-accept prolog-server))
;     (setf prolog-off nil)))

(defvar command-from 'keyboard)

(defvar command)

;;; **** read a single character either from keyboard or from GUI's socket
(defun get-command ()
  (loop
  (setf c (read-char-no-hang *standard-input*))
  (if (not (null c))
    (progn
      (setf command c)
      (setf command-from 'keyboard)
      (read-char-no-hang *standard-input*)
      (return)))
  (if (null GUI-off)       ; which means GUI is ON
    (progn
      (setf command (read-char-no-hang GUI-socket))
      (if (not (null command))
        (progn
          (setf command-from 'GUI)
          (return)))))
  (if (null prolog-off)    ; which means prolog is ON
    (progn
      (setf command (read-char-no-hang prolog-socket))
      (if (not (null command))
        (progn
          (setf command-from 'prolog)
          (return)))))
  (if (null studio-off)    ; which means studio is ON
    (progn
      (setf command (read-char-no-hang studio-socket))
      (if (not (null command))
        (progn
          (setf command-from 'studio)
          (return)))))))

;;; **** read a single line from either keyboard, GUI's socket, or Visual Studio socket
(defun get-input (prompt)
  (format t prompt)
  (finish-output *standard-output*)
  (if (equal command-from 'GUI)
    (progn
      (setf input (read GUI-socket))
      (read-char-no-hang GUI-socket))
    (if (equal command-from 'prolog)
      (progn
        (setf input (read prolog-socket))
        (read-char-no-hang prolog-socket))
      (if (equal command-from 'studio)
        (progn
          (setf input (read studio-socket))
          (read-char-no-hang studio-socket))
        (progn
          (setf input (read *standard-input*))
          (read-char-no-hang *standard-input*)))))
  input)

(defun get-input-string ()
  (if (equal command-from 'GUI)
    (read-line GUI-socket)
    (if (equal command-from 'prolog)
      (read-line prolog-socket)
      (if (equal command-from 'studio)
        (read-line studio-socket)
        (read-line *standard-input*)))))

;;; **** Main loop (REPL)
(defun main ()
  (init-memories)
  (defvar from-GUI)
  ;(trace resolve)
  ;(trace unify)
  ;(trace do-subst)
  ;(trace subst)
  (loop
    (format t "~%Commands:~%")
    (format t "    a = abduce ~%")
    (format t "    b = backward chain ~%")
    (format t "    d = set debug level, current = ~a ~%" *debug-level*)
    (format t "    i = induce ~%")
    (format t "    m = dump memory ~%")
    (format t "    t = system test ~%")
    (format t "    x = quit ~%?")
    (setf command-from 'keyboard)
    (setf command nil)
    ;(get-command)    ; this is a more sophisticated read-char but I've forgotten what good it is
    (setf command (read-char))
    ;(format t "command = [~a]~%" command)

    ;; Try to abduce the fact (see deduction.lisp)
    ;; algorithm: backward chain on the incoming fact
    (if (equal command #\a)
      (progn
        (setf incoming-fact (get-input "input:"))
        (setf timer (get-internal-run-time))
        (setf *abduce* t)
        (backward-chain incoming-fact)
        (format t "Command result:  ~a ~%" (print-solutions (solutions proof-tree)))
        (format t "Explanation:~%")
        (print-explanation *explanation*)
        (format t "~%Time elapsed: ~aus~%" (- (get-internal-run-time) timer))
        (setf command nil)))

    ;; Try to prove the fact (see deduction.lisp)
    ;; algorithm: backward chain on the incoming fact
    (if (equal command #\b)
      (progn
        (setf incoming-fact (get-input "query:"))
        (setf timer (get-internal-run-time))
        (setf *abduce* nil)
        (backward-chain incoming-fact)
        (format t "Command result:  ~a ~%" (print-solutions (solutions proof-tree)))
        (format t "Time elapsed: ~aus~%" (- (get-internal-run-time) timer))
        (setf command nil)))

    ;; Invoke inductive learning (see induction1.lisp, induction2.lisp, etc)
    (if (equal command #\i)
      (progn
        (setf timer (get-internal-run-time))
        (setf *abduce* nil)
        (induce)
        ;(format t "Command result:  ~a ~%" (print-solutions (solutions proof-tree)))
        (format t "Time elapsed: ~aus~%" (- (get-internal-run-time) timer))
        (setf command nil)))

    ;; Display contents of Working Memory
    (if (equal command #\m)
      (progn
        (format t "~% ************ dump of working memory... ~%")
        (dump-memory)
        (setf command nil)))

    ;; System test
    (if (equal command #\t)
      (progn
        (format t "~% ************ system test ************* ~%")
        (system-test)
        (setf command nil)))

    ;; Set debug level
    (if (equal command #\d)
      (progn
        (setf *debug-level*
              (get-input "new debug-level:"))
        (format t "~% Debug level set to ~a ~%" *debug-level*)
        (setf command nil)))

    (if (equal command #\x)
      (progn
        (format t "~%Exiting... ~%")
        (return)))
  ))
