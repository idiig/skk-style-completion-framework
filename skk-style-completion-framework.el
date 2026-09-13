;;; skk-style-completion-framework.el --- Generic 3-stage compose-completion protocol  -*- lexical-binding: t; -*-

;;; Commentary:

;; Any input method that composes a raw code before producing final
;; output (pyim's pinyin, kana-to-kanji, wubi, or any other "type a
;; code, get a word" scheme) needs the same three kinds of completion
;; help while that code is still being typed:
;;
;;   `continuation' -- the code itself isn't a complete/valid spelling
;;   yet (e.g. bare pinyin initial "b") -- offer suffixes that would
;;   complete it into one, so composing can continue.
;;
;;   `abbrev'       -- the code is (or might be) an abbreviated
;;   shorthand for a longer reading (pyim's jianpin, wubi's own
;;   multi-key shortcuts, ...) -- offer the full reading(s) it could
;;   expand to.
;;
;;   `convert'      -- the code already stands as a complete, valid
;;   spelling on its own -- offer the actual converted output (e.g.
;;   hanzi words) that spelling would produce.
;;
;; A BACKEND (`skk-completion/backend') bundles the functions needed
;; to answer these for one particular input method; every other piece
;; of machinery in this file that deals with candidates, CAPFs, or
;; accepting a choice goes through a backend rather than talking to any
;; one input method directly, so the same machinery works unmodified
;; for any backend implementing the same protocol.  `test/pyim-skk-
;; style.el' is pyim's own implementation on top of this file;
;; `skk-completion/backend-test' below is a mock backend exercising
;; the protocol in isolation, with no real input method involved.
;;
;; Most consumers only ever talk to one backend at a time -- see the
;; "state interface" section below for the short, backend-argument-
;; free accessors (`skk-completion/composing-p' etc.) built on top of
;; `skk-completion/active-backend' for exactly that case.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'ert)

(cl-defstruct (skk-completion/backend
               (:constructor skk-completion/backend--make))
  "A pluggable backend for the 3-stage compose-completion protocol.
See the Commentary above for what `continuation'/`abbrev'/`convert'
mean.  Every slot holds a function:

  COMPOSING-P  () -> non-nil while there's a code being composed that
               the other functions can meaningfully be asked about.
  ENTERED      () -> the raw code typed so far, as a string.
  COMPLETE-P   (entered) -> non-nil once ENTERED already stands as a
               complete, valid spelling on its own.
  CONTINUATION (entered) -> list of suffix strings, or nil.
  ABBREV       (entered) -> list of full-reading strings, or nil.
  CONVERT      (entered) -> list of converted output candidates, or
               nil; expected to itself return nil unless COMPLETE-P
               would too, mirroring how `convert' is only ever offered
               once a spelling is already complete.
  ACCEPT       (stage entered candidate) -> commit CANDIDATE (chosen at
               STAGE, one of `continuation'/`abbrev'/`convert') for
               ENTERED into the underlying input method's own state."
  name composing-p entered complete-p continuation abbrev convert accept)

(defmacro skk-completion/define-backend (name &rest keys)
  "Define a `skk-completion/backend' named NAME (an unquoted symbol)
and bind it to `skk-completion/backend-NAME'.

KEYS is a plist with keys :composing-p :entered :complete-p
:continuation :abbrev :convert :accept, each a form evaluating to the
function described on `skk-completion/backend'."
  (declare (indent 1))
  `(defvar ,(intern (format "skk-completion/backend-%s" name))
     (skk-completion/backend--make
      :name ',name
      :composing-p ,(plist-get keys :composing-p)
      :entered ,(plist-get keys :entered)
      :complete-p ,(plist-get keys :complete-p)
      :continuation ,(plist-get keys :continuation)
      :abbrev ,(plist-get keys :abbrev)
      :convert ,(plist-get keys :convert)
      :accept ,(plist-get keys :accept))
     ,(format "The `%s' backend for the 3-stage compose-completion protocol." name)))

(defun skk-completion/stage-candidates (backend stage entered)
  "Return BACKEND's candidates for STAGE
(`continuation'/`abbrev'/`convert') given ENTERED."
  (funcall (pcase stage
             ('continuation (skk-completion/backend-continuation backend))
             ('abbrev (skk-completion/backend-abbrev backend))
             ('convert (skk-completion/backend-convert backend)))
           entered))

(defun skk-completion/primary-stage (backend entered)
  "Return the stage BACKEND is currently offering for ENTERED, or nil.

`convert' once ENTERED already stands as a complete spelling;
otherwise `continuation' if there's a real suffix to extend it with,
else `abbrev' if there's an abbreviation expansion, else nil (nothing
to offer).  A generic convenience for backends/UIs that want the full
3-way fallback; a backend's own call sites often want a narrower
binary projection of this instead (e.g. automatic ghost text that only
ever alternates between `continuation'/`convert', never falling to
`abbrev') and should check `skk-completion/backend-complete-p'
directly in that case."
  (cond
   ((funcall (skk-completion/backend-complete-p backend) entered) 'convert)
   ((skk-completion/stage-candidates backend 'continuation entered) 'continuation)
   ((skk-completion/stage-candidates backend 'abbrev entered) 'abbrev)
   (t nil)))

(defun skk-completion/accept (backend stage entered candidate)
  "Commit CANDIDATE, chosen at STAGE for ENTERED, into BACKEND."
  (funcall (skk-completion/backend-accept backend) stage entered candidate))

(defvar skk-completion/rotate-to-target nil
  "Let-bound to a candidate string while opening a full candidate list
from a ghost-text cursor that had already cycled a few times, so
`skk-completion/rotate-to' can start the list from that same
candidate instead of resetting to the top.  Mirrors
`completion-preview-complete's own `(append (nthcdr cur all) (take cur
all))' rotation for its native list-opening path, which
`skk-completion/make-capf' bypasses entirely.  nil (the default) for
any other caller -- e.g. a manual \"open the full list\" command, which
has no ghost-text cursor position to rotate from in the first place.")

(defun skk-completion/rotate-to (candidates target)
  "Rotate CANDIDATES so TARGET is first, if present; otherwise return
CANDIDATES unchanged."
  (let ((idx (and target (seq-position candidates target #'equal))))
    (if idx
        (append (nthcdr idx candidates) (take idx candidates))
      candidates)))

(defun skk-completion/capf-for (backend stage entered candidates)
  "Return a `completion-at-point-functions' value offering CANDIDATES
for STAGE/ENTERED under BACKEND, completing an empty range at point.
nil if CANDIDATES is empty."
  (when candidates
    (list (point) (point)
          (lambda (string pred action)
            (complete-with-action action candidates "" pred))
          :exclusive 'yes
          ;; Preserve whatever order BACKEND's own candidates came in
          ;; (e.g. a frequency order) -- plain `:display-sort-function'/
          ;; `:cycle-sort-function' survives to the plain *Completions*
          ;; buffer and completion-preview's own popped-up list, but
          ;; NOT to vertico/consult -- a backend's own consumer needs a
          ;; further trick for that (e.g. `pyim-skk-completion/force-unsorted-
          ;; consult' in `pyim-skk-style.el').
          :display-sort-function #'identity
          :cycle-sort-function #'identity
          :exit-function
          (lambda (candidate _status)
            (delete-region (- (point) (length candidate)) (point))
            (skk-completion/accept backend stage entered candidate)))))

(defun skk-completion/make-capf (backend stage)
  "Return a function suitable for `completion-at-point-functions' that
offers BACKEND's current STAGE candidates, rotated to
`skk-completion/rotate-to-target' if set, whenever BACKEND reports
it's composing."
  (lambda ()
    (when (funcall (skk-completion/backend-composing-p backend))
      (let* ((entered (funcall (skk-completion/backend-entered backend)))
             (candidates (skk-completion/rotate-to
                          (skk-completion/stage-candidates backend stage entered)
                          skk-completion/rotate-to-target)))
        (skk-completion/capf-for backend stage entered candidates)))))

;; ---------------------------------------------------------------------
;; State interface: short, backend-argument-free accessors for whatever
;; backend a consumer has designated as "the one currently in use"
;; (`skk-completion/active-backend').  A consumer that only ever
;; activates one backend at a time (e.g. `pyim-skk-style.el', which
;; only ever uses `skk-completion/backend-pyim') can set this once
;; and then read/act on composing state through these functions instead
;; of repeating the backend argument at every call site.  Each still
;; takes an optional explicit BACKEND for callers that do need to name
;; one (tests, or a consumer juggling more than one backend at once).
;; ---------------------------------------------------------------------

(defvar skk-completion/active-backend nil
  "The `skk-completion/backend' the state-interface functions below
operate on when not given one explicitly.")

(defun skk-completion/composing-p (&optional backend)
  "Non-nil while BACKEND (default `skk-completion/active-backend')
reports there's a code being composed."
  (funcall (skk-completion/backend-composing-p
            (or backend skk-completion/active-backend))))

(defun skk-completion/entered (&optional backend)
  "The raw code composed so far under BACKEND (default
`skk-completion/active-backend')."
  (funcall (skk-completion/backend-entered
            (or backend skk-completion/active-backend))))

(defun skk-completion/complete-p (&optional backend)
  "Non-nil once BACKEND's (default `skk-completion/active-backend')
current `skk-completion/entered' already stands as a complete,
valid spelling on its own."
  (let ((backend (or backend skk-completion/active-backend)))
    (funcall (skk-completion/backend-complete-p backend)
              (skk-completion/entered backend))))

(defun skk-completion/current-stage (&optional backend)
  "The stage (`skk-completion/primary-stage') BACKEND (default
`skk-completion/active-backend') is currently offering for its
current `skk-completion/entered', or nil."
  (let ((backend (or backend skk-completion/active-backend)))
    (skk-completion/primary-stage backend (skk-completion/entered backend))))

(defun skk-completion/current-candidates (&optional stage backend)
  "BACKEND's (default `skk-completion/active-backend') candidates for
STAGE (default `skk-completion/current-stage') given its current
`skk-completion/entered'."
  (let* ((backend (or backend skk-completion/active-backend))
         (stage (or stage (skk-completion/current-stage backend))))
    (and stage (skk-completion/stage-candidates
                backend stage (skk-completion/entered backend)))))

(defun skk-completion/commit (candidate &optional stage backend)
  "Commit CANDIDATE into BACKEND (default
`skk-completion/active-backend') at STAGE (default
`skk-completion/current-stage'), for its current
`skk-completion/entered'."
  (let* ((backend (or backend skk-completion/active-backend))
         (entered (skk-completion/entered backend))
         (stage (or stage (skk-completion/primary-stage backend entered))))
    (skk-completion/accept backend stage entered candidate)))

;; ---------------------------------------------------------------------
;; Test interface: a mock backend for the 3-stage protocol, plus ERT
;; tests exercising the generic engine (and the state interface above)
;; through it in isolation -- no real input method involved.  Defining
;; these only registers the tests (`ert-deftest' doesn't run anything
;; by itself) -- run them with `M-x ert-run-tests-interactively RET
;; skk-completion/ RET' after loading this file.
;; ---------------------------------------------------------------------

(defvar skk-completion/test-composing nil
  "Mock `composing-p' state for `skk-completion/backend-test'.")
(defvar skk-completion/test-entered ""
  "Mock `entered' state for `skk-completion/backend-test'.")
(defvar skk-completion/test-complete-spellings nil
  "List of ENTERED strings `skk-completion/backend-test' considers complete.")
(defvar skk-completion/test-full-spellings nil
  "Full candidate strings `skk-completion/backend-test' derives
`continuation' suffixes from -- any of these prefixed by ENTERED (and
longer than it) contributes ENTERED's continuation suffixes.")
(defvar skk-completion/test-abbrev-table nil
  "Alist of (ENTERED . ABBREV-CANDIDATES) for `skk-completion/backend-test'.")
(defvar skk-completion/test-convert-table nil
  "Alist of (ENTERED . CONVERT-CANDIDATES) for `skk-completion/backend-test'.")
(defvar skk-completion/test-accept-log nil
  "Each `skk-completion/backend-test' ACCEPT call, most recent first,
as a (STAGE ENTERED CANDIDATE) list -- lets tests assert on what was
committed without needing any real underlying input method to receive it.")

(skk-completion/define-backend test
  :composing-p (lambda () skk-completion/test-composing)
  :entered (lambda () skk-completion/test-entered)
  :complete-p (lambda (entered)
                (and (member entered skk-completion/test-complete-spellings) t))
  :continuation (lambda (entered)
                   (delq nil
                         (mapcar (lambda (full)
                                   (and (string-prefix-p entered full)
                                        (> (length full) (length entered))
                                        (substring full (length entered))))
                                 skk-completion/test-full-spellings)))
  :abbrev (lambda (entered) (cdr (assoc entered skk-completion/test-abbrev-table)))
  :convert (lambda (entered) (cdr (assoc entered skk-completion/test-convert-table)))
  :accept (lambda (stage entered candidate)
            (push (list stage entered candidate) skk-completion/test-accept-log)))

(ert-deftest skk-completion/primary-stage-picks-continuation-when-incomplete ()
  (let ((skk-completion/test-complete-spellings nil)
        (skk-completion/test-full-spellings '("ba" "bi" "bu")))
    (should (eq 'continuation
                (skk-completion/primary-stage skk-completion/backend-test "b")))))

(ert-deftest skk-completion/primary-stage-falls-back-to-abbrev ()
  (let ((skk-completion/test-complete-spellings nil)
        (skk-completion/test-full-spellings nil)
        (skk-completion/test-abbrev-table '(("wm" "wo'men"))))
    (should (eq 'abbrev
                (skk-completion/primary-stage skk-completion/backend-test "wm")))))

(ert-deftest skk-completion/primary-stage-picks-convert-when-complete ()
  (let ((skk-completion/test-complete-spellings '("de")))
    (should (eq 'convert
                (skk-completion/primary-stage skk-completion/backend-test "de")))))

(ert-deftest skk-completion/primary-stage-nil-when-nothing-to-offer ()
  (let ((skk-completion/test-complete-spellings nil)
        (skk-completion/test-full-spellings nil)
        (skk-completion/test-abbrev-table nil))
    (should (null (skk-completion/primary-stage skk-completion/backend-test "zz")))))

(ert-deftest skk-completion/rotate-to-moves-target-to-front ()
  (should (equal '("c" "d" "a" "b")
                 (skk-completion/rotate-to '("a" "b" "c" "d") "c"))))

(ert-deftest skk-completion/rotate-to-unchanged-without-target ()
  (should (equal '("a" "b" "c") (skk-completion/rotate-to '("a" "b" "c") nil))))

(ert-deftest skk-completion/accept-dispatches-to-backend ()
  (let ((skk-completion/test-accept-log nil))
    (skk-completion/accept skk-completion/backend-test 'continuation "b" "a")
    (should (equal '((continuation "b" "a")) skk-completion/test-accept-log))))

(ert-deftest skk-completion/capf-for-nil-without-candidates ()
  (should (null (skk-completion/capf-for skk-completion/backend-test 'convert "de" nil))))

(ert-deftest skk-completion/capf-for-offers-candidates-and-calls-accept ()
  (let ((skk-completion/test-accept-log nil))
    (with-temp-buffer
      (let* ((capf-data (skk-completion/capf-for
                          skk-completion/backend-test 'convert "de" '("的" "地" "得")))
             (collection (nth 2 capf-data))
             (exit-fn (plist-get (nthcdr 3 capf-data) :exit-function)))
        (should (equal (nth 0 capf-data) (nth 1 capf-data)))
        (should (equal (funcall collection "" nil t) '("的" "地" "得")))
        (insert "地")
        (funcall exit-fn "地" 'finished)
        (should (equal '((convert "de" "地")) skk-completion/test-accept-log))))))

(ert-deftest skk-completion/make-capf-respects-composing-p ()
  (let ((skk-completion/test-composing nil))
    (with-temp-buffer
      (should (null (funcall (skk-completion/make-capf skk-completion/backend-test 'convert)))))))

(ert-deftest skk-completion/make-capf-fetches-live-state ()
  (let ((skk-completion/test-composing t)
        (skk-completion/test-entered "de")
        (skk-completion/test-complete-spellings '("de"))
        (skk-completion/test-convert-table '(("de" "的" "地"))))
    (with-temp-buffer
      (should (funcall (skk-completion/make-capf skk-completion/backend-test 'convert))))))

(ert-deftest skk-completion/make-capf-rotates-to-target ()
  (let ((skk-completion/test-composing t)
        (skk-completion/test-entered "de")
        (skk-completion/test-complete-spellings '("de"))
        (skk-completion/test-convert-table '(("de" "的" "地" "得")))
        (skk-completion/rotate-to-target "得"))
    (with-temp-buffer
      (let* ((capf-data (funcall (skk-completion/make-capf skk-completion/backend-test 'convert)))
             (collection (nth 2 capf-data)))
        (should (equal (funcall collection "" nil t) '("得" "的" "地")))))))

(ert-deftest skk-completion/state-interface-reads-active-backend ()
  (let ((skk-completion/active-backend skk-completion/backend-test)
        (skk-completion/test-composing t)
        (skk-completion/test-entered "de")
        (skk-completion/test-complete-spellings '("de"))
        (skk-completion/test-convert-table '(("de" "的" "地"))))
    (should (skk-completion/composing-p))
    (should (equal (skk-completion/entered) "de"))
    (should (skk-completion/complete-p))
    (should (eq (skk-completion/current-stage) 'convert))
    (should (equal (skk-completion/current-candidates) '("的" "地")))))

(ert-deftest skk-completion/commit-uses-active-backend-and-current-stage ()
  (let ((skk-completion/active-backend skk-completion/backend-test)
        (skk-completion/test-composing t)
        (skk-completion/test-entered "b")
        (skk-completion/test-complete-spellings nil)
        (skk-completion/test-full-spellings '("ba" "bi"))
        (skk-completion/test-accept-log nil))
    (skk-completion/commit "a")
    (should (equal '((continuation "b" "a")) skk-completion/test-accept-log))))

(provide 'skk-style-completion-framework)

;;; skk-style-completion-framework.el ends here
