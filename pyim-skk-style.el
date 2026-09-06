;; -*- lexical-binding: t; -*-
;;
;; Scratch test: make pyim behave like SKK's model.
;;   - lowercase letters and punctuation pass through untouched as
;;     plain ASCII -- pyim doesn't intercept at all.
;;   - an uppercase letter (shift) starts a pinyin composition, fed to
;;     pyim as its lowercase form (mirrors SKK using the shifted key's
;;     own romaji value to start a reading).
;;   - while composing, the inline preview shows the raw entered code
;;     prefixed with "▽" (SKK's own visual convention is cursor-color
;;     based, not literal characters -- this is our own choice of how
;;     to render the same "collecting a reading" idea).
;;   - the candidate page (posframe/popup/minibuffer/whatever
;;     `pyim-page-tooltip' is set to) stays hidden while composing.
;;   - pressing SPC the first time reveals the candidate page, and the
;;     inline preview switches to "▼" + the currently-selected
;;     candidate; press SPC again (or with nothing entered) to fall
;;     through to the normal `pyim-select-word' (confirm/cycle).
;;
;; How to try it:
;;   1. `M-x eval-buffer' in this file.
;;   2. Switch to pyim (`toggle-input-method' / your usual binding).
;;   3. Type a lowercase letter -- it should insert literally, no pyim
;;      UI at all.
;;   4. Type an uppercase letter to start a word, e.g. "Muqian" --
;;      first letter capital, rest lowercase -- you should see
;;      "▽muqian" inline and no popup.
;;   5. Press SPC -- popup appears, inline preview switches to "▼" +
;;      the first candidate.
;;   6. Press SPC again to confirm as usual.
;;
;; To fully undo without restarting Emacs, call
;; `idiig/pyim-skk-style-disable'; the one piece it can't cleanly undo
;; is the `pyim-preview-string' override (see the commented-out
;; original at the bottom -- eval that to restore it, or just restart
;; Emacs).

(require 'pyim)
(require 'completion-preview)
(require 'ert)

;; Generic 3-stage compose-completion protocol (backend struct, macro,
;; engine, and the state interface built on `idiig/completion-active-
;; backend') -- see `skk-style-completion-framework.el' in this same
;; directory for what it provides and why.  `idiig/completion-backend-
;; pyim' below (defined once its dependencies exist, alongside the old
;; capf definitions it replaces) is pyim's own implementation, wired up
;; everywhere pyim used to call its candidate/accept logic directly.
(let ((dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
  (add-to-list 'load-path dir))
(require 'skk-style-completion-framework)

(defvar-local idiig/pyim-page-revealed nil
  "Non-nil once SPC has revealed the candidate page for the current
composition.  Buffer-local; reset to nil whenever a composition ends
-- see `idiig/pyim-reset-reveal-flag' on `pyim-process-ui-hide-hook'.")

;; 1. Only letters are shift-gated: an uppercase ASCII letter starts a
;;    pyim composition, a lowercase one passes straight through as
;;    plain ASCII.  Everything else (digits, punctuation, nil) goes to
;;    ORIG-FN unchanged, so pyim's own punctuation handling (e.g.
;;    converting "," to "，") keeps working exactly as before -- only
;;    letters are affected by the shift distinction.
;;
;;    `pyim-outcome-trigger' (default "v") is excluded from the
;;    lowercase-passthrough case even though it's a lowercase letter:
;;    it's pyim's own built-in trigger key for magic features (toggle
;;    punctuation width after a punctuation char, delete/create-word
;;    after a numbered Chinese word, etc.), all gated by pyim's own
;;    context-sensitive predicates in `pyim-process--trigger-feature-run'
;;    -- not by whether shift was held.  Routing it to ORIG-FN as
;;    usual preserves that; when none of those predicates match, pyim
;;    just inserts it literally anyway, so plain English words
;;    containing "v" are unaffected.
(defun idiig/pyim-process-input-method-shift-gate (orig-fn key)
  (cond
   ((and (integerp key) (<= ?A key) (<= key ?Z))
    (funcall orig-fn (+ key (- ?a ?A))))
   ((and (integerp key) (<= ?a key) (<= key ?z)
         (not (eq key (aref pyim-outcome-trigger 0))))
    (char-to-string key))
   (t (funcall orig-fn key))))
(advice-add 'pyim-process-input-method :around
            #'idiig/pyim-process-input-method-shift-gate)

;; 1b. Confirm-then-redispatch: once there's a candidate ready to
;;     commit (▼ revealed, or ghost text showing one of ENTERED's own
;;     word candidates), a SECOND SHIFTED letter -- not just any key --
;;     confirms the currently-displayed candidate first, THEN processes
;;     the new key as a fresh keystroke, mirroring SKK's kakutei-
;;     before-anything-else behavior in ▼ mode, instead of folding new
;;     input into the same composition.  Only shift triggers this
;;     (see `idiig/pyim-syllable-boundary-insert', the sole caller of
;;     `idiig/pyim-confirm-then-redispatch' below) -- an ordinary
;;     lowercase letter or punctuation reaching
;;     `pyim-self-insert-command' while a candidate is ready keeps
;;     folding into ENTERED as usual instead, same as pyim's own
;;     unmodified behavior there.  This is exactly the pattern pyim's
;;     own `pyim-process--auto-select-word' already uses internally:
;;     push the key back for the next read cycle, then
;;     confirm/terminate -- `pyim-process--translating-p' becoming nil
;;     makes the enclosing `pyim-process-input-method' while loop exit
;;     on its next check, so the requeued key gets processed by a
;;     brand new top-level `pyim-input-method' call, hitting the
;;     shift-gate logic fresh.
(defun idiig/pyim-confirm-then-redispatch ()
  (pyim-add-unread-command-events last-command-event)
  ;; Two different "there's a candidate ready to commit" states share
  ;; this redispatch (see `idiig/pyim-conversion-ready-p', defined
  ;; further down once its dependencies exist): the ▼ page being
  ;; revealed, where pyim's own tracked word-position is exactly what's
  ;; shown and `pyim-select-word' confirms it correctly; and ghost text
  ;; showing one of ENTERED's own word candidates
  ;; (`idiig/pyim-pinyin-word-capf'), which TAB may have cycled to a
  ;; DIFFERENT candidate than pyim's own internal word-position ever
  ;; learns about (that cycling goes through `completion-preview-next-
  ;; candidate', entirely separate from pyim's own tracking) -- plain
  ;; `pyim-select-word' would confirm the wrong one there, so
  ;; `idiig/pyim-accept-shown-completion' is used instead, which reads
  ;; the actual shown candidate off the ghost text overlay itself.
  (if idiig/pyim-page-revealed
      (pyim-select-word)
    (idiig/pyim-accept-shown-completion)))

;; A second shift mid-composition (before reveal) marks a syllable
;; boundary, mirroring SKK's use of a second shifted letter mid-word
;; to mark the boundary before trailing okurigana -- here it's the
;; boundary between two pinyin syllables, e.g. typing "DiEr" should
;; enter "di'er", not the ambiguous "dier".  This only ever fires
;; mid-composition: `pyim-process-input-method' dispatches every key
;; after the first through `pyim-mode-map' via its own internal
;; `read-key-sequence' loop, never back through `pyim-input-method'
;; itself, so it doesn't overlap with the shift-gate advice above
;; (which only ever sees the first key of a fresh composition).
(defun idiig/pyim-syllable-boundary-insert ()
  (interactive)
  ;; A shifted letter while a candidate is already ready to commit
  ;; (`idiig/pyim-conversion-ready-p') is the ONLY key that
  ;; confirm-then-redispatches -- see the "1b." comment above.  Plain
  ;; lowercase letters/punctuation never reach this function at all
  ;; (they're bound to pyim's own unmodified self-insert-command
  ;; instead), so there's no separate gate needed for them here.
  (if (idiig/pyim-conversion-ready-p)
      (idiig/pyim-confirm-then-redispatch)
    (pyim-process-with-entered-buffer
      (insert "'" (char-to-string (downcase last-command-event))))
    (pyim-process-run)))
(let ((i ?A))
  (while (<= i ?Z)
    (define-key pyim-mode-map (char-to-string i)
                #'idiig/pyim-syllable-boundary-insert)
    (setq i (1+ i))))

;; 2. Inline preview: "▽" + raw entered code while composing, "▼" +
;;    currently-selected candidate once SPC has revealed the page.
(defun idiig/pyim-entered ()
  "Return ENTERED (point-before), with any leading syllable-boundary
apostrophe stripped.

After a multi-step selection consumes the first syllable of a
multi-syllable composition (e.g. \"jue'de\" -- select 角 for \"jue\"),
`pyim-process--select-word-in-next-step' leaves the *leftover*,
not-yet-translated ENTERED text with the apostrophe still attached
(literally \"'de\"), since that's exactly how pyim recorded the
syllable boundary in the imelem it copied the text out of.  There's no
ambiguity left for that boundary to resolve -- it's simply the first
character of what remains -- so keeping it is pure noise: shown raw it
produces a nonsensical leading-quote preview (\"▽'de\"), and fed as-is
into `idiig/pyim-pinyin-query-variants' it silently skips generating
any jianpin split variants, since that function already treats any
apostrophe already in ENTERED as reason enough not to guess further
splits."
  (string-remove-prefix "'" (pyim-process-get-entered 'point-before)))

(defun idiig/pyim-pinyin-query-variants (entered)
  "Return ENTERED plus simple apostrophe split variants."
  (if (string-match-p "'" entered)
      (list entered)
    (let ((variants (list entered))
          (i 1)
          (len (length entered)))
      (while (< i len)
        (push (concat (substring entered 0 i)
                      "'"
                      (substring entered i))
              variants)
        (setq i (1+ i)))
      (nreverse variants))))

(defun idiig/pyim-pinyin-spellings (entered)
  "Return full pinyin spellings for pyim candidates matching ENTERED."
  (let* ((scheme (pyim-scheme-current))
         (queries (idiig/pyim-pinyin-query-variants entered))
         (candidates
          (delete-dups
           (apply #'append
                  (mapcar (lambda (query)
                            (pyim-candidates-create
                             (pyim-imobjs-create query scheme) scheme))
                          queries)))))
    (delete-dups
     (delq nil
           (mapcar (lambda (word)
                     (pyim-cstring-to-pinyin word nil "'" nil t))
                   candidates)))))

(defun idiig/pyim-pinyin-continuation-suffixes (entered)
  "Return pinyin continuation suffixes for ENTERED.

Returns nil outright when ENTERED is already a complete, valid
spelling on its own (e.g. \"de\", 的/得/地) -- pyim has no notion of
\"this extension is more plausible than staying put\" for us to lean
on (verified: \"bi\"'s \"e\"/\"ng\"/\"an\"/... suffixes, matching
bie/bing/bian/..., and \"de\"'s \"i\"/\"ng\", matching dei/deng, are
produced by the exact same mechanism), so ghost text defaulting to
whichever extension happens to be first would just as often extend
\"de\" into the far less commonly wanted \"dei\" as it extends \"bi\"
into \"bie\".  The only reliable default is to never silently suggest
an extension once ENTERED already stands on its own; the composing
loop's TAB (see `idiig/pyim-composing-completion-preview-cycle') then
falls to opening the full spelling list directly (which already
includes \"bie\" etc. via `idiig/pyim-pinyin-overlay-capf') for anyone
who does want a longer spelling instead, and SPC falls through to
revealing candidates for ENTERED as typed.

Still returns real suffixes for genuinely incomplete input like a bare
initial (\"b\") that isn't a spelling of anything on its own -- only a
complete ENTERED suppresses this."
  (unless (member entered (idiig/pyim-pinyin-spellings entered))
    (delq nil
          (mapcar (lambda (spelling)
                    (when (and (string-prefix-p entered spelling)
                               (> (length spelling) (length entered)))
                      (substring spelling (length entered))))
                  (idiig/pyim-pinyin-spellings entered)))))

(defun idiig/pyim-pinyin-jianpin-candidates (entered)
  "Return ENTERED's jianpin (multi-syllable) full-spelling candidates.
Excludes ENTERED itself and any single-syllable spelling (those are
real continuations, handled separately by
`idiig/pyim-pinyin-continuation-suffixes').

Also returns nil outright when ENTERED is already a complete, valid
spelling on its own (e.g. \"bie\", a real single quanpin syllable) --
`idiig/pyim-pinyin-query-variants' tries every apostrophe split
position on any entered text with no apostrophe of its own, so without
this check, an already-complete continuation like \"bie\" gets treated
as if it were ALSO the jianpin abbreviation \"bi\"+\"e\" for some
unrelated two-character word (verified directly: entered \"bie\"
spuriously produced a \"[bi'e]\" hint).  A genuine jianpin abbreviation
like \"wm\" is never itself a valid pinyin syllable, so it never
appears in its own spellings list and this check never excludes it."
  (if (member entered (idiig/pyim-pinyin-spellings entered))
      nil
    (seq-filter (lambda (spelling)
                  (and (not (equal spelling entered))
                       (string-match-p "'" spelling)))
                (idiig/pyim-pinyin-spellings entered))))

;; TAB cycling for jianpin mirrors ghost-text cycling for real
;; continuations: each press advances which candidate the "[...]" hint
;; shows, the same way `completion-preview-next-candidate' advances
;; which suffix the ghost overlay shows.  Reset whenever ENTERED
;; changes (typing more, or a fresh composition) so a stale index from
;; a shorter/different ENTERED never lingers -- tracked by comparing
;; against the last ENTERED seen, not by any dedicated "new
;; composition" hook, since that comparison already covers both cases.
(defvar-local idiig/pyim-jianpin-hint-index 0)
(defvar-local idiig/pyim-jianpin-hint-last-entered nil)

(defun idiig/pyim-pinyin-jianpin-candidates-rotated (entered)
  "Return ENTERED's jianpin candidates, rotated to start at the
currently cycled-to index (`idiig/pyim-jianpin-hint-index'),
resetting that index first if ENTERED changed since the last call."
  (unless (equal entered idiig/pyim-jianpin-hint-last-entered)
    (setq idiig/pyim-jianpin-hint-last-entered entered
          idiig/pyim-jianpin-hint-index 0))
  (let ((candidates (idiig/pyim-pinyin-jianpin-candidates entered)))
    (if candidates
        (let ((idx (mod idiig/pyim-jianpin-hint-index (length candidates))))
          (append (nthcdr idx candidates) (take idx candidates)))
      nil)))

(defun idiig/pyim-pinyin-preview-hint (entered)
  "Return a bracketed preview of the currently cycled-to jianpin candidate.
Only shown when ENTERED has no literal quanpin continuation of its
own -- that case is already covered by real ghost text
(`idiig/pyim-pinyin-continuation-capf'), so the jianpin hint would
otherwise show alongside it for no reason."
  (if (idiig/pyim-pinyin-continuation-suffixes entered)
      ""
    (let ((top (car (idiig/pyim-pinyin-jianpin-candidates-rotated entered))))
      (if top (format " [%s]" top) ""))))

(cl-defmethod pyim-preview-string ((_scheme pyim-scheme-quanpin))
  (if idiig/pyim-page-revealed
      (let* ((candidates (pyim-process-get-candidates))
             (pos (min (pyim-process-word-position)
                       (1- (length candidates))))
             (preview (concat (pyim-process-get-select-result)
                              (nth pos candidates)))
             (rest (mapconcat
                    (lambda (py) (concat (nth 0 py) (nth 1 py)))
                    (nthcdr (length preview) (pyim-process-get-first-imobj))
                    "'")))
        (when (string< "" rest) (setq preview (concat preview rest)))
        (concat "▼" (pyim-process-magic-convert preview)))
    ;; Prepend `pyim-process-get-select-result' -- non-empty once a
    ;; multi-step selection has already confirmed an earlier syllable
    ;; in this same composition (e.g. "角" for "jue" in "jue'de") -- so
    ;; that confirmed hanzi stays visible ahead of the still-composing
    ;; "▽" marker instead of disappearing, giving "角▽de" rather than
    ;; a bare "▽de" that silently drops what's already been decided.
    (let ((entered (idiig/pyim-entered)))
      (concat (pyim-process-get-select-result)
              "▽" entered (idiig/pyim-pinyin-preview-hint entered)))))

;; 3. Suppress the candidate page until revealed; the preview refresh
;;    above is a separate hook function and keeps firing on every
;;    keystroke regardless.
(defun idiig/pyim-page-refresh-gate (orig-fn &rest args)
  (when idiig/pyim-page-revealed
    (apply orig-fn args)))
(advice-add 'pyim-page--refresh :around #'idiig/pyim-page-refresh-gate)

;; 4. Reset the flag whenever a composition actually ends (word
;;    selected, or aborted) -- `pyim-process-terminate' always calls
;;    `pyim-process-ui-hide' exactly once per composition, regardless
;;    of how it ended, so this is a more reliable reset point than
;;    trying to detect "start of next composition" from the input loop.
(defun idiig/pyim-reset-reveal-flag (&rest _)
  (setq idiig/pyim-page-revealed nil)
  ;; Defensive: the "entered changed" check in
  ;; `idiig/pyim-pinyin-jianpin-candidates-rotated' already resets this
  ;; for a fresh composition in practice, but clear it here too so
  ;; nothing can linger across compositions regardless.
  (setq idiig/pyim-jianpin-hint-index 0
        idiig/pyim-jianpin-hint-last-entered nil))
(add-hook 'pyim-process-ui-hide-hook #'idiig/pyim-reset-reveal-flag)

;; 5. SPC: first press reveals the candidate page instead of
;;    confirming/cycling; once revealed (or with nothing entered),
;;    fall through to the normal `pyim-select-word'.
;;
;;    Two different completion sources can be showing while composing
;;    (see `idiig/pyim-composing-completion-preview-refresh' /
;;    `idiig/pyim-pinyin-preview-hint' -- they're mutually exclusive,
;;    gated on whether ENTERED has a literal quanpin continuation of
;;    its own), and SPC accepts *whichever one is currently showing*
;;    before revealing, rather than revealing candidates for the raw
;;    ENTERED code:
;;    - Real ghost text (`completion-preview-active-mode' is on, e.g.
;;      "bi" showing "e" for "bie"): append the currently-shown suffix
;;      to ENTERED.
;;    - The jianpin "[...]" hint (e.g. "wm" showing "wo'men"): replace
;;      ENTERED with the hint's currently cycled-to spelling.
;;    Either way this stays in ▽ rather than revealing yet -- exactly
;;    the same two-step shape as confirm-then-redispatch elsewhere in
;;    this file (act on the current state first, treat the next press
;;    as fresh) -- so you see e.g. "▽bie" or "▽wo'men" first.  The
;;    *next* SPC press finds nothing left to accept (no ghost text for
;;    an already-complete "bie"; no hint since
;;    `idiig/pyim-pinyin-jianpin-candidates' excludes ENTERED itself)
;;    and falls through to the plain reveal below, now for the accepted
;;    spelling instead of the original code.
(defun idiig/pyim-conversion-ready-p ()
  "Non-nil when there's a hanzi candidate ready to commit right now:
either the ▼ page is revealed, or ghost text is showing one of
ENTERED's own word candidates (`idiig/pyim-pinyin-word-capf', once
ENTERED is already a complete spelling on its own) rather than a mere
pinyin continuation suggestion (`idiig/pyim-pinyin-continuation-capf',
for an ENTERED that isn't complete yet, e.g. \"b\" suggesting \"a\") --
that latter case has no real word to commit yet, so typing on there
should keep extending ENTERED as always, not confirm anything.

Used to decide whether typing another key should confirm-then-
redispatch (see `idiig/pyim-confirm-then-redispatch') instead of
folding straight into ENTERED: without this, typing on past a ghost-
text word candidate (e.g. \"de\" showing \"的\") silently extended
ENTERED into something else entirely (\"dei\") instead of committing
\"的\" and starting fresh, since the old gate only ever checked
`idiig/pyim-page-revealed', which this ghost-text state never sets."
  (or idiig/pyim-page-revealed
      (and (bound-and-true-p completion-preview-active-mode)
           (idiig/completion-complete-p))))

(defun idiig/pyim-accept-shown-completion ()
  "Accept whichever completion is currently shown (ghost text or the
jianpin hint) into ENTERED, without revealing.  Return non-nil if
either did something.

Ghost text means two different things depending on whether ENTERED is
already a complete spelling on its own (see
`idiig/pyim-composing-completion-preview-refresh'): a pinyin suffix to
append and keep composing with, or -- once ENTERED is already complete
-- one of ENTERED's own hanzi/word candidates, which there is nothing
left to \"append\" to; accepting it means selecting and confirming it
directly via `idiig/pyim-pinyin-word-capf's own machinery, the same way
choosing it from that CAPF's popped-up list would."
  (cond
   ((bound-and-true-p completion-preview-active-mode)
    (let* ((all (completion-preview--get 'completion-preview-suffixes))
           (cur (completion-preview--get 'completion-preview-index))
           (com (completion-preview--get 'completion-preview-common))
           (shown (concat com (nth cur all)))
           (complete (idiig/completion-complete-p)))
      (completion-preview-active-mode -1)
      (idiig/completion-commit shown (if complete 'convert 'continuation))
      t))
   (t
    (let* ((entered (idiig/completion-entered))
           (hinted (and (not (idiig/pyim-pinyin-continuation-suffixes entered))
                        (car (idiig/pyim-pinyin-jianpin-candidates-rotated entered)))))
      (when hinted
        (idiig/completion-commit hinted 'abbrev)
        t)))))
(defun idiig/pyim-space-reveal-or-select ()
  (interactive)
  (cond
   ((or idiig/pyim-page-revealed (pyim-process-without-entered-p))
    (pyim-select-word))
   ((idiig/pyim-accept-shown-completion))
   (t
    (setq idiig/pyim-page-revealed t)
    (pyim-process-ui-refresh))))
(define-key pyim-mode-map " " #'idiig/pyim-space-reveal-or-select)

;; SKK-style C-g: back out one level instead of always clearing the
;; whole composition.  Stock pyim binds C-g straight to `pyim-quit-clear'
;; (full cancel) regardless of state; here, if the candidate page is
;; currently revealed (▼), fall back to plain ▽ composing with ENTERED
;; left intact instead -- mirroring SKK's own C-g-in-conversion-mode
;; convention.  Only once there's nothing deeper left to fall back from
;; (▽, not revealed) does it fall through to the normal full clear.
;; (The OTHER level -- C-g inside the popped-up candidate list -- is
;; handled separately, by catching the `quit' it raises; see
;; `idiig/pyim-composing-completion-preview-open-list'.)
(defun idiig/pyim-quit-back-one-level ()
  (interactive)
  (cond
   ;; Level 3: ghost text (the "CP state") is showing a suggestion --
   ;; just hide it and reset the TAB cycle count, staying in plain ▽
   ;; with ENTERED untouched, rather than falling all the way to a full
   ;; clear.  (C-g inside the popped-up candidate LIST is a separate,
   ;; deeper level, handled by catching the `quit' it raises -- see
   ;; `idiig/pyim-composing-completion-preview-open-list'.)
   ((bound-and-true-p completion-preview-active-mode)
    (completion-preview-active-mode -1)
    (setq idiig/completion-preview-tab-cycle-count 0))
   ;; Level 2: the ▼ page is revealed -- un-reveal it, falling back to
   ;; plain ▽ with ENTERED intact.
   (idiig/pyim-page-revealed
    ;; `pyim-process-ui-hide' actually hides whatever tooltip toolkit is
    ;; showing the ▼ page (`pyim-page--hide', already on this hook) AND
    ;; resets `idiig/pyim-page-revealed' back to nil as a side effect
    ;; (`idiig/pyim-reset-reveal-flag', also already on this hook) --
    ;; both exactly what's needed here, without duplicating either.
    (pyim-process-ui-hide)
    (pyim-process-ui-refresh))
   ;; Level 1: plain ▽, nothing deeper to fall back from -- full clear.
   (t (pyim-quit-clear))))
(define-key pyim-mode-map "\C-g" #'idiig/pyim-quit-back-one-level)

;; Stock `pyim-delete-backward-char' calls `pyim-process-select-nothing'
;; (erase outcome + terminate) the moment ENTERED becomes empty,
;; kicking you out of composing entirely.  Here, deleting the last
;; character instead keeps the composition alive with an empty ENTERED
;; -- SKK itself stays in ▽ as long as you haven't explicitly cancelled
;; (C-g) or committed, so an empty reading is just "still composing,
;; nothing typed yet" rather than a reason to exit outright.  Calling
;; `pyim-process-run' here would immediately re-trigger the same
;; without-entered-p short-circuit (it starts with the identical check),
;; so this refreshes the UI directly instead, skipping straight past
;; that.
(defun idiig/pyim-delete-backward-char-keep-entered ()
  (interactive)
  (pyim-process-with-entered-buffer (delete-char -1))
  (if (pyim-process-without-entered-p)
      (pyim-process-ui-refresh)
    (pyim-process-run)))
(define-key pyim-mode-map [backspace] #'idiig/pyim-delete-backward-char-keep-entered)
(define-key pyim-mode-map "\177" #'idiig/pyim-delete-backward-char-keep-entered)

(defun idiig/pyim-skk-style-disable ()
  "Undo the shift-gate advice, SPC rebind, and page-hiding advice from
this test.  Does NOT restore the original `pyim-preview-string'
method -- see the commented-out original definition at the bottom of
this file."
  (interactive)
  (advice-remove 'pyim-process-input-method
                 #'idiig/pyim-process-input-method-shift-gate)
  (advice-remove 'pyim-page--refresh #'idiig/pyim-page-refresh-gate)
  (advice-remove 'completion-at-point
                 #'idiig/pyim-completion-at-point-overlay-advice)
  (remove-hook 'pyim-process-ui-hide-hook #'idiig/pyim-reset-reveal-flag)
  (define-key pyim-mode-map " " #'pyim-select-word)
  (define-key pyim-mode-map "\C-g" #'pyim-quit-clear)
  (define-key pyim-mode-map [backspace] #'pyim-delete-backward-char)
  (define-key pyim-mode-map "\177" #'pyim-delete-backward-char)
  (define-key pyim-mode-map (kbd "M-i") nil)
  (define-key pyim-mode-map (kbd "TAB") #'pyim-toggle-assistant-scheme)
  (define-key pyim-mode-map [?\t] #'pyim-toggle-assistant-scheme)
  (let ((i ?A))
    (while (<= i ?Z)
      (define-key pyim-mode-map (char-to-string i) #'pyim-self-insert-command)
      (setq i (1+ i))))
  (remove-hook 'pyim-activate-hook #'idiig/pyim-pinyin-capf-enable)
  (remove-hook 'pyim-deactivate-hook #'idiig/pyim-pinyin-capf-disable)
  (idiig/pyim-pinyin-capf-disable)
  (advice-remove 'consult--read #'idiig/pyim-consult-read-unsorted-advice)
  (remove-hook 'pyim-process-ui-refresh-hook
               #'idiig/pyim-composing-completion-preview-refresh)
  (remove-hook 'pyim-process-ui-hide-hook
               #'idiig/pyim-composing-completion-preview-hide)
  (when (bound-and-true-p completion-preview-active-mode)
    (completion-preview-active-mode -1))
  (message "pyim SKK-style test disabled (re-eval the original pyim-preview-string below, or restart Emacs, to fully restore the inline preview)"))

;; 6. While composing (▽, not yet revealed), offer full pinyin
;;    spellings for abbreviated/jianpin input as a CAPF, so e.g. typing
;;    "Br" can be completed to "bi'ru".  Reuses pyim's own candidate
;;    pipeline (`pyim-imobjs-create'/`pyim-candidates-create', same as
;;    `idiig/pyim-capf' in the real config) to get matching hanzi, then
;;    `pyim-cstring-to-pinyin' to turn each one back into its full
;;    spelling -- NOTE: the jianpin match itself comes from
;;    `ishortcode2word', which is built from your own selection
;;    history, not from the dictionary itself, so this only ever
;;    suggests words pyim has already learned you use.
;;
;;    Gate: must use `pyim-process--translating-p', NOT
;;    `(eq input-method-function 'pyim-input-method)' -- the latter is
;;    dynamically let-bound to nil by `pyim-process-input-method'
;;    itself while composing (pyim-process.el, around its main
;;    `(let* (... (input-method-function nil) ...)'), so it is *never*
;;    true at the same time as "entered is non-empty".  That combination
;;    made the earlier version of this CAPF a dead no-op.
;;
;;    Trigger: this CAPF is for manual full-spelling completion via
;;    `M-i'.  Automatic completion-preview below uses a separate suffix
;;    CAPF because pyim's entered code is preview overlay text, not real
;;    buffer text that CP can use as a prefix range.
(defun idiig/pyim-pinyin-capf ()
  (when (and (pyim-process--translating-p)
             (not idiig/pyim-page-revealed)
             (not (pyim-process-without-entered-p)))
    (let* ((entered (idiig/pyim-entered))
           (len (length entered))
           (spellings (idiig/pyim-pinyin-spellings entered)))
      (list (- (point) len) (point)
            (lambda (string pred action)
              (complete-with-action action spellings "" pred))
            :exclusive 'yes
            :display-sort-function #'identity
            :cycle-sort-function #'identity
            :exit-function
            (lambda (spelling _status)
              ;; The buffer text is already replaced with SPELLING at
              ;; this point (that's the default completion machinery),
              ;; but `pyim-preview--overlay' doesn't know that: it was
              ;; sized around the OLD entered text, and its end
              ;; boundary has no rear-advance, so the completion's
              ;; insertion (landing exactly at that boundary) ends up
              ;; outside the tracked region.  Left alone, the next
              ;; preview refresh would only erase the stale "▽" marker
              ;; and reinsert fresh preview text after the untouched
              ;; SPELLING, producing a visible duplicate like
              ;; "bi'ru▽bi'ru".  Re-sync the overlay to the actual
              ;; completed text first so the refresh below cleanly
              ;; replaces the whole thing instead.
              (when (and (overlayp pyim-preview--overlay)
                         (overlay-start pyim-preview--overlay))
                (move-overlay pyim-preview--overlay
                              (overlay-start pyim-preview--overlay)
                              (point)))
              ;; Sync pyim's own hidden entered-buffer to match and
              ;; recompute candidates from it, so the inline preview
              ;; and a later SPC-reveal reflect the completed spelling.
              (pyim-process-with-entered-buffer
                (erase-buffer)
                (insert spelling))
              (pyim-process-run))))))

;; pyim's own implementation of the 3-stage protocol above: COMPLETE-P
;; is exactly what decides which of continuation/convert is shown (see
;; `idiig/pyim-pinyin-continuation-suffixes' and
;; `idiig/pyim-pinyin-word-capf's old docstring, both folded into this
;; backend now); ABBREV mirrors the old `idiig/pyim-pinyin-overlay-capf'
;; exactly -- jianpin candidates first, then whatever other spellings
;; aren't already jianpin -- since that CAPF's whole job was already
;; "full alternate readings for what's typed, abbreviation-derived ones
;; first", just not expressed as a named protocol stage until now.
;;
;; CONVERT bakes its own COMPLETE-P check back in (rather than relying
;; solely on callers checking it first) so that nothing calling
;; `idiig/completion-stage-candidates' with `convert' on an incomplete
;; ENTERED gets pyim's current (possibly unrelated) candidate list by
;; accident.
(defun idiig/pyim-completion-accept (stage entered candidate)
  "pyim's ACCEPT function: commit CANDIDATE, chosen at STAGE for
ENTERED, into pyim's own composing state.

`continuation' and `abbrev' both fold CANDIDATE into pyim's hidden
entered buffer and keep composing -- the only difference is that a
`continuation' CANDIDATE is a bare suffix (append after ENTERED) while
an `abbrev' one is already a full spelling (replace ENTERED outright).
`convert' instead looks CANDIDATE up in pyim's own current candidate
list and plans+confirms it, precisely mirroring what choosing it from
`pyim-select-word's own page would do -- needed because ghost-text TAB-
cycling (`completion-preview-next-candidate') never touches pyim's own
internal word-position, so the word actually shown can differ from
whatever pyim's own tracking last set it to."
  (pcase stage
    ('continuation
     (pyim-process-with-entered-buffer
       (goto-char (point-max))
       (insert candidate))
     (pyim-process-run))
    ('abbrev
     (pyim-process-with-entered-buffer
       (erase-buffer)
       (insert candidate))
     (pyim-process-run))
    ('convert
     (let ((idx (seq-position (pyim-process-get-candidates) candidate #'equal)))
       (when idx
         (pyim-process-plan-to-select-word idx)
         (pyim-process-select-word (pyim-scheme-current)))))))

(idiig/define-completion-backend pyim
  :composing-p (lambda ()
                 (and (pyim-process--translating-p)
                      (not idiig/pyim-page-revealed)
                      (not (pyim-process-without-entered-p))))
  :entered #'idiig/pyim-entered
  :complete-p (lambda (entered)
                (and (member entered (idiig/pyim-pinyin-spellings entered)) t))
  :continuation #'idiig/pyim-pinyin-continuation-suffixes
  :abbrev (lambda (entered)
            (let ((jianpin (idiig/pyim-pinyin-jianpin-candidates-rotated entered)))
              (if jianpin
                  (append jianpin
                          (seq-remove (lambda (s) (member s jianpin))
                                      (idiig/pyim-pinyin-spellings entered)))
                (idiig/pyim-pinyin-spellings entered))))
  :convert (lambda (entered)
             (when (member entered (idiig/pyim-pinyin-spellings entered))
               (delete-dups (copy-sequence (pyim-process-get-candidates)))))
  :accept #'idiig/pyim-completion-accept)

;; This file only ever composes through pyim, so the state interface
;; (`idiig/completion-composing-p' etc.) can just default to this
;; backend everywhere below instead of naming it at every call site.
(setq idiig/completion-active-backend idiig/completion-backend-pyim)

;; The three automatic/escalated CAPFs are now just the generic engine
;; applied to `idiig/completion-backend-pyim' at each of its three
;; stages -- kept under their old names (via `defalias') since they're
;; referenced by symbol in several places below (`completion-at-point-
;; functions' lists, `idiig/pyim-completion-at-point-overlay-advice').
(defalias 'idiig/pyim-pinyin-continuation-capf
  (idiig/completion-make-capf idiig/completion-backend-pyim 'continuation)
  "CAPF for real completion-preview pinyin continuations while composing.
The pyim entered text is displayed by `pyim-preview--overlay', not
stored in the real buffer, so this CAPF completes an empty range at
point and offers only suffix strings.")
(defalias 'idiig/pyim-pinyin-overlay-capf
  (idiig/completion-make-capf idiig/completion-backend-pyim 'abbrev)
  "CAPF for choosing full pinyin spellings/jianpin expansions while pyim
is composing.  Unlike `idiig/pyim-pinyin-capf', this completes an empty
range at point because pyim's entered code lives in
`pyim-preview--overlay', not in the real buffer.")
(defalias 'idiig/pyim-pinyin-word-capf
  (idiig/completion-make-capf idiig/completion-backend-pyim 'convert)
  "CAPF offering ENTERED's own hanzi/word candidates, once ENTERED is
already a complete quanpin spelling on its own -- see
`idiig/pyim-composing-completion-preview-refresh' for when this takes
over from `idiig/pyim-pinyin-continuation-capf'.")

(defvar idiig/pyim-use-overlay-capf-for-completion nil
  "Non-nil means route `completion-at-point' to pyim's overlay CAPF.")

(defun idiig/pyim-completion-at-point-overlay-advice (orig-fn &rest args)
  "Use pyim's overlay CAPF while expanding a completion-preview menu."
  (if idiig/pyim-use-overlay-capf-for-completion
      (let ((completion-at-point-functions '(idiig/pyim-pinyin-overlay-capf)))
        (apply orig-fn args))
    (apply orig-fn args)))
(advice-add 'completion-at-point :around
            #'idiig/pyim-completion-at-point-overlay-advice)

(defvar idiig/pyim-force-unsorted-consult nil
  "Non-nil while a pyim pinyin CAPF list is open via `consult-completion-in-region'.

`pyim-candidates-create' already returns candidates in frequency
order, and `idiig/pyim-pinyin-spellings'/`idiig/pyim-pinyin-continuation-suffixes'
preserve that order, but it never survives to vertico: setting
`:display-sort-function'/`:cycle-sort-function' in the CAPF's plist
(the standard, documented mechanism) has no effect here, because
`consult--in-region' rebuilds `completion-extra-properties' from
scratch before handing off to `consult--read', keeping only
`:annotation-function'/`:affixation-function'/`:exit-function' and
silently dropping everything else -- verified by reading
`consult--in-region' directly.  The only lever `consult--read' itself
actually exposes for this is its own `:sort' keyword (\"SORT should be
set to nil if the candidates are already sorted\"), which
`consult--in-region' never passes, so it always defaults to `:sort t'.
Advising `consult--read' to force `:sort nil' while this flag is bound
is the only way found so far to make vertico display our candidates
in the order pyim's own frequency data puts them in -- without it,
\"wo'men\" (longer than most competing spellings) gets sorted to the
end of the list instead of appearing first.")

(defun idiig/pyim-consult-read-unsorted-advice (orig-fn table &rest options)
  (if idiig/pyim-force-unsorted-consult
      (apply orig-fn table (plist-put (copy-sequence options) :sort nil))
    (apply orig-fn table options)))
(advice-add 'consult--read :around #'idiig/pyim-consult-read-unsorted-advice)

(defmacro idiig/pyim-with-recursive-minibuffer-keys (&rest body)
  "Run BODY without pyim's composing keymaps overriding the minibuffer."
  (declare (indent 0) (debug t))
  `(let ((overriding-terminal-local-map nil)
         (overriding-local-map nil)
         (input-method-function nil)
         (current-input-method nil))
     ,@body))

;; `completion-at-point-functions' is local-variable-if-set, so a bare
;; `add-to-list' here would only ever affect whichever buffer happens
;; to be current when this file is eval'd, not every buffer where
;; pyim later gets activated.  Hook onto activate/deactivate instead,
;; same pattern as the wl-draft-capf/agent-shell CAPF registrations in
;; the real config.
(defun idiig/pyim-pinyin-capf-enable ()
  (add-hook 'completion-at-point-functions #'idiig/pyim-pinyin-capf nil t))
(defun idiig/pyim-pinyin-capf-disable ()
  (remove-hook 'completion-at-point-functions #'idiig/pyim-pinyin-capf t))
(add-hook 'pyim-activate-hook #'idiig/pyim-pinyin-capf-enable)
(add-hook 'pyim-deactivate-hook #'idiig/pyim-pinyin-capf-disable)

;; `M-i' and TAB (once it escalates, see below) both converge on this:
;; hide any inline ghost text and open the full candidate list via
;; `idiig/pyim-pinyin-overlay-capf' directly through `completion-at-point'
;; -- NEVER through `completion-preview-complete'.  `complete' would
;; instead reuse whatever suffix candidates the LAST automatic ghost-
;; text refresh happened to leave on the overlay
;; (`idiig/pyim-pinyin-continuation-capf', which deliberately only
;; deals in bare suffixes like "e"/"ei" for entered "l" -- correct for
;; ghost text appended right after the visible "l", but meaningless as
;; standalone list items), which is exactly why the popped-up list was
;; showing "e"/"ei" instead of "le"/"lei".  Going through
;; `idiig/pyim-pinyin-overlay-capf' (full spellings, always) sidesteps
;; that entirely and is the same CAPF already verified to work
;; correctly for both plain continuations and jianpin expansions like
;; "wm" -> "wo'men".
;;
;; `pyim-process-input-method' dynamically binds
;; `overriding-terminal-local-map' to `pyim-mode-map' for the ENTIRE
;; composing loop, and that variable outranks every other keymap --
;; including the minibuffer's own -- for the whole duration that
;; binding is on the stack.  Left alone, RET inside the picker hits
;; pyim-mode-map's own RET (`pyim-quit-no-clear') instead of
;; confirming the minibuffer, and C-g likewise hits
;; `pyim-quit-clear' instead of aborting it --
;; `idiig/pyim-with-recursive-minibuffer-keys' shadows that back to nil
;; for the duration of this call only.
;; C-g inside the popped-up list reaches the minibuffer's own abort
;; (thanks to `idiig/pyim-with-recursive-minibuffer-keys' unshadowing
;; `overriding-terminal-local-map'), but that abort itself signals
;; `quit' onward to US, not just to the minibuffer read -- that's the
;; standard, intentional `completing-read'/`consult--read' behavior
;; (the same way C-g during any minibuffer prompt aborts the whole
;; enclosing command).  Left uncaught, that `quit' would keep
;; unwinding straight out of `pyim-process-input-method's own dispatch
;; loop -- `condition-case-unless-debug's `(error ...)' handler there
;; does NOT catch it, since `quit' isn't a subtype of `error' --
;; abandoning pyim's translating state mid-composition instead of
;; landing anywhere sane.  Catching it here and falling back to
;; redisplaying ghost text (rather than leaving the composition in
;; that orphaned state, or letting it read as "C-g cancelled
;; everything") is what makes C-g here act like SKK's own "back out of
;; this level of selection, not out of the whole reading" convention.
(defun idiig/pyim-composing-completion-preview-open-list ()
  (when (bound-and-true-p completion-preview-active-mode)
    (completion-preview-active-mode -1))
  (condition-case nil
      (idiig/pyim-with-recursive-minibuffer-keys
        (let ((idiig/pyim-force-unsorted-consult t)
              (completion-at-point-functions '(idiig/pyim-pinyin-overlay-capf)))
          (unless (completion-at-point)
            (pyim-toggle-assistant-scheme))))
    (quit (idiig/pyim-composing-completion-preview-refresh))))

;; Same idea as `idiig/pyim-composing-completion-preview-open-list',
;; but for the one-stage-further-along case: ENTERED is already a
;; complete spelling on its own, so there's no more pinyin spelling to
;; browse (that's what `open-list' is for) -- escalating past the
;; ghost-text cycle limit here should instead browse more of ENTERED's
;; own hanzi/word candidates directly, via `idiig/pyim-pinyin-word-capf'.
;; Picking one from the popped-up list selects and confirms it right
;; there (see that CAPF's `:exit-function'), same as accepting it via
;; ghost text + SPC would.
(defun idiig/pyim-composing-completion-preview-open-word-list ()
  (when (bound-and-true-p completion-preview-active-mode)
    (completion-preview-active-mode -1))
  ;; See `idiig/pyim-composing-completion-preview-open-list' for why
  ;; `quit' needs catching here too.
  (condition-case nil
      (idiig/pyim-with-recursive-minibuffer-keys
        (let ((idiig/pyim-force-unsorted-consult t)
              (completion-at-point-functions '(idiig/pyim-pinyin-word-capf)))
          (completion-at-point)))
    (quit (idiig/pyim-composing-completion-preview-refresh))))
(defun idiig/pyim-composing-completion-preview-complete ()
  (interactive)
  (idiig/pyim-composing-completion-preview-open-list))
(define-key pyim-mode-map (kbd "M-i")
            #'idiig/pyim-composing-completion-preview-complete)

;; 7. Real `completion-preview-mode' for pinyin continuations while
;;    composing.  The raw entered code shown as "▽entered" is pyim
;;    preview overlay text rather than real buffer text, so the CP CAPF
;;    completes an empty range at the real point and offers suffixes
;;    only.  This keeps CP anchored after pyim's preview without letting
;;    ordinary buffer CAPFs contribute stale ghost text.
(defun idiig/pyim-composing-completion-preview-refresh (&rest _)
  (when (bound-and-true-p completion-preview-active-mode)
    (completion-preview-active-mode -1))
  ;; Two mutually-exclusive automatic ghost-text sources, chosen by
  ;; whether ENTERED already stands on its own as a complete spelling:
  ;; - Not yet complete (e.g. "l"): `idiig/pyim-pinyin-continuation-capf',
  ;;   a genuine literal-prefix continuation ("e"/"ei", read as "le"/"lei"
  ;;   once you account for the "l" already visible).  Jianpin expansions
  ;;   (e.g. "xiu'gai" for "xg") are NOT shown this way -- not a suffix of
  ;;   what's typed, so it would just render as a second, disconnected
  ;;   word glued onto whatever's already shown (including the
  ;;   "[xiu'gai]" hint from `idiig/pyim-pinyin-preview-hint', producing
  ;;   the observed "▽xg [xiu'gai]xiu'gai").  Jianpin guesses are covered
  ;;   by that hint instead; picking one is `M-i'/TAB-escalation's job.
  ;; - Already complete (e.g. "de"): `idiig/pyim-pinyin-word-capf' --
  ;;   there's no longer pinyin spelling left to suggest here, so ghost
  ;;   text switches to suggesting which of ENTERED's own hanzi/word
  ;;   candidates would be produced, one stage further along than pinyin
  ;;   completion.
  (when (idiig/completion-composing-p)
    (let ((capf (if (idiig/completion-complete-p)
                    #'idiig/pyim-pinyin-word-capf
                  #'idiig/pyim-pinyin-continuation-capf)))
      (let ((completion-at-point-functions (list capf)))
        (completion-preview--update)))))
(add-hook 'pyim-process-ui-refresh-hook
          #'idiig/pyim-composing-completion-preview-refresh 90)

(defun idiig/pyim-composing-completion-preview-hide (&rest _)
  (when (bound-and-true-p completion-preview-active-mode)
    (completion-preview-active-mode -1)))
(add-hook 'pyim-process-ui-hide-hook #'idiig/pyim-composing-completion-preview-hide)

;; TAB cycles through the currently showing CP candidates, mirroring
;; the README's global `completion-preview-active-mode-map' binding
;; (TAB/<tab> -> `idiig/completion-preview-tab', which cycles for the
;; first `idiig/completion-preview-tab-cycle-limit' consecutive presses
;; then escalates to `completion-preview-complete').  That global
;; binding can't reach us here though: `pyim-mode-map' is bound as
;; `overriding-terminal-local-map' for the whole composing loop, which
;; outranks every other keymap including minor-mode maps, so TAB has to
;; be rebound again right here or it would never even reach
;; `completion-preview-active-mode-map' while composing.
;;
;; We CANNOT just call `idiig/completion-preview-tab' directly here:
;; `pyim-process-input-method' tracks its own `this-command'/
;; `last-command' in a local `let*' that dynamically shadows the
;; globals for the whole composing loop (see pyim-process.el), so by
;; the time we're called, `last-command' is always
;; `idiig/pyim-composing-completion-preview-cycle' (whatever
;; `pyim-mode-map' actually dispatched), never
;; `idiig/completion-preview-tab' -- that function's own
;; consecutive-press check would never see two presses as consecutive
;; and would never escalate.  So this reimplements the same counting
;; against its OWN command symbol instead, while still sharing the
;; README's `idiig/completion-preview-tab-cycle-count'/`-limit'
;; variables so configuration and the press count stay unified across
;; both contexts.
;;
;; Escalating goes through `idiig/pyim-composing-completion-preview-
;; open-list', NOT `completion-preview-complete': that function would
;; rebuild its candidate list from whatever's already on the ghost-text
;; overlay, which is always suffix-only
;; (`idiig/pyim-pinyin-continuation-capf', e.g. "e"/"ei" for entered
;; "l") -- fine to show appended after the visible "l" as inline ghost
;; text, but meaningless as standalone items in a popped-up list.
;; Going through `open-list' instead always re-queries
;; `idiig/pyim-pinyin-overlay-capf' for full spellings ("le"/"lei"),
;; regardless of what the ghost text was showing.  When there's no
;; ghost text to cycle at all (e.g. a jianpin-only entry like "wm" that
;; can never get one, see `idiig/pyim-composing-completion-preview-
;; refresh'), TAB just opens the list immediately, same as `M-i'.
(defun idiig/pyim-composing-completion-preview-cycle ()
  (interactive)
  (if (bound-and-true-p completion-preview-active-mode)
      (progn
        ;; See the README's `idiig/completion-preview-tab' for why this
        ;; is needed: without it, `completion-preview--post-command'
        ;; (were it to run) would treat this wrapper as an unrecognized
        ;; command and hide the preview it just cycled.  Not actually
        ;; reachable mid-composition -- `post-command-hook' never runs
        ;; until `pyim-process-input-method's own loop returns -- but
        ;; kept here anyway so this mirrors the README version exactly
        ;; and stays correct if that ever changes.
        (completion-preview--inhibit-update)
        (let* ((entered (idiig/pyim-entered))
               (complete (idiig/completion-complete-p))
               (total (length (completion-preview--get 'completion-preview-suffixes))))
          (setq idiig/completion-preview-tab-cycle-count
                (if (eq last-command 'idiig/pyim-composing-completion-preview-cycle)
                    (1+ idiig/completion-preview-tab-cycle-count)
                  1))
          (if (or (<= total 1)
                  (> idiig/completion-preview-tab-cycle-count
                     idiig/completion-preview-tab-cycle-limit))
              ;; Capture exactly what ghost text is showing right now,
              ;; before the popped-up list resets the candidate order to
              ;; the top -- see `idiig/pyim-composing-completion-preview-
              ;; rotate-to'.  ENTERED already complete: ghost text shows
              ;; a WORD directly (`idiig/pyim-pinyin-word-capf'); still
              ;; incomplete: ghost text shows a SUFFIX, so the matching
              ;; full spelling is ENTERED+suffix.
              (let* ((com (completion-preview--get 'completion-preview-common))
                     (all (completion-preview--get 'completion-preview-suffixes))
                     (cur (completion-preview--get 'completion-preview-index))
                     (shown (concat com (nth cur all)))
                     (idiig/completion-rotate-to-target
                      (if complete shown (concat entered shown))))
                ;; ENTERED already complete: ghost text here is one of its
                ;; own word candidates, so escalating browses more of
                ;; THOSE, not alternate/longer pinyin spellings.
                (if complete
                    (idiig/pyim-composing-completion-preview-open-word-list)
                  (idiig/pyim-composing-completion-preview-open-list)))
            (completion-preview-next-candidate 1))))
    ;; No ghost text to cycle here -- a jianpin-only entry like "xg"
    ;; never gets one (see `idiig/pyim-composing-completion-preview-
    ;; refresh').  Cycle which candidate the "[...]" hint shows instead,
    ;; using the exact same press-count-then-escalate pattern as the
    ;; ghost-text branch above, just redrawing the hint via
    ;; `pyim-process-ui-refresh' instead of
    ;; `completion-preview-next-candidate'.
    (let* ((entered (idiig/pyim-entered))
           (total (length (idiig/pyim-pinyin-jianpin-candidates entered))))
      (setq idiig/completion-preview-tab-cycle-count
            (if (eq last-command 'idiig/pyim-composing-completion-preview-cycle)
                (1+ idiig/completion-preview-tab-cycle-count)
              1))
      (if (or (<= total 1)
              (> idiig/completion-preview-tab-cycle-count
                 idiig/completion-preview-tab-cycle-limit))
          (idiig/pyim-composing-completion-preview-open-list)
        (setq idiig/pyim-jianpin-hint-index (1+ idiig/pyim-jianpin-hint-index))
        (pyim-process-ui-refresh)))))
(define-key pyim-mode-map (kbd "TAB")
            #'idiig/pyim-composing-completion-preview-cycle)
(define-key pyim-mode-map [?\t]
            #'idiig/pyim-composing-completion-preview-cycle)

;; pyim conformance: exercises `idiig/completion-backend-pyim' directly,
;; assuming pyim is already active/configured (as in a real interactive
;; session) -- these don't spin up their own dcache/dictionary the way
;; the throwaway batch-test harnesses used while developing this file
;; did, so run them from a session where pyim already works normally.
(defun idiig/completion-test--start-pyim-composing (str)
  "Prime pyim's own composing state directly with STR, bypassing the
usual key-dispatch loop -- enough for `idiig/completion-backend-pyim's
functions to have something real to answer about."
  (pyim-process-ui-init)
  (pyim-process--set-translating-flag t)
  (setq idiig/pyim-page-revealed nil)
  (pyim-process-with-entered-buffer (insert str))
  (pyim-process-run))

(ert-deftest idiig/completion-backend-pyim-incomplete-entered-offers-continuation ()
  (idiig/completion-test--start-pyim-composing "b")
  (unwind-protect
      (progn
        (should-not (funcall (idiig/completion-backend-complete-p idiig/completion-backend-pyim) "b"))
        (should (idiig/completion-stage-candidates idiig/completion-backend-pyim 'continuation "b")))
    (pyim-process-terminate)))

(ert-deftest idiig/completion-backend-pyim-complete-entered-offers-convert ()
  (idiig/completion-test--start-pyim-composing "de")
  (unwind-protect
      (progn
        (should (funcall (idiig/completion-backend-complete-p idiig/completion-backend-pyim) "de"))
        (should (equal (idiig/completion-stage-candidates idiig/completion-backend-pyim 'convert "de")
                        (delete-dups (copy-sequence (pyim-process-get-candidates))))))
    (pyim-process-terminate)))

(ert-deftest idiig/completion-backend-pyim-convert-accept-confirms-word ()
  (idiig/completion-test--start-pyim-composing "de")
  (unwind-protect
      (let ((word (car (pyim-process-get-candidates))))
        (idiig/completion-accept idiig/completion-backend-pyim 'convert "de" word)
        (should-not (pyim-process--translating-p)))
    (when (pyim-process--translating-p) (pyim-process-terminate))))

;; To restore the original inline-preview-of-selected-candidate
;; behavior without restarting Emacs, eval this:
;;
;; (cl-defmethod pyim-preview-string ((_scheme pyim-scheme-quanpin))
;;   "获得 preview 字符串，适用于全拼输入法。"
;;   (let* ((candidates (pyim-process-get-candidates))
;;          (pos (min (pyim-process-word-position)
;;                    (1- (length candidates))))
;;          (preview (concat (pyim-process-get-select-result)
;;                           (nth pos candidates)))
;;          (rest (mapconcat
;;                 (lambda (py)
;;                   (concat (nth 0 py) (nth 1 py)))
;;                 (nthcdr (length preview)
;;                         (pyim-process-get-first-imobj))
;;                 "'")))
;;     (when (string< "" rest)
;;       (setq preview (concat preview rest)))
;;     (pyim-process-magic-convert preview)))

(provide 'pyim-skk-style)

;;; pyim-skk-style.el ends here
