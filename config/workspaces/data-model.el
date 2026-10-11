;;; data-model.el --- Workspaces pure data layer -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct workspace-buffer
  "Per-window-leaf reincarnation record.
Stored under the leaf's `parameters' map by `workspace--capture-frameset',
consumed by `workspace--deserialize-buffer' on restore.

Slots:
- `bookmark'        :: result of `bookmark-make-record', or nil for
                       non-bookmarkable buffers.
- `filename'        :: file path for file-backed buffers, else nil.
- `name'            :: `buffer-name' at save time.
- `narrowed-p'      :: t if the buffer was narrowed at save time.
- `indirect-p'      :: t if the buffer was an indirect buffer.
- `local-variables' :: reserved (always nil in v2); forward-compat hook.
- `etc'             :: alist; forward-compat slot."
  (bookmark nil)
  (filename nil)
  (name nil)
  (narrowed-p nil)
  (indirect-p nil)
  (local-variables nil)
  (etc nil))

(define-error 'workspace-restore-declined
  "Bookmark handler declined to restore"
  'user-error)

(defun workspace--layout-make (saved-state &optional timestamp)
  "Build a layout plist wrapping SAVED-STATE.
TIMESTAMP defaults to current time as an integer.  The freshly-built
layout has =:working-state= nil (no autosave drift yet) and =:etc= nil
(forward-compat alist)."
  (list :timestamp (or timestamp (time-convert nil 'integer))
        :saved-state saved-state
        :working-state nil
        :etc nil))

(defun workspace--layout-saved-state (layout)
  "Return the :saved-state window-state stored in LAYOUT.
This is the explicit-save slot; written only by =workspace-save= and
its variants.  Never written by autosave paths."
  (plist-get layout :saved-state))

(defun workspace--layout-working-state (layout)
  "Return the :working-state window-state stored in LAYOUT.
This is the autosave slot; may be nil.  Written by tab-switch advice,
the idle-save timer, and =workspace--kill-emacs-flush=.  Cleared to nil
by every explicit =workspace-save=."
  (plist-get layout :working-state))

(defun workspace--layout-effective-state (layout)
  "Return the window-state to apply on restore for LAYOUT.
Precedence: =:working-state= when non-nil, else =:saved-state=.
Returns nil when both slots are nil (no state ever captured).

This is the single dispatch point for the working-over-saved
precedence rule (register/invariant/restore-precedence-working-over-
saved); direct reads of either slot from the restore path are a
regression risk."
  (or (workspace--layout-working-state layout)
      (workspace--layout-saved-state layout)))

(defun workspace--layout-timestamp (layout)
  "Return the integer timestamp stored in LAYOUT."
  (plist-get layout :timestamp))

(cl-defun workspace--layout-with-metadata
    (layout &key (origin nil origin-p)
            (description nil description-p)
            (source nil source-p))
  "Return a new layout like LAYOUT with the given metadata set in its :etc.
Each keyword that is passed — ORIGIN (`user' or `agent'), DESCRIPTION,
SOURCE — sets its entry of the :etc alist; every other entry is kept.
Non-destructive: LAYOUT, its :etc alist included, is not modified."
  (let ((etc (copy-alist (plist-get layout :etc)))
        (copy (copy-sequence layout)))
    (when origin-p
      (setf (alist-get 'origin etc) origin))
    (when description-p
      (setf (alist-get 'description etc) description))
    (when source-p
      (setf (alist-get 'source etc) source))
    (plist-put copy :etc etc)))

(defun workspace--layout-origin (layout)
  "Return LAYOUT's origin, `user' or `agent'.
A layout without an origin entry — including one whose :etc is nil —
is the user's."
  (or (alist-get 'origin (plist-get layout :etc)) 'user))

(defun workspace--layout-description (layout)
  "Return LAYOUT's description string, or nil when it has none."
  (alist-get 'description (plist-get layout :etc)))

(defun workspace--layout-source (layout)
  "Return LAYOUT's source string, or nil when it has none."
  (alist-get 'source (plist-get layout :etc)))

(defconst workspace--reserved-group-names '("home")
  "Layout-group names that cannot be deleted by user commands.")

(defun workspace--group-name-reserved-p (name)
  "Return non-nil if NAME is a reserved layout-group name."
  (and (stringp name)
       (member name workspace--reserved-group-names)
       t))

(defun workspace--group-make (name layout)
  "Build a layout-group plist named NAME containing LAYOUT as its sole entry."
  (list :name name
        :layouts (list layout)))

(defun workspace--group-name (group)
  "Return the name of GROUP."
  (plist-get group :name))

(defun workspace--group-layouts (group)
  "Return the list of layouts in GROUP."
  (plist-get group :layouts))

(defun workspace--group-recent-layout (group)
  "Return the most recent layout in GROUP.
MVP keeps a single layout per group; this returns the head of the
:layouts list, which is also the most recently saved one."
  (car (workspace--group-layouts group)))

(defun workspace--group-add-layout (group layout)
  "Return a new group like GROUP with LAYOUT prepended to its :layouts.
Non-destructive: GROUP is not modified.  In MVP this replaces rather
than appends; the deferred history feature can change this without
disturbing callers."
  (list :name (workspace--group-name group)
        :layouts (list layout)))

(defun workspace--make (name home)
  "Build an empty workspace plist named NAME anchored at HOME.

NAME is the workspace name (and the registry key under which it is
stored).  HOME is the workspace's filesystem anchor — an absolute
path string.  By convention, =basename(HOME)= equals NAME; the data
layer does not enforce this here, but callers in the command layer
do (the canonical construction site derives NAME from HOME).

HOME is canonicalised on entry to the form pinned by
=register/shape/workspace-plist-v3=: absolute (=expand-file-name=
applied) and trailing-slash-present (=file-name-as-directory=
applied).  Symlinks are NOT resolved.  Callers that already pass a
canonical HOME observe a no-op; callers that pass the un-slashed or
tilde-prefixed form get normalised silently.  This makes the
constructor the single canonicalisation choke-point so that
downstream =equal= comparison on =:home= across all three producers
(=workspace--new-default-path=, =workspace--new-anchor-existing=,
=workspace-re-anchor=) agrees.

Both arguments are required: floating workspaces (without a home
directory) are structurally unrepresentable.  HOME is type-checked
so that a programmatic caller passing nil (the only way to land a
no-=:home= entry, since the deserialiser skips such entries) fails
loudly at the constructor rather than producing a workspace that
violates =register/invariant/home-required-no-floating-workspaces=."
  (cl-check-type home string)
  (list :name name
        :home (file-name-as-directory (expand-file-name home))
        :recent-layout-group nil
        :buffer-files nil
        :layout-groups nil))

(defun workspace--name (ws)
  "Return the name of workspace WS."
  (plist-get ws :name))

(defun workspace--home (ws)
  "Return the absolute home directory path of workspace WS.

Returns nil if WS has no =:home= slot (defensive: handles
hand-constructed or stale plists from a corrupted persistence file)."
  (plist-get ws :home))

(defun workspace--set-home (ws home)
  "Return a new workspace like WS with its =:home= slot set to HOME.
Non-destructive: WS is not modified.

HOME is canonicalised identically to =workspace--make=: absolute
(=expand-file-name= applied) and trailing-slash-present
(=file-name-as-directory= applied).  This keeps the invariant pinned
by =register/shape/workspace-plist-v3= holding across the post-
construction re-anchor path (=workspace-re-anchor= is the third known
producer of =:home= values, alongside =workspace--new-default-path=
and =workspace--new-anchor-existing= — see
=arch-cycle-20260526-191802-02=)."
  (let ((copy (copy-sequence ws)))
    (plist-put copy :home (file-name-as-directory (expand-file-name home)))))

(defun workspace--set-name (ws name)
  "Return a new workspace like WS with its =:name= slot set to NAME.
Non-destructive: WS is not modified.  Used by =workspace-re-anchor=
to keep =:name= in lockstep with =:home= when the new home's basename
differs from the workspace's current name (per
=register/invariant/registry-name-equals-basename=)."
  (let ((copy (copy-sequence ws)))
    (plist-put copy :name name)))

(defun workspace--layout-groups (ws)
  "Return the layout-groups of workspace WS."
  (plist-get ws :layout-groups))

(defun workspace--recent-group (ws)
  "Return the name of WS's most recently activated layout-group, or nil."
  (plist-get ws :recent-layout-group))

(defun workspace--set-recent-group (ws name)
  "Return a new workspace like WS with its recent-layout-group set to NAME.
Non-destructive: WS is not modified."
  (let ((copy (copy-sequence ws)))
    (plist-put copy :recent-layout-group name)))

(defun workspace--find-group (ws name)
  "Return the layout-group named NAME in WS, or nil if none."
  (seq-find (lambda (g) (equal (workspace--group-name g) name))
            (workspace--layout-groups ws)))

(defun workspace--upsert-group (ws name layout)
  "Return a new workspace like WS with a group named NAME holding LAYOUT.
If a group with NAME already exists, it is replaced; otherwise the new
group is appended.  Non-destructive."
  (let* ((existing (workspace--layout-groups ws))
         (filtered (seq-remove (lambda (g) (equal (workspace--group-name g) name))
                               existing))
         (replaced? (not (equal (length filtered) (length existing))))
         (new-group (workspace--group-make name layout))
         (next (if replaced?
                   ;; Preserve original position: walk existing, swap matching slot.
                   (mapcar (lambda (g)
                             (if (equal (workspace--group-name g) name)
                                 new-group
                               g))
                           existing)
                 (append existing (list new-group))))
         (copy (copy-sequence ws)))
    (plist-put copy :layout-groups next)))

(defun workspace--remove-group (ws name)
  "Return a new workspace like WS with the layout-group named NAME removed.
Non-destructive.  Reserved names are not specially handled here; callers
in the command layer must consult `workspace--group-name-reserved-p'
before invoking this on a user-supplied name."
  (let* ((next (seq-remove (lambda (g) (equal (workspace--group-name g) name))
                           (workspace--layout-groups ws)))
         (copy (copy-sequence ws)))
    (plist-put copy :layout-groups next)))

(defun workspace--buffer-files (ws)
  "Return the buffer-file membership list of WS."
  (plist-get ws :buffer-files))

(defun workspace--add-buffer-file (ws path)
  "Return a new workspace like WS with PATH added to its buffer-files.
Dedupes: adding the same PATH twice yields one entry.  Non-destructive."
  (let* ((files (workspace--buffer-files ws))
         (next (if (member path files) files (append files (list path))))
         (copy (copy-sequence ws)))
    (plist-put copy :buffer-files next)))

(defun workspace--remove-buffer-file (ws path)
  "Return a new workspace like WS with PATH removed from its buffer-files.
Non-destructive."
  (let* ((next (remove path (workspace--buffer-files ws)))
         (copy (copy-sequence ws)))
    (plist-put copy :buffer-files next)))

(defun workspace--broken-p (ws)
  "Return non-nil if WS is in a broken state.

A workspace is broken when its =:home= directory was missing at
persistence-load time.  The flag is runtime-only (never serialized);
on next save it is implicitly dropped."
  (plist-get ws :broken))

(defun workspace--mark-broken (ws)
  "Return a new workspace like WS marked broken.
Non-destructive: WS is not modified.  Sets =:broken t= on the copy."
  (let ((copy (copy-sequence ws)))
    (plist-put copy :broken t)))

(defun workspace--clear-broken (ws)
  "Return a new workspace like WS with the broken tag cleared.
Non-destructive: WS is not modified.  Sets =:broken nil= on the copy."
  (let ((copy (copy-sequence ws)))
    (plist-put copy :broken nil)))

(defun workspace--unhealthy-reason (ws)
  "Return why workspace WS is not healthy, or nil when it is.

WS is healthy when it is not tagged broken (`workspace--broken-p') and
its `:home' is an existing local directory.  Otherwise the reason, a
symbol, the first that applies: `broken' (the tag persistence sets at
load when the home was missing or remote), `no-home' (no `:home' on the
plist, or WS is nil), `home-remote' (remote homes are unsupported),
`home-missing' (the home is not a directory on disk, however it went —
the broken tag is load-time only and cannot record a deletion since).
`file-remote-p' is asked before `file-directory-p', so a remote home is
never stat'ed and no connection is opened."
  (let ((home (workspace--home ws)))
    (cond
     ((workspace--broken-p ws) 'broken)
     ((not (stringp home)) 'no-home)
     ((file-remote-p home) 'home-remote)
     ((not (file-directory-p home)) 'home-missing))))

(defun workspace--healthy-p (ws)
  "Return non-nil when workspace WS is healthy.
That is, WS is non-nil and `workspace--unhealthy-reason' finds nothing
wrong with it: not tagged broken, and its `:home' an existing local
directory.  Never contacts a remote host."
  (and ws (not (workspace--unhealthy-reason ws))))

(defun workspace--sessions-dir (home)
  "Return =<HOME>/sessions/= as an absolute path.
Pure function: does not touch the filesystem.  HOME is the
workspace's =:home= directory."
  (expand-file-name "sessions" home))

(defun workspace--same-file-p (a b)
  "Return non-nil when A and B name the same existing local file.

Compares the device and inode numbers of the two paths' truenames, so
spelling, symlinks and timestamps do not matter.  `file-equal-p' is
deliberately not used: it compares whole attribute lists, timestamps
included, and so reports a directory modified between its two stats as
unequal to itself.  Nil when either is not a string, is remote, or does
not exist.  Never signals."
  (and (stringp a) (stringp b)
       (not (file-remote-p a))
       (not (file-remote-p b))
       (condition-case nil
           (let ((attrs-a (file-attributes (file-truename (expand-file-name a))))
                 (attrs-b (file-attributes (file-truename (expand-file-name b)))))
             (and attrs-a attrs-b
                  (file-attribute-inode-number attrs-a)
                  (file-attribute-device-number attrs-a)
                  (equal (file-attribute-inode-number attrs-a)
                         (file-attribute-inode-number attrs-b))
                  (equal (file-attribute-device-number attrs-a)
                         (file-attribute-device-number attrs-b))))
         (error nil))))

(defun workspace--path-within-p (path dir)
  "Return non-nil when PATH is DIR or lies inside it.

Both must be local and exist (a missing PATH is inside nothing); DIR
must be a directory.  Containment is by file identity: PATH is
truenamed, then it and each of its ancestors is compared to DIR with
`workspace--same-file-p', so differences of spelling (trailing slash,
`~', symlinked prefixes, letter case on a case-insensitive filesystem)
do not matter, and neither do DIR's timestamps changing during the
walk.  `file-in-directory-p' is deliberately not used: it compares
spellings case-sensitively and ends in `file-equal-p'.  Never signals."
  (and (stringp path) (stringp dir)
       (not (file-remote-p path))
       (not (file-remote-p dir))
       (condition-case nil
           (let ((current (directory-file-name
                           (file-truename (expand-file-name path))))
                 (previous nil)
                 (found nil))
             (when (and (file-exists-p (expand-file-name path))
                        (file-directory-p (expand-file-name dir)))
               (while (and (not found) (not (equal current previous)))
                 (if (workspace--same-file-p current dir)
                     (setq found t)
                   (setq previous current
                         current (directory-file-name
                                  (file-name-directory current))))))
             found)
         (error nil))))

(defun workspace--recipe-split-p (dir)
  "Return non-nil if DIR is a recipe split direction, `columns' or `rows'."
  (memq dir '(columns rows)))

(defun workspace--recipe-path-p (path)
  "Return non-nil if PATH is an absolute file name a recipe pane may name.
Pure string test: a name starting with `~' is refused because its
meaning depends on the environment when it is read."
  (and (stringp path)
       (file-name-absolute-p path)
       (not (string-prefix-p "~" path))))

(defun workspace--recipe-pane-p (pane)
  "Return non-nil if PANE is a well-formed recipe pane.
A pane is `(:file PATH)', `(:file PATH :line N)' with N a positive
integer, or a nested `(:split DIR :panes PANES)'."
  (pcase pane
    (`(:file ,path) (workspace--recipe-path-p path))
    (`(:file ,path :line ,line)
     (and (workspace--recipe-path-p path)
          (natnump line)
          (> line 0)))
    (`(:split ,dir :panes ,panes)
     (and (workspace--recipe-split-p dir)
          (workspace--recipe-panes-p panes)))))

(defun workspace--recipe-panes-p (panes)
  "Return non-nil if PANES is a non-empty proper list of recipe panes."
  (and (consp panes)
       (proper-list-p panes)
       (cl-every #'workspace--recipe-pane-p panes)))

(defun workspace--recipe-p (state)
  "Return non-nil if STATE is a layout recipe, not a captured window-state.
A recipe is `(:recipe :split DIR :panes PANES)': DIR is `columns' or
`rows'; PANES is a non-empty list of `(:file PATH)', `(:file PATH
:line N)' or nested `(:split DIR :panes PANES)' panes, PATH an
absolute file name and N a positive integer.  Total over any input
and touches no file."
  (pcase state
    (`(:recipe :split ,dir :panes ,panes)
     (and (workspace--recipe-split-p dir)
          (workspace--recipe-panes-p panes)
          t))))

(provide 'workspace-data-model)
;;; data-model.el ends here
