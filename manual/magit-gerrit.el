;;; magit-gerrit.el --- Magit plugin for Gerrit Code Review
;;
;; Copyright (C) 2013 Brian Fransioli
;;
;; Author: Brian Fransioli <assem@terranpro.org>
;; URL: https://github.com/terranpro/magit-gerrit
;; Package-Requires: ((magit "2.3.1"))
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Magit plugin to make Gerrit code review easy-to-use from emacs and
;; without the need for a browser!
;;
;; Currently uses the [deprecated] gerrit ssh interface, which has
;; meant that obtaining the list of reviewers is not possible, only
;; the list of approvals (those who have already verified and/or code
;; reviewed).
;;
;;; To Use:
;;
;; (require 'magit-gerrit)
;; (setq-default magit-gerrit-ssh-creds "myid@gerrithost.org")
;;
;;
;; M-x `magit-status'
;; h R  <= magit-gerrit uses the R prefix, see help
;;
;;; Workflow:
;;
;; 1) *check out branch => changes => (ma)git commit*
;; 2) R P  <= [ger*R*it *P*ush for review]
;; 3) R A  <= [ger*R*it *A*dd reviewer] (by email address)
;; 4) *wait for verification/code reviews* [approvals shown in status]
;; 5) R S  <= [ger*R*it *S*ubmit review]
;;
;;; Other Comments:
;; `magit-gerrit-ssh-creds' is buffer local, so if you work with
;; multiple Gerrit's, you can make this a file or directory local
;; variable for one particular project.
;;
;; If your git remote for gerrit is not the default "origin", then
;; `magit-gerrit-remote' should be adjusted accordingly (e.g. "gerrit")
;;
;; Recommended to auto add reviewers via git hooks (precommit), rather
;; than manually performing 'R A' for every review.
;;
;; `magit-gerrit' will be enabled automatically on `magit-status' if
;; the git remote repo uses the same creds found in
;; `magit-gerrit-ssh-creds'.
;;
;; Ex:  magit-gerrit-ssh-creds == br.fransioli@gerrit.org
;; $ cd ~/elisp; git remote -v => https://github.com/terranpro/magit-gerrit.git
;; ^~~ `magit-gerrit-mode' would *NOT* be enabled here
;;
;; $ cd ~/gerrit/prja; git remote -v => ssh://br.fransioli@gerrit.org/.../prja
;; ^~~ `magit-gerrit-mode' *WOULD* be enabled here
;;
;;; Code:

(require 'magit)
(require 'transient)
(require 'json)

(eval-when-compile
  (require 'cl-lib))

;; Define a defvar-local macro for Emacs < 24.3
(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    `(progn
       (defvar ,var ,val ,docstring)
       (make-variable-buffer-local ',var))))

(defvar-local magit-gerrit-ssh-creds nil
  "Credentials used to execute gerrit commands via ssh of the form ID@Server")

(defvar-local magit-gerrit-remote "origin"
  "Default remote name to use for gerrit (e.g. \"origin\", \"gerrit\")")

(defcustom magit-gerrit-popup-prefix "R"
  "Key code to open magit-gerrit popup"
  :group 'magit-gerrit
  :type 'key-sequence)

(defun gerrit-command (cmd &rest args)
  (let ((gcmd (concat
	       "-x -p 29418 "
	       (or magit-gerrit-ssh-creds
		   (error "`magit-gerrit-ssh-creds' must be set!"))
	       " "
	       "gerrit "
	       cmd
	       " "
	       (mapconcat 'identity args " "))))
    ;; (message (format "Using cmd: %s" gcmd))
    gcmd))

(defun gerrit-query (prj &optional status)
  (gerrit-command "query"
		  "--format=JSON"
		  "--all-approvals"
		  "--comments"
		  "--current-patch-set"
		  (concat "project:" prj)
		  (concat "status:" (or status "open"))))

(defun gerrit-review ())

(defun gerrit-ssh-cmd (cmd &rest args)
  (apply #'call-process
	 "ssh" nil nil nil
	 (split-string (apply #'gerrit-command cmd args))))

(defun gerrit-review-abandon (prj rev)
  (gerrit-ssh-cmd "review" "--project" prj "--abandon" rev))

(defun gerrit-review-submit (prj rev &optional msg)
  (gerrit-ssh-cmd "review" "--project" prj "--submit"
		  (if msg msg "") rev))

(defun gerrit-code-review (prj rev score &optional msg)
  (gerrit-ssh-cmd "review" "--project" prj "--code-review" score
		  (if msg msg "") rev))

(defun gerrit-review-verify (prj rev score &optional msg)
  (gerrit-ssh-cmd "review" "--project" prj "--verified" score
		  (if msg msg "") rev))

(defun magit-gerrit-get-remote-url ()
  (magit-git-string "ls-remote" "--get-url" magit-gerrit-remote))

(defun magit-gerrit-get-project ()
 (let* ((regx (rx (zero-or-one ?:) (zero-or-more (any digit)) ?/
		  (group (not (any "/")))
		  (group (one-or-more (not (any "."))))))
	(str (or (magit-gerrit-get-remote-url) ""))
	(sstr (car (last (split-string str "//")))))
   (when (string-match regx sstr)
     (concat (match-string 1 sstr)
	     (match-string 2 sstr)))))

(defun magit-gerrit-string-real-length (s)
  (if (multibyte-string-p s)
      (progn
        (setq mm (- (string-bytes s) (length s)))
        (setq n (- (length s) (/ mm 2)))
        (+ mm n))
    (length s)))

(defun magit-gerrit-string-trunc (str maxlen)
  (if (> (magit-gerrit-string-real-length str) maxlen)
      (concat (substring str 0 maxlen)
	      "...")
    str))

(defun magit-gerrit-create-branch-force (branch parent)
  "Switch 'HEAD' to new BRANCH at revision PARENT and update working tree.
Fails if working tree or staging area contain uncommitted changes.
Succeed even if branch already exist
\('git checkout -B BRANCH REVISION')."
  (cond ((run-hook-with-args-until-success
	  'magit-create-branch-hook branch parent))
	((and branch (not (string= branch "")))
	 (magit-save-repository-buffers)
	 (magit-run-git "checkout" "-B" branch parent))))


(defun magit-gerrit-pretty-print-reviewer (name email crdone vrdone)
  (let* ((wid (1- (window-width)))
	 (crstr (propertize (if crdone (format "%+2d" (string-to-number crdone)) "  ")
			    'face '(magit-diff-lines-heading
				    bold)))
	 (vrstr (propertize (if vrdone (format "%+2d" (string-to-number vrdone)) "  ")
			    'face '(magit-diff-added-highlight
				    bold)))
	 (namestr (propertize (or name "") 'face 'magit-refname))
	 (emailstr (propertize (if email (concat "(" email ")") "")
			       'face 'change-log-name)))
    (format "  %-5s      %s %s" (concat crstr " " vrstr) namestr emailstr)))

(defun magit-gerrit-pretty-print-review (num patchsetn subj owner-name &optional draft)
  ;; window-width - two prevents long line arrow from being shown
  (let* ((wid (- (window-width) 2))
         (numstr (propertize (format "%-8s" num) 'face 'magit-hash))
         (left (- wid (length numstr)))

         (patchsetstr (propertize (format "%-5s" (format "[%s]" patchsetn))
                                  'face 'magit-hash))
         (left (- left (length patchsetstr)))

         (authmaxlen (/ wid 4))
         (author (propertize (magit-gerrit-string-trunc owner-name authmaxlen)
                             'face 'magit-log-author))
         (left (- left (magit-gerrit-string-real-length author)))

         (subjstr (propertize
                   (truncate-string-to-width
                    subj
                    (- left 1)
                    nil ?\s (make-string 1 magit-ellipsis))
                   'face
                   (if draft
                       'magit-signature-bad
                     'magit-signature-good))))
    (format "%s%s%s%s\n"
            numstr patchsetstr subjstr author))
  )

(defun magit-gerrit-wash-approval (approval)
  (let* ((approver (cdr-safe (assoc 'by approval)))
	 (approvname (cdr-safe (assoc 'name approver)))
	 (approvemail (cdr-safe (assoc 'email approver)))
	 (type (cdr-safe (assoc 'type approval)))
	 (verified (string= type "Verified"))
	 (codereview (string= type "Code-Review"))
	 (score (cdr-safe (assoc 'value approval))))

    (magit-insert-section (section approval)
      (insert (magit-gerrit-pretty-print-reviewer approvname approvemail
						  (and codereview score)
						  (and verified score))
	      "\n"))))

(defun magit-gerrit-wash-approvals (approvals)
  (mapc #'magit-gerrit-wash-approval approvals))

(defun magit-gerrit-wash-review ()
  (let* ((beg (point))
	 (jobj (json-read))
	 (end (point))
	 (num (cdr-safe (assoc 'number jobj)))
	 (subj (cdr-safe (assoc 'subject jobj)))
	 (owner (cdr-safe (assoc 'owner jobj)))
	 (owner-name (cdr-safe (assoc 'name owner)))
	 (owner-email (cdr-safe (assoc 'email owner)))
	 (patchsets (cdr-safe (assoc 'currentPatchSet jobj)))
     (patchset-num (cdr-safe (assoc 'number patchsets)))
	 ;; compare w/t since when false the value is => :json-false
	 (isdraft (eq (cdr-safe (assoc 'isDraft patchsets)) t))
	 (approvs (cdr-safe (if (listp patchsets)
				(assoc 'approvals patchsets)
			      (assoc 'approvals (aref patchsets 0))))))
    (if (and beg end)
	(delete-region beg end))
    (when (and num subj owner-name)
      (magit-insert-section (section subj)
	(insert (propertize
		 (magit-gerrit-pretty-print-review num patchset-num subj owner-name isdraft)
		 'magit-gerrit-jobj
		 jobj))
	(unless (oref (magit-current-section) hidden)
	  (magit-gerrit-wash-approvals approvs))
	(add-text-properties beg (point) (list 'magit-gerrit-jobj jobj)))
      t)))

(defun magit-gerrit-wash-reviews (&rest args)
  (magit-wash-sequence #'magit-gerrit-wash-review))

(defun magit-gerrit-section (section title washer &rest args)
  (let ((magit-git-executable "ssh")
	(magit-git-global-arguments nil))
    (magit-insert-section (section title)
      (magit-insert-heading title)
      (magit-git-wash washer (split-string (car args)))
      (insert "\n"))))

(defun magit-gerrit-remote-update (&optional remote)
  nil)

(defun magit-gerrit-review-at-point ()
  (get-text-property (point) 'magit-gerrit-jobj))

(defsubst magit-gerrit-process-wait ()
  (while (and magit-this-process
	      (eq (process-status magit-this-process) 'run))
    (sleep-for 0.005)))

(defun magit-gerrit-fetch-patchset ()
  "fetch a Gerrit Review Patchset"
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (let ((ref (cdr (assoc 'ref (assoc 'currentPatchSet jobj)))))
        (let* ((magit-proc (magit-git-fetch magit-gerrit-remote ref)))
          (message (format "Waiting a git fetch from %s to complete..."
                           magit-gerrit-remote))
          (magit-gerrit-process-wait))))))

(defun magit-gerrit-view-patchset-diff ()
  "View the Diff for a Patchset"
  (interactive)
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (let ((ref (cdr (assoc 'ref (assoc 'currentPatchSet jobj))))
	    (dir default-directory))
    (magit-gerrit-fetch-patchset)
	(message (format "Generating Gerrit Patchset for refs %s dir %s" ref dir))
	(magit-diff-range "FETCH_HEAD~1..FETCH_HEAD")))))

(defun magit-gerrit-download-patchset ()
  "Download a Gerrit Review Patchset"
  (interactive)
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (let ((ref (cdr (assoc 'ref (assoc 'currentPatchSet jobj))))
	    (dir default-directory)
	    (branch (format "review/%s/%s"
			    (cdr (assoc 'username (assoc 'owner jobj)))
			    (cdr (or (assoc 'topic jobj) (assoc 'number jobj))))))
    (magit-gerrit-fetch-patchset)
	(message (format "Checking out refs %s to %s in %s" ref branch dir))
	(magit-gerrit-create-branch-force branch "FETCH_HEAD")))))

(defun magit-gerrit-cherry-pick-patchset ()
  "Cherry-pick a Gerrit Review Patchset"
  (interactive)
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (magit-gerrit-fetch-patchset)
      (magit--cherry-pick '("FETCH_HEAD") nil))))

(defun magit-gerrit-browse-review ()
  "Browse the Gerrit Review with a browser."
  (interactive)
  (let ((jobj (magit-gerrit-review-at-point)))
    (if jobj
	(browse-url (cdr (assoc 'url jobj))))))

(defun magit-gerrit-copy-review (with-commit-message)
  "Copy review url and commit message."
  (let ((jobj (magit-gerrit-review-at-point)))
    (if jobj
      (with-temp-buffer
        (insert
         (concat (cdr (assoc 'url jobj))
                 (if with-commit-message
                     (concat " " (car (split-string (cdr (assoc 'commitMessage jobj)) "\n" t))))))
        (message "%s" (buffer-string))
        (clipboard-kill-region (point-min) (point-max))))))

(defun magit-gerrit-copy-review-url ()
  "Copy review url only"
  (interactive)
  (magit-gerrit-copy-review nil))

(defun magit-gerrit-copy-review-url-commit-message ()
  "Copy review url with commit message"
  (interactive)
  (magit-gerrit-copy-review t))

(defun magit-insert-gerrit-reviews ()
  (magit-gerrit-section 'gerrit-reviews
			"Reviews:" 'magit-gerrit-wash-reviews
			(gerrit-query (magit-gerrit-get-project))))

(defun magit-gerrit-add-reviewer ()
  (interactive)
  "ssh -x -p 29418 user@gerrit gerrit set-reviewers --project toplvlroot/prjname --add email@addr"

  (gerrit-ssh-cmd "set-reviewers"
		  "--project" (magit-gerrit-get-project)
		  "--add" (read-string "Reviewer Name/Email: ")
		  (cdr-safe (assoc 'id (magit-gerrit-review-at-point)))))

(defun magit-gerrit-arguments ()
  (transient-args 'magit-gerrit-dispatch))

(defun magit-gerrit-popup-args (&optional something)
  (or (magit-gerrit-arguments) (list "")))

(defun magit-gerrit-verify-review (args)
  "Verify a Gerrit Review"
  (interactive (magit-gerrit-popup-args))

  (let ((score (completing-read "Score: "
				    '("-2" "-1" "0" "+1" "+2")
				    nil t
				    "+1"))
	(rev (cdr-safe (assoc
		      'revision
		      (cdr-safe (assoc 'currentPatchSet
				       (magit-gerrit-review-at-point))))))
	(prj (magit-gerrit-get-project)))
    (gerrit-review-verify prj rev score args)
    (magit-refresh)))

(defun magit-gerrit-code-review (args)
  "Perform a Gerrit Code Review"
  (interactive (magit-gerrit-popup-args))
  (let ((score (completing-read "Score: "
				    '("-2" "-1" "0" "+1" "+2")
				    nil t
				    "+1"))
	(rev (cdr-safe (assoc
		      'revision
		      (cdr-safe (assoc 'currentPatchSet
				       (magit-gerrit-review-at-point))))))
	(prj (magit-gerrit-get-project)))
    (gerrit-code-review prj rev score args)
    (magit-refresh)))

(defun magit-gerrit-submit-review (args)
  "Submit a Gerrit Code Review"
  ;; "ssh -x -p 29418 user@gerrit gerrit review REVISION  -- --project PRJ --submit "
  (interactive (magit-gerrit-popup-args))
  (let ((prj (magit-gerrit-get-project))
    (rev (cdr-safe (assoc
		            'revision
		            (cdr-safe (assoc 'currentPatchSet
				                     (magit-gerrit-review-at-point)))))))
    (gerrit-review-submit prj rev args)
    (magit-fetch-all-no-prune)
    (magit-refresh)))

(defun magit-gerrit-push-review (status)
  (let* ((branch (or (magit-get-current-branch)
		     (error "Don't push a detached head.  That's gross")))
	 (commitid (or (when (eq (oref (magit-current-section) type)
				 'commit)
                     (oref (magit-current-section) value))
		       (error "Couldn't find a commit at point")))
	 (rev (magit-rev-parse (or commitid
				   (error "Select a commit for review"))))

	 (branch-remote (and branch (magit-get "branch" branch "remote"))))

    ;; (message "Args: %s "
    ;;	     (concat rev ":" branch-pub))

    (let* ((branch-merge (if (or (null branch-remote)
				 (string= branch-remote "."))
			     (completing-read
			      "Remote Branch: "
			      (let ((rbs (magit-list-remote-branch-names)))
				(mapcar
				 #'(lambda (rb)
				     (and (string-match (rx bos
							    (one-or-more (not (any "/")))
							    "/"
							    (group (one-or-more any))
							    eos)
							rb)
					  (concat "refs/heads/" (match-string 1 rb))))
				 rbs)))
			   (and branch (magit-get "branch" branch "merge"))))
	   (branch-pub (progn
			 (string-match (rx "refs/heads" (group (one-or-more any)))
				       branch-merge)
			 (format "refs/%s%s/%s" status (match-string 1 branch-merge) branch))))


      (when (or (null branch-remote)
		(string= branch-remote "."))
	(setq branch-remote magit-gerrit-remote))

      (magit-run-git-async "push" "-v" branch-remote
			   (concat rev ":" branch-pub)))))

(defun magit-gerrit-create-review ()
  (interactive)
  (magit-gerrit-push-review 'for))

(defun magit-gerrit-create-draft ()
  (interactive)
  (magit-gerrit-push-review 'drafts))

(defun magit-gerrit-publish-draft ()
  (interactive)
  (let ((prj (magit-gerrit-get-project))
	(id (cdr-safe (assoc 'id
		     (magit-gerrit-review-at-point))))
	(rev (cdr-safe (assoc
			'revision
			(cdr-safe (assoc 'currentPatchSet
					 (magit-gerrit-review-at-point)))))))
    (gerrit-ssh-cmd "review" "--project" prj "--publish" rev))
  (magit-refresh))

(defun magit-gerrit-delete-draft ()
  (interactive)
  (let ((prj (magit-gerrit-get-project))
	(id (cdr-safe (assoc 'id
		     (magit-gerrit-review-at-point))))
	(rev (cdr-safe (assoc
			'revision
			(cdr-safe (assoc 'currentPatchSet
					 (magit-gerrit-review-at-point)))))))
    (gerrit-ssh-cmd "review" "--project" prj "--delete" rev))
  (magit-refresh))

(defun magit-gerrit-abandon-review ()
  (interactive)
  (let ((prj (magit-gerrit-get-project))
	(id (cdr-safe (assoc 'id
		     (magit-gerrit-review-at-point))))
	(rev (cdr-safe (assoc
			'revision
			(cdr-safe (assoc 'currentPatchSet
					 (magit-gerrit-review-at-point)))))))
    ;; (message "Prj: %s Rev: %s Id: %s" prj rev id)
    (gerrit-review-abandon prj rev)
    (magit-refresh)))

(defun magit-gerrit-read-comment (&rest args)
  (format "\'\"%s\"\'"
	  (read-from-minibuffer "Message: ")))

(define-infix-argument magit-gerrit-message:--message ()
  :description "Message"
  :class 'transient-option
  :key "-m"
  :argument "--message "
  :reader 'magit-gerrit-read-comment
  )

(defun magit-gerrit-create-branch (branch parent))

;;;###autoload (autoload 'magit-gerrit-dispatch "magit-gerrit" nil t)
(define-transient-command magit-gerrit-dispatch ()
  "Popup console for magit gerrit commands."
  ["Arguments"
   ("-m" magit-gerrit-message:--message)]
  [["Actions"
    ("A" "Add Reviewer"                    magit-gerrit-add-reviewer)
    ("B" "Abandon Review"                  magit-gerrit-abandon-review)
    ("k" "Delete Draft"                    magit-gerrit-delete-draft)
    ("p" "Publish Draft Patchset"          magit-gerrit-publish-draft)
    ("P" "Push Commit For Review"          magit-gerrit-create-review)
    ("S" "Submit Review"                   magit-gerrit-submit-review)
    ("W" "Push Commit For Draft Review"    magit-gerrit-create-draft)]
   ["Review"
    ("b" "Browse Review"                   magit-gerrit-browse-review)
    ("C" "Code Review"                     magit-gerrit-code-review)
    ("d" "View Patchset Diff"              magit-gerrit-view-patchset-diff)
    ("D" "Download Patchset"               magit-gerrit-download-patchset)
    ("F" "Cherry-pick Patchset"            magit-gerrit-cherry-pick-patchset)
    ("V" "Verify"                          magit-gerrit-verify-review)]]
  ["Others"
   ("y" "Copy Review URL"                  magit-gerrit-copy-review-url)
   ("Y" "Copy Review URL And Message"      magit-gerrit-copy-review-url-commit-message)])

(defvar magit-gerrit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map magit-gerrit-popup-prefix 'magit-gerrit-dispatch)
    map))

(define-minor-mode magit-gerrit-mode "Gerrit support for Magit"
  :lighter " Gerrit" :require 'magit-topgit :keymap 'magit-gerrit-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (or magit-gerrit-ssh-creds
      (error "You *must* set `magit-gerrit-ssh-creds' to enable magit-gerrit-mode"))
  (or (magit-gerrit-get-remote-url)
      (error "You *must* set `magit-gerrit-remote' to a valid Gerrit remote"))
  (cond
   (magit-gerrit-mode
    (magit-add-section-hook 'magit-status-sections-hook
			    'magit-insert-gerrit-reviews
			    'magit-insert-stashes t t)
    (add-hook 'magit-create-branch-command-hook
	      'magit-gerrit-create-branch nil t)
    ;(add-hook 'magit-pull-command-hook 'magit-gerrit-pull nil t)
    (add-hook 'magit-remote-update-command-hook
	      'magit-gerrit-remote-update nil t)
    (add-hook 'magit-push-command-hook
	      'magit-gerrit-push nil t))

   (t
    (remove-hook 'magit-after-insert-stashes-hook
		 'magit-insert-gerrit-reviews t)
    (remove-hook 'magit-create-branch-command-hook
		 'magit-gerrit-create-branch t)
    ;(remove-hook 'magit-pull-command-hook 'magit-gerrit-pull t)
    (remove-hook 'magit-remote-update-command-hook
		 'magit-gerrit-remote-update t)
    (remove-hook 'magit-push-command-hook
		 'magit-gerrit-push t)))
  (when (called-interactively-p 'any)
    (magit-refresh)))

(defun magit-gerrit-detect-ssh-creds (remote-url)
  "Derive magit-gerrit-ssh-creds from remote-url.
Assumes remote-url is a gerrit repo if scheme is ssh
and port is the default gerrit ssh port."
  (let ((url (url-generic-parse-url remote-url)))
    (when (and (string= "ssh" (url-type url))
	       (eq 29418 (url-port url)))
      (set (make-local-variable 'magit-gerrit-ssh-creds)
	   (format "%s@%s" (url-user url) (url-host url)))
      (message "Detected magit-gerrit-ssh-creds=%s" magit-gerrit-ssh-creds))))

(defun magit-gerrit-check-enable ()
  (let ((remote-url (magit-gerrit-get-remote-url)))
    (when (and remote-url
               (or magit-gerrit-ssh-creds
                   (magit-gerrit-detect-ssh-creds remote-url))
               (string-match magit-gerrit-ssh-creds remote-url))
      (magit-gerrit-mode t))

    (cond
     (magit-gerrit-mode
      ;; update keymap with prefix incase it has changed
      (define-key magit-mode-map magit-gerrit-popup-prefix 'magit-gerrit-dispatch)

      ;; Attach Magit Gerrit to Magit's default help popup
      (transient-append-suffix 'magit-dispatch "z"
        `(,magit-gerrit-popup-prefix "Gerrit" magit-gerrit-dispatch)))
     (t
      ;; FIXME: how to resume magit default keybind?
      (define-key magit-mode-map magit-gerrit-popup-prefix 'magit-file-rename)
      ;; Dettach Magit Gerrit to Magit's default help popup
      (transient-remove-suffix 'magit-dispatch magit-gerrit-popup-prefix)))))

;; Hack in dir-local variables that might be set for magit gerrit
(add-hook 'magit-status-mode-hook #'hack-dir-local-variables-non-file-buffer t)

;; Try to auto enable magit-gerrit in the magit-status buffer
(add-hook 'magit-status-mode-hook #'magit-gerrit-check-enable t)
(add-hook 'magit-log-mode-hook #'magit-gerrit-check-enable t)

(provide 'magit-gerrit)

;;; magit-gerrit.el ends here
