;;; sfdx-mode.el --- minor mode for working with sfdx projects


;; Version: 0.0.1
;; Author: Rodolphe Blancho <http://github/rody>
;; Url: https://github.com/rody/sfdx-mode
;; Keywords: project, sfdx, salesforce, apex, tools
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;  description
;;
;;; Code:
(require 'compile)
(require 'json)
(require 'cl-lib)

(defvar sfdx-mode--project-file-name "sfdx-project.json"
  "The name of sfdx project file.")

(defvar sfdx-mode--modeline-name " sfdx"
  "Name of sfdx mode modeline name.")

(defun sfdx-mode--ensure-sfdx-project ()
  "Asserts that you are currently inside a sfdx project."
  (sfdx-mode--project-file))

(defun sfdx-mode--project-file()
  "Return path to the project file, or nil.
If project file exists in the current working directory, or
a parent directory recursively, return its path. Otherwise
return nil."
  (let ((dir (locate-dominating-file default-directory sfdx-mode--project-file-name)))
    (unless dir
      (error (concat "Error: cannot find " sfdx-mode--project-file-name)))
    (concat dir sfdx-mode--project-file-name)))

(defun sfdx-mode-visit-project-file ()
  "Visit the project file."
  (interactive)
  (find-file (sfdx-mode--project-file)))

(defun sfdx-mode-org-list-clean ()
  "Remove all local org authorizations for non-active orgs."
  (interactive)
  (shell-command "sfdx force:org:list --clean --noprompt"))

(defvar sfdx-mode--org-names-cache '())
(defvar sfdx-mode--devhub-names-cache '())

(defun sfdx-mode-invalidate-org-list-cache ()
  "Invalidate the org names cache."
  (interactive)
  (setq sfdx-mode--org-names-cache '())
  (setq sfdx-mode--devhub-names-cache '()))

(defun sfdx-mode-refresh-org-list-cache ()
  "Refresh the org list cache."
  (interactive)
  (message "Retrieving org usernames/aliases (this may take some time)...")
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-parse-string (shell-command-to-string "sfdx force:org:list --skipconnectionstatus --json 2> /dev/null")))
         (result (gethash "result" json))
         (scratch-orgs (gethash "scratchOrgs" result))
         (non-scratch-orgs (gethash "nonScratchOrgs" result)))
    (sfdx-mode-invalidate-org-list-cache)
    (mapc (lambda(arg) (push (gethash "alias" arg (gethash "username" arg)) sfdx-mode--org-names-cache)) non-scratch-orgs)
    (mapc (lambda(arg) (push (gethash "alias" arg (gethash "username" arg)) sfdx-mode--org-names-cache)) scratch-orgs)
    (mapc (lambda(arg)
	    (let ((is-devhub (gethash "isDevHub" arg :false)))
	      (unless (eq is-devhub :false)
		  (push (gethash "alias" arg (gethash "username" arg)) sfdx-mode--devhub-names-cache)))
	    ) non-scratch-orgs)
    (cl-sort sfdx-mode--org-names-cache 'string-lessp :key 'downcase)
    (cl-sort sfdx-mode--devhub-names-cache 'string-lessp :key 'downcase)
))

(defun sfdx-mode--org-names ()
  "Return all org names/aliases."
  (unless sfdx-mode--org-names-cache
    (sfdx-mode-refresh-org-list-cache))
  sfdx-mode--org-names-cache)

(defun sfdx-mode--devhub-names ()
  "Return all devhub names/aliases."
  (unless sfdx-mode--devhub-names-cache
    (sfdx-mode-refresh-org-list-cache))
  sfdx-mode--devhub-names-cache)

(defun sfdx-mode-org-open (org-name)
  "Open an org in the browser.

  ORG-NAME name or alias of the org"

  (interactive (list (completing-read "org: " (sfdx-mode--org-names))))
  (message "Opening org %s..." org-name)
  (shell-command (concat "sfdx force:org:open -u \"" org-name "\"")))

(defun sfdx-mode-create-scratch-org (devhub config alias)
  "Create a scratch org.

   DEVHUB Dev Hub username or alias
   CONFIG the scratch org config file
   ALIAS the alias to create"

  (interactive (list (completing-read "devhub: " (sfdx-mode--devhub-names))
		     (read-file-name "config: ")
		     (read-string "alias: ")))
  (unless (sfdx-mode--ensure-sfdx-project)
    (message "this command must be run inside an sfdx project")
  (shell-command (format "sfdx force:org:create --type=scratch --targetdevhubusername='%s' --configfile='%s' -a '%s' -json" devhub config alias))))

(defun sfdx-mode-apex-class-create (outputdir classname)
  "Create a new Apex class file.

   OUTPUTDIR output directory
   CLASSNAME name of the class to create"

  (interactive (list
		(read-directory-name "Output dir: ")
		(read-string "Class name: ")))
  (let ((absdir (file-truename outputdir)))
  (unless (file-exists-p absdir)
    (make-directory absdir :parents))
  (shell-command (format "sfdx force:apex:class:create --template=DefaultApexClass --outputdir='%s' --classname='%s'" absdir classname))
  (find-file (concat (file-name-as-directory absdir) classname ".cls"))))

(defgroup sfdx-mode nil
  "Customization group for sfdx-mode."
  :group 'convenience)

(defvar sfdx-mode-keymap (make-sparse-keymap)
  "Keymap for sfdx-mode.")

(define-key sfdx-mode-keymap (kbd "C-c s o l") 'sfdx-mode-org-list)
(define-key sfdx-mode-keymap (kbd "C-c s o o") 'sfdx-mode-org-open)
(define-key sfdx-mode-keymap (kbd "C-c s p v") 'sfdx-mode-visit-project-file)

;;;###autoload
(define-minor-mode sfdx-mode
  "Minor mode for working with sfdx projects."
  :global nil
  :lighter sfdx-mode--modeline-name
  :keymap sfdx-mode-keymap
  :group 'sfdx-mode)

;;;###autoload
(define-globalized-minor-mode sfdx-global-mode
  sfdx-mode
  sfdx-mode)

(provide 'sfdx-mode)
;;; sfdx-mode.el ends here
