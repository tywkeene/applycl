(defvar *sudo* "/usr/bin/sudo")
(defvar *pkg-manager* "/usr/bin/pacman")
(defvar *installed-pkg-file* (merge-pathnames ".apply/installed-packages" (user-homedir-pathname)))

(defun run-pacman (args)
  "Run '*sudo* pkg-manager pkg-manager-args"
  (sb-ext:run-program *sudo* (cons *pkg-manager* args)
                      :wait t
                      :output t
                      :input t
                      :directory nil
                      :search nil))

(defun pacman-search (pkg-name)
  "Search pacman mirrors for pkg-name"
  (run-pacman (list "-Ss" pkg-name)))

(defun pacman-install (pkg-name)
  "Install pkg-name"
  (run-pacman (list "--noconfirm" "-S" pkg-name)))

(defun pacman-uninstall (pkg-name)
  "Uninstall pkg-name"
  (run-pacman (list "--noconfirm" "-R" pkg-name)))

(defun pacman-purge (pkg-name)
  "Purge pkg-name"
  (run-pacman (list "--noconfirm" "-Rs"  pkg-name)))

(defun pacman-query (pkg-name)
  "Query the pacman database"
  (sb-ext:run-program *pkg-manager* (list "-Ql" pkg-name) :output :output :wait nil))

(defun pacman-upgrade ()
  "Sync and perform system-wide upgrade"
  (run-pacman (list "--noconfirm" "-Syuu" )))

(defun pacman-write-package-list ()
  "Write list of installed packages to *installed-pkg-file*"
  (if (does-file-exist? *installed-pkg-file*)
    (delete-file *installed-pkg-file*))
  (sb-ext:run-program *pkg-manager* (list "-Q") :output *installed-pkg-file*))
