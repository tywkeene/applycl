(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(load "pacman.cl")
(ql:quickload :cl-fad)
(ql:quickload :split-sequence)

(defvar *file-manifest* (merge-pathnames ".apply/manifest" (user-homedir-pathname)))

(defun does-file-exist? (filepath)
  "Returns nil if file pointed to by filepath does not exist"
  (cl-fad:file-exists-p filepath))

(defun does-dir-exist? (dirpath)
  "Returns nil if directory pointed to by filepath does not exist"
  (cl-fad:directory-exists-p dirpath))

(defun read-installed-package-file ()
  "Read and 'parse' *installed-pkg-file* into a list of installed packages"
  (if (eq nil (does-file-exist? *installed-pkg-file*)) (pacman-write-package-list))
  (with-open-file (stream *installed-pkg-file*)
    (loop for line = (read-line stream nil)
          while line
          collect (car(split-sequence:split-sequence #\space line)))))

(defun get-name-from-path (path)
  "Split path into /path/to/directory/ and filename list"
  (car (last (split-sequence:split-sequence #\/ path))))

(defun make-file-manifest-path (path)
  "Concatenate filename from path to *file-manifest* path"
  (merge-pathnames (get-name-from-path path) *file-manifest*))

(defun create-file (dest-path)
  "Copy file pointed to by dest-path if it does not exist"
  (if (eq nil (does-file-exist? (make-file-manifest-path dest-path)))
    (format t "File '~a' does not exist in file-manifest~%" (get-name-from-path dest-path))
    (cl-fad:copy-file (make-file-manifest-path dest-path) dest-path)))

(defun verify-file-in-manifest (filepath)
"Make sure file exists in *file-manifest*"
(if (eq nil (does-file-exist? filepath))
  (format t "Warning: '~a' does not exist in ~a~%" filepath *file-manifest*)))

(defun ensure-file-exists (filepath)
  "Create file pointed to by filepath if it does not exist"
  (verify-file-in-manifest (make-file-manifest-path filepath))
  (if (does-file-exist? filepath)
    (format t "File '~a' already exists~%" filepath)
    (create-file filepath)))

(defun is-installed? (pkg-name)
  "Returns nil if the package pkg-name is not installed"
  (find pkg-name (read-installed-package-file) :test #'equal))

(defun ensure-is-installed (pkg-name)
  "Ensure package pkg-name is installed, install it if it's not"
  (if (eq nil (is-installed? pkg-name))
    (pacman-install pkg-name)
    (format t "~a is already installed~%" pkg-name)))

(defun ensure-collection-installed (collection)
  "Ensure a collection of software is installed using ensure-is-installed"
  (dolist (pkg collection)
    (ensure-is-installed pkg)))

(defun ensure-files-exist (files)
  "Ensure a collection of files exist using ensure-file-exists"
  (dolist (file files)
    (ensure-file-exists file)))
