(let ((applycl-path (merge-pathnames ".apply/apply.cl"
                                     (user-homedir-pathname))))
  (when (probe-file applycl-path)
    (load applycl-path)))
(ql:quickload :cl-ansi-text)
(import 'cl-ansi-text:green)
(import 'cl-ansi-text:red)

(pacman-write-package-list)

(defvar *lisp-dev* (list :name "lisp-dev" :collection (list "sbcl" "clisp" "racket")))
(defvar *go-dev* (list :name "go-dev" :collection (list "go")))
(defvar *c-dev* (list :name "c-dev" :collection (list "gcc" "gdb" "valgrind" "autoconf" "automake" "make" "clang" "ctags")))
(defvar *video* (list :name "video" :collection (list "xorg-server" "xorg-xinit" "nvidia-340xx-libgl"
                                                      "nvidia-340xx-utils" "nvidia-settings")))
(defvar *video-32bit* (list :name "video-32bit" :collection (list "lib32-nvidia-340xx-utils" "lib32-nvidia-libgl")))
(defvar *misc* (list :name "misc" :collection (list "cmatrix" "tmux" "rxvt-unicode" "zsh" "sloc")))
(defvar *sound* (list :name "sound" :collection (list "alsa-lib" "alsa-firmware" "alsa-plugins" "alsa-utils")))

(defvar *collections* (list *lisp-dev* *go-dev* *c-dev* *video* *video-32bit* *misc* *sound*))

(dolist (pkg-list *collections*)
  (format t (green (concatenate 'string ">> Ensuring collection " (getf pkg-list :name) "~%")))
  (ensure-collection-installed (getf pkg-list :collection)))

(format t (green ">> Updating pacman~%"))
(pacman-upgrade)
