(in-package :cl-user)
(uiop:define-package :trivial-version/main
    (:nicknames :trivial-version)
  (:export :version-template))
(in-package :trivial-version/main)

(defun get-version (system-name)
  (ignore-errors
    (let ((system (asdf:find-system system-name)))
      (append (list system-name)
              (let ((type
                     (cond
                       #+quicklisp
                       ((ql-dist:find-system system-name)
                        `(:quicklisp
                          :version ,(ql-dist:short-description (ql-dist:release (ql-dist:find-system system-name)))))
                       ;;git
                       ((ignore-errors
                          (when (asdf:system-source-directory system)
                            (uiop:with-current-directory ((asdf:system-source-directory system))
                              (list :git :version (subseq (uiop:run-program "git log -n 1 --oneline" :output :string) 0 7))))))
                       (t nil))))
                (when type
                  `(:type ,@type)))
              (ignore-errors
                (when (asdf:component-version system)
                  (list :component-version (asdf:component-version system))))))))

(defun get-dependency (system-names &key old)
  (if (consp system-names)
      (get-dependency (loop for i in system-names
                         append (unless (find i old :test 'equal)
                                  (setf old (cons i old))
                                  (asdf:system-depends-on (asdf:find-system i))))
                      :old old)
      (if (or (null system-names)
              (find system-names old :test 'equal))
          old
          (get-dependency (asdf:system-depends-on (asdf:find-system system-names))
                          :old (cons system-names old)))))

(defmacro version-template (system-name &key (if-does-not-exist :error))
  (let ((version (intern (format nil "+~@:(~A-~A~)+" system-name :versions)))
        (system (ignore-errors (get-dependency system-name))))
    (cond ((and (eql if-does-not-exist :error) (not system))
           (error "~A not found" system-name))
          (t
           `(export
             (defconstant ,version
               (if (boundp ',version)
                   ,version
                   ',(mapcar 'get-version (nreverse system)))))))))

#|
(version-template "lem")
;;=>
(EXPORT
 (DEFCONSTANT +LEM-VERSIONS+
   (IF (BOUNDP '+LEM-VERSIONS+)
       +LEM-VERSIONS+
       '(("lem" :TYPE :GIT :VERSION "4125502" :COMPONENT-VERSION "1.0")
         ("cffi" :TYPE :QUICKLISP :VERSION "cffi_0.19.0" :COMPONENT-VERSION
          "0.19.0")
         ("lem-core" :TYPE :GIT :VERSION "4125502")
         ("lem-ncurses" :TYPE :GIT :VERSION "4125502")
         ("lem-lisp-mode" :TYPE :GIT :VERSION "4125502")
         ("lem-go-mode" :TYPE :GIT :VERSION "4125502")
         ("lem-c-mode" :TYPE :GIT :VERSION "4125502")
         ("uiop" :TYPE :QUICKLISP :VERSION "uiop-3.2.1" :COMPONENT-VERSION
          "3.1.5")
         ("alexandria" :TYPE :QUICKLISP :VERSION "alexandria-20170830-git"
          :COMPONENT-VERSION "0.0.0")
         ("trivial-features" :TYPE :QUICKLISP :VERSION
          "trivial-features-20161204-git")
         ("babel" :TYPE :QUICKLISP :VERSION "babel-20170630-git")
         ("swank" :TYPE :QUICKLISP :VERSION "slime-v2.20")
         ("trivial-gray-streams" :TYPE :QUICKLISP :VERSION
          "trivial-gray-streams-20140826-git" :COMPONENT-VERSION "2.0")
         ("cl-ppcre" :TYPE :QUICKLISP :VERSION "cl-ppcre-2.0.11"
          :COMPONENT-VERSION "2.0.11")
         ("inquisitor" :TYPE :QUICKLISP :VERSION "inquisitor-20170830-git"
          :COMPONENT-VERSION "0.5")
         ("bordeaux-threads" :TYPE :QUICKLISP :VERSION
          "bordeaux-threads-v0.8.5" :COMPONENT-VERSION "0.8.5")
         ("trivial-clipboard" :TYPE :QUICKLISP :VERSION
          "trivial-clipboard-20170630-git" :COMPONENT-VERSION "0.0.0.0")
         ("yason" :TYPE :QUICKLISP :VERSION "yason-v0.7.6" :COMPONENT-VERSION
          "0.7.6")
         ("lem-base" :TYPE :GIT :VERSION "4125502")
         ("lem-lisp-syntax" :TYPE :GIT :VERSION "4125502")
         ("cl-charms" :TYPE :QUICKLISP :VERSION "cl-charms-20170227-git"
          :COMPONENT-VERSION "0.2.0")
         ("trivial-types" :TYPE :QUICKLISP :VERSION
          "trivial-types-20120407-git" :COMPONENT-VERSION "0.1")
         ("usocket" :TYPE :QUICKLISP :VERSION "usocket-0.7.0.1"
          :COMPONENT-VERSION "0.7.0.1")
         ("optima" :TYPE :QUICKLISP :VERSION "optima-20150709-git"
          :COMPONENT-VERSION "1.0")
         ("anaphora" :TYPE :QUICKLISP :VERSION "anaphora-20170227-git"
          :COMPONENT-VERSION "0.9.6")
         ("iterate" :TYPE :QUICKLISP :VERSION "iterate-20160825-darcs")
         ("cl-annot" :TYPE :QUICKLISP :VERSION "cl-annot-20150608-git"
          :COMPONENT-VERSION "0.1")
         ("sb-bsd-sockets")
         ("split-sequence" :TYPE :QUICKLISP :VERSION "split-sequence-1.2"
          :COMPONENT-VERSION "1.2")
         ("closer-mop" :TYPE :QUICKLISP :VERSION "closer-mop-20170830-git"
          :COMPONENT-VERSION "1.0.0")))))
;; how to deal uiop version?
(version-template "rove")
;;=>
(EXPORT
 (DEFCONSTANT +ROVE-VERSIONS+
   (IF (BOUNDP '+ROVE-VERSIONS+)
       +ROVE-VERSIONS+
       '(("rove" :TYPE :GIT :VERSION "06acecb" :COMPONENT-VERSION "0.1.0")
         ("rove/main") ("rove/core/assertion") ("rove/core/test")
         ("rove/core/suite") ("rove/core/stats") ("rove/core/result")
         ("rove/reporter") ("rove/misc/color") ("rove/reporter/spec")
         ("rove/core/suite/package")
         ("dissect" :TYPE :QUICKLISP :VERSION "dissect-20170830-git"
          :COMPONENT-VERSION "0.11.0")
         ("bordeaux-threads" :TYPE :QUICKLISP :VERSION
          "bordeaux-threads-v0.8.5" :COMPONENT-VERSION "0.8.5")
         ("rove/misc/stream")
         ("alexandria" :TYPE :QUICKLISP :VERSION "alexandria-20170830-git"
          :COMPONENT-VERSION "0.0.0")
         ("trivial-gray-streams" :TYPE :QUICKLISP :VERSION
          "trivial-gray-streams-20140826-git" :COMPONENT-VERSION "2.0")))))
;; get information from package infered system...
|#
