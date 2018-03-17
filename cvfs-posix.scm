;; posix backend for cvfs
[module cvfs-posix ()
  (import chicken scheme)
  (use cvfs posix srfi-13 files)

  (define (cvfs-posix:get-path inst subpath)
    (string-concatenate/shared (list (cdr inst) subpath)))

  (create-backend
   'posix
   ;; init
   (lambda (root) (string-concatenate (list root "/" )))
   ;; open
   (lambda (inst path mode)
     (if (eq? mode 'write)
         (open-output-file (cvfs-posix:get-path inst path))
         (open-input-file (cvfs-posix:get-path inst path))))
   ;; dir
   (lambda (inst path)
     (let ([dir (if (string=? path "") (cdr inst) (cvfs-posix:get-path inst path))])
       (if (directory? dir)
           (directory dir #t)
           #f)))

   ;; mkdir
   (lambda (inst path)
     (create-directory (cvfs-posix:get-path inst path) #t))

   ;; delete
   (lambda (inst path)
     (let ([path (cvfs-posix:get-path inst path)])
       (if (directory? path)
           (delete-directory path #t)
           (if (regular-file? path)
               (delete-file path)
               #f))))
   ;; same device copy
   (lambda (inst f1 f2)
     (file-copy (cvfs-posix:get-path inst f1)
                (cvfs-posix:get-path inst f2)
                #t))

   ;; exists?
   (lambda (inst path)
     (regular-file? (cvfs-posix:get-path inst path)))
   ;; dir?
   (lambda (inst path)
     (directory? (cvfs-posix:get-path inst path))))

  ]
