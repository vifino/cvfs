(use posix srfi-13)

(define (cvfs-posix:get-path inst subpath)
  (string-concatenate/shared (list (cdr inst) subpath)))

(cvfs:create-backend "posix"
 ;; init
 (lambda (root) (string-concatenate (list root "/" )))
 ;; open
 (lambda (inst path mode)
   (if (eq? mode 'write)
       (open-output-file (cvfs-posix:get-path inst path))
       (open-input-file (cvfs-posix:get-path inst path))))
 ;; list
 (lambda (inst path)
   (let ([dir (if (string=? path "") (cdr inst) (cvfs-posix:get-path inst path))])
     (if (directory? dir)
         (directory dir #t)
         #f)))

 ;; delete
 #f
 ;; copy
 #f

 ;; exists?
 (lambda (inst path)
   (regular-file? (cvfs-posix:get-path inst path)))
 ;; dir?
 (lambda (inst path)
   (directory? (cvfs-posix:get-path inst path))))
