;; in-memory backend for cvfs
;; uses srfi-69, no directories, really.

[module cvfs-mem ()
  (import chicken scheme)
  (use cvfs ports srfi-13 srfi-69)

  (create-backend
   'mem
   ;; init
   (lambda () (make-hash-table string=?))
   ;; open
   (lambda (inst path mode)
     (if (eq? mode 'read)
         (if (hash-table-exists? (cdr inst) path)
             (open-input-string (hash-table-ref (cdr inst) path))
             (error "(cvfs-mem) can't open non-existant file" path))
         (let ([buf (open-output-string)]
               [hashtable (cdr inst)])
           (begin
             (if (and (eq? mode 'append) (hash-table-exists? hashtable path))
                 (display (hash-table-ref hashtable path) buf))
             (make-output-port
              (lambda (str) (display str buf))
              (lambda () (begin
                           (hash-table-set! hashtable path (get-output-string buf))
                           (close-output-port buf))))))))
   ;; dir
   (lambda (inst path)
     (hash-table-keys (cdr inst)))

   ;; mkdir
   (lambda (inst path)
     #f)

   ;; delete
   (lambda (inst path)
     (hash-table-delete! (cdr inst) path))
   ;; same device copy
   (lambda (inst f1 f2)
     (hash-table-set! (cdr inst) f2
                      (hash-table-ref (cdr inst) f1)))

   ;; exists?
   (lambda (inst path)
     (hash-table-exists? (cdr inst) path))
   ;; dir?
   (lambda (inst path)
     #f))

  ]
