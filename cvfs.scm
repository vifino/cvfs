;; small vfs thing
;;
;; small definition list:
;;  mode: symbol, 'read or 'write are the common ones, 'append or custom things depend on the backend.
;;  dpath: string, a unix like filepath, passed to the drivers, ie /path/to/stuff.txt
;;  inst: (backend-name . state), cons of backend specific state and the backend's registered name, a symbol.

[module cvfs (new
              create-backend call-backend list-backends
              def-drive call-drive
              get-default set-default
              call

              open dir mkdir
              delete copy
              exists? dir?)
  (import chicken scheme)
  (use srfi-1 srfi-13 srfi-69 data-structures regex ports)

  ;; backends
  (define-record backend
    init ; (init ...), passed on from cvfs, returns state

    open ; (open inst dpath mode)
    dir ; (dir inst dpath)
    mkdir ; (mkdir inst dpath), creates parent paths if needed

    delete ; (delete inst dpath)
    copy ; (copy inst dpath dpath)

    exists? ; (dir? inst dpath)
    dir? ; (dir? inst dpath)
    )

  (define backends (make-hash-table))
  (define (list-backends) (hash-table-keys backends))
  (define (register-backend backend-name backend)
    (hash-table-set! backends backend-name backend))

  (define (create-backend backend-name
                               init
                               open dir mkdir
                               delete copy
                               exists dir?)
    (register-backend backend-name
                           (make-backend
                            init
                            open dir mkdir
                            delete copy
                            exists dir?)))

  (define (call-backend backend-name fn state . args)
     (apply
     (let ([backend (hash-table-ref backends backend-name)])
       (cond ; ugly but i don't know how to access the record elements any better. rip.
        ((eq? fn 'open) (backend-open backend))
        ((eq? fn 'dir) (backend-dir backend))
        ((eq? fn 'mkdir) (backend-mkdir backend))
        ((eq? fn 'delete) (backend-delete backend))
        ((eq? fn 'copy) (backend-copy backend))
        ((eq? fn 'exists?) (backend-exists? backend))
        ((eq? fn 'dir?) (backend-dir? backend))
        (#t (error "(cvfs) no backend function" backend-name))))
     (cons
      (cons backend-name state)
      args)))

  ;; drives
  (define-record vfs
    default-drive ; string
    drives ; hash table
    )

  (define (new)
    (make-vfs
     #f
     (make-hash-table string=?)))

  (define (def-drive vfs drive-name backend-name . args)
    (if (hash-table-exists? backends backend-name)
        (hash-table-set!
         (vfs-drives vfs) drive-name
         (cons backend-name
               (apply
                (backend-init (hash-table-ref backends backend-name))
                args)))
        (error "(cvfs) no such backend" backend-name)))

  (define (call-drive vfs drive-name fn . args)
    (let ([drives (vfs-drives vfs)])
      (if (hash-table-exists? drives drive-name)
          (let ([drive (hash-table-ref drives drive-name)])
            (apply call-backend
                   (cons (car drive)
                         (cons fn
                               (cons (cdr drive) args)))))
          (error "(cvfs) no such drive" drive-name))))

  ;; default device
  (define (get-default vfs)
    (let ([default (vfs-default-drive vfs)])
      (if default
          default
          (error "(cvfs) no default vfs drive set"))))
  (define (set-default vfs default)
    (vfs-default-drive-set! vfs default))

  ;; path parsing and fixup
  (define (clean-path path)
    (let ([split (string-split (string-append "/" path) "/")]
          [fn (lambda (self work parsed)
                (if (eq? work '())
                    parsed
                    (self self (cdr work)
                          (cond
                           ((string=? (car work) "..")
                            (if (eq? parsed '())
                                (error "(cvfs) tried to escape root?" path)
                                (cdr parsed)))
                           ((string=? (car work) ".") parsed)
                           (#t (cons (car work) parsed))))))])
      (string-join
       (reverse!
        (fn fn split '()))
       "/")))

  (define fpregexp (regexp "^([A-Za-z0-9\\-_]*:)?(.*)$"))
  (define (parse-path vfs path)
    (let ([parsed (string-match fpregexp path)])
      (if parsed
          (if (cadr parsed)
              (cons (string-trim-right (cadr parsed) #\:) (clean-path (third parsed)))
              (cons (get-default vfs) (clean-path (third parsed))))
          (cons (get-default vfs) ""))))

  ;; router stuff, makes things work
  (define (call vfs fn path . args)
    (cond
     ((eq? fn 'copy) (let ([p1 (parse-path vfs path)] ; copy extrawurst, same device copy just calls 'copy, interdevice must open and pipe the ports
                           [p2 (parse-path vfs (car args))])
                       (if (string=? (car p1) (car p2))
                           (call-drive vfs (car p1) 'copy (cdr p1) (cdr p2)) ; same-device copy
                           (let ([in  (call vfs (car p1) 'open (cdr p1) 'read)] ; inter device copy
                                 [out (call vfs (car p2) 'open (cdr p2) 'write)])
                             (copy-port in out)
                             #t))))
     (#t (let ([parsed (parse-path vfs path)])
           (apply
            call-drive
            (cons vfs
                  (cons (car parsed)
                        (cons fn
                              (cons (cdr parsed)
                                    args)))))))))

  ;; handy aliases
  (define (open vfs path mode)
    (call vfs 'open path mode))
  (define (dir vfs path)
    (call vfs 'dir path))
  (define (mkdir vfs path)
    (call vfs 'mkdir path))
  (define (delete vfs path)
    (call vfs 'delete path))
  (define (copy vfs path1 path2)
    (call vfs 'copy path1 path2))
  (define (exists? vfs path)
    (call vfs 'exists? path))
  (define (dir? vfs path)
    (call vfs 'dir? path))
  ]
