;; small vfs thing
;;
;; small definition list:
;;  mode: symbol, 'read or 'write are the common ones, 'append or custom things depend on the backend.
;;  dpath: string, a unix like filepath, passed to the drivers, ie /path/to/stuff.txt
;;  inst: (backend-name . state), cons of backend specific state and the backend's registered name, a symbol.

[module cvfs (cvfs:new
              cvfs:create-backend cvfs:call-backend cvfs:list-backends
              cvfs:def-drive cvfs:call-drive
              cvfs:get-default cvfs:set-default
              cvfs:call

              cvfs:open cvfs:list cvfs:mkdir
              cvfs:delete cvfs:copy
              cvfs:exists? cvfs:dir?)
  (import chicken scheme)
  (use srfi-1 srfi-13 srfi-69 data-structures regex ports)

  ;; backends
  (define-record cvfs:backend
    init ; (init ...), passed on from cvfs, returns state

    open ; (open inst dpath mode)
    list ; (list inst dpath)
    mkdir ; (mkdir inst dpath), creates parent paths if needed

    delete ; (delete inst dpath)
    copy ; (copy inst dpath dpath)

    exists? ; (dir? inst dpath)
    dir? ; (dir? inst dpath)
    )

  (define cvfs:backends (make-hash-table))
  (define (cvfs:list-backends) (hash-table-keys cvfs:backends))
  (define (cvfs:register-backend backend-name backend)
    (hash-table-set! cvfs:backends backend-name backend))

  (define (cvfs:create-backend backend-name
                               init
                               open list mkdir
                               delete copy
                               exists dir)
    (cvfs:register-backend backend-name
                           (make-cvfs:backend
                            init
                            open list mkdir
                            delete copy
                            exists dir)))

  (define (cvfs:call-backend backend-name fn state . args)
     (apply
     (let ([backend (hash-table-ref cvfs:backends backend-name)])
       (cond ; ugly but i don't know how to access the record elements any better. rip.
        ((eq? fn 'open) (cvfs:backend-open backend))
        ((eq? fn 'list) (cvfs:backend-list backend))
        ((eq? fn 'mkdir) (cvfs:backend-mkdir backend))
        ((eq? fn 'delete) (cvfs:backend-delete backend))
        ((eq? fn 'copy) (cvfs:backend-copy backend))
        ((eq? fn 'exists?) (cvfs:backend-exists? backend))
        ((eq? fn 'dir?) (cvfs:backend-dir? backend))
        (#t (error "(cvfs) no backend function" backend-name))))
     (cons
      (cons backend-name state)
      args)))

  ;; drives
  (define-record cvfs:vfs
    default-drive ; string
    drives ; hash table
    )

  (define (cvfs:new)
    (make-cvfs:vfs
     #f
     (make-hash-table string=?)))

  (define (cvfs:def-drive vfs drive-name backend-name . args)
    (if (hash-table-exists? cvfs:backends backend-name)
        (hash-table-set!
         (cvfs:vfs-drives vfs) drive-name
         (cons backend-name
               (apply
                (cvfs:backend-init (hash-table-ref cvfs:backends backend-name))
                args)))
        (error "(cvfs) no such backend" backend-name)))

  (define (cvfs:call-drive vfs drive-name fn . args)
    (let ([drives (cvfs:vfs-drives vfs)])
      (if (hash-table-exists? drives drive-name)
          (let ([drive (hash-table-ref drives drive-name)])
            (apply cvfs:call-backend
                   (cons (car drive)
                         (cons fn
                               (cons (cdr drive) args)))))
          (error "(cvfs) no such drive" drive-name))))

  ;; default device
  (define (cvfs:get-default vfs)
    (let ([default (cvfs:vfs-default-drive vfs)])
      (if default
          default
          (error "(cvfs) no default vfs drive set"))))
  (define (cvfs:set-default vfs default)
    (cvfs:vfs-default-drive-set! vfs default))

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
              (cons (cvfs:get-default vfs) (clean-path (third parsed))))
          (cons (cvfs:get-default vfs) ""))))

  ;; router stuff, makes things work
  (define (cvfs:call vfs fn path . args)
    (cond
     ((eq? fn 'copy) (let ([p1 (parse-path vfs path)] ; copy extrawurst, same device copy just calls 'copy, interdevice must open and pipe the ports
                           [p2 (parse-path vfs (car args))])
                       (if (string=? (car p1) (car p2))
                           (cvfs:call-drive vfs (car p1) 'copy (cdr p1) (cdr p2)) ; same-device copy
                           (let ([in  (cvfs:call vfs (car p1) 'open (cdr p1) 'read)] ; inter device copy
                                 [out (cvfs:call vfs (car p2) 'open (cdr p2) 'write)])
                             (copy-port in out)
                             #t))))
     (#t (let ([parsed (parse-path vfs path)])
           (apply
            cvfs:call-drive
            (cons vfs
                  (cons (car parsed)
                        (cons fn
                              (cons (cdr parsed)
                                    args)))))))))

  ;; handy aliases
  (define (cvfs:open vfs path mode)
    (cvfs:call vfs 'open path mode))
  (define (cvfs:list vfs path)
    (cvfs:call vfs 'list path))
  (define (cvfs:mkdir vfs path)
    (cvfs:call vfs 'mkdir path))
  (define (cvfs:delete vfs path)
    (cvfs:call vfs 'delete path))
  (define (cvfs:copy vfs path1 path2)
    (cvfs:call vfs 'copy path1 path2))
  (define (cvfs:exists? vfs path)
    (cvfs:call vfs 'exists? path))
  (define (cvfs:dir? vfs path)
    (cvfs:call vfs 'dir? path))
  ]
