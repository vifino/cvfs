;; small vfs thing
;;
;; small definition list:
;;  mode: symbol, 'read or 'write
;;  dpath: string, a unix like filepath, passed to the drivers, ie /path/to/stuff.txt
;;  inst: (state . backend-name), cons of backend specific state and the backend's registered name, a string.

[module cvfs (cvfs:new
              cvfs:create-backend cvfs:call-backend
              cvfs:def-drive cvfs:call-drive)
  (use srfi-1 srfi-13 srfi-69 data-structures)

  ;; backends
  (define-record cvfs:backend
    init ; (init ...), passed on from cvfs, returns state

    open ; (open inst 'mode dpath)
    list ; (list inst dpath)

    delete ; (delete inst dpath)
    copy ; (copy inst dpath dpath)

    exists? ; (dir? inst dpath)
    dir? ; (dir? inst dpath)
    )

  (define cvfs:backends (make-hash-table string=?))
  (define (cvfs:register-backend backend-name backend)
    (hash-table-set! cvfs:backends backend-name backend))

  (define (cvfs:create-backend backend-name
                               init
                               open list
                               delete copy
                               exists dir)
    (cvfs:register-backend backend-name
                           (make-cvfs:backend
                            init
                            open list
                            delete copy exists dir)))

  (define (cvfs:call-backend backend-name fn state . args)
    (apply
     (let ([backend (hash-table-ref cvfs:backends backend-name)])
       (cond ; ugly but i don't know how to access the record elements any better. rip.
        ((eq? fn 'open) (cvfs:backend-open backend))
        ((eq? fn 'list) (cvfs:backend-list backend))
        ((eq? fn 'delete) (cvfs:backend-delete backend))
        ((eq? fn 'copy) (cvfs:backend-copy backend))
        ((eq? fn 'exists?) (cvfs:backend-exists? backend))
        ((eq? fn 'dir?) (cvfs:backend-dir? backend))
        (#t (error "no backend function named ~A" name))))
     (cons
      (cons state backend-name)
      args))

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
        (error "no such backend: ~A" backend-name)))

  (define (cvfs:call-drive vfs drive-name . args)
    (let ([drives (cvfs:vfs-drives vfs)])
      (if (hash-table-exists? drives drive-name)
          (let ([drive (hash-table-ref drives drive-name)])
            (apply cvfs:call-backend
                   (cons backend-name
                         (cons (car drive) args))))
          (error "no such drive: ~A" drive-name))))

  ;; todo: router crap.

  ]
