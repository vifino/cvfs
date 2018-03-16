(use cvfs cvfs-mem)
(define vfs (cvfs:new))
(cvfs:def-drive vfs "mem" 'mem)

(cvfs:set-default vfs "mem")
(cvfs:list vfs "")
