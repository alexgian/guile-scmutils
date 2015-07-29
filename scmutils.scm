(define-module (scmutils))

(eval-when (load compile eval)
           (load-from-path "load.scm"))

;; Add all bindings from generic-environment to the current module
(module-for-each (lambda (sym var)
                   (module-add! (current-module) sym var))
                 generic-environment)

;; Export bindings -- hack to just export everything
(module-for-each (lambda (sym var)
                   (module-replace! (current-module) (list sym)))
                 (current-module))
