(define-module (wlroots xwayland))

(eval-when (eval load compile)
  (begin
    (define %public-modules
      (map (lambda (a) (cons 'wlroots a))
           '((xwayland xwayland))))

    (let* ((current-module (current-module))
           (current-module-interface (resolve-interface (module-name current-module))))
      (for-each
       (lambda (submodule)
         (let ((submodule-interface (resolve-interface submodule)))
           (module-use! current-module submodule-interface)
           (module-use! current-module-interface submodule-interface)))
       %public-modules))))
