(define-module (wlroots backend session)
  #:use-module (wlroots utils)
  #:use-module (wlroots types)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (wlroots backend)
  #:export (wlr-backend-get-session
            wlr-session-change-vt
            wrap-wlr-session
            unwrap-wlr-session))

(define-wlr-types-class wlr-session ())
(define-wlr-procedure (wlr-session-change-vt session vt)
  (ffi:int "wlr_session_change_vt" (list '* ffi:unsigned-int))
  (= (% (unwrap-wlr-session session ) vt) 1))

(define-wlr-procedure (wlr-backend-get-session backend)
  ('* "wlr_backend_get_session" (list '*))
  (wrap-wlr-session (% (unwrap-wlr-backend backend))))
