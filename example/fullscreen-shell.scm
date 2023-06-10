#!/usr/bin/env -S guile --no-auto-compile -e main
!#
;;; An implementation of https://gitlab.freedesktop.org/wlroots/wlroots/-/blob/0.16/examples/fullscreen-shell.c?ref_type=heads
;; functions retrieved from each module as comments above the module
(use-modules (ice-9 curried-definitions)
             (srfi srfi-71)
             ((ice-9 format) #:select (format))
             (oop goops)
             ((bytestructures guile) #:select (bs:struct bs:pointer))
             ((util572 color) #:select (make-rgba-color))
             ((wayland display) #:select (wl-display-create
                                          wl-display-add-socket-auto
                                          wl-display-run
                                          wl-display-destroy-clients
                                          wl-display-destroy))
             ((wayland list) #:select (make-wl-list
                                       wl-list-remove
                                       wl-list-insert))
             ((wayland listener) #:select (make-wl-listener))
             ((wayland signal) #:select (wl-signal-add))
             ((wlroots backend) #:select (wlr-backend-autocreate
                                          wlr-backend-start))
             ((wlroots render allocator) #:select (wlr-allocator-autocreate))
             ((wlroots render renderer)
              #:select (wlr-renderer-autocreate
                        wlr-renderer-init-wl-display
                        wlr-renderer-begin
                        wlr-renderer-clear
                        wlr-renderer-end
                        wlr-render-texture-with-matrix))
             ((wlroots time) #:select (<timespec> clock-gettime))
             ((wlroots types) #:select (get-event-signal
                                        define-bytestructure-class
                                        %timespec-struct
                                        %wlr-output-struct
                                        %wlr-renderer-struct))
             ((wlroots types compositor) #:select
              (wlr-compositor-create
               wlr-surface-for-each-surface
               wlr-surface-get-texture
               wlr-surface-send-frame-done))
             ((wlroots types fullscreen-shell)
              #:select (wlr-fullscreen-shell-v1-create))
             ((wlroots types matrix) #:select (wlr-matrix-project-box))
             ((wlroots types output) #:select (wrap-wlr-output
                                               wlr-output-init-render
                                               wlr-output-create-global
                                               wlr-output-preferred-mode
                                               wlr-output-set-mode
                                               wlr-output-commit
                                               wlr-output-transform-invert
                                               wlr-output-effective-resolution
                                               wlr-output-attach-render))
             ((wlroots types output-layout)
              #:select (wlr-output-layout-create
                        wlr-output-layout-add-auto))
             ((wlroots util box) #:select (make-wlr-box))
             ((wlroots util log) #:select (wlr-log-init)))

(default-duplicate-binding-handler
  '(merge-generics replace warn-override-core warn last))

(define* (add-listen* obj symbol proc
                      #:key (destroy-when obj)
                      (remove-when-destroy? #t))
  (let ((listener (make-wl-listener proc)))
    (wl-signal-add (get-event-signal obj symbol) listener)

    (when remove-when-destroy?
      (wl-signal-add (get-event-signal destroy-when 'destroy)
                     (make-wl-listener (lambda _
                                         (wl-list-remove (.link listener))))))))

;;; Lines 48–52
;; struct render_data {
;; 	struct wlr_output *output;
;; 	struct wlr_renderer *renderer;
;; 	struct timespec *when;
;; };
(define-bytestructure-class <render-data> ()
  (bs:struct `((output   ,(bs:pointer %wlr-output-struct))
               (renderer ,(bs:pointer %wlr-renderer-struct))
               (when     ,(bs:pointer %timespec-struct))))
  wrap-render-data unwrap-render-data render-data?
  (output   #:getter get-render-data-output   #:init-keyword #:output)
  (renderer #:getter get-render-data-renderer #:init-keyword #:pass)
  (when     #:getter get-render-data-when     #:init-keyword #:when))

;;; Lines 38–46
;; struct fullscreen_output {
;; 	struct wl_list link;
;; 	struct fullscreen_server *server;
;; 	struct wlr_output *wlr_output;
;; 	struct wlr_surface *surface;
;; 	struct wl_listener surface_destroy;

;; 	struct wl_listener frame;
;; };
(define-class <fullscreen-output> ()
  (link    #:getter get-output-link    #:init-keyword #:link)
  (server  #:getter get-output-server  #:init-keyword #:server)
  (output  #:getter get-output-output  #:init-keyword #:output)
  (surface #:getter get-output-surface #:init-keyword #:surface
           #:setter set-output-surface))

;;; Lines 54–55
;; static void render_surface(struct wlr_surface *surface,
;; 		int sx, int sy, void *data) {
(define (render-surface surface sx sy data)
  ;;; Line 56
  ;; struct render_data *rdata = data;
  (let* ((rdata (wrap-render-data data))
         ;;; Line 57
         ;; struct wlr_output *output = rdata->output;
         (output (get-render-data-output data))
         ;;; Line 59
         ;; struct wlr_texture *texture = wlr_surface_get_texture(surface);
         (texture (wlr-surface-get-texture surface)))
    ;;; Line 60
    ;; if (texture == NULL) {
    (when texture
      ;;; Lines 64–69
      ;; struct wlr_box box = {
      ;; 	.x = sx * output->scale,
      ;; 	.y = sy * output->scale,
      ;; 	.width = surface->current.width * output->scale,
      ;; 	.height = surface->current.height * output->scale,
      ;; };
      (let* ((box (make-wlr-box
                   (* sx (.scale output))
                   (* sy (.scale output))
                   (* (.width  (.current surface)) (.scale output))
                   (* (.height (.current surface)) (.scale output))))
             ;;; Lines 72–73
             ;; enum wl_output_transform transform =
             ;; 	wlr_output_transform_invert(surface->current.transform);
             (transform (wlr-output-transform-invert (.transform (.current surface))))
             ;;; Line 71
             ;; float matrix[9];
             ;;; Lines 74–75
             ;; wlr_matrix_project_box(matrix, &box, transform, 0,
             ;; 	output->transform_matrix);
             (matrix (wlr-matrix-project-box box transform 0 (.transform output))))
        ;;; Line 77
        ;; wlr_render_texture_with_matrix(rdata->renderer, texture, matrix, 1);
        (wlr-render-texture-with-matrix (get-render-data-renderer data) texture matrix 1)

        ;;; Line 79
        ;; wlr_surface_send_frame_done(surface, rdata->when);
        (wlr-surface-send-frame-done surface (get-render-data-when data))))))

;;; Listeners
;;; Line 82
;; static void output_handle_frame(struct wl_listener *listener, void *data) {
;;; Lines 83–84
;; struct fullscreen_output *output =
;; 	wl_container_of(listener, output, frame);
;
; (we can skip a call to wl-container-of by just passing in the
; output we created at the time we were adding this listener)
(define ((output-handle-frame fullscreen-output) listener data)
  ;;; Line 85
  ;; struct wlr_renderer *renderer = output->server->renderer;
  (let ((renderer (get-server-renderer (get-output-server fullscreen-output)))
        ;;; Line 89
        ;; int width, height;
        ;;; Line 90
        ;; wlr_output_effective_resolution(output->wlr_output, &width, &height);
        (width+height (wlr-output-effective-resolution (get-output-output fullscreen-output)))
        (_ now (clock-gettime 'CLOCK_MONOTONIC)))

    ;;; Line 92
    ;; if (!wlr_output_attach_render(output->wlr_output, NULL)) {
    (when (wlr-output-attach-render (get-output-output fullscreen-output))
      ;;; Line 96
      ;; wlr_renderer_begin(renderer, width, height);
      (wlr-renderer-begin renderer (car width+height) (cdr width+height))

      ;;; Line 98
      ;; float color[4] = {0.3, 0.3, 0.3, 1.0};
      (let* ((->16as10 (lambda (n) (inexact->exact (round (* 255 n)))))
             (color (make-rgba-color (->16as10 0.3)
                                     (->16as10 0.3)
                                     (->16as10 0.3)
                                     (->16as10 1.0))))
        ;;; Line 99
        ;; wlr_renderer_clear(renderer, color);
        (wlr-renderer-clear renderer color))

      ;;; Line 101
      ;; if (output->surface != NULL) {
      (when (get-output-surface fullscreen-output)
        ;;; Lines 102–106
        ;; struct render_data rdata = {
        ;; 	.output = output->wlr_output,
        ;; 	.renderer = renderer,
        ;; 	.when = &now,
        ;; };
        (let ((rdata (make <render-data>
                       #:output   (get-output-output fullscreen-output)
                       #:renderer renderer
                       #:when     now)))
          ;;; Line 107
          ;; wlr_surface_for_each_surface(output->surface, render_surface, &rdata);
          (wlr-surface-for-each-surface (get-output-surface fullscreen-output)
                                        render-surface rdata)))
      ;;; Line 110
      ;; wlr_renderer_end(renderer);
      (wlr-renderer-end renderer)

        ;;; Line 111
      ;; wlr_output_commit(output->wlr_output);
      (wlr-output-commit (get-output-output fullscreen-output)))))



;;; Lines 117–118
;; static void output_handle_surface_destroy(struct wl_listener *listener,
;;                                           void *data) {
;;; Lines 119–120
;; struct fullscreen_output *output =
;; 	wl_container_of(listener, output, surface_destroy);
;
; (we can skip a call to wl-container-of by just passing in the
; output we created at the time we were adding this listener)
(define ((output-handle-surface-destroy fullscreen-output) listener data)
  ;;; Line 121
  ;; output_set_surface(output, NULL);
  (output-set-surface fullscreen-output #f))

;;; Lines 124–125
;; static void output_set_surface(struct fullscreen_output *output,
;;                                struct wlr_surface *surface) {
(define (output-set-surface fullscreen-output surface)
  ;;; Line 126
  ;; if (output->surface == surface) {
  (unless (equal? (get-output-surface surface) surface)
    ;;; Line 130
    ;; if (output->surface != NULL) {
    (when (get-output-surface surface)
      ;;; Line 131
      ;; wl_list_remove(&output->surface_destroy.link);
      ;
      ; (add-listen*, in addition to adding a listener, adds another
      ; listener to the object's destroy event to do this when the
      ; object is destroyed; so this should get taken care of when
      ; the object is garbage collected)
      ;;; Line 132
      ;; output->surface = NULL;
      (set-output-surface fullscreen-output #f))

    ;;; Line 135
    ;; if (surface != NULL) {
    (when surface
      ;;; Lines 136–137
      ;; output->surface_destroy.notify = output_handle_surface_destroy;
      ;
      ; (defined above, under the output-handle-frame,
      ; rather than stored on the struct (like in the C code))
      ;; wl_signal_add(&surface->events.destroy,
      ;
      ; (technically, wl-signal-add is called in add-listen*)
      (add-listen* surface 'destroy
      ;;               &output->surface_destroy);
                   (output-handle-surface-destroy fullscreen-output))

      ;;; Line 138
      ;; output->surface = surface;
      (set-output-surface surface))

    ;;; Lines 141–142
    ;; wlr_log(WLR_DEBUG, "Presenting surface %p on output %s",
    (format #t "Presenting surface ~p on output ~s"
    ;;         surface, output->wlr_output->name);
               surface  (.name (get-output-output fullscreen-output)))))



;;; Lines 24–36
;; struct fullscreen_server {
;; 	struct wl_display *wl_display;
;; 	struct wlr_backend *backend;
;; 	struct wlr_renderer *renderer;
;; 	struct wlr_allocator *allocator;

;; 	struct wlr_fullscreen_shell_v1 *fullscreen_shell;
;; 	struct wl_listener present_surface;

;; 	struct wlr_output_layout *output_layout;
;; 	struct wl_list outputs;
;; 	struct wl_listener new_output;
;; };
(define-class <fullscreen-server> ()
  (display          #:getter get-server-display)
  (backend          #:getter get-server-backend)
  (renderer         #:getter get-server-renderer)
  (allocator        #:getter get-server-allocator)
  (fullscreen-shell #:getter get-server-fullscreen-shell)
  (output-layout    #:getter get-server-output-layout)
  (outputs          #:getter get-server-outputs))

;;; Listeners
;;; Line 145
;; static void server_handle_new_output(struct wl_listener *listener, void *data) {
;;; Lines 146–147
;; struct fullscreen_server *server =
;; 	wl_container_of(listener, server, new_output);
                                        ;
(define ((server-handle-new-output server) listener data)
  ;;; Line 148
  ;; struct wlr_output *wlr_output = data;
  (let* ((output (wrap-wlr-output data))
         (fullscreen-output (make <fullscreen-output>
                              #:link (make-wl-list)
                              ;;; Line 155
                              ;; output->server = server;
                              #:server  server
                              ;;; Line 154
                              ;; output->wlr_output = wlr_output;
                              #:output  output
                              #:surface #f)))
    ;;; Line 150
    ;; wlr_output_init_render(wlr_output, server->allocator, server->renderer);
    (wlr-output-init-render output
                            (get-server-allocator server)
                            (get-server-renderer server))

    ;;; Lines 156–157
    ;; output->frame.notify = output_handle_frame;
    ;;
    ;; (defined above, under the <fullscreen-output> class,
    ;; rather than stored on the struct (like in the C code))
    ;; wl_signal_add(&wlr_output->events.frame,
    (add-listen* output 'frame
                 ;;               &output->frame);
                 (output-handle-frame fullscreen-output))

    ;;; Line 158
    ;; wl_list_insert(&server->outputs, &output->link);
    (wl-list-insert (get-server-outputs server)
                    (get-output-link fullscreen-output))

    ;;; Line 160
    ;; wlr_output_layout_add_auto(server->output_layout, wlr_output);
    (wlr-output-layout-add-auto (get-server-output-layout server)
                                (get-output-output fullscreen-output))

    ;;; Line 161
    ;; wlr_output_create_global(wlr_output);
    (wlr-output-create-global output)

    ;;; Line 163
    ;; struct wlr_output_mode *mode = wlr_output_preferred_mode(wlr_output);
    (let ((mode (wlr-output-preferred-mode output)))
      ;;; Line 164
      ;; if (mode != NULL) {
      (when mode
        ;;; Line 165
        ;; wlr_output_set_mode(wlr_output, mode);
        (wlr-output-set-mode output mode)))

    ;;; Line 168
    ;; wlr_output_commit(wlr_output);
    (wlr-output-commit output)))

;;; Line  171–172
;; static void server_handle_present_surface(struct wl_listener *listener,
;;                                           void *data) {
;;; Lines 173–174
;; struct fullscreen_server *server =
;; 	wl_container_of(listener, server, present_surface);
;
; (we can skip a call to wl-container-of by just passing in the
; server we created at the time we were adding this listener)
(define ((server-handle-present-surface server) listener data)
  ;;; Line 175
  ;; struct wlr_fullscreen_shell_v1_present_surface_event *event = data;
  (let ((event (wrap-wlr-fullscreen-shell-v1-present-surface-event data)))
    ;;; Lines 177–178
    ;; struct fullscreen_output *output;
    ;; wl_list_for_each(output, &server->outputs, link) {
    (wl-list-for-each (lambda (obj wl-lst)
                        (let ((out-put            (.output event))
                              (fullscreen-out-put (get-output-output obj)))
                          ;;; Line 179
                          ;; if (event->output == NULL || event->output == output->wlr_output) {
                          (when (or (not out-put)
                                    (equal? out-put fullscreen-out-put))
                            ;;; Line 180
                            ;; output_set_surface(output, event->surface);
                            (output-set-surface fullscreen-out-put
                                                (.surface event)))))
                                        ; (&server->outputs – Line 178)
                      (get-server-outputs server)
                                        ; (output – Line 178)
                      <fullscreen-output>
                                        ; (link – Line 178)
                      'link)))


(define-method (initialize (object <fullscreen-server>) args)
  ;; to run the default ~initialize~ method, after
  (next-method)

  ;; Line 207
  ;; server.wl_display = wl_display_create();
  (let* ((display (wl-display-create))
         ;;; Line 208
         ;; server.backend = wlr_backend_autocreate(server.wl_display);
         (backend (wlr-backend-autocreate   display))
         ;;; Line 209
         ;; server.renderer = wlr_renderer_autocreate(server.backend);
         (renderer (wlr-renderer-autocreate  backend))
         ;;; Lines 211–212
         ;; server.allocator = wlr_allocator_autocreate(server.backend,
         ;;                                             server.renderer);
         (allocator (wlr-allocator-autocreate backend renderer))
         ;;; Line 214
         ;; wlr_compositor_create(server.wl_display, server.renderer);
         (compositor (wlr-compositor-create display renderer))
         ;;; Line 216
         ;; server.output_layout = wlr_output_layout_create();
         (layout (wlr-output-layout-create))
         ;;; Line 218
         ;; wl_list_init(&server.outputs);
         (outputs (make-wl-list))
         ;;; Line 222
         ;; server.fullscreen_shell = wlr_fullscreen_shell_v1_create(server.wl_display);
         (shell (wlr-fullscreen-shell-v1-create display)))
    ;;; Line 210
    ;; wlr_renderer_init_wl_display(server.renderer, server.wl_display);
    (wlr-renderer-init-wl-display renderer display)
    (slot-set! object 'display          display)
    (slot-set! object 'backend          backend)
    (slot-set! object 'renderer         renderer)
    (slot-set! object 'allocator        allocator)
    (slot-set! object 'fullscreen-shell shell)
    (slot-set! object 'output-layout    layout)
    (slot-set! object 'outputs          outputs)))



(define-method (run! (obj <fullscreen-server>))
  ;;; Line 233
  ;; if (!wlr_backend_start(server.backend)) {
  (wlr-backend-start (get-server-backend obj))

  ;;; Line 238
  ;; setenv("WAYLAND_DISPLAY", socket, true);
  (setenv "WAYLAND_DISPLAY"
          ;;; Line 227
          ;; const char *socket = wl_display_add_socket_auto(server.wl_display);
          (wl-display-add-socket-auto (get-server-display obj)))

  ;;; Line 247
  ;; wl_display_run(server.wl_display);
  (wl-display-run (get-server-display obj)))

(define-method (cleanup! (obj <fullscreen-server>))
  ;;; Lines 249–250
  ;; wl_display_destroy_clients(server.wl_display);
  ;; wl_display_destroy(server.wl_display);
  (wl-display-destroy-clients (get-server-display obj))
  (wl-display-destroy (get-server-display obj)))

;;; Line 185
;; int main(int argc, char *argv[]) {
(define (main . args)

  ;;; Line 206
  ;; struct fullscreen_server server = {0};
  ;;; ~make~ triggers the ~initialize~ method, above
  (define server (make <fullscreen-server>))
  ;;; Line 186
  ;; wlr_log_init(WLR_DEBUG, NULL);
  (wlr-log-init 'debug)

  (unless (getenv "XDG_RUNTIME_DIR")
    (display "XDG_RUNTIME_DIR must be set.")
    (newline)
    (exit 1))

  ;;; Line 219
  ;; server.new_output.notify = server_handle_new_output;
  ;;
  ;; (defined above, under the <fullscreen-server> class,
  ;; rather than stored on the struct (like in the C code))
  ;;; Line 220
  ;; wl_signal_add(&server.backend->events.new_output,
  (add-listen* (get-server-backend server) 'new-output
               ;;               &server.new_output);
               (server-handle-new-output server))
  ;;; Line 223
  ;; server.present_surface.notify = server_handle_present_surface;
  ;;
  ;; (same as above)
  ;;; Lines 224–225
  ;; wl_signal_add(&server.fullscreen_shell->events.present_surface,
  (add-listen* (get-server-fullscreen-shell server) 'present-surface
               ;;               &server.present_surface);
               (server-handle-present-surface server))

  (run! server)
  (cleanup! server))
