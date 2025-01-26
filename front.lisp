(in-package #:lander)

(defstruct panel
  (title "")
  (order 0)
  (icon NIL)
  (link NIL)
  (style NIL)
  (content NIL))

(defun render-content (content)
  (cl-markless:output content :target (plump:make-root) :format 'cl-markless-plump:plump))

(defun panel (panel)
  (etypecase panel
    (string
     (find panel (config :panels) :key #'panel-title :test #'string-equal))
    (integer
     (nth panel (config :panels)))))

(defun panel< (a b)
  (if (= (panel-order a) (panel-order b))
      (string< (panel-title a) (panel-title b))
      (< (panel-order a) (panel-order b))))

(defun index-cache-file ()
  (environment-module-pathname #.*package* :cache #p"front.html"))

(defun purge-cache ()
  (uiop:delete-file-if-exists (index-cache-file)))

(defun update-panels (&key (panels (config :panels)) (output (index-cache-file)))
  (let* ((panels (setf (config :panels) (loop for panel in (sort panels #'panel<)
                                              for i from 1 by 2
                                              do (setf (panel-order panel) i)
                                              collect panel)))
         (result (r-clip:process (@template "front.ctml") :panels panels
                                                          :title (config :title)
                                                          :description (config :description)
                                                          :image (config :image))))
    (handler-bind ((plump:invalid-xml-character #'abort)
                   (plump:discouraged-xml-character #'muffle-warning))
      (let ((plump:*tag-dispatchers* plump:*html-tags*))
        (ensure-directories-exist output)
        (with-open-file (stream output :direction :output :if-exists :supersede)
          (plump:serialize result stream))))))

(defun update-icon-index (file &optional (template (@template "icons.html")))
  (let ((css (alexandria:read-file-into-string file)))
    (with-open-file (stream template :direction :output :if-exists :supersede)
      (cl-ppcre:do-register-groups (cls) ("\\}\\.(fa-[A-Za-z0-9-]+):before(?:,fa-[A-Za-z0-9-]+:before)*\\{content:" css template)
        (format stream "<option value=~s><i class=\"fas ~:*~a\"></i></option>~%" cls)))))

(define-trigger startup ()
  (defaulted-config (list (make-panel :title "Hello!" :icon "fa-hand-sparkles" :content "Welcome to the lander page")) :panels)
  (defaulted-config "Lander" :title)
  (defaulted-config "Landing page portal" :description)
  (defaulted-config NIL :image)
  (purge-cache))

(define-page lander "/" ()
  (let ((path (index-cache-file)))
    (when (or (null (probe-file path))
              (post/get "refresh"))
      (update-panels))
    (serve-file path)))

(define-page edit "/edit" (:access (perm lander) :clip ("edit.ctml" "text/html;charset=utf-8"))
  (r-clip:process T :panels (config :panels)
                    :title (config :title)))

(define-api lander/update (&optional title title[] icon[] link[] content[] order[] style[]) (:access (perm lander))
  (setf (config :title) (or* title))
  (setf (config :panels)
        (loop for title in title[]
              for icon in icon[]
              for link in link[]
              for content in content[]
              for order in order[]
              for style in style[]
              when (string/= "" title)
              collect (make-panel :title title
                                  :icon (or* icon)
                                  :link (or* link)
                                  :content (when (or* content)
                                             (cl-ppcre:regex-replace-all "\\r" content ""))
                                  :order (or (parse-integer order :junk-allowed T) most-positive-fixnum)
                                  :style (or* style))))
  (update-panels)
  (redirect #@"/edit"))

(define-api lander/panel/add (title &optional icon link content order style) (:access (perm lander))
  (when (panel title)
    (error 'api-argument-invalid :argument 'title :message "Panel with this title already exists!"))
  (push (make-panel :title title
                    :icon (or* icon)
                    :link (or* link)
                    :content (when (or* content)
                               (cl-ppcre:regex-replace-all "\\r" content ""))
                    :order (parse-integer order)
                    :style (or* style))
        (config :panels))
  (update-panels)
  (api-output () :message "Panel added."))

(define-api lander/panel/edit (panel &optional title icon link content order style) (:access (perm lander))
  (when (and title (panel title))
    (error 'api-argument-invalid :argument 'title :message "Panel with this title already exists!"))
  (let ((panel (panel panel)))
    (unless panel
      (error 'api-argument-invalid :argument 'panel :message "No panel with this title exists!"))
    (when title (setf (panel-title panel) title))
    (when icon (setf (panel-icon panel) (or* icon)))
    (when link (setf (panel-link panel) (or* link)))
    (when content (setf (panel-content panel) (when (or* content)
                                                (cl-ppcre:regex-replace-all "\\r" content ""))))
    (when order (setf (panel-order panel) (or* order)))
    (when style (setf (panel-style panel) (or* style)))
    (update-panels)
    (api-output () :message "Panel edited.")))

(define-api lander/panel/delete (panel) (:access (perm lander))
  (unless (panel panel)
    (error 'api-argument-invalid :argument 'panel :message "No panel with this title exists!"))
  (setf (config :panels) (remove panel (config :panels) :key #'panel-title :test #'string-equal))
  (update-panels)
  (api-output () :message "Panel deleted."))
