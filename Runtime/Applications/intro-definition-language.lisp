
(defclass intro-definition-grammar (context-free-grammar)
  ())


(defparameter *intro-definition-grammar-tokens*
  '((:camera :camera)
    (:text :text)
    (:child :child)
    (:position :position)
    (:description :description)
    (:center :center)
    (:object :object)
    (:default-camera :default-camera)
    (:update :update)
    (:no-update :no-update)
    (:color :color)
    (:texture :texture)
    (:scene :scene)
    (:scenes :scenes)
    (:scene-selector :scene-selector)
    (:default-scene-selector :default-scene-selector)
    (:pos :pos)
    (:float :float)
    (make-intro-definition :make-intro-definition)
    (make-scene :make-scene)
    (make-elements-list :make-elements-list)
    (make-elements-from :make-elements-from)
    (make-camera :make-camera)
    (make-hierarchy-group :make-hierarchy-group)
    (make-updater :make-updater)
    (make-cube-object :make-cube-object)
    (make-rectangle-object :make-rectangle-object)
    (make-sphere-object :make-sphere-object)
    (make-polygon-object :make-polygon-object)
    (make-point-object :make-point-object)
    (make-text-object :make-text-object)
    (get-default-camera :camera-get-function)
    (get-ortho-2d-camera :camera-get-function)))


(defun intro-definition-grammar-lexer (grammar)
  (let ((symbol (pop *parser-input*)))
    (if symbol 
        (intro-definition-grammar-get-token grammar symbol)
      nil)))

(defun intro-definition-grammar-get-token (grammar word)
  "Answer the token type of <word> for <grammar>."
  (let ((token-type (search-on-symbol-table (tokens grammar) word)))
    (if (equal token-type :unknown) 
        (setf token-type 
              (if (integerp word) :integer 
                (if (floatp word) :real
                  (if (listp word) :list
                    (if (stringp word) :string
                      (if (arrayp word) :array)))))))
    (when (null token-type) (error (format nil "Unknown token for <~A>" word)))
    (values token-type (list token-type word))))

(defun initialize-intro-definition-grammar-parser (name)
  (intro-definition-grammar-parser name))

(defun intro-definition-grammar-parser (name)
  (eval
   `(defparser ,name
               ;; Start
               ((start intro-definition) 
                $1)
               ((intro-definition :open :make-intro-definition :scene scene :close)
                `((:intro-definition make-intro-definition-scene) ,$4))
               ((intro-definition :open :make-intro-definition :scene-selector :default-scene-selector :scenes scene-list :close)
                `((:intro-definition make-intro-definition-scenes) ,$4 ,$6))
               ;; Scenes
               ((scene-list scene)
                `(:scene-list ,$1))
               ((scene-list scene scene-list)
                `(:scene-list ,$1 ,$2))
               ((scene :open :make-scene :camera camera-description :description scene-description :close)
                `((:scene make-scene) ,$4 ,$6))
               ((camera-description :open :camera-get-function :close)
                `((:camera-description ,$2)))
               ((scene-description elements)
                `(:scene-description ,$1))
               ;; List connectors / iterators
               ((elements elements-from)
                `(:elements ,$1))
               ((elements elements-list)
                `(:elements ,$1))
               ((elements-list :open :make-elements-list element :close)
                `((:elements-list make-elements-list) ,$3))
               ((elements-list :open :make-elements-list element element-list :close)
                `((:elements-list make-elements-list) ,$3 ,$4))
               ((elements-from :open :make-elements-from element :integer :close)
                `((:elements-from make-elements-from) ,$3 ,$4))
               ;; Objects
               ((element-list element element-list)
                `(:element-list ,$1 ,$2))
               ((element-list element)
                `(:element-list ,$1))
               ((element hierarchy-group)
                `(:element ,$1))
               ((element updater)
                `(:element ,$1))
               ((element cube-object)
                `(:element ,$1))
               ((element rectangle-object)
                `(:element ,$1))
               ((element sphere-object)
                `(:element ,$1))
               ((element point-object)
                `(:element ,$1))
               ((element text-object)
                `(:element ,$1))
               ;; Groups
               ((hierarchy-group :open :make-hierarchy-group :update hierarchy-updater-exp :elements elements :close)
                `((:hierarchy-group make-hierarchy-group) ,$2 ,$4 ,$6))
               ((hierarchy-group :open :make-hierarchy-group :update hierarchy-updater-exp :elements elements :child hierarchy-group :close)
                `((:hierarchy-group make-hierarchy-group-child) ,$2 ,$4 ,$6 ,$8))
               ((updater :open :make-updater :update updater-exp :child elements :close)
                `((:updater make-updater) ,$4 ,$6))
               ((updater :no-update)
                `(:updater ,$1))

               ;; Base objects
               ((cube-object :open :make-cube-object side geometry-object-arguments :close)
                `((:cube-object make-cube-object) ,$3 ,$5))
               ((rectangle-object :open :make-rectangle-object side geometry-object-arguments :close)
                `((:rectangle-object make-rectangle-object) ,$3 ,$4))
               ((sphere-object :open :make-sphere-object radius geometry-object-arguments :close)
                `((:sphere-object make-sphere-object) ,$3 ,$4))
               ;; #TODO: Adjust texture (maybe it's not important)
               ((point-object :open :make-point-object geometry-object-arguments :close)
                `((:point-object make-point-object) ,$3 ,$5 ,$7))
               ;; #TODO: Adjust updater
               ((text-object :open :make-text-object text-spec font-name-spec pos-spec color-spec texture-spec :text-update text-updater-exp :close)
                `((:text-object make-text-object) ,$3 ,$4 ,$5 ,$6 ,$8))
               

               ((polygon-object :open 
                                :pos pos-spec :fill-color color-spec :outline-color color-spec :outline-size integer :texture texture-spec polygon-points
                                :close)
                `((:text-object make-text-object) ,$3 ,$5 ,$5 ,$7 ,$9 ,$11))


               ;; Properties
               ((geometry-object-arguments pos-spec)
                `((:arguments make-arguments-pos) ,$1))
               ((geometry-object-arguments color-spec)
                `((:arguments make-arguments-color) ,$1))
               ((geometry-object-arguments texture-spec) 
                `((:arguments make-arguments-texture) ,$1))
               ((geometry-object-arguments update-spec) 
                `((:arguments make-arguments-update) ,$1))
               ((geometry-object-arguments pos-spec color-spec texture-spec update-spec)
                `((:arguments make-arguments-all) ,$1 ,$2 ,$3 ,$4))
   
               ;; Specifications
               ((pos-spec :pos :array)
                $2)
               ((pos-spec :pos :float)
                $2)
               ((color-spec :color :array)
                $2)
               ((color-spec :color :float)
                $2)
               ((texture-spec :texture texture-name-spec)
                $2)
               ((update-spec :update updater-exp)
                $2)

               ;; Text
               ((text-spec string)
                `(:text-spec ,$1))
               ;; Texture spec
               ((texture-name-spec string)
                `(:texture-name-spec ,$1))
               ((font-name-spec string)
                `(:font-name-spec ,$1))
               ;; Updater expressions
               ((updater-exp string)
                `(:updater-exp ,$1))
               ((text-updater-exp string)
                `(:text-updater-exp ,$1))
               ((hierarchy-updater-exp string)
                `(:hierarchy-updater-exp ,$1))
               ((side :real)
                $1)
               ((side :integer)
                $1)
               ((radius :real)
                $1)
               ((radius :integer)
                $1)
               ;; Final terminals
               ((string :string)
                $1)
               ((integer :integer)
                $1))))


(defun intro-definition-grammar-productions ()
  '((start intro-definition)
    (intro-definition :open :make-intro-definition :scene-selector :default-scene-selector :scenes scene-list :close)
    ;; Scenes
    (scene-list scene-list scene)
    (scene-list scene)
    (scene :open :make-scene :camera camera-description :description scene-description :close)
    (camera-description :open :camera-get-function :close)
    (scene-description elements-list)
    (scene-description elements-from)
    ;; List connectors / iterators
    (elements-list :open :make-elements-list element :close)
    (elements-list :open :make-elements-list element element-list :close)
    (elements-from :open :make-elements-from element :integer :close)
    ;; Objects
    (element-list element element-list)
    (element-list element)
    (element hierarchy-group)
    (element updater)
    (element cube-object)
    (element rectangle-object)
    (element sphere-object)
    (element point-object)
    (element text-object)
    ;; Groups
    (hierarchy-group :open :make-hierarchy-group :update hierarchy-updater-exp :elements elements :close)
    (hierarchy-group :open :make-hierarchy-group :update hierarchy-updater-exp :elements elements :child hierarchy-group :close)
    (updater :open :make-updater :update updater-exp :close)
    (updater :no-update)
    ;; Base objects
    (cube-object :open :make-cube-object side pos-texture-specs :update updater-exp :close)
    (rectangle-object :open :make-rectangle-object side pos-texture-specs :update updater-exp :close)
    (sphere-object :open :make-sphere-object radius pos-texture-specs :update updater-exp :close)
    (point-object :open :make-point-object pos-color-specs :update updater-exp :close)
    (text-object :open :make-text-object text-spec font-name-spec pos-texture-specs :update text-updater-exp :close)
    ;; Specifications
    (pos-spec :pos :array)
    (pos-spec :pos :float)
    (text-spec string)
    (texture-name-spec string)
    (font-name-spec string)
    (pos-spec nil)
    (pos-spec :array)
    (color-spec :array)
    ;; Updater expressions
    (updater-exp string)
    (text-updater-exp string)
    (hierarchy-updater-exp string)
    (side :real)
    (side :integer)
    (radius :real)
    (radius :integer)
    ;; Final terminals
    (string :string)
    (integer :integer)))


;; Specific functions
(defun make-intro-definition-scene (scene)
  (make-instance 'intro-definition :scene-selector nil :scenes (list scene)))

(defun make-intro-definition-scenes (selector scenes)
  (make-instance 'intro-definition :scene-selector selector :scenes scenes))

(defun make-scene (camera-description scene-description)
  (make-instance 'scene :camera camera-description :description scene-description))

(defun get-default-camera ()
  (make-instance 'camera-object))

(defun get-ortho-2d-camera ()
  (make-instance 'ortho-2d-camera-object))

(defun make-updater (updater-exp child)
  (if (keywordp updater-exp)
      updater-exp
    (make-instance 'geometry-updater :expression updater-exp :child child)))

(defun make-cube-object (side args)
  ;; #TODO: Add updater with geometry-updater
  (make-instance 'cube-object :pos (gethash :pos args) :side side :texture (gethash :texture args)))

(defun make-rectangle-object (side args)
  ;; #TODO: Add updater with geometry-updater
  (make-instance 'rectangle-object :pos (gethash :pos args) :side side :texture (gethash :texture args)))

(defun make-point-object (size args)
  ;; #TODO: Add updater with geometry-updater
  (make-instance 'point-object :pos (gethash :pos args) :size size :texture (gethash :texture args)))

(defun make-sphere-object (radius args)
  ;; #TODO: Add updater with geometry-updater
  (make-instance 'sphere-object :pos (gethash :pos args) :radius radius :texture (gethash :texture args)))

(defun make-text-object (font-name-spec text args)
  ;; #TODO: Add updater with text-updater-exp
  (make-instance 'text-object :pos (gethash :pos args) :font font-name-spec :text text :texture (gethash :texture args)))

;; #NOTE: Here args represent different polygon points, up to N (8 by the moment)
(defun make-polygon-object (pos fill-color outline-color outline-size texture args)
  (make-instance 'text-object :pos pos :points args :fill-color fill-color :outline-color outline-color :outline-size outline-size :texture texture))

(defun make-elements-list (&rest args)
  args)

(defun make-elements-from (model n &optional variation)
  (loop for i from 0 to n
        collect (copy-cyclic model)))

(defun make-hierarchy-group (hierarchy-updater-exp elements)
  (make-instance 'hierarchy-group-object :elements elements :update hierarchy-updater-exp))

(defun make-hierarchy-group-child (hierarchy-updater-exp elements hierarchy-group)
  (make-instance 'hierarchy-group-object :elements elements :update hierarchy-updater-exp :child hierarchy-group))

(defun make-arguments-pos (pos)
  (let ((table (make-hash-table)))
    (setf (gethash :pos table) pos)
    table))

(defun make-arguments-color (value)
  (let ((table (make-hash-table)))
    (setf (gethash :color table) value)
    table))

(defun make-arguments-texture (value)
  (let ((table (make-hash-table)))
    (setf (gethash :texture table) value)
    table))

(defun make-arguments-update (value)
  (let ((table (make-hash-table)))
    (setf (gethash :update table) value)
    table))

(defun make-arguments-all (pos color texture update)
  (let ((table (make-hash-table)))
    (setf (gethash :update table) update
          (gethash :texture table) texture
          (gethash :color table) color
          (gethash :pos table) pos)
    table))

(defun default-genetic-operators-probability-intro-sequencer ()
  "Answer a structure with default operations with each normalized probability."
  (list (list (system-get 'crossover-cfg)              0.2)
        (list (system-get 'mutate-cfg)                 0.2)
        (list (system-get 'mutate-reuse-cfg)           0.2)
        (list (system-get 'mutate-production-cfg)      0.2)
        (list (system-get 'branch-delete-cfg)          0.2)
        (list (system-get 'mutate-production-cfg)      0.0)))

(setf *intro-sequencer-grammar*
  (make-instance 'intro-definition-grammar
                 :name 'intro-definition-sample-grammar
                 :lexer 'intro-definition-grammar-lexer
                 :parser-initializer 'initialize-intro-definition-grammar-parser
                 :productions (intro-definition-grammar-productions)
                 :crossover-nodes '(:constant
                                     :search-object-description
                                     :task-description-list
                                     :builder-description
                                     :elite-manager-description
                                     :selection-method-description)))

(setf *intro-sequencer-language*
   (make-instance 'cfg-tree-language
                  :name 'intro-sequencer-default-language
                  :grammar *intro-sequencer-grammar*
                  :functions (functions-list-from-tokens *intro-definition-grammar-tokens*)
                  :operators (default-genetic-operators-probability-intro-sequencer)
                  :valid-new-expresion-function 'create-new-first-parent-copy
                  :tokens *intro-definition-grammar-tokens*))

#|
;;; #TEST
(make-instance 'intro-sequencer
               :definition
               '(make-intro-definition 
                 :scene (make-scene 
                          :camera (get-default-camera)
                          :description 
                          (make-elements-list
                           (make-updater 
                            :update ""
                            :child (make-elements-list
                                    (make-text-object "Hi" "Arial" :texture "TEXTURE01" :update "")))))))


(make-instance 'intro-sequencer
               :definition
               '(make-intro-definition 
                 :scene (make-scene 
                          :camera (get-default-camera)
                          :description 
                          (make-elements-list
                           (make-updater 
                            :update ""
                            :child (make-elements-list
                                    (make-rectangle-object 10 :pos #(0 0 0) :texture "TEXTURE01" :update "")
                                    (make-text-object "Hi" "Arial" :pos #(0 0 0) :texture "TEXTURE01" :update "")
                                    (make-sphere-object 10 :pos #(0 0 0) :texture "TEXTURE01" :update "")
                                    (make-sphere-object 10 :pos :float :texture "TEXTURE01" :update "")))))))

(let ((sequencer (make-instance 'intro-sequencer 
                                :definition '(make-intro-definition 
                                              :scene (make-scene 
                                                      :camera (get-ortho-2d-camera)
                                                      :description 
                                                      (make-elements-from
                                                       (make-rectangle-object 20 :pos #(11 20 0) :color #(1 1 0) :texture "" :update "") 10)))
                                :draw-method (make-instance 'direct-render))))
  (make-instance 'pane-intro-sequencer :model sequencer))


:update '(setf (x (pos *object*))
               (* (cos (* (/ 360 *group-elements*) *element-index*))
                  (/ 40.0 *element-level*))
               (y (pos *object*))
               (* (sin (* (/ 360 *group-elements*) *element-index*))
                  (/ 40.0 *element-level*)))

;; :text '((0 . "") (2 . "Hi") (4 . ""))
|#