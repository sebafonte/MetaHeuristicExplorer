
(defvar *default-sample-search-objects* nil)


(defun system-add-default-objects (&rest args)
  (dolist (i args)
    (system-add i)
    (appendf *default-sample-search-objects* (list i))))

(defun initialize-default-sample-objects ()
  (system-add-default-objects
   ;; XY functions (or black and white graphics too)
   (make-instance 'registrable-object-wrapper 
                  :name 'function-xy-default-1
                  :description "F(XY) - Simple 1"
                  :subject (default-object-in-search
                            (make-instance 'entity-function-x-y
                                           :expresion '(* (sin x) (cos y)))))
   (make-instance 'registrable-object-wrapper 
                  :name 'function-xy-default-2
                  :description "F(XY) - Simple 2"
                  :subject (default-object-in-search
                            (make-instance 'entity-function-x-y
                                           :expresion '(+ 1 x (* x x) (* x x x) (* x x x x)))))
   (make-instance 'registrable-object-wrapper 
                  :name 'function-xy-default-3
                  :description "F(XY) - Simple 3"
                  :subject (default-object-in-search
                            (make-instance 'entity-function-x-y
                                           :expresion ' (/- y (tan (* x y))))))
   (make-instance 'registrable-object-wrapper 
                  :name 'function-xy-default-4
                  :description "F(XY) - Ej. Larson"
                  :subject (default-object-in-search
                            (make-instance 'entity-function-x-y
                                           :expresion '(cos (/- (+ (* x x) (* 2 (* y y))) 4)))))
   ;; XY functions (or black and white graphics too) with SRT language
   (make-instance 'registrable-object-wrapper
                  :name 'function-xy-srt-default-1
                  :description "F(XY) SRT - Simple 1"
                  :subject (default-object-in-search
                            (make-instance 'entity-function-x-y
                                           :expresion '(* (sin x) (cos y)))
                            (system-get 'compression-lisp-math-function-xy)))
   ;; Black and white image objects
   (make-instance 'registrable-object-wrapper 
                  :name 'function-xy-default-5
                  :description "IMAGE - Bad 1"
                  :subject 
                  (default-object-in-search
                   (make-instance 
                    'entity-function-x-y
                    :expresion '(- 0.139 (* (sin (* x y)) (- 0.139 (* (abs (cos x)) (cos x))))))))
   (make-instance 'registrable-object-wrapper 
                  :name 'function-xy-default-6
                  :description "IMAGE - Ok 1"
                  :subject 
                  (default-object-in-search
                   (make-instance 
                    'entity-function-x-y
                    :expresion '(/- (cos y) (- (cos x) (* (sin (* x y)) (+ 0.139 (* (cos y) (cos x)))))))))
   (make-instance 
    'registrable-object-wrapper 
    :name 'function-xy-default-7
    :description "IMAGE - Ok 2"
    :subject 
    (default-object-in-search
     (make-instance 'entity-function-x-y
                    :expresion '(- (sin (* x y)) (* (cos x) (/- 0.139 (+ 0.139 (* (cos x) (cos y)))))))))
   (make-instance 
    'registrable-object-wrapper 
    :name 'function-xy-default-8
    :description "IMAGE - Medium 1"
    :subject 
    (default-object-in-search
     (make-instance 'entity-function-x-y
                    :expresion '(- (sin (* x y)) (+ -0.653 (/- 0.139 (* 0.78907234 (* (cos x) (cos y)))))))))
   (make-instance 
    'registrable-object-wrapper 
    :name 'function-xy-default-9
    :description "IMAGE - Oval"
    :subject 
    (default-object-in-search
     (make-instance 
      'entity-function-x-y
      :expresion '(/- 0.139 (- (cos y) (* (+ (cos y) (cos (/- (cos y) (cos x)))) (cos x)))))))
   (make-instance 
    'registrable-object-wrapper 
    :name 'function-xy-default-10
    :description "IMAGE - Medium, stretched"
    :subject 
    (default-object-in-search
     (make-instance 
      'entity-function-x-y
      :expresion '(/- 0.139 (- (cos (cos x)) (* (sin (* x y)) (+ 0.139 (+ (cos y) (cos x)))))))))
   (make-instance 
    'registrable-object-wrapper 
    :name 'example-2d-animation-1
    :description "Example 2D animation 1"
    :subject 
    (default-object-in-search
     (make-instance 
      'entity-function-x-y
      :expresion '(/- 0.139 (- (cos y) (* (+ (cos y) (cos (/- (cos *time-variable*) (cos x)))) (cos x)))))))
   ;; Black and white image objects (with SRT language)
   (make-instance 
    'registrable-object-wrapper 
    :name 'function-xy-default-11
    :description "F(XY) SRT example 1"
    :subject 
    (default-object-in-search
     (make-instance 
      'entity-function-x-y
      :expresion '(sin (* x y)))
     (system-get 'compression-lisp-math-function-xy)))
   ;; Texture deformation separate image objects
   (make-instance 
    'registrable-object-wrapper 
    :name 'texture-separate-1
    :description "Texture deformation separate 1"
    :subject (default-object-in-search
              (make-instance 'entity-texture-deformation-separate :expresion '(values x y))
              (system-get 'rgb-color-images-separate)))
   (make-instance 
    'registrable-object-wrapper 
    :name 'texture-separate-2
    :description "Texture deformation separate 2"
    :subject (default-object-in-search
              (make-instance 'entity-texture-deformation-separate 
                             :expresion '(VALUES 
                                          (+ Y X)
                                          (* 0.087503366
                                             (* (TAN (COS 
                                                      (/- 0.12655929
                                                          (TAN (* (/- (SIN (COS X))
                                                                      0.081989765)
                                                                  (COS (/- X Y)))))))
                                                X))))
              (system-get 'rgb-color-images-separate)))
   (make-instance 
    'registrable-object-wrapper 
    :name 'texture-separate-3
    :description "Texture deformation separate 3"
    :subject (default-object-in-search
              (make-instance 'entity-texture-deformation-separate 
                             :expresion '(VALUES 0.20364399
                                                 (/- (+ Y
                                                        (- (* X
                                                              (/- Y
                                                                  0.11360502))
                                                           (* (TAN (* X
                                                                      (/- 0.89675177
                                                                          0.11360502)))
                                                              (TAN (+ Y
                                                                      (- (* X
                                                                            (/- Y
                                                                                0.11360502))
                                                                         (* (TAN (* X
                                                                                    (/- 0.89675177
                                                                                        0.11360502)))
                                                                            (COS (/- Y
                                                                                     0.11360502)))))))))
                                                     (TAN (/- (TAN (TAN 0.7158174))
                                                              (COS (COS (COS (SIN 0.7432082)))))))))
              (system-get 'rgb-color-images-separate)))
   (make-instance 
    'registrable-object-wrapper 
    :name 'texture-separate-4
    :description "Texture deformation separate 4"
    :subject (default-object-in-search
              (make-instance 'entity-texture-deformation-separate 
                             :expresion '(VALUES (COS (TAN (TAN (/- (* 0.017616615
                                                                       (+ X
                                                                          X))
                                                                    0.64455295))))
                                                 (/- (+ Y
                                                        (- 0.017616615
                                                           (- (SIN (* X
                                                                      (/- (* X
                                                                             (/- Y
                                                                                 0.11360502))
                                                                          0.11360502)))
                                                              (TAN (/- Y
                                                                       0.11360502)))))
                                                     (* (- Y
                                                           (SIN (SIN (* (/- X
                                                                            X)
                                                                        (* X
                                                                           (SIN 0.028516166))))))
                                                        (COS (/- (+ 0.27120072
                                                                    X)
                                                                 0.07031121))))))
              (system-get 'rgb-color-images-separate)))
   (make-instance 
    'registrable-object-wrapper 
    :name 'texture-separate-5
    :description "Texture deformation separate 5"
    :subject (default-object-in-search
              (make-instance 'entity-texture-deformation-separate 
                             :expresion '(VALUES (COS (SIN (* (* 0.20364399
                                                                 (+ X
                                                                    (TAN (+ (- (COS 0.12536875)
                                                                               0.072468005)
                                                                            0.64455295))))
                                                              0.64455295)))
                                                 (/- (+ Y
                                                        (- (* X
                                                              (/- Y
                                                                  0.11360502))
                                                           (* (TAN (* X
                                                                      (/- 0.89675177
                                                                          0.11360502)))
                                                              (TAN (/- Y
                                                                       0.11360502)))))
                                                     (TAN (/- (TAN (TAN 0.7158174))
                                                              (COS (COS (COS (SIN 0.7432082)))))))))
              (system-get 'rgb-color-images-separate)))
   (make-instance 
    'registrable-object-wrapper 
    :name 'texture-separate-6
    :description "Texture deformation separate 6"
    :subject (default-object-in-search
              (make-instance 'entity-texture-deformation-separate 
                             :expresion '(VALUES 0.8494017
                                                 (/- (+ Y
                                                        (- (/- X
                                                               (/- Y
                                                                   0.11360502))
                                                           (* (TAN (* X
                                                                      (/- 0.89675177
                                                                          0.11360502)))
                                                              (TAN (/- Y
                                                                       0.11360502)))))
                                                     (TAN (/- (TAN (TAN 0.7158174))
                                                              (COS (COS (COS (SIN 0.7432082)))))))))
              (system-get 'rgb-color-images-separate)))
   ;; Color rgb image objects
   (make-instance 
    'registrable-object-wrapper 
    :name 'rgb-default-1
    :description "RGB Image 1"
    :subject 
    (default-object-in-search
     (make-instance 'entity-image-rgb :expresion '(vec-sin x))))
   (make-instance 
    'registrable-object-wrapper 
    :name 'rgb-default-2
    :description "RGB Image 2"
    :subject 
    (default-object-in-search
     (make-instance 'entity-image-rgb :expresion '(COLOR-MAP-3 (VEC-/- 0.24419174 (COLOR-MAP-3 (COLOR-MAP-3 0.25484648 Y Y) Y X)) (COLOR-MAP-3 (VEC-/- 0.24419174 (COLOR-MAP-3 (COLOR-MAP-3 0.25484648 Y Y) Y X)) (VEC-/- (VEC-INOISE-X-Y X X) (COLOR-MAP-1 X 0.70072377)) (VEC-SIN (COLOR-MAP-1 Y 0.69074375))) (VEC-+ (VEC-+ Y 0.8535191) (VEC-COS Y))))))
   (make-instance 
    'registrable-object-wrapper 
    :name 'rgb-default-3
    :description "RGB Image 3"
    :subject 
    (default-object-in-search
     (make-instance 'entity-image-rgb :expresion '(COLOR-MAP-3 (VEC-/- 0.24419174 (COLOR-MAP-3 (COLOR-MAP-3 0.25484648 Y Y) Y X)) (VEC-/- (VEC-INOISE-X-Y X X) (VEC-* (COLOR-MAP-1 X 0.27842534) (VEC-+ X Y))) (VEC-+ (VEC-+ Y 0.8535191) (VEC-COS Y))))))
   (make-instance 
    'registrable-object-wrapper 
    :name 'rgb-default-4
    :description "RGB Image 4"
    :subject 
    (default-object-in-search
     (make-instance 'entity-image-rgb :expresion '(COLOR-MAP-3 (VEC-/- (VEC-PERLIN-X-Y Y 0.32249382) (COLOR-MAP-3 (COLOR-MAP-3 0.25484648 Y Y) Y X)) (VEC-/- (VEC-INOISE-X-Y X X) (VEC-* (VEC-+ Y X) (COLOR-MAP-1 0.5710345 X))) (VEC-+ (VEC-/- (VEC-PERLIN-X-Y Y 0.32249382) (COLOR-MAP-3 (COLOR-MAP-3 0.25484648 Y Y) Y X)) (VEC-COS Y))))))
   (make-instance 
    'registrable-object-wrapper 
    :name 'rgb-default-5
    :description "RGB Image 5"
    :subject 
    (default-object-in-search
     (make-instance 'entity-image-rgb :expresion '(COLOR-MAP-3 (VEC-/- 0.24419174 (COLOR-MAP-3 (COLOR-MAP-3 0.25484648 Y Y) Y X)) (VEC-/- (VEC-INOISE-X-Y Y (VEC-/- 0.24419174 (COLOR-MAP-3 (COLOR-MAP-3 0.25484648 Y Y) Y X))) (VEC-* (COLOR-MAP-1 X 0.27842534) (VEC-+ X Y))) (VEC-+ (VEC-+ Y 0.8535191)  (VEC-COS Y))))))
   ;; Color rgb image objects (with SRT language)
   (make-instance 'registrable-object-wrapper
                  :name 'rgb-srt-default-1
                  :description "RGB with SRT image 1"
                  :subject (default-object-in-search
                            (make-instance 'entity-image-rgb :expresion '(vec-sin x))
                            (system-get 'rgb-color-images-srt)))
   ;; LOP simple example objects
   (make-instance 'registrable-object-wrapper
                  :name 'lop-default-1
                  :description "LOP - Simple demo example"
                  :subject (default-object-in-search 
                            (make-instance 'entity-linear-ordering)))
   (make-instance 'registrable-object-wrapper
                  :name 'lop-marti-1
                  :description "LOP - Marti 100x100 1 easy"
                  :subject (default-object-in-search
                            (make-instance 'entity-linear-ordering)))
   (make-instance 'registrable-object-wrapper
                  :name 'lop-marti-2
                  :description "LOP - Marti 100x100 1 hard"
                  :subject (default-object-in-search
                            (make-instance 'entity-linear-ordering)))))

(defun default-sample-search-objects ()
  *default-sample-search-objects*)

(defun default-object-in-search (object &optional language)
  "Answer a default object-in-search instance for <object>."
  (let ((task (make-instance 'search-task)))
    (set-value-for-property-named task 'objetive-class (class-name (class-of object)))
    (when language (setf (language task) language))
    (prepare-to-search task)
    (make-instance 'object-in-search :object object :context task)))
