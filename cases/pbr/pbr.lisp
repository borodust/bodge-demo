(cl:in-package :cl-bodge.pbr.demo)

;;;
;;; This demo uses DamagedHelmet model from https://github.com/KhronosGroup/glTF-WebGL-PBR
;;;
;;; Model: https://sketchfab.com/models/b81008d513954189a063ff901f7abfe4
;;;

(ge:defshader (pbr-vert
               (:sources "pbr-vert.glsl")
               (:base-path (merge-showcase-pathname "pbr/")))
  (position :name "a_Position" :size 3)
  (normal :name "a_Normal" :size 3)
  (tangent :name "a_Tangent" :size 3)
  (uv :name "a_UV" :size 2)
  (mvp :name "u_MVPMatrix")
  (model-mat :name "u_ModelMatrix")
  (normal-mat :name "u_NormalMatrix"))


(ge:defshader (pbr-frag
               (:sources "pbr-frag.glsl")
               (:base-path (merge-showcase-pathname "pbr/")))
  (light-direction :name "u_LightDirection")
  (light-color :name "u_LightColor")
  (camera :name "u_Camera")
  ;; IBL
  (diffuse-env-sampler :name "u_DiffuseEnvSampler")
  (specular-env-sampler :name "u_SpecularEnvSampler")
  (brdf-lut :name "u_brdfLUT")
  ;; Base color
  (base-color-sampler :name "u_BaseColorSampler")
  (base-color-factor :name "u_BaseColorFactor")
  ;; Normal Mapping
  (normal-sampler :name "u_NormalSampler")
  (normal-scale :name "u_NormalScale")
  ;; Emissive Map
  (emissive-sampler :name "u_EmissiveSampler")
  (emissive-factor :name "u_EmissiveFactor")
  ;; Metallic-roughness
  (metallic-roughness-sampler :name "u_MetallicRoughnessSampler")
  (metallic-roughness-values :name "u_MetallicRoughnessValues")
  ;; Occulusion
  (occlusion-sampler :name "u_OcclusionSampler")
  (occlusion-strength :name "u_OcclusionStrength")
  ;; Debug params
  (scale-diff-base :name "u_ScaleDiffBaseMR")
  (scale-fgd-spec :name "u_ScaleFGDSpec")
  (scale-ibl-ambient :name "u_ScaleIBLAmbient"))


(defparameter *scale-diff-base* (ge:vec4 0.0 0.0 0.0 0.0))
(defparameter *scale-fgd-spec* (ge:vec4 0.0 0.0 0.0 0.0))
(defparameter *scale-ibl-ambient* (ge:vec4 0.2 0.2))


(ge:defpipeline pbr-pipeline
  :vertex pbr-vert
  :fragment pbr-frag)


(defparameter *projection-matrix* (ge:perspective-projection-mat 1 (/ 600 800) 1 10))


;;;
;;; SHOWCASE
;;;
(defclass pbr-showcase ()
  ((pipeline :initform (mt:make-guarded-reference nil))
   (scene :initform nil)
   (brdf-tex :initform nil)
   (ibl-diffuse :initform nil)
   (ibl-specular :initform nil)
   (shared-context :initform nil)))


(defmethod initialize-instance :after ((this pbr-showcase) &key)
  (ge:mount-container "/bodge/demo/pbr/helmet/" (merge-showcase-pathname "pbr/assets/DamagedHelmet.brf"))
  (ge:mount-filesystem "/bodge/demo/pbr/assets/" (merge-showcase-pathname "pbr/assets/")))


(register-showcase 'pbr-showcase "PBR")


(defmethod showcase-revealing-flow ((this pbr-showcase) ui)
  (with-slots (pipeline scene brdf-tex ibl-diffuse ibl-specular shared-context) this
    (flet ((~scene-init ()
             (ge:for-graphics :context shared-context ()
               (setf scene (make-instance 'pbr-scene
                                          :resource (ge:load-resource "/bodge/demo/pbr/helmet/DamagedHelmet")
                                          :base-path "/bodge/demo/pbr/helmet/")
                     brdf-tex (load-brdf-texture)
                     ibl-diffuse (load-diffuse-ibl-cubemap)
                     ibl-specular (load-specular-ibl-cubemap))))
           (~pipeline-init ()
             (ge:for-graphics ()
               (mt:with-guarded-reference (pipeline)
                 (setf pipeline (ge:make-shader-pipeline 'pbr-pipeline))))))
      (ge:>> (ge:graphics-context-assembly-flow)
             (ge:instantly (context)
               (setf shared-context context)
               (ge:run (ge:>> (~scene-init)
                              (~pipeline-init))))))))


(defmethod showcase-closing-flow ((this pbr-showcase))
  (with-slots (pipeline scene brdf-tex ibl-diffuse ibl-specular shared-context) this
    (ge:instantly ()
      (ge:run
       (ge:>>
        (ge:for-graphics ()
          (mt:with-guarded-reference (pipeline)
            (ge:dispose pipeline)
            (setf pipeline nil)))
        (ge:for-graphics :context shared-context ()
          (ge:dispose scene)
          (ge:dispose brdf-tex)
          (ge:dispose ibl-diffuse)
          (ge:dispose ibl-specular)))))))


(defun render-helmet (this pipeline)
  (with-slots (scene brdf-tex ibl-diffuse ibl-specular) this
    (let* ((time (bodge-util:epoch-seconds))
           (model-mat (ge:mult (ge:translation-mat4-homo 0.3 0 -4)
                               (ge:euler-angles->mat4-homo (ge:vec3 (+ (/ pi 2) (/ (sin time) 2))
                                                                    0
                                                                    (+ pi (/ (cos time) 0.5))))))
           (view-mat (ge:identity-mat4))
           (view-model-mat (ge:mult view-mat model-mat))
           (mvp (ge:mult *projection-matrix*
                         view-model-mat))
           (normal-mat (ge:inverse (ge:transpose (ge:value->mat3 (ge:mult view-model-mat))))))
      (do-scene-meshes (mesh id scene)
        (ge:render t pipeline
                   :primitive (primitive-of mesh)
                   :index-buffer (index-array-of mesh)
                   'position (position-array-of mesh)
                   'normal (normal-array-of mesh)
                   'tangent (tangent-array-of mesh)
                   'uv (tex-coord-array-of mesh)

                   'mvp mvp
                   'model-mat model-mat
                   'normal-mat normal-mat

                   'light-direction (ge:vec3 0 1 0)
                   'light-color (ge:vec3 1.0 1.0 1.0)
                   'camera (ge:vec3 0 0 0)

                   'base-color-factor (ge:vec4 1.0 1.0 1.0 1.0)
                   'base-color-sampler (scene-texture scene "Default_albedo.jpg")

                   'normal-sampler (scene-texture scene "Default_normal.jpg")
                   'normal-scale 1f0

                   'emissive-sampler (scene-texture scene "Default_emissive.jpg")
                   'emissive-factor (ge:vec3 0.7 0.7 0.7)

                   'metallic-roughness-sampler (scene-texture scene "Default_metalRoughness.jpg")
                   'metallic-roughness-values (ge:vec2 1.0 1.0)

                   'occlusion-sampler (scene-texture scene "Default_AO.jpg")
                   'occlusion-strength 1f0

                   'diffuse-env-sampler ibl-diffuse
                   'specular-env-sampler ibl-specular
                   'brdf-lut brdf-tex

                   'scale-diff-base *scale-diff-base*
                   'scale-fgd-spec *scale-fgd-spec*
                   'scale-ibl-ambient *scale-ibl-ambient*)))))


(defmethod render-showcase ((this pbr-showcase))
  (with-slots (pipeline) this
    (ge:clear-rendering-output t :color (ge:vec4 0.2 0.2 0.2 1.0))
    (mt:with-guarded-reference (pipeline)
      (if pipeline
          (render-helmet this pipeline)
          (render-loading-screen (ge:vec4 1 1 1 1))))))
