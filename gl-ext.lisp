(defun gl-uniform-vec2 (void (location i32) (v2 vec2))
  (gl:uniform location 
	      (cast (member v2 x) f32) (cast (member v2 y) f32)))
(defun gl-uniform-vec3 (void (location i32) (v2 vec3))
  (gl:uniform location 
	      (cast (member v2 x) f32) (cast (member v2 y) f32) 
	      (cast (member v2 z) f32)))
(defun gl-uniform-vec4 (void (location i32) (v2 vec4))
  (gl:uniform location 
	      (cast (member v2 x) f32) (cast (member v2 y) f32) 
	      (cast (member v2 z) f32) (cast (member v2 w) f32)))
(overload gl:uniform gl-uniform-vec2)
(overload gl:uniform gl-uniform-vec3)
(overload gl:uniform gl-uniform-vec4)
