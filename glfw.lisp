
(defvar libglfw (load-lib "libglfw.so"))

(defcmacro glfw:load-sym (lisp-name c-name type)
  (expr (load-symbol libglfw (quote (unexpr lisp-name)) (quote (unexpr c-name)) (type (unexpr type)))))

(glfw:load-sym glfw:init glfwInit (fcn void))
(glfw:load-sym glfw:terminate glfwTerminate (fcn void))
(glfw:load-sym glfw:get-version glfwGetVersion (fcn void (major (ptr i32)) (minor (ptr i32)) (rev (ptr i32))))
(glfw:load-sym glfw:get-version-string glfwGetVersionString (fcn (ptr char)))

(glfw:load-sym glfw:create-window glfwCreateWindow
	       (fcn (ptr void) (width i32) (height i32) 
		    (title (ptr char)) (a (ptr void)) (b (ptr void))))

(glfw:load-sym  glfw:swap-buffers glfwSwapBuffers (fcn void (a ( ptr void))))
(glfw:load-sym glfw:make-current glfwMakeContextCurrent (fcn void (win (ptr void))))
(glfw:load-sym  glfw:poll-events glfwPollEvents (fcn void))



;; clipboard
(glfw:load-sym glfw:set-clipboard-string glfwSetClipboardString (fcn void (win (ptr void)) (_str (ptr char))))
(glfw:load-sym glfw:get-clipboard-string glfwGetClipboardString (fcn (ptr char) (win (ptr void))))


;; event callbacks

 (glfw:load-sym glfw:set-mouse-button-callback glfwSetMouseButtonCallback
		(fcn void (win-ptr (ptr void))
		     (callback (ptr (fcn void (win (ptr void)) (button i32) (action i32) (mods i32))))))
(glfw:load-sym glfw:set-key-callback glfwSetKeyCallback
	       (fcn void (win (ptr void))
		    (callback (ptr (fcn void (win (ptr void)) (key i32) (scancode i32) (action i32) (mods i32))))))

(glfw:load-sym glfw:set-error-callback glfwSetErrorCallback
	       (fcn void (callback (ptr (fcn void (code i32) (str (ptr char)))))))

(glfw:load-sym glfw:set-cursor-pos-callback glfwSetCursorPosCallback
	       (fcn void (win (ptr void)) 
		    (callback (ptr (fcn void (win (ptr void)) (x f64) (y f64))))))

(glfw:load-sym glfw:set-cursor-enter-callback glfwSetCursorEnterCallback
	       (fcn void (win (ptr void)) (callback (ptr (fcn void (win (ptr void)) (entered i32))))))

(glfw:load-sym glfw:set-char-callback glfwSetCharCallback 
	       (fcn void (win (ptr void)) (cb (ptr (fcn void (win (ptr void)) (chr char))))))
(glfw:load-sym glfw:set-scroll-callback glfwSetScrollCallback
	       (fcn void (win (ptr void)) (cb (ptr (fcn void (win (ptr void)) (scroll-x f64)
							(scroll-y f64))))))
(glfw:load-sym glfw:joystick-present? glfwJoystickPresent
	       (fcn bool (joy-idx i32)))
(glfw:load-sym glfw:get-joystick-axes glfwGetJoystickAxes (fcn (ptr f32) (joy i32) (axis-count (ptr i32))))
(glfw:load-sym glfw:get-joystick-buttons glfwGetJoystickButtons (fcn (ptr u8) (joy i32) (button-count (ptr i32))))

