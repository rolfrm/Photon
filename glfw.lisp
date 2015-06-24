
(defvar libglfw (load-lib "libglfw.so"))
(load-symbol libglfw (quote glfw:init) (quote glfwInit) (type (fcn void)))
(load-symbol libglfw (quote glfw:create-window) (quote glfwCreateWindow) 
	     (type (fcn (ptr void) (width i32) (height i32) 
			(title (ptr char)) (a (ptr void)) (b (ptr void)))))
(load-symbol libglfw (quote glfw:swap-buffers) (quote glfwSwapBuffers) 
	     (type (fcn void (a ( ptr void)))))
(load-symbol libglfw (quote glfw:make-current) (quote glfwMakeContextCurrent) (type (fcn void (win (ptr void)))))
(load-symbol libglfw (quote glfw:poll-events) (quote glfwPollEvents) (type (fcn void)))

(load-symbol libglfw (quote glfw:set-mouse-button-callback) (quote glfwSetMouseButtonCallback)
	     (type (fcn void (win-ptr (ptr void)) 
			(fcn (ptr void)))));(ptr (fcn void (win (ptr void)) (button i32) (action i32) (mods i32)))))))
