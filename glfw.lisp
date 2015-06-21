
(defvar libglfw (load-lib "libglfw.so"))
(load-symbol libglfw (quote glfw:init) (quote glfwInit) (type (fcn void)))
(load-symbol libglfw (quote glfw:create-window) (quote glfwCreateWindow) 
	     (type (fcn (ptr void) (width i32) (height i32) 
			(title (ptr char)) (a (ptr void)) (b (ptr void)))))
(load-symbol libglfw (quote glfw:swap-buffers) (quote glfwSwapBuffers) 
	     (type (fcn void (a ( ptr void)))))
(load-symbol libglfw (quote glfw:make-current) (quote glfwMakeContextCurrent) (type (fcn void (win (ptr void)))))
