(ns clojure-ces.example.input
  (:require [play-cljc.gl.core :as pc]
            [play-cljc.macros-java :refer [gl math]]
            [clojure-ces.example.game :as game]
            [clojure-ces.system :as system]
            [nrepl.server :refer [start-server stop-server]]
            )
  (:import  [org.lwjgl.glfw GLFW Callbacks GLFWCursorPosCallbackI GLFWKeyCallbackI]
            [org.lwjgl.opengl GL GL41]
            [org.lwjgl.system MemoryUtil]
            [javax.sound.sampled AudioSystem Clip])
  )

(def state (atom {
                  :mouse-x 0
                  :mouse-y 0
                  :pressed-keys #{}
                  }))

(defn listen-for-mouse [window]
  (GLFW/glfwSetCursorPosCallback window
                                 (reify GLFWCursorPosCallbackI
                                   (invoke [this window xpos ypos]
                                     (swap! state
                                            (fn [state]
                                              (let [*fb-width (MemoryUtil/memAllocInt 1)
                                                    *fb-height (MemoryUtil/memAllocInt 1)
                                                    *window-width (MemoryUtil/memAllocInt 1)
                                                    *window-height (MemoryUtil/memAllocInt 1)
                                                    _ (GLFW/glfwGetFramebufferSize window *fb-width *fb-height)
                                                    _ (GLFW/glfwGetWindowSize window *window-width *window-height)
                                                    fb-width (.get *fb-width)
                                                    fb-height (.get *fb-height)
                                                    window-width (.get *window-width)
                                                    window-height (.get *window-height)
                                                    width-ratio (/ fb-width window-width)
                                                    height-ratio (/ fb-height window-height)
                                                    x (* xpos width-ratio)
                                                    y (* ypos height-ratio)]
                                                (MemoryUtil/memFree *fb-width)
                                                (MemoryUtil/memFree *fb-height)
                                                (MemoryUtil/memFree *window-width)
                                                (MemoryUtil/memFree *window-height)
                                                (assoc state :mouse-x x :mouse-y y))))))))

(defn keycode->keyword [keycode]
  (condp = keycode
    GLFW/GLFW_KEY_LEFT :left
    GLFW/GLFW_KEY_RIGHT :right
    GLFW/GLFW_KEY_UP :up
    GLFW/GLFW_KEY_LEFT_SHIFT :shoot
    nil))

(defn listen-for-keys [window]
  (GLFW/glfwSetKeyCallback window
                           (reify GLFWKeyCallbackI
                             (invoke [this window keycode scancode action mods]
                               (when-let [k (keycode->keyword keycode)]
                                 (condp = action
                                   GLFW/GLFW_PRESS (swap! state update :pressed-keys conj k)
                                   GLFW/GLFW_RELEASE (swap! state update :pressed-keys disj k)
                                   nil))))))

(defonce server (start-server :port 7888))

(defn start [ & [args]]
  (when-not (GLFW/glfwInit)
    (throw (Exception. "Unable to initialize GLFW")))
  (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
  (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR 4)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR 1)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_FORWARD_COMPAT GL41/GL_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_PROFILE GLFW/GLFW_OPENGL_CORE_PROFILE)
  (if-let [window (GLFW/glfwCreateWindow 800 600 "Hello, world!" 0 0)]
    (do
      (GLFW/glfwMakeContextCurrent window)
      (GLFW/glfwSwapInterval 1)
      (GLFW/glfwShowWindow window)
      (GL/createCapabilities)
      (listen-for-mouse window)
      (listen-for-keys window)
      ;; (play-music)
      (let [game (assoc (pc/->game window)
                           :delta-time 0
                           :total-time 0)]
        (gl game enable (gl game BLEND))
        (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))

        (loop [world (game/init-world)]
          (when-not (GLFW/glfwWindowShouldClose window)
            (let [ts (GLFW/glfwGetTime)
                  world (assoc world
                         :delta-time (- ts (or (:total-time world) ts))
                         :total-time ts)
                  world (system/game-loop world)]
              (GLFW/glfwSwapBuffers window)
              (GLFW/glfwPollEvents)
              (recur world)))))
      (Callbacks/glfwFreeCallbacks window)
      (GLFW/glfwDestroyWindow window)
      (GLFW/glfwTerminate))
    (throw (Exception. "Failed to create window"))))