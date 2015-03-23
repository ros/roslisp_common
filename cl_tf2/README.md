# CL-TF2

## EXAMPLE USAGE

### Requirements
Make sure you have either installed the following packages from debians
or have them checked out (and compiled) in your ROS workspace:

```roslisp, roslisp_common, roslisp_repl, tf2_ros```

### Setup
Let's start a roscore, a buffer-server, and make sure tf has one simple transform to play with:

```$ roscore```

```$ rosrun tf2_ros buffer_server```

```$ rosrun tf static_transform_publisher 0 0 0.1 0 0 0 a b 100```

Enter the REPL:

```$ roslisp_repl```

```lisp
CL-USER> (asdf:load-system :cl-tf2)
CL-USER> (swank:set-package "CL-TRANSFORMS-PLUGIN")
CL-TRANSFORMS-PLUGIN> (roslisp:start-ros-node "tf2-test")
CL-TRANSFORMS-PLUGIN> (defparameter *buffer* (make-buffer-client))
CL-TRANSFORMS-PLUGIN> (defparameter *broadcaster* (make-broadcast-publisher))
```

This should have worked all fine without any warning or error. You can use the ros topic
commandline tools to check that the relevant connections have been set up:
```
$ rostopic info /tf2_buffer_server/goal
Type: tf2_msgs/LookupTransformActionGoal

Publishers: 
 * /tf2-test (...)

Subscribers: 
 * /tf_buffer (...)
```
```
$ rostopic info /tf
Type: tf2_msgs/TFMessage

Publishers: 
 * /static_transform_publisher_1427118667416645421 (...)
 * /tf2-test (...)

Subscribers: 
 * /tf_buffer (...)
```
```
$ rostopic info /tf_static
Type: tf2_msgs/TFMessage

Publishers: 
 * /tf2-test (...)

Subscribers: 
 * /tf_buffer (...)
```

### Transforming objects
```CL-TF2``` ships with a plugin for stamped points, poses, and transforms. These datatypes
are based on ```CL-TRANSFORMS```. Let's try to transform some of them using TF!

First we need some data to play with:
```lisp
CL-TRANSFORMS-PLUGIN> (defparameter *point-stamped* 
                        (make-point-stamped
                         (cl-transforms:make-3d-vector 0.2 0.0 0.0) "b" 0.0))
CL-TRANSFORMS-PLUGIN> (defparameter *pose-stamped*
                        (make-pose-stamped
                         (cl-transforms:make-pose
                          (cl-transforms:make-identity-vector)
                          (cl-transforms:axis-angle->quaternion
                           (cl-transforms:make-3d-vector 0.0 0.0 1.0) (/ PI 2.0)))
                         "a" 0.0))
```

Here is how to ask ```TF``` to express our point in frame A:
```lisp
CL-TRANSFORMS-PLUGIN> (do-transform *buffer* *point-stamped* "a")
#<POINT-STAMPED 
  HEADER:
    #<HEADER (FRAME-ID: "a" STAMP: 1.4271194756062057d9)>
  POINT:
    #<3D-VECTOR (0.20000000298023224d0 0.0d0 0.1d0)>
```

Similarly, you ```TF``` can represent our pose in frame B:
```lisp
CL-TRANSFORMS-PLUGIN> (do-transform *buffer* *pose-stamped* "b")
#<POSE-STAMPED 
  HEADER:
    #<HEADER (FRAME-ID: "b" STAMP: 1.4271196155499847d9)>
  POSE:
    #<POSE 
   #<3D-VECTOR (0.0d0 0.0d0 -0.1d0)>
   #<QUATERNION (0.0d0 0.0d0 0.7071067811865475d0 0.7071067811865476d0)>>>
   ```

### Broadcasting transforms
You can also tell the external world about static and volatile transforms using ```CL-TF2```.

Let's inform the ```buffer-server``` about a new frame C, statically linked to frame B:
```lisp
CL-TRANSFORMS-PLUGIN> (defparameter *transform-stamped*
                        (make-transform-stamped
                         "b" "c" (ros-time)
                         (cl-transforms:make-transform
                          (cl-transforms:make-3d-vector 0.0 0.3 0.0)
                          (cl-transforms:make-identity-rotation))))
CL-TRANSFORMS-PLUGIN> (send-static-transform *broadcaster* *transform-stamped*)
```

Now, we can again ask whether ```TF``` has a transform between the frames A and C:
```lisp
CL-TRANSFORMS-PLUGIN> (has-transform *buffer* "c" "a" (ros-time) 0.2)
[GEOMETRY_MSGS-MSG:TRANSFORMSTAMPED
   HEADER:
     (STD_MSGS-MSG:HEADER (:SEQ . 0) (:STAMP . 1.427120208047d9)
      (:FRAME_ID . "c"))
   CHILD_FRAME_ID:
     "a"
   TRANSFORM:
     (GEOMETRY_MSGS-MSG:TRANSFORM
      (:TRANSLATION
       . [GEOMETRY_MSGS-MSG:VECTOR3
            X:
              0.0d0
            Y:
              -0.30000001192092896d0
            Z:
              -0.1d0])
      (:ROTATION
       . [GEOMETRY_MSGS-MSG:QUATERNION
            X:
              0.0d0
            Y:
              0.0d0
            Z:
              0.0d0
            W:
              1.0d0]))]
```
