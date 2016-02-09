# cl_tf

Lisp implementation of the TF library, including a ``transform-stamped``-s cacher.

The caching mechanism is illustrated below through an example instance of ``transform-listener``.
(In the example, frames are named by the old convention with the "/" as prefix, the new convention is not to use "/").

The transforms are stored per coordinate frame basis: the listener contains a hash table with transforms for each frame of the TF tree: the keys are the names of the frames, and the values are the cached transforms.

The cache is two-level. The first upper level is on per-second basis, therefore, if the default TF buffer size is 10 seconds, there will be 10 entries in the first cache level. The second level contains all the transforms that happened within one second. Depending on the frequency rate of the ``transform-stamped``-s of the specific frame, different frames can have different amount of entries in the second level.

In the illustration below, the transform between ``/odom`` and ``/base_footprint``, i.e. the location of the origin of ``/base_footprint`` (root frame of the robot's URDF tree) in the ``/odom`` coordinate system, which on a physical robot is used to track the traversed path of the robot w.r.t. the starting point of its trajectory, is published with a frequency of about 100 Hz, hence the 101 entries in the second level of the ``/base_footprint`` cache. In contrast, the pose of origin of ``/odom`` (starting point of robot's trajectory) w.r.t. ``/map`` (origin of the occupancy map of the robot environment, e.g. 2D floor plan) should be constant in case of ideal robot localization on the map and ideal calculation of odometry. In reality it is not ideal, so the localization algorithm keeps adjusting it from time to time. As localization is more computationally intensive etc., the transform between ``/map`` and ``/odom`` is not published as often as the odometry, 30 Hz, hence the 31 entries in the corresponding second-level cache.

The first cache level makes garbage collection very fast. We collect all transforms of the complete 1 second interval at once.

The second level cache is resized on demand. It starts with ``+initial-cache-size+``, which is currently 20, and is resized by the ``+cache-adjust-factor`` (1.5), i.e.: 20, 30, 45, 67, 100, 150, etc. We see the values ``45`` and ``150`` in the illustration below.

![transform-listener](https://github.com/gaya-/roslisp_common/blob/master/cl_tf/transform-listener.png)
