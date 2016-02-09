cmake_minimum_required(VERSION 2.8.3)
project(cl_transforms_stamped)

find_package(catkin REQUIRED)

catkin_package()

foreach(dir
    src)
  install(DIRECTORY ${dir}
      DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION}
      PATTERN ".svn" EXCLUDE
      PATTERN ".git" EXCLUDE)
endforeach()

install (FILES cl-transforms-stamped.asd
    DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
