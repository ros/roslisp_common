cmake_minimum_required(VERSION 2.8.3)
project(cl_urdf)

find_package(catkin REQUIRED COMPONENTS
  roslisp
  cl_transforms
)

catkin_package()

foreach(dir
    src)
  install(DIRECTORY ${dir}
      DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION}
      PATTERN ".svn" EXCLUDE
      PATTERN ".git" EXCLUDE)
endforeach()

install (FILES cl-urdf.asd
    DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
