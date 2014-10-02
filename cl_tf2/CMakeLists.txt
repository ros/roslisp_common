cmake_minimum_required(VERSION 2.8.3)
project(cl_tf2)

find_package(catkin REQUIRED) 

catkin_package()

foreach(dir
    asdf
    src)
  install(DIRECTORY ${dir}
      DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION}
      PATTERN ".svn" EXCLUDE
      PATTERN ".git" EXCLUDE)
endforeach()

install (FILES cl-tf2.asd
    DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION})
