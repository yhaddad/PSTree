set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${PROJECT_SOURCE_DIR}/cmake/Modules/)

find_package (fastjet REQUIRED)
find_package (ROOT REQUIRED)

include_directories (${FASTJET_INCLUDE_DIRS} ${ROOT_INCLUDE_DIRS})
set (CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} "-pthread -m32")


add_library (PSTree SHARED fInterface.cxx PSTree.cxx ini.cxx INIReader.cxx)
target_link_libraries (PSTree ${FASTJET_LIBRARIES} ${ROOT_LIBRARIES})

#add_executable (test test.cxx)
#target_link_libraries (test PSTree)



install (TARGETS PSTree DESTINATION lib)
install (FILES PSTree.h PSTree.inc fInterface.h ini.h INIReader.h DESTINATION include)
