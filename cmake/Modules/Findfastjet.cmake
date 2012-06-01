# Package finder for FastJet
# Variables that must be set:
# - FASTJET_FOUND
# - FASTJET_INCLUDE_DIRS
# - FASTJET_LIBRARIES
# - FASTJET_DEFINITIONS

set (CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS TRUE)

# Search PATH for fastjet-config
execute_process (COMMAND which fastjet-config
  OUTPUT_VARIABLE fastjet_CONFIG
)

if (fastjet_CONFIG)
  execute_process (COMMAND fastjet-config --prefix
    OUTPUT_VARIABLE fastjet_PREFIX
    )
  if (fastjet_PREFIX)
    string (STRIP ${fastjet_PREFIX} fastjet_PREFIX)
    message (STATUS "Found fastjet-config. FastJet location is ${fastjet_PREFIX}")
  endif ()
  
  # Check FastJet version high enough
  execute_process (COMMAND fastjet-config --version
    OUTPUT_VARIABLE fastjet_VERSION
    )
  if (NOT fastjet_VERSION VERSION_GREATER "2.9")
    message (STATUS "PSTree requires FastJet 3.0+")
  endif ()

  # Get library info from fastjet-config
  execute_process (COMMAND fastjet-config --libs
    OUTPUT_VARIABLE FASTJETLIBS
    )
  string (STRIP "${FASTJETLIBS}" FASTJETLIBS)
  separate_arguments (FASTJETLIBS)
  string (REGEX MATCHALL "-L[^;]+" fastjet_LIBRARY_DIR "${FASTJETLIBS}")
  string (REPLACE "-L" "" fastjet_LIBRARY_DIR "${fastjet_LIBRARY_DIR}")
  string (REPLACE ";" " " fastjet_LIBRARY_DIR "${fastjet_LIBRARY_DIR}")
  string (REGEX MATCHALL "-l[^;]+" fastjet_LIBNAMES "${FASTJETLIBS}")
  string (REPLACE "-l" "" fastjet_LIBNAMES "${fastjet_LIBNAMES}")

  # Get include info from fastjet-config
  execute_process (COMMAND fastjet-config --cxxflags
    OUTPUT_VARIABLE FASTJETFLAGS
    )
  string (STRIP "${FASTJETFLAGS}" FASTJETFLAGS)
  separate_arguments (FASTJETFLAGS)
  string (REGEX MATCHALL "-I[^;]+" fastjet_INCLUDE_DIR "${FASTJETFLAGS}")
  string (REPLACE "-I" "" fastjet_INCLUDE_DIR "${fastjet_INCLUDE_DIR}")
  string (REPLACE ";" " " fastjet_INCLUDE_DIR "${fastjet_INCLUDE_DIR}")
endif()

# Find an example header file
find_path (FASTJET_INCLUDE_DIRS fastjet/ClusterSequence.hh PATH ${fastjet_INCLUDE_DIR})

# Find required libraries
foreach (libname ${fastjet_LIBNAMES})
  unset (lib CACHE)
  find_library (lib ${libname} ${fastjet_LIBRARY_DIR})
  if (lib)
    set (FASTJET_LIBRARIES ${FASTJET_LIBRARIES} ${lib})
  else()
    message (STATUS "Could NOT find library: ${libname}")
  endif()
endforeach()

# Return FASTJET_FOUND=TRUE if everything OK
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (fastjet DEFAULT_MSG FASTJET_LIBRARIES FASTJET_INCLUDE_DIRS)

