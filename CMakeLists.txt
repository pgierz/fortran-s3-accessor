cmake_minimum_required(VERSION 3.14)
project(fortran-s3-accessor VERSION 0.2.0 LANGUAGES Fortran)

# Set default build type
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

# Fortran compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra -std=f2008")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -fbacktrace -fcheck=all")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -warn all -stand f08")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -check all")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

# Source files - direct implementation
set(LIB_SOURCES
  src/s3_logger.f90
  src/curl_stream.f90
  src/s3_http.f90
  src/s3_io.f90
)

# Create library
add_library(s3_accessor ${LIB_SOURCES})
target_include_directories(s3_accessor PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
  $<INSTALL_INTERFACE:include>
)

# Test executable
add_executable(s3_test app/test_simple.f90)
target_link_libraries(s3_test s3_accessor)

# Installation rules
install(TARGETS s3_accessor
  EXPORT s3_accessor_targets
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)

install(FILES
  ${CMAKE_CURRENT_BINARY_DIR}/curl_stream.mod
  ${CMAKE_CURRENT_BINARY_DIR}/s3_http.mod
  ${CMAKE_CURRENT_BINARY_DIR}/s3_io.mod
  DESTINATION include
)