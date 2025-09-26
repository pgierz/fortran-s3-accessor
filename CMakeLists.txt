cmake_minimum_required(VERSION 3.14)
project(fortran-s3-accessor VERSION 0.1.0 LANGUAGES Fortran)

# Set default build type
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

# Get version from git
find_package(Git)
if(GIT_FOUND)
  execute_process(
    COMMAND ${GIT_EXECUTABLE} describe --tags --always --dirty
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )
  execute_process(
    COMMAND ${GIT_EXECUTABLE} rev-parse HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_HASH
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )
else()
  set(GIT_VERSION "0.1.0-unknown")
  set(GIT_HASH "unknown")
endif()

string(TIMESTAMP BUILD_DATE "%Y-%m-%d %H:%M:%S UTC" UTC)

# Generate version module
configure_file(
  ${CMAKE_SOURCE_DIR}/src/version_mod.f90.in
  ${CMAKE_SOURCE_DIR}/src/version_mod.f90
  @ONLY
)

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

# Source files in dependency order
set(LIB_SOURCES
  src/version_mod.f90
  src/s3_config_mod.f90
  src/s3_backend_mod.f90
  src/s3_http_backend_mod.f90
  src/s3_backend_selector_mod.f90
  src/s3_interface_mod.f90
  src/s3_file_ops_mod.f90
  src/fortran-s3-accessor.f90
)

# Create library
add_library(fortran_s3_accessor ${LIB_SOURCES})
target_include_directories(fortran_s3_accessor PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
  $<INSTALL_INTERFACE:include>
)

# Example executable
add_executable(s3_example app/main.f90)
target_link_libraries(s3_example fortran_s3_accessor)

# Enable testing
enable_testing()

# Test executable can be added when test/check.f90 is created
# add_executable(test_s3_accessor test/check.f90)
# target_link_libraries(test_s3_accessor fortran_s3_accessor)
# add_test(NAME s3_accessor_tests COMMAND test_s3_accessor)

# Installation rules
install(TARGETS fortran_s3_accessor
  EXPORT fortran_s3_accessor_targets
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)

install(FILES
  ${CMAKE_CURRENT_BINARY_DIR}/version_mod.mod
  ${CMAKE_CURRENT_BINARY_DIR}/s3_config_mod.mod
  ${CMAKE_CURRENT_BINARY_DIR}/s3_backend_mod.mod
  ${CMAKE_CURRENT_BINARY_DIR}/s3_http_backend_mod.mod
  ${CMAKE_CURRENT_BINARY_DIR}/s3_backend_selector_mod.mod
  ${CMAKE_CURRENT_BINARY_DIR}/s3_interface_mod.mod
  ${CMAKE_CURRENT_BINARY_DIR}/s3_file_ops_mod.mod
  ${CMAKE_CURRENT_BINARY_DIR}/fortran_s3_accessor.mod
  DESTINATION include
)

# Export targets
install(EXPORT fortran_s3_accessor_targets
  FILE fortran_s3_accessor.cmake
  NAMESPACE fortran_s3_accessor::
  DESTINATION lib/cmake/fortran_s3_accessor
)