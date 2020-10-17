
win32Build = os.target() == "windows"
linuxBuild = os.target() == "linux"

project "Adder"
configurations { "Debug", "Release" }

if (not win32Build and not linuxBuild) then
  print("This operating system is not supported.")
  exit()
end

-- Setup WIN32 Project
if (win32Build == true) then
  defines { "ADDER_PLATFORM_WIN32" }
  print("Creating project for Windows...")
end

-- Setup Linux Project
if (linuxBuild == true) then
  defines { "ADDER_PLATFORM_LINUX" }
  includedirs { "/usr/include" }
  libdirs {"/usr/lib"}
  print("Creating project for Linux...")
end

kind "ConsoleApp"
architecture "x64"
language "C++"
characterset ("MBCS")

-- Set Directories

bin_path = "..\\..\\bin"

symbolspath '$(OutDir)$(TargetName).pdb'
targetdir "../../bin/"
debugdir "../../bin/"
objdir "../../builds/output/%{cfg.platform}_%{cfg.buildcfg}"

-- Project Flags

flags { "FatalWarnings" }
flags { "MultiProcessorCompile" }

-- Build Options

-- buildoptions { "/bigobj" }

-- Linker options

linkoptions { "/ignore:4006" }
linkoptions { "/ignore:4221" }
linkoptions { "/ignore:4075" }

-- Shared Defines

  defines { "_CRT_SECURE_NO_WARNINGS" }

-- Includes

  includedirs { "src", "src/**" } 

-- Project Files

  files { "src/**.cpp", "src/**.h", "src/**.inl" , "**.natvis" }

-- Debug Configuration Settings

  filter { "configurations:Debug" }
    defines { "DEBUG"}
    symbols "On"
	  editandcontinue "On"

-- Release Configuration Settings

  filter { "configurations:Release" }
    flags { "LinkTimeOptimization" }
    defines { "NDEBUG" }
    optimize "On"
	  editandcontinue "Off"
