solution "adder"

SOLUTION_ROOT = _MAIN_SCRIPT_DIR .. '/' .. _ACTION
location (SOLUTION_ROOT)

configurations { "Debug", "Release" }
platforms { "x64" }

-- Disable frivolous warnings
buildoptions {
  '/wd"4530"', -- Disable exception warning
  '/wd"4505"', -- Disable unused function warning
  '/wd"4127"', -- Disable constant conditional warning
  '/wd"4201"', -- Disable nameless struct/union warning
  '/Gm-',
  '/MP',
  '/Zc:__cplusplus'
}

project "adder"

kind "consoleApp"
language "C++"
staticruntime "on"
cppdialect "C++17"

includedirs { "include/" }

files {
    "include/**.h",
    "include/**.hpp",
    "include/**.inl",
    "src/**.h",
    "src/**.inl",
    "src/**.hpp",
    "src/**.cpp"
}
defines { "_CRT_SECURE_NO_WARNINGS" }

-- common stuff that we want to be common among all projects
warnings "Extra"
targetname "%{prj.name}"
flags { "NoMinimalRebuild", "NoPCH" }
exceptionhandling "Off"
rtti "Off"

objdir "%{wks.location}/../builds/intermediate/%{prj.name}/%{cfg.buildcfg}_%{cfg.platform}"

libdirs {
  "%{wks.location}/../builds/lib/%{cfg.buildcfg}_%{cfg.platform}",
  "%{wks.location}/../builds/bin/%{cfg.buildcfg}_%{cfg.platform}"
}

targetdir "%{wks.location}/../builds/bin/%{cfg.buildcfg}_%{cfg.platform}"
debugdir "%{wks.location}/../builds/bin"

filter { "kind:StaticLib" }
targetdir "%{wks.location}/../builds/lib/%{cfg.buildcfg}_%{cfg.platform}"

-- configurations
filter { "configurations:Debug*" }
  defines { "_DEBUG" }
  optimize "Off"
  symbols "On"

filter { "configurations:DebugOpt*" }
  defines { "_DEBUG" }
  optimize "Debug"
  symbols "On"

filter { "configurations:Release*" }
  defines { "NDEBUG" }
  optimize "Full"
  symbols "On"
  flags { "NoBufferSecurityCheck" }
  omitframepointer "On"
