solution "adder"

SOLUTION_ROOT = _MAIN_SCRIPT_DIR .. '/' .. _ACTION

location (SOLUTION_ROOT)

configurations { "Debug", "Release" }

platforms { "x64" }

objdir "%{wks.location}/../builds/intermediate/%{prj.name}/%{cfg.buildcfg}_%{cfg.platform}"

libdirs {
  "%{wks.location}/../builds/lib/%{cfg.buildcfg}_%{cfg.platform}",
  "%{wks.location}/../builds/bin/%{cfg.buildcfg}_%{cfg.platform}"
}

targetdir "%{wks.location}/../builds/bin/%{cfg.buildcfg}_%{cfg.platform}"
debugdir "%{wks.location}/../builds/bin"

filter { "kind:StaticLib" }
targetdir "%{wks.location}/../builds/lib/%{cfg.buildcfg}_%{cfg.platform}"

function add_variant(name, projKind, srcFiles, libs)
  project (name)
  kind (projKind)
  language "C++"
  staticruntime "on"
  cppdialect "C++17"

  includedirs { "lib/include" }

  files     { "adder.natvis" }
  files     (srcFiles)
  dependson (libs or {})
  links     (libs or {})

  -- common stuff that we want to be common among all projects
  warnings "Extra"
  targetname "%{prj.name}"
  flags { "NoMinimalRebuild", "NoPCH" }
  exceptionhandling "Off"
  rtti "Off"
  
  -- configurations
  filter { "configurations:Debug*" }
    defines { "_DEBUG" }
    optimize "Off"
    symbols "On"
  
  filter { "configurations:Release*" }
    defines { "NDEBUG" }
    optimize "Full"
    symbols "On"
    flags { "NoBufferSecurityCheck" }
    omitframepointer "On"
end

add_variant("adder",  "StaticLib", {
    "lib/include/**.h",
    "lib/include/**.inl",
    "lib/include/**.hpp",
    "lib/src/**.h",
    "lib/src/**.inl",
    "lib/src/**.hpp",
    "lib/src/**.cpp",
  },
  {}
)

add_variant("adderTest", "consoleApp", {
    "test/**.h",
    "test/**.hpp",
    "test/**.inl",
    "test/**.cpp",
    "test/**.ad"
  },
  { "adder" }
)

-- add_variant("adderlibc", "StaticLib")
-- add_variant("adderlibc", "SharedLib")
