
param(
  [string]$vsVer
)

mkdir build

msbuild-extractor.exe --solution ".\$vsVer\Adder.sln" --all-configurations --merge --deduplicate --prefer-configuration Debug --c-cpp-properties -o compile_commands.json
