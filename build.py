import os, shutil, sys
#os.chdir("../")
#if os.path.isdir("./rootfs/build"): shutil.rmtree("./rootfs/build")
#os.mkdir("./rootfs/build")

if not os.path.isfile("./coreutils.kpm"):
    print("Run in coreutils directory")
    exit(1)
if len(sys.argv) != 3:
    print("Give tools and lib path as arg")
    exit(1)

tools = sys.argv[1];
assert(tools != "")
libs = sys.argv[1];
assert(libs != "")
os.environ["LUA_PATH"] = libs + "/../?.lua"

if os.path.isdir("./build"): shutil.rmtree("./build")
os.mkdir("./build")

def build(package):
    for f in os.listdir(f"./{package}"):
        if f.endswith(".lua"):
            name = f.removesuffix(".lua")
            assert(os.system(f"lua {tools}/luac.lua -o ./build/{name}.o -m main ./{package}/{f}") == 0)
            assert(os.system(f"lua {tools}/ld.lua -o ./build/{name} ./build/{name}.o -l{libs}/liblua.so" + (f" -l{libs}/libkelp.so" if name == "klc" else "")) == 0)
build("coreutils")
build("util")
build("extra")

# def build_rootfs():
#     if os.path.isdir("./bin"): shutil.rmtree("./bin")
#     os.mkdir("./bin")

#     if not os.path.isdir("./mnt"): os.mkdir("./mnt")
#     if not os.path.isdir("./tmp"): os.mkdir("./tmp")
#     if not os.path.isdir("./dev"): os.mkdir("./dev")

#     for f in os.listdir(f"./rootfs/build/"):
#         if not f.endswith(".o"):
#             os.system(f"cp ./rootfs/build/{f} ./bin/")
#     os.system("cp luart ./bin/lua")
#     os.system("cp basicTTY.lua ./bin/")
#     os.system("cp ./bin/kterm ./bin/sh")

# build_rootfs()