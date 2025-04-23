import os, shutil, subprocess
with open("/etc/hostname", "r") as f:
    if f.read() != "calion-Computer\n":
        print("TODO: Make the build system work for others")
        exit(1)

os.chdir("../")
if os.path.isdir("./rootfs/build"): shutil.rmtree("./rootfs/build")
os.mkdir("./rootfs/build")

def build(package):
    for f in os.listdir(f"./rootfs/{package}"):
        if f.endswith(".lua"):
            name = f.removesuffix(".lua")
            os.system(f"lua ./tools/luac.lua -o ./rootfs/build/{name}.o -m main ./rootfs/{package}/{f}")
            os.system(f"lua ./tools/ld.lua -o ./rootfs/build/{name} ./rootfs/build/{name}.o -l/lib/liblua.so" + (" -l/lib/libkelp.so" if name == "klc" else ""))
build("coreutils")
build("util")
build("extra")

def build_rootfs():
    if os.path.isdir("./bin"): shutil.rmtree("./bin")
    os.mkdir("./bin")

    if not os.path.isdir("./mnt"): os.mkdir("./mnt")
    if not os.path.isdir("./tmp"): os.mkdir("./tmp")
    if not os.path.isdir("./dev"): os.mkdir("./dev")

    for f in os.listdir(f"./rootfs/build/"):
        if not f.endswith(".o"):
            os.system(f"cp ./rootfs/build/{f} ./bin/")
    os.system("cp luart ./bin/lua")
    os.system("cp basicTTY.lua ./bin/")
    os.system("cp ./bin/kterm ./bin/sh")

build_rootfs()