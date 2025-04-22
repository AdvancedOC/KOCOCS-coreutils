import os, shutil
os.chdir("../")
if os.path.isdir("./rootfs/build"): shutil.rmtree("./rootfs/build")
os.mkdir("./rootfs/build")

def build(package):
    for f in os.listdir(f"./rootfs/{package}"):
        if f.endswith(".lua"):
            name = f.removesuffix(".lua");
            os.system(f"lua ./tools/luac.lua -o ./rootfs/build/{name}.o -m main ./rootfs/{package}/{f}")
            os.system(f"lua ./tools/ld.lua -o ./rootfs/build/{name} ./rootfs/build/{name}.o -l/lib/liblua.so")
build("coreutils")
build("util")
build("extra")

def build_rootfs():
    if os.path.isdir("./rootfs/rootfs"): shutil.rmtree("./rootfs/rootfs")
    os.mkdir("./rootfs/rootfs/")
    os.mkdir("./rootfs/rootfs/bin")
    os.mkdir("./rootfs/rootfs/etc")
    os.mkdir("./rootfs/rootfs/mnt")
    os.mkdir("./rootfs/rootfs/tmp")
    os.mkdir("./rootfs/rootfs/dev")
    os.mkdir("./rootfs/rootfs/lib")
    for f in os.listdir(f"./rootfs/build/"):
        if not f.endswith(".o"):
            os.system(f"cp ./rootfs/build/{f} ./rootfs/rootfs/bin/")
    os.system("cp luart ./rootfs/rootfs/bin/lua")
    os.system("cp basicTTY.lua ./rootfs/rootfs/bin/")
    os.system("cp repl.lua ./rootfs/rootfs/")
    os.system("cp lib/liblua.so ./rootfs/rootfs/lib")
    os.system("cp lib/libkelp.so ./rootfs/rootfs/lib")
    os.system("cp ./rootfs/rootfs/bin/kterm ./rootfs/rootfs/bin/sh")

build_rootfs()