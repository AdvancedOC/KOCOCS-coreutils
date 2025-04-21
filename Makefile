.PHONY: rootfs

rootfs:
	mkdir rootfs
	mkdir rootfs/bin
	mkdir rootfs/etc
	mkdir rootfs/mnt
	mkdir rootfs/tmp
	mkdir rootfs/dev
	mkdir rootfs/lib
	cp coreutils/* rootfs/bin/
	cp extra/* rootfs/bin/
	cp util/* rootfs/bin/
	cp ../luart rootfs/bin/luart.lua
	cp ../basicTTY.lua rootfs/bin/
	cp ../repl.lua rootfs/
	cp ../lib/liblua.so rootfs/lib/
clean:
	rm -r rootfs