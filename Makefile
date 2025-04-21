.PHONY: rootfs

rootfs:
	mkdir rootfs
	mkdir rootfs/bin
	mkdir rootfs/etc
	mkdir rootfs/mnt
	mkdir rootfs/tmp
	mkdir rootfs/dev
	cp coreutils/* rootfs/bin/
	cp extra/* rootfs/bin/
	cp util/* rootfs/bin/
clean:
	rm -r rootfs