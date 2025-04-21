.PHONY: rootfs

rootfs:
	mkdir rootfs
	mkdir rootfs/bin
	mkdir rootfs/etc
	mkdir rootfs/mnt
	mkdir rootfs/tmp
	echo "computer" > rootfs/etc/hostname
	cp coreutils/* rootfs/bin/
	cp extra/* rootfs/bin/
clean:
	rm -r rootfs