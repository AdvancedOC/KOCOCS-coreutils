local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function main(argv)
    if #argv == 0 then
        write(0, "requires atleast 1 argument\n");
        return 1;
    end

    local partUUID = assert(argv[1], "missing partition");
    local fs = argv[2] or "okffs";

    local part = assert(_K.fs.partitionFromUuid(partUUID), "partition not found");
    assert(_K.fs.format(part, fs));
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    os.exit(1);
end
os.exit(err or 0);