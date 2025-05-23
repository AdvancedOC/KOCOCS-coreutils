local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function ftype(path)
    local err, x = syscall("ftype", path)
    return x, err
end

local function main(argv)
    if #argv ~= 2 then
        write(0, "requires 2 arguments\n");
        return 1;
    end
    local partUUID = assert(argv[1], "missing partition");
    local dir = argv[2];
    local part = assert(_K.fs.partitionFromUuid(partUUID), "partition not found");
    assert(ftype(dir) == "directory", "mountpoint must be a directory");
    _K.fs.mount(dir, part);
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    os.exit(1);
end
os.exit(err or 0);