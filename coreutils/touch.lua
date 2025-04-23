local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function touch(path, perms)
    local err = syscall("touch", path, perms)
    return err == nil, err
end

local function main(argv)
    for i = 1, #argv do
        touch(argv[i], (2^16)-1);
    end
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    os.exit(1);
end
os.exit(err or 0);