local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function exit(status)
    -- WILL NEVER RETURN IF IT WORKED
    local err = syscall("exit", status)
    return err == nil, err
end

local function ftype(path)
    local err, x = syscall("ftype", path)
    return x, err
end

local function read(fd, len)
    local err, data = syscall("read", fd, len)
    return data, err
end

local function open(path, mode)
    local err, fd = syscall("open", path, mode)
    return fd, err
end

local function close(fd)
    local err = syscall("close", fd)
    return err == nil, err
end

local function main(argv)
    for i = 1, #argv do
        local path = argv[i];
        if path == "-" then
            write(0, assert(read(0, math.huge)));
            goto continue;
        end
        if ftype(path) == "file" then
            local file = assert(open(path, "r"));
            while true do
                local chunk, err = read(file, math.huge);
                if err then error(err) end
                if not chunk then break end
                write(0, chunk);
                coroutine.yield();
            end
            assert(close(file));
        else
            write(2, string.format("cat: %s: Is a directory", path));
        end
        ::continue::
    end
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    assert(exit(1));
end
assert(exit(err or 0));