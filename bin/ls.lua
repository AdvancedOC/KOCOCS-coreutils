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

local function list(path)
    local err, x = syscall("list", path)
    return x, err
end

local function main(argv)
    local function do_thing(path)
        local stuff = assert(list(path));
        for i = 1, #stuff do
            local type = assert(ftype(stuff[i])); -- TOOD: With cwd u dont need to do this
            if type == "directory" then
                write(0, "\x1b[34m");
            end
            write(0, stuff[i] .. "\x1b[0m ");
        end
        write(0, "\n");
    end

    if #argv == 0 then
        argv[1] = "/"; -- TODO: Cwd
    end

    if #argv == 1 then
        local type = assert(ftype(argv[1]));
        if type ~= "file" then
            do_thing(argv[1]);
        end
    else
        for i = 1, #argv do
            local type = assert(ftype(argv[i]));
            if type ~= "file" then
                write(0, argv[i] .. ":\n");
                do_thing(argv[i]);
            end
        end
    end
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(0, err)
    write(0, "\n")
    assert(exit(1));
end
assert(exit(err or 0));