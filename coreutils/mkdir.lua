local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function exit(status)
    -- WILL NEVER RETURN IF IT WORKED
    local err = syscall("exit", status)
    return err == nil, err
end

local function mkdir(path, perms)
    local err = syscall("mkdir", path, perms)
    return err == nil, err
end

local function ftype(path)
    local err, x = syscall("ftype", path)
    return x, err
end

local function main(argv)
    local arg_p = false;

    local i = 0;
    while argv[i] ~= nil do
        local thing = argv[i];
        if string.startswith(thing, "-") and #thing > 1 then
            thing = string.sub(table.remove(argv, i), 2);
            while #thing > 0 do
                local char = thing:sub(1, 1);
                if char == "p" then
                    arg_p = true;
                else
                    write(2, string.format("mkdir: invalid option -- '%s'\nTry 'ls --help' for more information\n", char));
                    return 1;
                end
                thing = string.sub(thing, 2);
            end
        else
            i = i + 1;
        end
    end

    for i = 1, #argv do
        if arg_p then
            -- local splits = string.split(argv[i], "/");
            -- local path = splits[1];
            -- for j = 1, #splits - 1 do
            --     if ftype(path) == "missing" then
            --         assert(mkdir(path, (2^16)-1));
            --     end
            --     path = path .. "/" .. splits[j];
            -- end
            write(0, "fuck you idk how to make p work\n");
            return 1;
        else
            assert(mkdir(argv[i], (2^16)-1));
        end
    end
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    assert(exit(1));
end
assert(exit(err or 0));