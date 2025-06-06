local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function remove(proc, path)
    local err = syscall("remove", path)
    return err == nil, err
end

local function rm(path, arg_r)
    local type = io.ftype(path);
    if type == "file" then
        assert(os.remove(path))
    elseif type == "directory" then
        if arg_r then
            if #io.list(path) == 0 then
                assert(os.remove(path))
            else
                for _, file in ipairs(io.list(path)) do
                    local filepath = path .. "/" .. file;
                    coroutine.yield();
                    rm(filepath);
                end
                assert(os.remove(path))
            end
        else
            write(2, string.format("rm: cannot remove '%s': Is a directory\n"))
        end
    end
end

local function main(argv)
    local arg_r = false;

    local i = 0;
    while argv[i] ~= nil do
        local thing = argv[i];
        if string.startswith(thing, "-") and #thing > 1 then
            thing = string.sub(table.remove(argv, i), 2);
            while #thing > 0 do
                local char = thing:sub(1, 1);
                if char == "r" then
                    arg_r = true;
                else
                    write(2, string.format("rm: invalid option -- '%s'\nTry 'ls --help' for more information\n", char));
                    return 1;
                end
                thing = string.sub(thing, 2);
            end
        else
            i = i + 1;
        end
    end

    for i = 1, #argv do
        rm(argv[i], arg_r);
    end
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    os.exit(1);
end
os.exit(err or 0);