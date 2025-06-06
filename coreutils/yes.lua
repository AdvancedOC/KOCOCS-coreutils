local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function main(argv)
    local stuff;
    if #argv == 0 then
        stuff = "y";
    else
        stuff = table.concat(argv, " ");
    end
    while true do
        write(0, stuff .. "\n");
        coroutine.yield();
    end
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    os.exit(1);
end
os.exit(err or 0);