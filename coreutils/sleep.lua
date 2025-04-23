local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function main(argv)
    if #argv == 0 then
        write(0, "sleep: missing operand\n");
        return 1;
    end
    coroutine.yield(assert(tonumber(argv[1])));
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    os.exit(1);
end
os.exit(err or 0);