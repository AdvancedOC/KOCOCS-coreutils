local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function uinfo(user)
    local err, x = syscall("uinfo", user)
    return x, err
end

local function pinfo(pid)
    local err, info = syscall("pinfo", pid)
    return info, err
end

local function main(_)
    write(0, assert(uinfo(assert(pinfo()).uid)).name .. "\n");
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    os.exit(1);
end
os.exit(err or 0);
