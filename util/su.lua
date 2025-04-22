local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function login(user, ring, password)
    local err, x = syscall("login", user, ring, password)
    return x, err
end

local function ufindUser(user)
    local err, x = syscall("ufindUser", user)
    return x, err
end

local function pspawn(init, config)
    local err, pid = syscall("pspawn", init, config)
    return pid, err
end

local function pself()
    local err, pid = syscall("pself")
    return pid, err
end

local function pinfo(pid)
    local err, info = syscall("pinfo", pid)
    return info, err
end

local function main(argv)
    local user = ufindUser(argv[1] or "root");
    if user == nil then
        os.exit(1);
    end

    write(0, "Password: \x1b[28m");
    local password = io.read("l");
    write(0, "\x1b[0m");

    login(user, assert(pinfo(pself())).ring, password);

    local child = assert(pspawn("/bin/sh", {}));
    syscall("pawait", child);
    os.exit(0);
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    assert(os.exit(1));
end
assert(os.exit(err or 0));