local function listen(f, id)
    local err, eid = syscall("listen", f, id)
    return eid, err
end

local function mopen(mode, contents, limit)
    local err, fd = syscall("mopen", mode, contents, limit)
    return fd, err
end

local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function read(fd, len)
    local err, data = syscall("read", fd, len)
    return data, err
end

local function queued(fd, ...)
    local err, isQueued = syscall("queued", fd, ...)
    return isQueued, err
end

local function mkpipe(input, output)
    local err, fd = syscall("mkpipe", input, output)
    return fd, err
end

local function attach(func, name)
    local err, tid = syscall("attach", func, name)
    return tid, err
end

local function clear(fd)
    local err = syscall("clear", fd)
    return err == nil, err
end

local function isEscape(char)
    return char < 0x20 or (char >= 0x7F and char <= 0x9F)
end

local function pnext(pid)
    local err, npid = syscall("pnext", pid)
    return npid, err
end

local function pinfo(pid)
    local err, info = syscall("pinfo", pid)
    return info, err
end

local function pspawn(init, config)
    local err, pid = syscall("pspawn", init, config)
    return pid, err
end

local function ufindUser(user)
    local err, x = syscall("ufindUser", user)
    return x, err
end

local function hostname(hostname)
    local err, x = syscall("hostname", hostname)
    return x, err
end

local function login(user, ring, password)
    local err = syscall("login", user, ring, password)
    return err == nil, err
end

local function ttyopen()
    local err, fd = syscall("ttyopen")
    return fd, err
end


local tty = assert(ttyopen())
local stdout, stdin = tty, tty;
write(stdout, "\x1b[2J");

local commandStdinBuffer = ""
local function readLine()
    while true do
        commandStdinBuffer = commandStdinBuffer .. assert(read(stdin, math.huge))
        local lineEnd = commandStdinBuffer:find('%\n')
        if lineEnd then
            local line = commandStdinBuffer:sub(1, lineEnd-1)
            commandStdinBuffer = commandStdinBuffer:sub(lineEnd+1)
            return line
        else
            coroutine.yield()
        end
    end
end

listen(function(name, ...)
    if name == "event_err" then
        _K.logAll(name, ...)
    end
end)

local function myBeloved()
    local state = "username"
    local user;
    local password;

    write(stdout, string.format("KOCOS %s (tty1)\n\n", string.gsub(_KVERSION, "KOCOS ", "")));
    while true do
        if state == "username" then
            write(stdout, string.format("%s login: ", hostname()));
            local username = readLine();
            user = ufindUser(username);
            if user ~= nil then
                state = "password";
            else
                write(0, "bruh\n");
            end
        elseif state == "password" then
            write(stdout, "Password: \x1b[28m");
            password = readLine();
            write(stdout, "\x1b[0m");

            local _, err = login(user, 0, password);
            if err then
                write(stdout, string.format("Login failed: %s\n", err));
            else
                local child = assert(pspawn("/bin/sh", {
                    args = {},
                    fdMap = {
                        [0] = stdout,
                        [1] = stdin,
                        [2] = stdout,
                    }
                }));
                syscall("pawait", child); -- Shouldnt exit?
                syscall("pexit", child);
            end
            state = "username";
        end
    end
end

myBeloved()
