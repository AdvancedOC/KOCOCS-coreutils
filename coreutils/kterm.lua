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

local function pself()
    local err, pid = syscall("pself")
    return pid, err
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

local function open(path, mode)
    local err, fd = syscall("open", path, mode)
    return fd, err
end

local function close(fd)
    local err = syscall("close", fd)
    return err == nil, err
end

local logPid

do
    local attempt = assert(pnext())
    while true do
        local info = assert(pinfo(attempt))
        if info.cmdline == "OS:logproc" then
            logPid = attempt
            break
        end
        attempt = pnext(attempt)
        if not attempt then break end
    end
end

assert(logPid, "log pid failed")
syscall("pwait", logPid)

local tty = _K.tty.create(_OS.component.gpu, _OS.component.screen)
tty:clear();

local programOut = assert(mopen("w", "", math.huge))
local programIn = assert(mopen("w", "", math.huge))
local stdout = assert(mkpipe(programIn, programOut))
local stdin = assert(mopen("w", "", math.huge))

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
        KOCOS.logAll(name, ...)
    end
end)

local current_working_directory = "/"; -- No cwd?
local function myBeloved()
    while true do
        local me = assert(pinfo(assert(pself())));
        -- Why is KOCOS nil here
        local user = assert(_K.auth.userInfo(me.uid));

        local hostfile = assert(open("/etc/hostname", "r"));
        local hostname = assert(read(hostfile, math.huge));
        assert(close(hostfile));

        write(stdout, string.format(
            "\x1b[0m\x1b[32m%s\x1b[0m@%s \x1b[32m%s\n❯ \x1b[0m",
            user.name,
            hostname,
            current_working_directory
        ));

        local line = readLine();
        local parts = string.split(line, " ");

        local cmd = parts[1] or "";
        if cmd == "" then
            goto continue
        end

        -- Idk why but it crashes if you have more than 1 argument
        local args = {table.unpack(parts, 2)};
--[[
calion@calion-Computer ~/Programming/KOCOS (main) 
❯
USER@HOSTNAME CWD
❯
]]
        local path = "/bin/" .. cmd .. ".lua"; -- TODO: Respect PATH env
        if _K.fs.exists(path) then
            local child = assert(pspawn(path, {
                args = args
            }));
            syscall("pawait", child);
        else
            write(stdout, string.format("\x1b[0mkterm: Unknown command: %s\n\x1b[0m", cmd));
            _OS.computer.beep();
        end

        coroutine.yield()
        ::continue::
    end
end

attach(myBeloved, "command")

_K.event.listen(function(event, _, char, code)
    if event == "key_down" then
        tty:handleChar(char, code)
    end
end)

local inputBuffer
while true do
    if queued(programOut, "write") then
        local data, err = read(programOut, math.huge)
        if err then tty:write(err) end
        clear(programOut)
        assert(data, "no data")
        tty:write(data)
        coroutine.yield()
    end

    while true do
        local response = tty:popResponse()
        if not response then break end
        assert(write(programIn, response))
    end

    if queued(stdin, "read") and not inputBuffer then
        clear(stdin)
        inputBuffer = ""
    end

    if tty.auxPort then
        _K.event.pop("key_down")
    end

    if inputBuffer and not tty.auxPort then
        local ok, _, char, code = _K.event.pop("key_down")
        if ok then
            local lib = unicode or string
            local backspace = 0x0E
            local enter = 0x1C
            if code == enter then
                clear(stdin)
                write(stdin, inputBuffer .. "\n")
                tty:write('\n')
                inputBuffer = nil
            elseif code == backspace then
                local t = lib.sub(inputBuffer, -1)
                tty:unwrite(t)
                inputBuffer = lib.sub(inputBuffer, 1, -2)
            elseif not isEscape(char) then
                tty:write(lib.char(char))
                inputBuffer = inputBuffer .. lib.char(char)
            end
        end
    end

    coroutine.yield()
end
