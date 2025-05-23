local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function pself()
    local err, pid = syscall("pself")
    return pid, err
end

local function pinfo(pid)
    local err, info = syscall("pinfo", pid)
    return info, err
end

local function pspawn(init, config)
    local err, pid = syscall("pspawn", init, config)
    return pid, err
end

local function uinfo(user)
    local err, x = syscall("uinfo", user)
    return x, err
end

local function hostname(hostname)
    local err, x = syscall("hostname", hostname)
    return x, err
end

local function pexit(pid)
    local err = syscall("pexit", pid)
    return err == nil, err
end

if arg[1] == "-c" then
    table.remove(arg, 1);
    
    local command = string.split(table.remove(arg, 1), " ");
    local thingy = table.remove(command, 1);
    local path = io.searchpath(thingy);
    if path then
        local child = assert(pspawn(path, {
            args = command
        }));
        syscall("pawait", child);
        local status = assert(pinfo(child)).status
        pexit(child);
        os.exit(status);
    else
        write(2, string.format("%s doesnt exists in search path\n", thingy));
        os.exit(1);
    end
end

local status = 0;
while true do
    local me = assert(pinfo(assert(pself())));
    local user = "";
    if me.uid == 0 then
        user = "root";
    elseif me.uid == 63 then
        user = "all"
    else
        user = assert(uinfo(me.uid));
    end

    write(0, string.format(
        "\x1b[0m\x1b[32m%s\x1b[0m@%s \x1b[32m%s",
        user,
        hostname(),
        io.cwd()
    ));
    if status ~= 0 then
        write(0, " \x1b[31m[" .. tostring(status) .. "]\x1b[32m ");
    end
    write(0, "\n❯ \x1b[0m");

    local line = io.read("l");
    local args = string.split(line, " ");
    local cmd = table.remove(args, 1) or "";

    if cmd == "" then
        goto continue
    end

    if cmd == "exit" then
        os.exit(tonumber(args[1] or "0"));
    elseif cmd == "cd" then
        io.cd(args[1] or "/");
        goto continue
    end

--[[
calion@calion-Computer ~/Programming/KOCOS (main) 
❯
USER@HOSTNAME CWD
❯
]]
    local path = io.searchpath(cmd);
    if path then
        local child = assert(pspawn(path, {
            args = args
        }));
        syscall("pawait", child);
        status = assert(pinfo(child)).status;
        pexit(child);
    else
        write(0, string.format("\x1b[0mkterm: Unknown command: %s\n\x1b[0m", cmd));
        _OS.computer.beep();
    end

    coroutine.yield()
    ::continue::
end
