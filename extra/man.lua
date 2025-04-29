local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function read(fd, len)
    local err, data = syscall("read", fd, len)
    return data, err
end

local function pinfo(pid)
    local err, info = syscall("pinfo", pid)
    return info, err
end

local function pspawn(init, config)
    local err, pid = syscall("pspawn", init, config)
    return pid, err
end

local function pexit(pid)
    local err = syscall("pexit", pid)
    return err == nil, err
end

local function main(argv)
    if #argv ~= 1 then
        write(0, "What manual page do you want?\nFor example, try 'man man'.\n");
        return 1;
    end

    local pager = os.getenv("MANPAGER");
    if pager ~= nil then
        local path = io.searchpath(pager);
        if path then
            local child = assert(pspawn(path, {
                args = {argv[1]}
            }));
            syscall("pawait", child);
            local status = assert(pinfo(child)).status;
            pexit(child);
            os.exit(status)
        end
        write(0, "Could not find pager\n");
        os.exit(1);
    end

    local entry = "/usr/man/" ..argv[1];
    local bright = false;
    local escaped = false;
    local color = 37;

    write(0, "\x1b[5n")
    local back = read(1, math.huge)
    local _, _, width, height = string.find(back, "(%d+);(%d+)")

    if io.exists(entry) then
        local file = assert(io.open(entry, "rb"));
        write(0, string.format("\x1b[%dm", color));
        local data = file:read("a")
        file:close();
        local i = 1;
        while true do
            if i > #data then
                break
            end

            local char = string.sub(data, i, i);
            if not escaped then
                if char == "\\" then
                    escaped = true;
                elseif char == "*" then
                    if bright then
                        bright = false;
                    else
                        bright = true;
                    end
                elseif char == "@" then
                    assert(i < #data, "Missing character for @");
                    local char2 = string.sub(data, i + 1, i + 1);
                    i = i + 1;
                    if char2 == "0" then
                        color = 37
                    elseif char2 == "b" then
                        color = 30
                    elseif char2 == "R" then
                        color = 31
                    elseif char2 == "G" then
                        color = 32
                    elseif char2 == "Y" then
                        color = 33
                    elseif char2 == "B" then
                        color = 34
                    elseif char2 == "M" then
                        color = 35
                    elseif char2 == "C" then
                        color = 36
                    elseif char2 == "W" then
                        color = 37
                    else
                        error("invalid command for @")
                    end
                elseif char == "~" then
                    assert(i < #data, "Missing character for ~");
                    local char2 = string.sub(data, i + 1, i + 1);
                    i = i + 1;
                    if char2 == "C" then
                        local loc = string.find(string.sub(data, i + 1, #data), "\n")
                        assert(loc ~= nil, "Cannot center if no newline")

                        local thing = string.sub(data, i + 1, i + loc - 1);
                        local thingy = math.floor((width / 2) - (#thing / 2));

                        write(0, "\x1b[6n")
                        local _, _, _, y = string.find(read(1, math.huge), "(%d+);(%d+)")

                        write(0, string.format("\x1b[%d;%dH", thingy, y))
                        write(0, thing .. "\n");

                        i = i + loc
                    elseif char2 == "R" then
                        local loc = string.find(string.sub(data, i + 1, #data), "\n")
                        assert(loc ~= nil, "Cannot align to the right if no newline")

                        local thing = string.sub(data, i + 1, i + loc - 1);
                        local thingy = math.floor(width - (#thing / 2)) - 1;

                        write(0, "\x1b[6n")
                        local _, _, _, y = string.find(read(1, math.huge), "(%d+);(%d+)")

                        write(0, string.format("\x1b[%d;%dH", thingy, y))

                        local real = color
                        if bright then
                            real = real + 60;
                        end
                        write(0, string.format("\x1b[%dm", real) .. thing);
                        i = i + loc
                    end
                elseif char == "^" then
                    assert(i < #data, "Missing character for ~");
                    local char2 = string.sub(data, i + 1, i + 1);
                    i = i + 1;
                    if char2 == "L" then
                        local start1, end1 = string.find(string.sub(data, i + 1, #data), "^L", nil, true)
                        assert(start1 ~= nil, "Cannot find first ending ^L");
                        local title = string.sub(data, i + 1, i + start1 - 1);

                        i = i + end1;

                        local start2, end2 = string.find(string.sub(data, i + 1, #data), "^L", nil, true)
                        assert(start1 ~= nil, "Cannot find second ending ^L");

                        local url = string.sub(data, i + 1, i + start2 - 1); -- TODO: Idk
                        write(0, "\x1b[38;2;157;3;252m" .. title .. "\x1b[0m");
                        --write(0, "\x1b[38;2;157;3;252m" .. url .. "\x1b[0m");
                        
                        i = i + end2
                    else
                        error("Invalid command for ^")
                    end
                else
                    local real = color
                    if bright then
                        real = real + 60;
                    end
                    write(0, string.format("\x1b[%dm", real) .. char);
                end
            else
                local real = color
                if bright then
                    real = real + 60;
                end
                write(0, string.format("\x1b[%dm", real) .. char);
                escaped = false;
            end

            i = i + 1;
        end
        write(0, "\x1b[0m\n");
    else
        write(2, string.format("No manual entry for %s\n", entry));
        return 1;
    end
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    os.exit(1);
end
os.exit(err or 0);