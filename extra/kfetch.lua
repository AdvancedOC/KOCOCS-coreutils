local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function exit(status)
    -- WILL NEVER RETURN IF IT WORKED
    local err = syscall("exit", status)
    return err == nil, err
end

local function stat(path)
    local err, info = syscall("stat", path)
    return info, err
end

local function cstat()
    local err, info = syscall("cstat")
    return info, err
end

function clist(all)
    local err, l = syscall("clist", all)
    return l, err
end

local kocosAsciiArt = [[
 /$$   /$$ /$$   /$$ /$$   /$$      /$$ /$$   /$$  /$$$$$$   /$$$$$$   /$$$$$$   /$$$$$$ 
| $$  /$$/| $$$ | $$| $$  | $$     /$$/| $$  /$$/ /$$__  $$ /$$__  $$ /$$__  $$ /$$__  $$
| $$ /$$/ | $$$$| $$| $$  | $$    /$$/ | $$ /$$/ | $$  \ $$| $$  \__/| $$  \ $$| $$  \__/
| $$$$$/  | $$ $$ $$| $$  | $$   /$$/  | $$$$$/  | $$  | $$| $$      | $$  | $$|  $$$$$$ 
| $$  $$  | $$  $$$$| $$  | $$  /$$/   | $$  $$  | $$  | $$| $$      | $$  | $$ \____  $$
| $$\  $$ | $$\  $$$| $$  | $$ /$$/    | $$\  $$ | $$  | $$| $$    $$| $$  | $$ /$$  \ $$
| $$ \  $$| $$ \  $$|  $$$$$$//$$/     | $$ \  $$|  $$$$$$/|  $$$$$$/|  $$$$$$/|  $$$$$$/
|__/  \__/|__/  \__/ \______/|__/      |__/  \__/ \______/  \______/  \______/  \______/
]]

local function main(_)
    local asciiLines = string.split(kocosAsciiArt, "\n")
    local asciiWidth = 0
    for i=1,#asciiLines do asciiWidth = math.max(asciiWidth, #asciiLines[i]) end

    local spacing = string.rep(" ", 2)

    local data = {}
    local info = assert(cstat())

    local uptime = info.uptime
    local ms = tostring(math.floor((uptime * 1000) % 1000))
    local secs = tostring(math.floor(uptime) % 60)
    local mins = tostring(math.floor(uptime / 60) % 60)
    local hours = tostring(math.floor(uptime / 3600) % 60)

    while #ms < 3 do ms = "0" .. ms end
    while #secs < 2 do secs = "0" .. secs end
    while #mins < 2 do mins = "0" .. mins end
    while #hours < 2 do hours = "0" .. hours end

    local function coloriser(color)
        return function(text)
            return "\x1b[" .. color .. "m" .. text .. "\x1b[0m"
        end
    end

    local infoColor = coloriser("38;2;85;85;255")
    local art = coloriser(34)

    table.insert(data, "OS: " .. _OSVERSION)
    table.insert(data, "Kernel: " .. _KVERSION)
    local memUsed = info.memTotal - info.memFree
    table.insert(data, string.format("Memory: %s / %s (%.2f%%)", string.memformat(memUsed), string.memformat(info.memTotal), memUsed / info.memTotal * 100))
    table.insert(data, string.format("Uptime: %s:%s:%s.%s", hours, mins, secs, ms))
    table.insert(data, "Terminal: kterm")
    table.insert(data, "Shell: kterm")
    table.insert(data, "Boot: " .. info.boot:sub(1, 6) .. "...")
    table.insert(data, "Architecture: " .. info.arch)
    table.insert(data, "Components: " .. #assert(clist(true)))
    table.insert(data, "Threads: " .. info.threadCount)
    table.insert(data, string.format("Battery: %.2f%%", info.energy / info.maxEnergy * 100))
    do
        local color = ""
        for i=0,7 do
            local dark = 40 + i
            local bright = 100 + i
            color = color .. string.format("\x1b[%dm  \x1b[%dm  \x1b[0m", dark, bright)
        end
        table.insert(data, "Color: " .. color)
    end
    for mountpoint in _K.fs.mountedPartitions() do
        local mountInfo = assert(stat("/" .. mountpoint))
        if mountInfo.total > 0 then
            table.insert(data, string.format("Disk (%s): %s / %s (%.2f%%)", "/" .. mountpoint, mountInfo.used, mountInfo.total, mountInfo.used / mountInfo.total * 100))
        end
    end

    if #data > #asciiLines then
        local toPrepend = math.floor((#data - #asciiLines) / 2)
        for i=1,toPrepend do
            table.insert(asciiLines, 1, "")
        end
    end

    local lineCount = math.max(#asciiLines, #data)
    for i=1,lineCount do
        local line = asciiLines[i] or ""
        line = art(line) .. string.rep(" ", asciiWidth - #line)
        if data[i] then
            local colon = string.find(data[i], ":", nil, true)
            local before = string.sub(data[i], 1, colon)
            local after = string.sub(data[i], colon + 1)
            line = line .. spacing .. infoColor(before) .. after
        end
        write(0, line .. "\n");
    end
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    assert(exit(1));
end
assert(exit(err or 0));