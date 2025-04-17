do
local config = ...

KOCOS = {}
KOCOS_CONFIG = config

---@generic T
---@param val any
---@param def T
---@return T
function KOCOS.default(val, def)
    if type(val) == "nil" then return def else return val end
end

---@type string
KOCOS.defaultRoot = KOCOS.default(KOCOS_CONFIG.rootfs, computer.getBootAddress())
---@type string?
KOCOS.rootPart = KOCOS.default(KOCOS_CONFIG.rootfsPartition, nil)
KOCOS.allowGreenThreads = KOCOS.default(KOCOS_CONFIG.allowGreenThreads, true)
-- insecure will overwrite the ring to 0 for all processes
KOCOS.insecure = KOCOS.default(KOCOS_CONFIG.insecure, false)
---@type string?
KOCOS.init = KOCOS.default(KOCOS_CONFIG.init, "/sbin/init.lua")
KOCOS.maxEventBacklog = KOCOS.default(KOCOS_CONFIG.maxEventBacklog, 256)
KOCOS.rebootOnCrash = KOCOS.default(KOCOS_CONFIG.rebootOnCrash, true)
KOCOS.logThreadEvents = KOCOS.default(KOCOS_CONFIG.logThreadEvents, false)
KOCOS.selfTest = KOCOS.default(KOCOS_CONFIG.selfTest, computer.totalMemory() >= 2^19)
KOCOS.syscallTraceback = KOCOS.default(KOCOS_CONFIG.syscallTraceback, false)

KOCOS.version = "KOCOS incomplete"

function KOCOS.logAll(...)
    local t = {...}
    for i=1,#t do t[i] = tostring(t[i]) end
    return KOCOS.log("%s", table.concat(t, " "))
end

local function oceLog(s)
    if component.ocelot then
        component.ocelot.log(s)
    end
end

function KOCOS.log(fmt, ...)
    local time = computer.uptime()
    local s = string.format(fmt, ...)
    KOCOS.event.push("klog", s, time)
    oceLog(s)
end

function KOCOS.logPanic(fmt, ...)
    local time = computer.uptime()
    local s = string.format(fmt, ...)
    KOCOS.event.push("kpanic", s, time)
    oceLog("PANIC: " .. s)
end

local deferred = {}

function KOCOS.defer(func, prio)
    table.insert(deferred, {func = func, prio = prio})
    table.sort(deferred, function(a, b) return a.prio > b.prio end)
end

function KOCOS.hasDeferred()
    return #deferred > 0
end

function KOCOS.runDeferred(timeout)
    local start = computer.uptime()

    while computer.uptime() < start + timeout do
        local f = table.remove(deferred, 1)
        if not f then break end
        f.func()
    end
    return #deferred > 0
end

local looped = {}

function KOCOS.runOnLoop(func)
    table.insert(looped, func)
end

function KOCOS.runLoopedFuncs()
    for i=1,#looped do
        looped[i]()
    end
end

function KOCOS.pcall(f, ...)
    local ok, err = xpcall(f, debug.traceback, ...)
    if not ok then
        pcall(computer.beep)
        pcall(KOCOS.event.push, "kpanic", err, computer.uptime())
        pcall(oceLog, err)
    end
    return ok
end

function KOCOS.bsod()
    local panics = {}
    while KOCOS.event.queued("kpanic") do
        local _, msg, time = KOCOS.event.pop("kpanic")
        table.insert(panics, string.format("%.2f %s", time, msg))
    end
    local allPanicsText = "KERNEL CRASH\n" .. table.concat(panics, "\n") .. "\nPRESS ANY KEY TO REBOOT"
    if KOCOS.rebootOnCrash then
        allPanicsText = allPanicsText .. "\nSYSTEM WILL REBOOT AUTOMATICALLY"
    end
    local gpu = component.gpu
    for _, screen in component.list("screen") do
        gpu.bind(screen)
        gpu.setForeground(0xFFFFFF)
        if gpu.getDepth() > 1 then
            gpu.setBackground(0x0000FF)
        else
            gpu.setBackground(0x000000)
        end
        gpu.setResolution(gpu.maxResolution())
        local w, h = gpu.getResolution()

        gpu.fill(1, 1, w, h, " ")
        local i = 1
        for line in allPanicsText:gmatch("[^\n]+") do
            line = line:gsub("%\t", "    ")
            gpu.set(1, i, line)
            i = i + 1
            if i > h then
                i = h
                gpu.copy(1, 2, w, h - 1, 0, -1)
                gpu.fill(1, h, w, 1, " ")
            end
        end
    end
    local start = computer.uptime()
    if KOCOS.rebootOnCrash then
        while computer.uptime() - start < 5 do
            local event = computer.pullSignal(start + 5 - computer.uptime())
            if event == "key_down" then break end
        end
        computer.shutdown(true)
    else
        repeat
            local event = computer.pullSignal()
        until event == "key_down"
    end
end

function KOCOS.loop()
    local lastPanicked = false
    local function processEvents()
        KOCOS.event.process(0.05)
    end
    local function runProcesses()
        KOCOS.process.run()
    end
    local function reportCrash()
        KOCOS.event.push("kcrash", computer.uptime())
    end
    while true do
        local panicked = false
        panicked = panicked or not KOCOS.pcall(processEvents)
        panicked = panicked or not KOCOS.pcall(KOCOS.runLoopedFuncs)
        panicked = panicked or not KOCOS.pcall(runProcesses)
        panicked = panicked or not KOCOS.pcall(KOCOS.runDeferred, 0.05)
        if lastPanicked and panicked then
            pcall(reportCrash)
            pcall(KOCOS.bsod) -- LETS HOPE IT DOES NOT
            if KOCOS.rebootOnCrash then
                computer.shutdown(true)
            else
                computer.shutdown()
            end
        end
        lastPanicked = panicked
    end
end

-- For autocomplete
if 1<0 then
    _K = KOCOS
    _OS = _G
    _OSVERSION = "Unknown KOCOS"
    _KVERSION = KOCOS.version

    ---@param sys string
    ---@return ...
    function syscall(sys, ...) end
end

if KOCOS.init then
    KOCOS.defer(function()
        KOCOS.log("Running " .. KOCOS.init)
        assert(KOCOS.process.spawn(KOCOS.init, {
            traced = true,
        }))
    end, 0)
end

local yield = coroutine.yield
local resume = coroutine.resume

function coroutine.yield(...)
    return yield(false, ...)
end

function KOCOS.yield(...)
    return yield(true, ...)
end

function coroutine.resume(co, ...)
    while true do
        local t = {resume(co, ...)}
        if not t[1] then
            return table.unpack(t)
        end
        if not t[2] then
            return true, table.unpack(t, 3)
        end
        KOCOS.yield(table.unpack(t, 3))
    end
end

function KOCOS.resume(co, ...)
    local t = {resume(co, ...)}
    if not t[1] then
        return table.unpack(t)
    end
    -- Don't care if it IS sysyield or not
    return true, table.unpack(t, 3)
end

end
do
function table.copy(t)
    if type(t) == "table" then
        local nt = {}
        for k, v in pairs(t) do nt[k] = table.copy(v) end
        return nt
    else
        return t
    end
end

function string.split(inputstr, sep)
  if sep == nil then
    sep = "%s"
  end
  local t = {}
  for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
    table.insert(t, str)
  end
  return t
end

local memunits = {"B", "KiB", "MiB", "GiB", "TiB"}
---@param amount number
---@param spacing? string
function string.memformat(amount, spacing)
    spacing = spacing or ""
    local unit = 1
    local factor = 1024
    while unit < #memunits and amount >= factor do
        unit = unit + 1
        amount = amount / factor
    end

    return string.format("%.2f%s%s", amount, spacing, memunits[unit])
end

function string.startswith(s, prefix)
    return s:sub(1, #prefix) == prefix
end

function string.endswith(s, suffix)
    return s:sub(-#suffix) == suffix
end

function math.clamp(x, min, max)
    return math.min(max, math.max(x, min))
end

function math.map(x, min1, max1, min2, max2)
    return min2 + ((x - min1) / (max1 - min1)) * (max2 - min2)
end

end
do
local event

---@param maximum number
event = function(maximum)
    local buffer = {}
    local callbacks = {}
    ---@class KOCOS.EventSystem
    local system = {}

    function system.push(name, ...)
        table.insert(buffer, {name, ...})
        -- callback order is undefined
        for _, callback in pairs(callbacks) do
            local ok, err = pcall(callback, name, ...)
            if not ok and name ~= "event_err" then
                system.push("event_err", err, name)
            end
        end
        while #buffer > maximum do
            -- THIS MAY DISCARD SHIT SO CAREFULLLL
            table.remove(buffer, 1)
        end
    end


    function system.popWhere(f)
        for i=1,#buffer do
            if f(table.unpack(buffer[i])) then
                return table.unpack(table.remove(buffer, i))
            end
        end
    end

    function system.pop(...)
        local allowed = {...}
        if #allowed == 0 then
            -- we love heap allocs
            return table.unpack(table.remove(buffer, 1) or {})
        end

        return system.popWhere(function(kind)
            for i=1,#allowed do
                if kind == allowed[i] then return true end
            end
            return false
        end)
    end

    function system.queued(...)
        local allowed = {...}
        if #allowed == 0 then return #buffer > 1 end
        for i=1,#buffer do
            for j=1,#allowed do
                if buffer[i][1] == allowed[j] then return true end
            end
        end
        return false
    end

    function system.process(timeout)
        local s = {computer.pullSignal(timeout)}
        if #s > 0 then
            system.push(table.unpack(s))
        end
    end

    function system.listen(callback, id)
        id = id or tostring(callback)
        while callbacks[id] do id = "_" .. id end
        callbacks[id] = callback
        return id
    end

    function system.forget(id)
        callbacks[id] = nil
    end

    function system.clear()
        buffer = {}
    end

    system.create = event

    return system
end

KOCOS.event = event(KOCOS.maxEventBacklog)

KOCOS.log("Event subsystem loaded")

end
do
local testing = {}

function testing.uuid()
    local hexDigits = "0123456789abcdef"
    local rawHex = ""
    for _=1,32 do
        local i = math.random(1, #hexDigits)
        rawHex = rawHex .. hexDigits:sub(i, i)
    end
    return rawHex:sub(1, 8) .. '-'
        .. rawHex:sub(9, 12) .. '-'
        .. rawHex:sub(13, 16) .. '-'
        .. rawHex:sub(17, 20) .. '-'
        .. rawHex:sub(21, 32)
end

-- Creates a fake "drive" proxy with a random testing address
function testing.drive(sectorSize, capacity, name)
    name = name or ("TEST " .. testing.uuid())
    local invalidSector = math.floor(capacity / sectorSize) + 1

    local defaultSector = string.rep("\0", sectorSize)
    local sectors = {}

    local drive = {
        slot = -1,
        type = "drive",
        address = testing.uuid(),
        getLabel = function()
            return name
        end,
        setLabel = function()
            return name
        end,
        getPlatterCount = function()
            return 1
        end,
        getSectorSize = function()
            return sectorSize
        end,
        readSector = function(sector)
            assert(math.floor(sector) == sector, "DRIVE: sector is not integer")
            assert(sector > 0 and sector < invalidSector, "DRIVE: sector out of bounds")
            return sectors[sector] or defaultSector
        end,
        writeSector = function(sector, value)
            assert(math.floor(sector) == sector, "DRIVE: sector is not integer")
            assert(sector >= 0 and sector < invalidSector, "DRIVE: sector out of bounds")
            assert(#value == sectorSize, "DRIVE: sector value is not correct")
            sectors[sector] = value
            return true
        end,
        getCapacity = function()
            return capacity
        end,
    }
    local function sectorFromOff(off)
        local sec = off
        sec = sec - 1
        sec = sec / sectorSize
        sec = math.floor(sec)
        return sec + 1, off - sec * sectorSize
    end
    function drive.readByte(off)
        assert(math.floor(off) == off, "DRIVE: byte offset is not integer")
        if off < 1 or off > capacity then
            error("DRIVE: out of bounds")
        end
        local sector, idx = sectorFromOff(off)
        return drive.readSector(sector):byte(idx, idx)
    end
    function drive.writeByte(off, value)
        assert(math.floor(off) == off, "DRIVE: byte offset is not integer")
        if off < 1 or off > capacity then
            error("DRIVE: out of bounds")
        end
        assert(math.floor(value) == value, "DRIVE: byte is not integer")
        assert(value >= 0 and value < 256, "DRIVE: invalid byte")
        local sector, idx = sectorFromOff(off)
        local buffer = drive.readSector(sector)
        local pre = buffer:sub(1, idx-1)
        local post = buffer:sub(idx+1)
        buffer = pre .. string.char(value) .. post
        if #buffer ~= sectorSize then
            error(string.format("%d %d %d", sector, idx, off))
        end
        drive.writeSector(sector, buffer)
        return true
    end
    return drive
end

function testing.expectFail(f, ...)
    local ok = pcall(f, ...)
    assert(not ok, "operation should have failed")
end

---@generic T
---@param a T[]
---@param b T[]
function testing.expectSameSorted(a, b)
    assert(#a == #b, "mismatched lengths")
    table.sort(a)
    table.sort(b)
    for i=1,#a do
        assert(a[i] == b[i], "different data")
    end
end

local testCount = 0
local checkedCount = 0
function KOCOS.test(name, func)
    if not KOCOS.selfTest then return end
    testCount = testCount + 1
    KOCOS.defer(function()
        checkedCount = checkedCount + 1
        KOCOS.log("Testing %s [%d / %d]...", name, checkedCount, testCount)
        local ok, err = xpcall(func, debug.traceback)
        if ok then
            KOCOS.log("\x1b[32mPASSED\x1b[0m")
        else
            KOCOS.logPanic("\x1b[31mFAILED\x1b[0m: %s", err)
        end
    end, -1000 - testCount)
end

KOCOS.testing = testing

-- Testing our actual testing mechanism
KOCOS.test("self-tests", function()
    testing.expectFail(error, "can't fail")
end)

do
    -- Testing drive proxies
    local sectorSizes = {512, 1024, 2048}
    local sectorCounts = {1, 2, 4, 16}
    for _, sectorSize in ipairs(sectorSizes) do
        for _, sectorCount in ipairs(sectorCounts) do
            local capacity = sectorSize * sectorCount
            local name = string.format(
                "Drive proxy (%s, %s)",
                string.memformat(sectorSize),
                string.memformat(capacity)
            )
            KOCOS.test(name, function()
                local drive = testing.drive(sectorSize, capacity)
                assert(drive.getCapacity() == capacity)

                testing.expectFail(drive.writeByte, 2^32, 0)
                testing.expectFail(drive.writeByte, 1, 256)
                testing.expectFail(drive.writeSector, 2^32, string.rep(" ", sectorSize))
                testing.expectFail(drive.writeSector, 1, string.rep(" ", sectorSize-1))
                for _=1,32 do
                    local randomByte = math.random(0, 255)
                    local randomPos = math.random(1, capacity)
                    assert(drive.writeByte(randomPos, randomByte))
                    assert(drive.readByte(randomPos) == randomByte)
                end

                for i=1,sectorCount do
                    local data = ""
                    for _=1,sectorSize do
                        data = data .. string.char(math.random(0, 255))
                    end
                    assert(drive.writeSector(i,data))
                    assert(drive.readSector(i) == data)
                end

                local sectors = {}
                local sectorIdx = {}
                for i=1,sectorCount do
                    sectorIdx[i] = i
                end
                for i=1,#sectorIdx do
                    local j = math.random(i)
                    sectorIdx[i], sectorIdx[j] = sectorIdx[j], sectorIdx[i]
                end
                for j=1,sectorCount do
                    local i = sectorIdx[j]
                    local data = ""
                    for _=1,sectorSize do
                        data = data .. string.char(math.random(0, 255))
                    end
                    sectors[i] = data
                    assert(drive.writeSector(i, data))
                end
                for j=1,sectorCount do
                    local i = sectorIdx[j]
                    assert(drive.readSector(i) == sectors[i], "bad storage")
                end
            end)
        end
    end
end

KOCOS.log("Testing subsystem loaded")

end
do
local bit32Code = [[
bit32 = {}

function bit32.arshift(x, disp)
    return x >> disp
end

function bit32.band(...)
    local t = -1
    local n = select("#", ...)
    for i=1,n do
        local m = select(i, ...)
        t = t & m
    end
    return t
end

function bit32.bnot(x)
    return ~x
end

function bit32.bor(...)
    local t = 0
    local n = select("#", ...)
    for i=1,n do
        local m = select(i, ...)
        t = t | m
    end
    return t
end

function bit32.btest(...)
    return bit32.band(...) ~= 0
end

function bit32.bxor(...)
    local t = 0
    local n = select("#", ...)
    for i=1,n do
        local m = select(i, ...)
        t = t ~ m
    end
    return t
end

-- TODO: rest of bit32
-- See https://www.lua.org/manual/5.2/manual.html#6.7

KOCOS.test("bit32", function()
    assert(bit32.band(5, 3) == 1)
    assert(bit32.bor(5, 3) == 7)
    assert(bit32.bxor(5, 3) == 6)
    assert(bit32.arshift(65536, 16) == 1)
    assert(bit32.arshift(1, -16) == 65536)
end)
]]

if not bit32 then
    load(bit32Code, "=bit32")()
end

end
do
setmetatable(component, {
    __index = function(_, key)
        local primary = component.list(key)()
        if not primary then return nil end
        -- TODO: cache primaries
        return component.proxy(primary)
    end,
})

local addrRings = {}
local typeRings = {}

function component.setRingForAddress(address, ring)
    addrRings[address] = ring
end

function component.setRingForType(type, ring)
    typeRings[type] = ring
end

---@param address string
---@return integer
function component.ringFor(address)
    if addrRings[address] then return addrRings[address] end
    local t = component.type(address)
    if typeRings[t] then return typeRings[t] end
    return math.huge
end

component.setRingForType("gpu", 1)
component.setRingForType("screen", 1)
component.setRingForType("filesystem", 1)
component.setRingForType("drive", 1)
component.setRingForType("computer", 1)
component.setRingForType("eeprom", 0)

KOCOS.log("Component subsystem loaded")

end
do
-- TODO: support symlinks

---@alias KOCOS.FileSystemDriver table

---@class KOCOS.File
---@field mode "w"|"r"
---@field refc integer
---@field kind "disk"|"memory"|"pipe"
---@field events KOCOS.EventSystem

---@class KOCOS.DiskFile: KOCOS.File
---@field kind "disk"
---@field fd any
---@field manager KOCOS.FileSystemDriver

---@class KOCOS.MemoryFile: KOCOS.File
---@field kind "memory"
---@field buffer string
---@field bufcap integer
---@field cursor integer

---@class KOCOS.PipeFile: KOCOS.File
---@field kind "pipe"
---@field output KOCOS.File
---@field input KOCOS.File

local fs = {}

fs.drivers = {}

---@type {[string]: KOCOS.FileSystemDriver}
local globalTranslation = {}

---@param path string
function fs.canonical(path)
    if path:sub(1, 1) == "/" then path = path:sub(2) end
    local parts = string.split(path, "%/")
    local stack = {}

    for _, part in ipairs(parts) do
        if #part > 0 then
            table.insert(stack, part)
            if part == string.rep(".", #part) then
                for _=1,#part do
                    stack[#stack] = nil
                end
            end
        end
    end

    return "/" .. table.concat(stack, "/")
end

function fs.join(...)
    return fs.canonical(table.concat({...}, "/"))
end

---@return KOCOS.FileSystemDriver, string
function fs.resolve(path)
    path = fs.canonical(path)
    if path:sub(1, 1) == "/" then path = path:sub(2) end
    local parts = string.split(path, "%/")

    for i=#parts, 1, -1 do
        local subpath = table.concat(parts, "/", 1, i)
        local manager = globalTranslation[subpath]
        if type(manager) == "table" then
            return manager, table.concat(parts, "/", i+1)
        end
    end

    return globalTranslation[""], path
end

---@param mode "w"|"r"
---@param content? string
---@param maximum? integer
---@return KOCOS.MemoryFile
function fs.mopen(mode, content, maximum)
    content = content or ""
    maximum = maximum or math.huge
    if #content > maximum then content = content:sub(1, maximum) end
    ---@type KOCOS.MemoryFile
    return {
        mode = mode,
        kind = "memory",
        refc = 1,
        events = KOCOS.event.create(KOCOS.maxEventBacklog),
        -- Memory shit
        buffer = content,
        bufcap = maximum,
        cursor = 0,
    }
end

---@param input KOCOS.File
---@param output KOCOS.File
---@return KOCOS.PipeFile
function fs.mkpipe(input, output)
    ---@type KOCOS.PipeFile
    local pipe = {
        mode = "w",
        kind = "pipe",
        refc = 1,
        events = KOCOS.event.create(KOCOS.maxEventBacklog),
        input = input,
        output = output,
    }

    fs.retain(input)
    fs.retain(output)
    return pipe
end

---@param path string
---@param mode "w"|"r"
---@return KOCOS.DiskFile?, string
function fs.open(path, mode)
    if fs.type(path) == "missing" then return nil, "missing file" end
    -- Pre-alloc file to OOM early
    ---@type KOCOS.DiskFile
    local file = {
        mode = mode,
        kind = "disk",
        refc = 1,
        events = KOCOS.event.create(KOCOS.maxEventBacklog),
        fd = 0, -- we set to 0 and not nil so .fd = fd doesn't OOM us
        manager = fs,
    }

    local manager, truepath = fs.resolve(path)
    file.manager = manager
    local fd, err = manager:open(truepath, mode)
    if err then return nil, err end
    file.fd = fd
    return file, ""
end


---@param file KOCOS.File
---@param n? integer
function fs.retain(file, n)
    n = n or 1
    file.refc = file.refc + n
end

---@alias KOCOS.FileType "file"|"directory"|"missing"

---@param path string
---@return KOCOS.FileType
function fs.type(path)
    local manager, realpath = fs.resolve(path)

    return manager:type(realpath)
end

---@param path string
---@return boolean
function fs.readonly(path)
    local manager, realpath = fs.resolve(path)

    return manager:isReadOnly(realpath)
end

---@param path string
---@return string[]?, string
function fs.list(path)
    local manager, realpath = fs.resolve(path)

    return manager:list(realpath)
end

---@param path string
function fs.exists(path)
    return fs.type(path) ~= "missing"
end

---@param file KOCOS.File
---@return boolean, string
function fs.close(file)
    file.refc = file.refc - 1
    if file.refc > 0 then return true, "" end

    file.events.clear()

    if file.kind == "disk" then
        ---@cast file KOCOS.DiskFile
        file.manager:close(file.fd)
    elseif file.kind == "pipe" then
        ---@cast file KOCOS.PipeFile
        pcall(fs.close, file.input)
        pcall(fs.close, file.output)
    end
    return true, ""
end

---@param file KOCOS.File
---@param data string
---@return boolean, string
function fs.write(file, data)
    pcall(file.events.push, "write", data)
    if file.kind == "disk" then
        ---@cast file KOCOS.DiskFile
        return file.manager:write(file.fd, data)
    elseif file.kind == "memory" then
        ---@cast file KOCOS.MemoryFile
        if file.mode == "w" then
            if (#file.buffer + #data) > file.bufcap then
                return false, "out of space"
            end
            file.buffer = file.buffer .. data
            return true, ""
        end

        if #file.buffer + #data > file.bufcap then return false, "out of space" end

        local before = file.buffer:sub(1, file.cursor)
        local after = file.buffer:sub(file.cursor+1)
        file.buffer = before .. data .. after
        file.cursor = file.cursor + #data
    elseif file.kind == "pipe" then
        ---@cast file KOCOS.PipeFile
        return fs.write(file.output, data)
    end
    return false, "bad file"
end

---@param file KOCOS.File
---@param len integer
---@return string?, string?
function fs.read(file, len)
    pcall(file.events.push, "read", len)
    if file.kind == "disk" then
        ---@cast file KOCOS.DiskFile
        return file.manager:read(file.fd, len)
    elseif file.kind == "memory" then
        ---@cast file KOCOS.MemoryFile
        if file.mode == "w" then
            if len < #file.buffer then
                local chunk = file.buffer:sub(1, len)
                file.buffer = file.buffer:sub(len+1)
                return chunk, nil
            end
            local data = file.buffer or ""
            file.buffer = ""
            return data, nil
        end

        local data = file.buffer:sub(file.cursor+1, (len ~= math.huge) and file.cursor+len or nil)
        if #data == 0 then
            if file.mode == "w" then
                -- w means this is used as an endless stream
                return "", nil
            end
            return nil, nil
        end
        file.cursor = file.cursor + #data
        if file.cursor > #file.buffer then
            file.cursor = #file.buffer
        end
        return data, nil
    elseif file.kind == "pipe" then
        ---@cast file KOCOS.PipeFile
        return fs.read(file.input, len)
    end
    return nil, "bad file"
end

---@param file KOCOS.File
---@param whence "set"|"cur"|"end"
---@param offset integer
---@return integer?, string
function fs.seek(file, whence, offset)
    pcall(file.events.push, "seek", whence, offset)

    if file.kind == "memory" then
        ---@cast file KOCOS.MemoryFile
        if file.mode == "w" then
            return nil, "unable to seek stream"
        end
        if whence == "set" then
            file.cursor = offset
        elseif whence == "cur" then
            file.cursor = file.cursor + offset
        elseif whence == "end" then
            file.cursor = #file.buffer - offset
        end
        if file.cursor < 0 then file.cursor = 0 end
        if file.cursor > #file.buffer then file.cursor = #file.buffer end
        return file.cursor, ""
    end

    if file.kind == "disk" then
        ---@cast file KOCOS.DiskFile
        return file.manager:seek(file.fd, whence, offset)
    end

    -- TODO: implement
    return nil, "bad file"
end

---@param file KOCOS.File
---@param action string
---@return ...
function fs.ioctl(file, action, ...)
    if file.kind == "memory" then
        ---@cast file KOCOS.MemoryFile
        if action == "clear" then
            file.buffer = ""
            file.cursor = 0
            file.events.clear()
            return true
        end
        if action == "fetch" then
            return file.buffer
        end
    end

    if file.kind == "disk" then
        ---@cast file KOCOS.DiskFile
        return file.manager:ioctl(file.fd, ...)
    end

    error("bad file")
end

---@param path string
---@return integer
function fs.spaceUsed(path)
    local manager = fs.resolve(path)
    return manager:spaceUsed()
end

---@param path string
---@return integer
function fs.spaceTotal(path)
    local manager = fs.resolve(path)
    return manager:spaceTotal()
end

---@param path string
---@return integer
function fs.size(path)
    local manager, truePath = fs.resolve(path)
    return manager:size(truePath)
end

---@param path string
function fs.parentOf(path)
    local parts = string.split(fs.canonical(path), "%/")
    parts[#parts] = nil
    return "/" .. table.concat(parts, "/")
end

---@return KOCOS.Partition
function fs.partitionOf(path)
    local manager = fs.resolve(path)
    return manager:getPartition()
end

---@class KOCOS.Partition
---@field drive table
---@field startByte integer
---@field byteSize integer
---@field name string
---@field uuid string
---@field storedKind string
---@field kind "boot"|"root"|"user"|"reserved"
---@field readonly boolean

---@alias KOCOS.PartitionParser fun(component: table): KOCOS.Partition[]?, string?

---@type KOCOS.PartitionParser[]
fs.partitionParsers = {}

---@param parser KOCOS.PartitionParser
function fs.addPartitionParser(parser)
    table.insert(fs.partitionParsers, parser)
end

---@param drive table
---@return KOCOS.Partition[], string
function fs.getPartitions(drive)
    for i=#fs.partitionParsers,1,-1 do
        local parts, format = fs.partitionParsers[i](drive)
        if parts then
            return parts, format or "unknown"
        end
    end

    -- Can't partition if unknown lol
    return {}, "unsupported"
end

function fs.addDriver(driver)
    table.insert(fs.drivers, driver)
end

---@param partition KOCOS.Partition
---@return KOCOS.FileSystemDriver?
function fs.driverFor(partition)
    for i=#fs.drivers,1,-1 do
        local driver = fs.drivers[i]
        local manager = driver.create(partition)
        if manager then return manager end
    end
end

---@param path string
---@param partition KOCOS.Partition
function fs.mount(path, partition)
    path = fs.canonical(path)
    assert(not fs.isMounted(partition), "already mounted")
    assert(fs.type(path) == "directory", "not a directory")
    assert(#fs.list(path) == 0, "not empty directory")
    local location = fs.canonical(path):sub(2)
    assert(not globalTranslation[location], "duplicate mountpoint")
    globalTranslation[location] = assert(fs.driverFor(partition), "missing driver")
end

---@param path string
function fs.unmount(path)
    assert(fs.isMount(path), "missing mountpoint")
    local location = fs.canonical(path):sub(2)
    globalTranslation[location] = nil
end

---@param path string
function fs.isMount(path)
    local location = fs.canonical(path):sub(2)
    return globalTranslation[location] ~= nil
end

---@return boolean, string
function fs.touch(path, permissions)
    local manager, truepath = fs.resolve(path)
    return manager:touch(truepath, permissions)
end

---@param path string
---@return boolean, string
function fs.mkdir(path, permissions)
    path = fs.canonical(path)
    if fs.type(fs.parentOf(path)) ~= "directory" then
        return false, "parent is not directory"
    end
    local manager, truePath = fs.resolve(path)
    return manager:mkdir(truePath, permissions)
end

---@param path string
---@return integer
function fs.permissionsOf(path)
    local manager, truePath = fs.resolve(path)
    return manager:permissionsOf(truePath)
end

---@param path string
---@return boolean, string
function fs.remove(path)
    if fs.isMount(path) then return false, "is mountpoint" end
    if fs.type(path) == "directory" then
        local l, err = fs.list(path)
        if err then return false, err end
        if #l > 0 then return false, "directory not empty" end
    end
    local manager, truePath = fs.resolve(path)
    return manager:remove(truePath)
end

---@param partition KOCOS.Partition
---@param format string
---@param opts table?
---@return boolean, string?
function fs.format(partition, format, opts)
    for i=#fs.drivers,1,-1 do
        local driver = fs.drivers[i]
        local ok, err = driver.format(partition, format, opts)
        if ok then return true end
        if err then return false, err end
    end
    return false, "unsupported"
end

---@param tested string
---@param reference string
---@param autocomplete boolean
local function sameUuid(tested, reference, autocomplete)
    if autocomplete then
        return string.startswith(tested:gsub("%-", ""), reference)
    end
    return tested == reference
end

-- ONLY SUPPORTS VANILLA UNMANAGED DRIVES !!!!!!!!!!
---@return KOCOS.Partition?
function fs.wholeDrivePartition(drive)
    if drive.type ~= "drive" then return end
    ---@type KOCOS.Partition
    return {
        name = drive.getLabel() or drive.address:sub(1, 6),
        drive = drive,
        uuid = drive.address,
        startByte = 0,
        byteSize = drive.getCapacity(),
        kind = "root",
        readonly = false,
        storedKind = drive.address,
    }
end

---@param opts {allowFullDrivePartition: boolean, mountedOnly: boolean}
---@return KOCOS.Partition[]
function fs.findAllPartitions(opts)
    local parts = {}
    for _, partition in fs.mountedPartitions() do
        table.insert(parts, partition)
    end

    if not opts.mountedOnly then
        for addr in component.list() do
            local drive = component.proxy(addr)
            local localParts = fs.getPartitions(drive)
            if opts.allowFullDrivePartition then
                local wholeDrive = fs.wholeDrivePartition(drive)
                if wholeDrive then
                    table.insert(parts, wholeDrive)
                end
            end
            for i=1,#localParts do
                table.insert(parts, localParts[i])
            end
        end
    end
    local dupeMap = {}
    local deduped = {}
    for i=1,#parts do
        local part = parts[i]
        if not dupeMap[part.uuid] then
            dupeMap[part.uuid] = true
            table.insert(deduped, part)
        end
    end
    return deduped
end

-- Supports autocomplete
---@param uuid string
---@param opts {autocomplete: boolean, allowFullDrivePartition: boolean, mountedOnly: boolean}?
---@return KOCOS.Partition?
function fs.partitionFromUuid(uuid, opts)
    opts = opts or {
        autocomplete = true,
        allowFullDrivePartition = true,
        mountedOnly = false,
    }

    local parts = fs.findAllPartitions(opts)
    for i=1,#parts do
        if sameUuid(parts[i].uuid, uuid, opts.autocomplete) then
            return parts[i]
        end
    end
    return nil
end

---@return fun(...): string?, KOCOS.Partition
function fs.mountedPartitions()
    return function(_, mountpoint)
        mountpoint = next(globalTranslation, mountpoint)
        ---@diagnostic disable-next-line: missing-return-value
        if not mountpoint then return end
        local manager = globalTranslation[mountpoint]
        return mountpoint, manager:getPartition()
    end
end

---@param partition KOCOS.Partition
---@return boolean, string
function fs.isMounted(partition)
    for mountpoint, part in fs.mountedPartitions() do
        if part.uuid == partition.uuid then
            return true, "/" .. mountpoint
        end
    end
    return false, ""
end

KOCOS.fs = fs

KOCOS.log("Loaded filesystem")

KOCOS.defer(function()
    local root = KOCOS.defaultRoot
    local parts = fs.getPartitions(component.proxy(root))
    ---@type KOCOS.Partition?
    local rootPart

    for i=1,#parts do
        if (parts[i].kind == "root") or (KOCOS.rootPart == parts[i].uuid) then
            if parts[i].uuid == (KOCOS.rootPart or parts[i].uuid) then
                rootPart = parts[i]
            end
        end
    end

    assert(rootPart, "missing root partition on " .. root)
    globalTranslation[""] = assert(fs.driverFor(rootPart), "MISSING ROOTFS DRIVER OH NO")
    KOCOS.log("Mounted default root")
end, 3)

end
do
---@class KOCOS.Lock
---@field locked boolean
local lock = {}
lock.__index = lock

function lock.create()
    return setmetatable({locked = false}, lock)
end

function lock:tryLock()
    if self.locked then return false end
    self.locked = true
    return true
end

---@param timeout integer
function lock:lock(timeout)
    local deadline = computer.uptime() + timeout
    while self.locked do
        if computer.uptime() > deadline then
            error("timeout")
        end
        coroutine.yield()
    end
    self.locked = true
end

function lock:unlock()
    self.locked = false
end

KOCOS.lock = lock

---@class KOCOS.Thread
---@field process integer
---@field coro thread
---@field mode "alive"|"suspended"|"dead"
---@field name string
---@field nextTime integer
---@field id integer
local thread = {}
thread.__index = thread

function thread.create(func, name, process, id)
    local t = setmetatable({
        process = process,
        coro = coroutine.create(func),
        mode = "alive",
        name = name,
        id = id,
        nextTime = 0,
    }, thread)
    table.insert(KOCOS.process.nextThreads, t)
    return t
end

function thread:dead()
    return self.mode == "dead"
end

function thread:suspended()
    return self.mode == "suspended"
end

function thread:suspend()
    if not self:dead() then self.mode = "suspended" end
end

function thread:resume()
    if not self:dead() then self.mode = "alive" end
end

function thread:status()
    return self.mode, coroutine.status(self.coro)
end

function thread:kill(msg, trace)
    if self:dead() then return end
    if KOCOS.logThreadEvents then
        KOCOS.logAll(self.name, msg, trace)
    end
    msg = msg or "thread killed"
    trace = trace or debug.traceback(self.coro)
    self.mode = "dead" -- End game

    if coroutine.close then coroutine.close(self.coro) end

    local proc = KOCOS.process.procs[self.process]
    if not proc then return end -- Process died before us???

    proc:raise("thread_killed", self.id, msg, trace)

    proc.threads[self.id] = nil -- bye bye
    if next(proc.threads) then return end
    if not proc.parent then return end

    local parent = KOCOS.process.procs[proc.parent]
    if not parent then return end
    parent:raise("child_terminated", proc.pid)
end

function thread:tick()
    if thread:dead() then return end
    if thread:suspended() then return end
    if computer.uptime() < self.nextTime then return end
    local ok, val = KOCOS.resume(self.coro)
    if ok then
        if type(val) == "number" then
            self.nextTime = computer.uptime() + val
        end
    elseif not ok then
        self:kill(val, debug.traceback(self.coro))
    end
    if coroutine.status(self.coro) == "dead" then
        self:kill("terminated", "")
    end
end

KOCOS.thread = thread

---@alias KOCOS.ResourceKind "file"|"lock"|"event"|"socket"

---@class KOCOS.Resource
---@field kind KOCOS.ResourceKind

---@class KOCOS.FileResource: KOCOS.Resource
---@field kind "file"
---@field file KOCOS.File

---@class KOCOS.LockResource: KOCOS.Resource
---@field kind "lock"
---@field lock KOCOS.Lock

---@class KOCOS.EventResource: KOCOS.Resource
---@field kind "event"
---@field event KOCOS.EventSystem

---@class KOCOS.SocketResource: KOCOS.Resource
---@field kind "socket"
---@field rc integer
---@field socket KOCOS.NetworkSocket

---@class KOCOS.Process
---@field ring number
---@field cmdline string
---@field args {[number]: string}
---@field env {[string]: string}
---@field pid integer
---@field uid integer
---@field status integer
---@field events KOCOS.EventSystem
---@field namespace _G
---@field parent? integer
---@field threads {[integer]: KOCOS.Thread}
---@field children {[integer]: KOCOS.Process}
---@field modules {[string]: string}
---@field resources {[integer]: KOCOS.Resource}
---@field traced boolean
local process = {}
process.__index = process

---@type {[integer]: KOCOS.Process}
process.procs = {}
process.lpid = 0

---@type KOCOS.Thread[]
process.nextThreads = {}
---@type KOCOS.Thread[]
process.currentThreads = {}

---@class KOCOS.Loader
---@field check fun(proc: KOCOS.Process, path: string): boolean, any
---@field load fun(proc: KOCOS.Process, data: any)

---@type KOCOS.Loader[]
process.loaders = {}

---@param loader KOCOS.Loader
function process.addLoader(loader)
    table.insert(process.loaders, loader)
end

---@param err string
local function trimLoc(err)
    return err:gsub("[^:]+:[^:]+:%s", "", 1)
end

local function rawSpawn(init, config)
    config = config or {}

    local ring = math.floor(config.ring or 0)
    local cmdline = config.cmdline or ""
    local args = config.args or {}
    local env = config.env or {}
    local uid = config.uid or 0
    local pid = process.lpid + 1

    ---@type KOCOS.Process
    local proc = setmetatable({}, process)
    proc.traced = not not config.traced

    local namespace = {}

    if KOCOS.allowGreenThreads then
        namespace.coroutine = table.copy(coroutine)
    else
        namespace.coroutine = {
            yield = coroutine.yield,
        }
    end
    local syscall = function(name, ...)
        local sys = KOCOS.syscalls[name]
        if not sys then return "bad syscall" end
        local t = {xpcall(sys, KOCOS.syscallTraceback and debug.traceback or trimLoc, proc, ...)}
        if t[1] then
            return nil, table.unpack(t, 2)
        else
            return t[2]
        end
    end
    -- Small optimization to do it like this
    if proc.traced then
        function namespace.syscall(name, ...)
            local tracer = process.procs[proc.parent or proc.pid]
            -- Should never happen though
            if not tracer then return syscall(name, ...) end
            local a = {...}
            tracer:raise("syscall", name, a)
            local r = {syscall(name, ...)}
            tracer:raise("sysret", name, a, r)
            return table.unpack(r)
        end
    else
        namespace.syscall = syscall
    end
    if ring <= 1 then
        namespace._K = KOCOS
    end
    if ring == 0 then
        namespace._OS = _G
    end
    namespace.arg = table.copy(args)
    namespace._G = namespace
    namespace._VERSION = _VERSION
    namespace._OSVERSION = _OSVERSION or "Unknown KOCOS"
    namespace._KVERSION = KOCOS.version
    namespace.assert = assert
    namespace.error = error
    namespace.getmetatable = getmetatable
    namespace.ipairs = ipairs
    namespace.load = function(code, name, kind, _G)
        return load(code, name, kind, _G or namespace)
    end
    namespace.next = next
    namespace.pairs = pairs
    namespace.pcall = pcall
    namespace.rawequal = rawequal
    namespace.rawget = rawget
    namespace.rawset = rawset
    namespace.rawlen = rawlen
    namespace.select = select
    namespace.setmetatable = setmetatable
    namespace.tonumber = tonumber
    namespace.tostring = tostring
    namespace.type = type
    namespace.xpcall = xpcall
    namespace.bit32 = table.copy(bit32)
    namespace.table = table.copy(table)
    namespace.string = table.copy(string)
    namespace.math = table.copy(math)
    namespace.debug = table.copy(debug)
    namespace.os = table.copy(os)
    namespace.checkArg = checkArg
    namespace.unicode = table.copy(unicode)
    namespace.utf8 = table.copy(utf8)

    proc.pid = pid
    proc.parent = config.parent
    proc.children = {}
    proc.events = KOCOS.event.create(KOCOS.maxEventBacklog)
    proc.args = table.copy(args)
    proc.env = table.copy(env)
    proc.ring = ring
    proc.cmdline = cmdline
    proc.namespace = namespace
    proc.threads = {}
    proc.modules = {}
    proc.resources = {}
    proc.uid = uid

    if type(init) == "function" then
        proc:attach(init)
    elseif type(init) == "string" then
        local loaded = false
        for _, loader in ipairs(process.loaders) do
            local ok, data = loader.check(proc, init)
            if ok then
                loader.load(proc, data)
                loaded = true
                break
            end
        end
        assert(loaded, "missing loader")
    end

    process.procs[pid] = proc
    process.lpid = pid
    return proc
end

function process.spawn(init, config)
    local ok, val = pcall(rawSpawn, init, config)
    if ok then
        return val
    end
    return nil, val
end

function process:attach(func, name)
    local id = 1
    while self.threads[id] do id = id + 1 end
    name = name or tostring("thread #" .. id)

    local t = thread.create(func, name, self.pid, id)
    self.threads[id] = t
    return t
end

function process:raise(name, ...)
    self.events.push(name, ...)
end

function process:define(module, data)
    self.modules[module] = data
end

function process.byPid(pid)
    return process.procs[pid]
end

function process:kill()
    for _, thread in pairs(self.threads) do
        thread:kill("process terminated", "")
    end

    for _, resource in pairs(self.resources) do
        process.closeResource(resource)
    end

    if self.parent then
        local parent = process.byPid(self.parent)
        if parent then
            parent.children[self.pid] = nil
        end
    end
    process.procs[self.pid] = nil
end

---@param resource KOCOS.Resource
---@param n? integer
function process.retainResource(resource, n)
    n = n or 1
    if resource.kind == "file" then
        ---@cast resource KOCOS.FileResource
        KOCOS.fs.retain(resource.file, n)
    elseif resource.kind == "socket" then
        ---@cast resource KOCOS.SocketResource
        resource.rc = resource.rc + n
    end
end

---@return integer
function process:newFD()
    local fd = #self.resources
    while self.resources[fd] do fd = fd + 1 end
    return fd
end

---@param resource KOCOS.Resource
---@return integer
function process:moveResource(resource)
    local fd = self:newFD()
    self.resources[fd] = resource
    return fd
end

---@param resource KOCOS.Resource
---@return integer
function process:giveResource(resource)
    -- if OOM, we're still good!!!!
    local fd = self:moveResource(resource)
    local ok, err = pcall(process.retainResource, resource)
    if not ok then
        self.resources[fd] = nil -- Nope, bye
        error(err)
    end
    return fd
end

---@param pid integer
---@return boolean
function process:isDescendant(pid)
    if self.children[pid] then return true end

    for _, child in pairs(self.children) do
        if child:isDescendant(pid) then return true end
    end

    return false
end

---@param resource KOCOS.Resource
function process.closeResource(resource)
    if resource.kind == "file" then
        ---@cast resource KOCOS.FileResource
        KOCOS.fs.close(resource.file)
    elseif resource.kind == "socket" then
        ---@cast resource KOCOS.SocketResource
        resource.rc = resource.rc - 1
        if resource.rc <= 0 then
            KOCOS.network.close(resource.socket)
        end
    end
end

function process.run()
    if #process.currentThreads == 0 then
        process.currentThreads = process.nextThreads
        process.nextThreads = {}
    end
    for i=1,#process.currentThreads do
        local thread = process.currentThreads[i]
        thread:tick()
        if not thread:dead() then
            table.insert(process.nextThreads, thread)
        end
    end

    process.currentThreads = process.nextThreads
    process.nextThreads = {}
end

process.addLoader({
    check = function (proc, path)
        local f = assert(KOCOS.fs.open(path, "r"))
        local data = ""
        while true do
            local chunk, err = KOCOS.fs.read(f, math.huge)
            if err then
                KOCOS.fs.close(f)
                error(err)
            end
            if not chunk then break end
            data = data .. chunk
        end
        KOCOS.fs.close(f)
        local fun = load(data, "=" .. (proc.args[0] or path), "bt", proc.namespace)
        return fun ~= nil, fun
    end,
    load = function (proc, data)
        proc:attach(function()
            return data(table.unpack(proc.args))
        end, "main")
    end,
})

KOCOS.process = process

-- In case syscalls are deleted, technically allowed!
KOCOS.syscalls = {}

end
do
local network = {}

network.drivers = {}
network.resolvers = {}

network.EVENT_READ_RESPONSE = "packet"
network.EVENT_WRITE_RESPONSE = "response"
network.EVENT_CONNECT_RESPONSE = "connect"
network.EVENT_CONNECT_REQUEST = "pending_connect"
network.EVENT_CLOSE_RESPONSE = "closed"

function network.addDriver(driver)
    table.insert(network.drivers, driver)
end

function network.addResolver(resolver)
    table.insert(network.resolvers, resolver)
end

---@class KOCOS.NetworkAddressInfo
---@field address string
---@field port? number

---@param address string
---@param protocol? string
---@return KOCOS.NetworkAddressInfo?
function network.getAddressInfo(address, protocol)
    for i=#network.resolvers,1,-1 do
        local addrinfo = network.resolvers[i](address, protocol)
        if addrinfo then return addrinfo end
    end
end

---@class KOCOS.NetworkSocket
---@field protocol string
---@field subprotocol string
---@field process KOCOS.Process
---@field events KOCOS.EventSystem
---@field state "connected"|"listening"|"none"
---@field manager table

---@param protocol string
---@param subprotocol string
---@param options any
---@param process KOCOS.Process
---@return KOCOS.NetworkSocket?, string
function network.newSocket(protocol, subprotocol, options, process)
    options = options or {}
    for i=#network.drivers,1,-1 do
        local driver = network.drivers[i]
        local manager, err = driver(protocol, subprotocol, options, process)
        -- err means accepted, but with an error
        if err then return nil, err end
        if manager then
            ---@type KOCOS.NetworkSocket
            local sock = {
                protocol = protocol,
                subprotocol = subprotocol,
                process = process,
                events = KOCOS.event.create(options.backlog or KOCOS.maxEventBacklog),
                state = "none",
                manager = manager,
            }
            return sock, ""
        end
    end
    return nil, "missing protocol"
end

---@param socket KOCOS.NetworkSocket
function network.listen(socket, options)
    if socket.state ~= "none" then
        error("bad state")
    end
    socket.manager:listen(socket, options)
    socket.state = "listening"
end

---@param socket KOCOS.NetworkSocket
function network.connect(socket, address, options)
    if socket.state ~= "none" then
        error("bad state")
    end
    socket.manager:connect(socket, address, options)
    socket.state = "connected"
end

---@param socket KOCOS.NetworkSocket
---@return string -- Packet ID
function network.async_connect(socket, address, options)
    if socket.state ~= "none" then
        error("bad state")
    end
    return socket.manager:async_connect(socket, address, options)
end

---@param socket KOCOS.NetworkSocket
---@return KOCOS.NetworkSocket -- The client
function network.accept(socket)
    if socket.state ~= "listening" then
        error("bad state")
    end
    return socket.manager:accept(socket)
end

---@param socket KOCOS.NetworkSocket
---@param data string
---@return integer
-- Returns how many bytes were written
function network.write(socket, data)
    assert(socket.state == "connected", "not connected")
    return socket.manager:write(socket, data)
end

---@param socket KOCOS.NetworkSocket
---@param data string
---@return string
-- Returns packet ID
function network.async_write(socket, data)
    return socket.manager:async_write(socket, data)
end

---@param socket KOCOS.NetworkSocket
---@param len integer
---@return string?
function network.read(socket, len)
    assert(socket.state == "connected", "not connected")
    return socket.manager:read(socket, len)
end

---@param socket KOCOS.NetworkSocket
---@param len integer
---@return string
-- Returns packet ID
function network.async_read(socket, len)
    return socket.manager:async_read(socket, len)
end

---@param socket KOCOS.NetworkSocket
---@param action string
function network.ioctl(socket, action, ...)
    return socket.manager:ioctl(socket, action, ...)
end

---@param socket KOCOS.NetworkSocket
function network.close(socket)
    socket.manager:close(socket)
end

KOCOS.network = network

KOCOS.log("Network subsystem loaded")

end
do
---@type {[string]: fun(proc: KOCOS.Process, ...):...}
local syscalls = {}

-- File and socket syscalls

---@param path string
---@param mode "w"|"r"
function syscalls.open(proc, path, mode)
    assert(type(path) == "string", "invalid path")
    assert(mode == "w" or mode == "r", "invalid mode")

    assert(KOCOS.fs.exists(path), "not found")

    local perms = KOCOS.fs.permissionsOf(path)
    assert(KOCOS.perms.canRead(proc.uid, perms), "permission denied")

    if mode ~= "r" then
        assert(KOCOS.perms.canWrite(proc.uid, perms), "permission denied")
    end

    local f = assert(KOCOS.fs.open(path, mode))

    ---@type KOCOS.FileResource
    local res = {
        kind = "file",
        file = f,
    }

    local ok, fd = pcall(KOCOS.process.moveResource, proc, res)
    if ok then
        return fd
    end

    KOCOS.fs.close(f)
    error(fd)
end

---@param path string
---@param permissions string
function syscalls.touch(proc, path, permissions)
    assert(type(path) == "string", "bad path")
    assert(permissions >= 0 and permissions < 2^16, "bad permissions")
    assert(not KOCOS.fs.exists(path), "already exists")

    local parentPerms = KOCOS.fs.permissionsOf(KOCOS.fs.parentOf(path))
    assert(KOCOS.perms.canWrite(proc.uid, parentPerms), "permission denied")

    assert(KOCOS.fs.touch(path, permissions))
end

---@param path string
---@param permissions string
function syscalls.mkdir(proc, path, permissions)
    assert(type(path) == "string", "bad path")
    assert(permissions >= 0 and permissions < 2^16, "bad permissions")
    assert(not KOCOS.fs.exists(path), "already exists")

    local parentPerms = KOCOS.fs.permissionsOf(KOCOS.fs.parentOf(path))
    assert(KOCOS.perms.canWrite(proc.uid, parentPerms), "permission denied")

    assert(KOCOS.fs.mkdir(path, permissions))
end

---@param path string
function syscalls.remove(proc, path)
    assert(type(path) == "string", "bad path")
    assert(KOCOS.fs.exists(path), "not found")
    local perms = KOCOS.fs.permissionsOf(path)
    assert(KOCOS.perms.canWrite(proc.uid, perms), "permission denied")
    assert(KOCOS.fs.remove(path))
end

---@param mode "w"|"r"
---@param contents string
---@param limit integer
function syscalls.mopen(proc, mode, contents, limit)
    assert(mode == "w" or mode == "r", "invalid mode")
    assert(type(contents) == "string", "invalid contents")
    assert(type(limit) == "number", "invalid limit")

    local f = assert(KOCOS.fs.mopen(mode, contents, limit))
    -- theoretical OOM case here. TODO: fix it

    ---@type KOCOS.FileResource
    local res = {
        kind = "file",
        file = f,
    }

    local ok, fd = pcall(KOCOS.process.moveResource, proc, res)
    if ok then
        return fd
    end

    KOCOS.fs.close(f)
    error(fd)
end

---@param inFd integer
---@param outFd integer
function syscalls.mkpipe(proc, inFd, outFd)
    assert(type(inFd) == "number", "bad input fd")
    assert(type(outFd) == "number", "bad output fd")

    local inRes = assert(proc.resources[inFd], "bad input fd")
    assert(inRes.kind == "file", "bad input fd")
    ---@cast inRes KOCOS.FileResource
    local outRes = assert(proc.resources[outFd], "bad output fd")
    assert(outRes.kind == "file", "bad output fd")
    ---@cast outRes KOCOS.FileResource

    local f = assert(KOCOS.fs.mkpipe(inRes.file, outRes.file))

    ---@type KOCOS.FileResource
    local res = {
        kind = "file",
        file = f,
    }

    local ok, fd = pcall(KOCOS.process.moveResource, proc, res)
    if ok then
        return fd
    end

    KOCOS.fs.close(f)
    error(fd)
end

---@param protocol string
---@param subprotocol string
function syscalls.socket(proc, protocol, subprotocol, config)
    assert(type(protocol) == "string", "bad protocol")
    assert(type(subprotocol) == "string", "bad subprotocol")
    config = table.copy(config)

    local s = assert(KOCOS.network.newSocket(protocol, subprotocol, config, proc))

    ---@type KOCOS.SocketResource
    local res = {
        kind = "socket",
        socket = s,
        rc = 1,
    }

    local ok, fd = pcall(KOCOS.process.moveResource, proc, res)
    if ok then
        return fd
    end

    KOCOS.network.close(res.socket)
    error(fd)
end

---@param address string
---@param protocol? string
function syscalls.getaddrinfo(proc, address, protocol)
    assert(type(address) == "string", "bad address")
    assert(type(protocol) == "string" or type(protocol) == "nil", "bad protocol")
    return KOCOS.network.getAddressInfo(address, protocol)
end

---@param fd integer
---@param address any
---@param options any
function syscalls.connect(proc, fd, address, options)
    local res = assert(proc.resources[fd], "bad file descriptor")
    assert(res.kind == "socket", "bad file descriptor")
    ---@cast res KOCOS.SocketResource
    KOCOS.network.connect(res.socket, address, options)
end

---@param fd integer
---@param address any
---@param options any
function syscalls.aio_connect(proc, fd, address, options)
    local res = assert(proc.resources[fd], "bad file descriptor")
    assert(res.kind == "socket", "bad file descriptor")
    ---@cast res KOCOS.SocketResource
    return KOCOS.network.async_connect(res.socket, address, options)
end

---@param fd integer
---@param options any
function syscalls.serve(proc, fd, options)
    local res = assert(proc.resources[fd], "bad file descriptor")
    assert(res.kind == "socket", "bad file descriptor")
    ---@cast res KOCOS.SocketResource
    KOCOS.network.listen(res.socket, options)
end

---@param fd integer
function syscalls.accept(proc, fd)
    local res = assert(proc.resources[fd], "bad file descriptor")
    assert(res.kind == "socket", "bad file descriptor")
    ---@cast res KOCOS.SocketResource
    local client = KOCOS.network.accept(res.socket)

    local clientRes = {
        kind = "socket",
        socket = client,
        rc = 1,
    }

    local ok, cfd = pcall(KOCOS.process.moveResource, proc, clientRes)
    if ok then return cfd end
    KOCOS.network.close(client)
    error(cfd)
end

---@param fd integer
function syscalls.close(proc, fd)
    local res = proc.resources[fd]
    assert(res, "bad file descriptor")

    KOCOS.process.closeResource(res)
    proc.resources[fd] = nil
end

---@param fd integer
---@param limit integer
function syscalls.read(proc, fd, limit)
    assert(type(limit) == "number", "bad limit")
    local res = proc.resources[fd]
    assert(res, "bad file descriptor")

    if res.kind == "file" then
        ---@cast res KOCOS.FileResource
        local f = res.file
        local data, err = KOCOS.fs.read(f, limit)
        if err then error(err) end
        return data
    elseif res.kind == "socket" then
        ---@cast res KOCOS.SocketResource
        local s = res.socket
        return KOCOS.network.read(s, limit)
    end
    error("bad resource type")
end

---@param fd integer
---@param data string 
function syscalls.write(proc, fd, data)
    assert(type(data) == "string", "bad data")
    local res = proc.resources[fd]
    assert(res, "bad file descriptor")

    if res.kind == "file" then
        ---@cast res KOCOS.FileResource
        local f = res.file
        assert(KOCOS.fs.write(f, data))
        return
    elseif res.kind == "socket" then
        ---@cast res KOCOS.SocketResource
        return KOCOS.network.write(res.socket, data)
    end
    error("bad resource type")
end

---@param fd integer
---@param limit integer
function syscalls.aio_read(proc, fd, limit)
    assert(type(limit) == "number", "bad limit")
    local res = proc.resources[fd]
    assert(res, "bad file descriptor")

    if res.kind == "socket" then
        ---@cast res KOCOS.SocketResource
        local s = res.socket
        return KOCOS.network.async_read(s, limit)
    end
    error("bad resource type")
end

---@param fd integer
---@param data string 
function syscalls.aio_write(proc, fd, data)
    assert(type(data) == "string", "bad data")
    local res = proc.resources[fd]
    assert(res, "bad file descriptor")

    if res.kind == "socket" then
        ---@cast res KOCOS.SocketResource
        return KOCOS.network.async_write(res.socket, data)
    end
    error("bad resource type")
end

---@param fd integer
---@param whence "set"|"cur"|"end"
---@param off integer
function syscalls.seek(proc, fd, whence, off)
    assert(whence == "set" or whence == "cur" or whence == "end", "bad whence")
    assert(type(off) == "number", "bad offset")
    local res = proc.resources[fd]
    assert(res, "bad file descriptor")

    if res.kind == "file" then
        ---@cast res KOCOS.FileResource
        local f = res.file
        return assert(KOCOS.fs.seek(f, whence, off))
    end
    error("bad resource type")
end

---@param path string
function syscalls.stat(proc, path)
    assert(type(path) == "string", "bad path")

    local info = {}
    info.type = KOCOS.fs.type(path)
    info.used = KOCOS.fs.spaceUsed(path)
    info.total = KOCOS.fs.spaceTotal(path)
    info.size = KOCOS.fs.size(path)
    info.perms = KOCOS.fs.permissionsOf(path)
    info.mtime = 0
    info.uauth = 2^16-1
    info.isMount = KOCOS.fs.isMount(path)
    local partition = KOCOS.fs.partitionOf(path)
    info.partition = partition.uuid
    info.driveType = partition.drive.type
    info.deviceName = partition.name
    info.driveName = partition.drive.getLabel() or "no label"
    return info
end

function syscalls.cstat(proc)
    local info = {}
    info.boot = computer.getBootAddress()
    info.tmp = computer.tmpAddress()
    info.uptime = computer.uptime()
    info.kernel = KOCOS.version
    info.memTotal = computer.totalMemory()
    info.memFree = computer.freeMemory()
    info.arch = computer.getArchitecture()
    info.energy = computer.energy()
    info.maxEnergy = computer.maxEnergy()
    info.isRobot = component.robot ~= nil
    info.users = {computer.users()}
    info.threadCount = #KOCOS.process.currentThreads
    return info
end

---@param fd integer
---@param action string
function syscalls.ioctl(proc, fd, action, ...)
    local res = proc.resources[fd]
    assert(res, "bad file descriptor")

    if res.kind == "file" then
        ---@cast res KOCOS.FileResource
        return KOCOS.fs.ioctl(res.file, action, ...)
    end
    if res.kind == "socket" then
        ---@cast res KOCOS.SocketResource
        return KOCOS.network.ioctl(res.socket, action, ...)
    end

    error("bad file descriptor")
end

---@param path string
function syscalls.ftype(proc, path)
    return KOCOS.fs.type(path)
end

---@param path string
function syscalls.list(proc, path)
    local perms = KOCOS.fs.permissionsOf(path)
    assert(KOCOS.perms.canWrite(proc.uid, perms), "permission denied")
    return assert(KOCOS.fs.list(path))
end

-- End of file and socket syscalls

-- Event syscalls

---@param res KOCOS.Resource
---@return KOCOS.EventSystem?
local function eventsOf(res)
    if res.kind == "file" then
        ---@cast res KOCOS.FileResource
        return res.file.events
    end
    if res.kind == "event" then
        ---@cast res KOCOS.EventResource
        return res.event
    end
end

function syscalls.queued(proc, fd, ...)
    local res = proc.resources[fd]
    assert(res, "bad file descriptor")

    local events = eventsOf(res)
    assert(events, "bad file descriptor")

    return events.queued(...)
end

function syscalls.pop(proc, fd, ...)
    local res = proc.resources[fd]
    assert(res, "bad file descriptor")

    local events = eventsOf(res)
    assert(events, "bad file descriptor")

    return events.pop(...)
end

function syscalls.popWhere(proc, fd, f)
    assert(type(f) == "function", "bad validator")

    local res = proc.resources[fd]
    assert(res, "bad file descriptor")

    local events = eventsOf(res)
    assert(events, "bad file descriptor")

    return events.popWhere(f)
end

function syscalls.clear(proc, fd)
    local res = proc.resources[fd]
    assert(res, "bad file descriptor")

    local events = eventsOf(res)
    assert(events, "bad file descriptor")

    events.clear()
    return true
end

---@param name string
function syscalls.push(proc, fd, name, ...)
    assert(type(name) == "string", "bad event")

    local res = proc.resources[fd]
    assert(res, "bad file descriptor")

    local events = eventsOf(res)
    assert(events, "bad file descriptor")

    return events.push(name, ...)
end

---@param f function
---@param id? string
function syscalls.listen(proc, f, id)
    assert(type(f) == "function", "bad callback")
    assert(type(id) == "string" or id == nil, "bad id")
    return proc.events.listen(f, id)
end

---@param id string
function syscalls.forget(proc, id)
    assert(type(id) == "string", "bad id")
    proc.events.forget(id)
end

-- End of event syscalls

-- Thread syscalls

function syscalls.attach(proc, func, name)
    assert(type(func) == "function", "bad function")
    assert(type(name) == "string" or type(name) == "nil", "bad name")
    local thread = proc:attach(func, name)
    assert(thread, "failed")
    return thread.id
end

function syscalls.openlock(proc)
    local newLock = KOCOS.lock.create()

    ---@type KOCOS.LockResource
    local res = {
        kind = "lock",
        lock = newLock,
    }

    return proc:moveResource(res)
end

function syscalls.tryLock(proc, fd)
    local res = assert(proc.resources[fd], "bad file descriptor")
    if res.kind ~= "lock" then error("bad resource") end
    ---@cast res KOCOS.LockResource
    return res.lock:tryLock()
end

function syscalls.lock(proc, fd, timeout)
    assert(type(timeout) == "number", "bad timeout")
    local res = assert(proc.resources[fd], "bad file descriptor")
    if res.kind ~= "lock" then error("bad resource") end
    ---@cast res KOCOS.LockResource
    return res.lock:lock(timeout)
end

function syscalls.unlock(proc, fd)
    local res = assert(proc.resources[fd], "bad file descriptor")
    if res.kind ~= "lock" then error("bad resource") end
    ---@cast res KOCOS.LockResource
    return res.lock:unlock()
end

function syscalls.tkill(proc, tid, msg, trace)
    assert(type(tid) == "number", "bad thread id")
    assert(type(msg) == "string" or type(msg) == "nil", "bad error")
    assert(type(trace) == "string" or type(trace) == "nil", "bad trace")
    local thread = proc.threads[tid]
    assert(thread, "bad thread id")
    thread:kill(msg, trace)
end

function syscalls.tjoin(proc, tid)
    assert(type(tid) == "number", "bad thread id")
    while proc.threads[tid] do KOCOS.yield() end
end

function syscalls.tstatus(proc, tid)
    assert(type(tid) == "number", "bad thread id")
    local thread = proc.threads[tid]
    assert(thread, "bad thread id")
    return thread:status()
end

function syscalls.tsuspend(proc, tid)
    assert(type(tid) == "number", "bad thread id")
    local thread = proc.threads[tid]
    assert(thread, "bad thread id")
    thread:suspend()
end

function syscalls.tresume(proc, tid)
    assert(type(tid) == "number", "bad thread id")
    local thread = proc.threads[tid]
    assert(thread, "bad thread id")
    thread:resume()
end

-- End of thread syscalls

-- Process syscalls

function syscalls.pself(proc)
    return proc.pid
end

---@param pid? integer
function syscalls.pnext(proc, pid)
    assert(type(pid) == "number" or pid == nil, "bad pid")
    return next(KOCOS.process.procs, pid)
end

---@param pid integer
function syscalls.pinfo(proc, pid)
    assert(type(pid) == "number", "bad pid")

    local requested = KOCOS.process.byPid(pid)
    assert(requested, "bad pid")

    local data = {
        args = requested.args,
        env = requested.env,
        cmdline = requested.cmdline,
        ring = requested.ring,
        uid = requested.uid,
        parent = requested.parent,
        status = requested.status,
        children = {},
        threads = {},
    }

    for child, _ in pairs(requested.children) do
        table.insert(data.children, child)
    end

    for thread, _ in pairs(requested.threads) do
        table.insert(data.threads, thread)
    end

    local safe = table.copy(data) -- Get rid of references to kernel memory

    if proc.ring < 2 then
        -- Add references to kernel memory for trusted processes
        data.namespace = requested.namespace
    end

    return safe
end

---@param pid integer
function syscalls.pawait(_, pid)
    assert(type(pid) == "number", "bad pid")

    while true do
        local proc = KOCOS.process.byPid(pid)
        if not proc then error("process terminated") end
        if not next(proc.threads) then break end
        KOCOS.yield()
    end
end

---@param pid integer
function syscalls.pwait(_, pid)
    assert(type(pid) == "number", "bad pid")

    while KOCOS.process.byPid(pid) do KOCOS.yield() end
end

---@param status integer
function syscalls.pstatus(proc, status)
    assert(type(status) == "number", "bad status")
    proc.status = status
end

---@param pid integer
function syscalls.pexit(proc, pid)
    assert(type(pid) == "number", "bad pid")

    local other = KOCOS.process.byPid(pid)
    assert(other, "bad pid")
    if pid == proc.pid or proc:isDescendant(pid) or proc.ring < 2 then
        other:kill()
        if pid == proc.pid then KOCOS.yield() end
    else
        error("permission denied")
    end
end

function syscalls.pspawn(proc, init, config)
    assert(type(init) == "string", "bad init path")
    assert(type(config) == "table", "bad config")
    local data = {
        ring = config.ring or proc.ring,
        cmdline = config.cmdline or init,
        args = table.copy(config.args) or {[0]=init},
        env = table.copy(config.env or proc.env),
        traced = not not config.traced,
        -- User ID changes can only happen with a login()
        uid = proc.uid,
        parent = proc.pid,
    }
    local fdMap = table.copy(config.fdMap) or {
        -- Passing through stdout, stdin, stderr.
        -- Field is the fd for the child process.
        [0] = 0,
        [1] = 1,
        [2] = 2,
    }
    assert(type(data.ring) == "number", "bad ring")
    assert(data.ring >= proc.ring, "permission denied")
    assert(type(data.cmdline) == "string", "bad cmdline")
    assert(type(data.args) == "table", "bad args")
    data.args[0] = data.args[0] or init
    for i, arg in pairs(data.args) do
        assert(type(i) == "number", "args is not array")
        assert(i >= 0 and i <= #arg, "args is not array")
        assert(type(arg) == "string", "args are not strings")
    end
    assert(type(data.env) == "table", "bad env")
    for k, v in pairs(data.env) do
        assert(type(k) == "string", "env name is not string")
        assert(type(v) == "string", "env value is not string")
    end
    assert(type(fdMap) == "table", "bad fdmap")
    for childFd, parentFd in pairs(fdMap) do
        assert(type(childFd) == "number", "corrupt fdmap")
        assert(type(parentFd) == "number", "corrupt fdmap")
        assert(proc.resources[parentFd], "bad file descriptor in fdmap")
    end
    local child = assert(KOCOS.process.spawn(init, data))
    for childFd, parentFd in pairs(fdMap) do
        local res = proc.resources[parentFd]
        -- Pcalled in case of OOM
        local ok, err = pcall(rawset, child.resources, childFd, res)
        if not ok then
            child:kill() -- badly initialized process. Kill it.
            error(err)
        end
        KOCOS.process.retainResource(res)
    end
    proc.children[child.pid] = child
    return child.pid
end

---@param symbol string
function syscalls.psymbol(proc, symbol)
    assert(type(symbol) == "string", "bad symbol")
    local data = proc.modules[symbol]
    assert(data, "not found")
    return data
end

---@param pid integer
---@param event string
function syscalls.psignal(proc, pid, event, ...)
    -- we allow shared memory like this btw
    -- also shared functions
    -- can be used to optimize a lot
    assert(type(pid) == "integer", "bad pid")
    assert(type(event) == "string", "bad event name")
    local target = KOCOS.process.procs[pid]
    assert(target, "bad pid")

    if target.ring < proc.ring then
        error("permission denied")
    end

    target.events.push(event, ...)
end

function syscalls.exit(proc, status)
    syscalls.pstatus(proc, status)
    while true do
        local tid, thread = next(proc.threads)
        if not thread then break end
        thread:kill("exit")
    end
    KOCOS.yield() -- system yield moment
end

-- End of process syscalls

-- Start of user syscalls

function syscalls.login(proc, user, ring, password)
    assert(type(user) == "number", "bad uid")
    assert(math.floor(user) == user, "bad uid")
    assert(type(ring) == "number", "bad ring")
    assert(math.floor(ring) == ring, "bad ring")
    assert(ring >= 0 and ring <= 3, "bad ring")
    assert(type(password) == "string", "bad string")

    if not KOCOS.auth.isAllowed(user, ring, password) then
        error("permission denied")
    end

    proc.uid = user
    proc.ring = ring
end

function syscalls.uinfo(proc, user)
    assert(type(user) == "number", "bad uid")
    assert(math.floor(user) == user, "bad uid")
    return KOCOS.auth.userInfo(user)
end

function syscalls.uginfo(proc, group)
    assert(type(group) == "number", "bad gid")
    assert(math.floor(group) == group, "bad gid")
    return KOCOS.auth.groupInfo(group)
end

function syscalls.ulist(proc, group)
    assert(type(group) == "nil" or type(group) == "number", "bad gid")
    if group then
        assert(math.floor(group) == group, "bad gid")
    end
    return KOCOS.auth.listUsers(group)
end

function syscalls.ugroups(proc)
    return KOCOS.auth.listGroups()
end

function syscalls.ufindUser(proc, name)
    assert(type(name) == "string", "bad name")
    return KOCOS.auth.userByName(name)
end

function syscalls.ufindGroup(proc, name)
    assert(type(name) == "string", "bad name")
    return KOCOS.auth.groupByName(name)
end

-- End of user syscalls

-- Start of component syscalls

function syscalls.clist(proc, all)
    local filtered = {}
    for addr in component.list() do
        if (proc.ring <= component.ringFor(addr)) or all then
            table.insert(filtered, addr)
        end
    end
    return filtered
end

function syscalls.ctype(proc, addr)
    return component.type(addr)
end

function syscalls.cprimary(proc, kind, exact)
    local filtered = {}
    for addr in component.list(kind, exact) do
        if (proc.ring <= component.ringFor(addr)) or all then
            table.insert(filtered, addr)
        end
    end
    return filtered[1]
end

function syscalls.cproxy(proc, addr)
    assert(component.type(addr), "missing component")
    assert(proc.ring <= component.ringFor(addr), "permission denied")
    return component.proxy(addr)
end

function syscalls.cinvoke(proc, addr, ...)
    assert(proc.ring <= component.ringFor(addr), "permission denied")
    return component.invoke(addr, ...)
end

-- End of component syscalls

KOCOS.syscalls = syscalls

KOCOS.log("Syscalls subsystem loaded")

end
do
local tty = {}
tty.__index = tty

local lib = unicode or string

local function color(r, g, b)
    return r * 0x10000 + g * 0x100 + b
end

local stdClrs = {
    -- taken from https://en.wikipedia.org/wiki/ANSI_escape_code#Control_Sequence_Introducer_commands
    -- Mix of VS Code and VGA.
    -- BG is auto-computed.
    [30] = color(0, 0, 0), -- black
    [31] = color(205, 49, 49), -- red
    [32] = color(13, 188, 121), -- green
    [33] = color(229, 229, 16), -- yellow
    [34] = color(36, 114, 200), -- blue
    [35] = color(188, 63, 188), -- magenta
    [36] = color(17, 168, 205), -- cyan
    [37] = color(229, 229, 229), -- white
    [90] = color(85, 85, 85), -- bright black (gray)
    [91] = color(255, 85, 85), -- bright red
    [92] = color(85, 255, 85), -- bright green
    [93] = color(255, 255, 85), -- bright yellow
    [94] = color(59, 142, 234), -- bright blue
    [95] = color(255, 85, 255), -- bright magenta
    [96] = color(85, 255, 255), -- bright cyan
    [97] = color(255, 255, 255), -- bright white
}

function tty.create(gpu, screen)
    gpu.bind(screen.address)
    local w, h = gpu.maxResolution()
    gpu.setResolution(w, h)
    local t = setmetatable({
        gpu = gpu,
        screen = screen,
        x = 1,
        y = 1,
        w = w,
        h = h,
        buffer = "",
        escape = nil,
        commands = {},
        responses = {},
        -- Aux Port is about keyboard input, not yet implemented
        auxPort = false,
        conceal = false,
        defaultFg = stdClrs[37],
        defaultBg = stdClrs[30],
        standardColors = table.copy(stdClrs),
    }, tty)
    t:setActiveColors(t.defaultFg, t.defaultBg)
    return t
end

function tty:clear()
    self.x = 1
    self.y = 1
    self.gpu.fill(1, 1, self.w, self.h, " ")
end

function tty:flush()
        self.gpu.set(self.x - lib.len(self.buffer), self.y, self.buffer)
        self.buffer = ""
end

function tty:getActiveColors()
    return self.gpu.getForeground(), self.gpu.getBackground()
end

function tty:setDefaultActiveColors(fg, bg)
    self.defaultFg = fg
    self.defaultBg = bg
end

function tty:setActiveColors(fg, bg)
    return self.gpu.setForeground(fg), self.gpu.setBackground(bg)
end

function tty:getColorDepth()
    return self.gpu.getDepth()
end

function tty:popCommand()
    return table.remove(self.commands, 1)
end

function tty:popResponse()
    return table.remove(self.responses, 1)
end

function tty:doGraphicalAction(args)
    local function pop()
        return table.remove(args, 1) or 0
    end

    local action = pop()
    if action == 0 then
        self.conceal = false
        self.auxPort = false
        self:setActiveColors(self.defaultFg, self.defaultBg)
    elseif action == 8 then
        self.conceal = true
    elseif action == 28 then
        self.conceal = false
    elseif (action >= 30 and action <= 37) or (action >= 90 and action <= 97) then
        local fg, bg = self:getActiveColors()
        fg = self.standardColors[action]
        self:setActiveColors(fg, bg)
    elseif action == 38 then
        local fg, bg = self:getActiveColors()
        local mode = pop()
        if mode == 2 then
            -- 24-bit
            local r = pop()
            local g = pop()
            local b = pop()
            fg = color(r, g, b)
        end
        self:setActiveColors(fg, bg)
    elseif (action >= 40 and action <= 47) or (action >= 100 and action <= 107) then
        local fg, bg = self:getActiveColors()
        bg = self.standardColors[action-10]
        self:setActiveColors(fg, bg)
    elseif action == 48 then
        local fg, bg = self:getActiveColors()
        local mode = pop()
        if mode == 2 then
            -- 24-bit
            local r = pop()
            local g = pop()
            local b = pop()
            bg = color(r, g, b)
        end
        self:setActiveColors(fg, bg)
    end
end

---@param num integer
---@return string
local function paramBase16(num)
    local base = "0123456789:;<=>?"
    local s = ""
    while num > 0 do
        local n = num % #base
        num = math.floor(num / #base)
        s = s .. base:sub(n+1,n+1)
    end
    if s == "" then return "0" end
    return s:reverse()
end

local function isControlCharacter(char)
	return type(char) == "number" and (char < 0x20 or (char >= 0x7F and char <= 0x9F))
end

function tty:handleChar(char, code)
    -- Keyboard disabled
    if not self.auxPort then return end
    -- KOCOS custom escape sequences cuz yeah
    local mods = 0
    local num = char
    local term = "|"
    if isControlCharacter(char) then
        -- Send as code
        num = code
        term = "\\"
    end
    local s = "\x1b[" .. paramBase16(num * 16 + mods) .. term
    table.insert(self.responses, s)
end

-- https://en.wikipedia.org/wiki/ANSI_escape_code
---@param c string
function tty:processEscape(c)
    local start = self.escape:sub(1, 1)

    if start == '[' then
        -- Process as CSI
        local b = string.byte(c)
        if b < 0x40 or b > 0x7E then
            -- We dont check if its actually parameter or intermediate bytes, cuz we don't care
            self.escape = self.escape .. c
            return
        end
        -- Terminator!!!!
        local data = self.escape:sub(2)
        local paramLen = 0
        while paramLen < #data do
            local paramByte = data:byte(paramLen+1, paramLen+1)
            if paramByte < 0x30 or paramByte > 0x3F then break end
            paramLen = paramLen + 1
        end
        local params = data:sub(1, paramLen)
        local action = c

        self.escape = nil

        -- In terms of colors, we support the normal colors and 24-bit colors, but not 256 color mode yet.
        if action == "m" then
            local strArgs = string.split(params, ";")
            local args = {}
            for i=1,#strArgs do
                args[i] = tonumber(strArgs[i]) or 0
            end
            if #args == 0 then args = {0} end
            while #args > 0 do
                self:doGraphicalAction(args)
            end
        end

        if action == "H" then
            local strArgs = string.split(params, ";")
            local args = {}
            for i=1,#strArgs do
                args[i] = tonumber(strArgs[i]) or 0
            end
            local x = tonumber(args[1] or "") or 1
            local y = tonumber(args[2] or "") or 1

            x = math.clamp(x, 1, self.w)
            y = math.clamp(y, 1, self.h)

            self.x = x
            self.y = y
        end

        if action == "J" then
            -- Only support full screen clearing for now
            if params == "2" then
                self:clear()
            end
        end

        if action == "K" then
            local y = self.y
            if params ~= "" then
                y = tonumber(params) or 1
            end
            self.gpu.fill(1, y, self.w, 1, " ")
        end

        if action == "i" then
            -- CSI 5i for ON and CSI 4i for OFF, though we let any invalid param also be off.
            self.auxPort = params == "5"
        end

        if action == "n" then
            -- standard
            if params == "6" then
                -- CSI 6n asks for a status report
                table.insert(self.responses, "\x1b[" .. tostring(self.x) .. ";" .. tostring(self.y) .. "R")
            end
            -- non-standard
            if params == "5" then
                table.insert(self.responses, "\x1b[" .. tostring(self.w) .. ";" .. tostring(self.h) .. "R")
            end
            if params == "7" then
                table.insert(self.responses, "\x1b[" .. tostring(self.gpu.address) .. ";" .. tostring(self.gpu.getScreen()) .. "R")
            end
        end
    elseif start == ']' then
        -- Process as OSC
        self.escape = self.escape .. c
        -- Bell, single character ST and full ST escape are the terminators.
        local terminators = {"\b", "\x9C", "\x1b\x5C"}
        ---@type string?
        local data = nil
        for _, terminator in ipairs(terminators) do
            if string.endswith(self.escape, terminator) then
                data = self.escape:sub(2, -#terminator - 1) -- from 2nd char (past ]) up until terminator
                break
            end
        end
        if data then
            table.insert(self.commands, data)
            self.escape = nil
        end
    end
end

function tty:put(c)
    if self.y > self.h then
        self:flush()
        self.y = self.h
        self.gpu.copy(1, 1, self.w, self.h, 0, -1)
        self.gpu.fill(1, self.h, self.w, 1, " ")
    end

    if self.escape then
        -- Likely a TTY OOM attack.
        if #self.escape >= 8192 then
            self.escape = nil
            return
        end
        if #self.escape == 0 then
            -- We support CSI and OSC. Other ones are invalid
            if c == '[' then
                self.escape = '['
            elseif c == ']' then
                self.escape = ']'
            else
                -- Invalid
                self.escape = nil
            end
        elseif #c > 0 then
            self:processEscape(c)
        end
        return
    end

    if self.conceal then
        if #self.buffer > 0 then self:flush() end
        return
    end

    if c == "\n" then
        self:flush()
        self.y = self.y + 1
        self.x = 1
    elseif c == "\t" then
        self:flush()
        self.x = self.x + 4
        self.x = math.floor(self.x / 4) * 4
    elseif c:byte() == 0x07 then
        self:flush()
        computer.beep() -- Bell beeps.
    elseif c:byte() == 0x08 then
        self:flush()
        self.x = self.x - 1
        if self.x == 0 then self.x = 1 end
    elseif c == "\r" then
        self:flush()
        self.x = 1
    elseif c == "\f" then
        self:flush()
        -- There is no next printer page, so we just move to top of screen
        self.x = 1
        self.y = 1
    elseif c == "\x1b" then
        self:flush()
        self.escape = ""
        return
    else
        self.buffer = self.buffer .. c
        self.x = self.x + 1
    end

    if self.x > self.w then
        self:flush()
        self.x = 1
        self.y = self.y + 1
    end
end

function tty:unput(c)
    if c == "\n" then
        -- Assume this never happens
        error("cant remove newline")
    end
    local w = c == "\t" and 4 or 1

    self.x = self.x - w
    self.gpu.set(self.x,self.y,string.rep(" ", w))
    if self.x == 0 then
        self.x = self.w
        self.y = self.y - 1
    end

    if self.y == 0 then
        self.y = 1
    end
end

function tty:write(data)
    for i=1,lib.len(data) do
        self:put(lib.sub(data, i, i))
    end
    self:flush()
end

function tty:unwrite(data)
    for i=lib.len(data), 1, -1 do
        self:unput(lib.sub(data, i, i))
    end
end

function tty:print(fmt, ...)
    self:write(string.format(fmt, ...))
end

KOCOS.tty = tty

KOCOS.log("TTY subsystem loaded")

end
do
local auth = {}

---@param user integer
---@param group integer
---@return boolean
function auth.isUserInGroup(user, group)
    return false
end

---@return integer[]
function auth.listGroups()
    return {}
end

---@param group integer?
---@return integer[]
function auth.listUsers(group)
    if group then return {} end
    -- Only root exists
    return {0}
end

---@param user integer
---@return {name: string, groups: integer[], hasPassword: boolean}?
function auth.userInfo(user)
    if user ~= 0 then return end
    return {
        name = "root",
        groups = {},
        hasPassword = false,
    }
end

---@param group integer
---@return {name: string, desc: string, users: integer[]}?
function auth.groupInfo(group)
    return nil -- no groups
end

---@param user integer
---@param ring integer
---@param password string 
---@return boolean
function auth.isAllowed(user, ring, password)
    return user == 0
end

---@param name string
---@return integer?
function auth.userByName(name)
    if name == "root" then return 0 end
end

---@param name string
---@return integer?
function auth.groupByName(name)
    return nil -- no groups
end

-- Overwrite with better auth plz
KOCOS.auth = auth

local perms = {}

perms.BIT_WRITABLE = 1
perms.BIT_READABLE = 2
perms.ID_BITS = 6
perms.ID_ALL = 2^perms.ID_BITS - 1

function perms.encode(user, userRW, group, groupRW)
    local userPerms = user * 4 + userRW
    local groupPerms = group * 4 + groupRW

    return groupPerms * 256 + userPerms
end

function perms.decode(num)
    local userPerms = num % 256
    local groupPerms = math.floor(num / 256)

    local user = math.floor(userPerms / 4)
    local userRW = userPerms % 4

    local group = math.floor(groupPerms / 4)
    local groupRW = groupPerms % 4

    return user, userRW, group, groupRW
end

function perms.canWrite(puser, permissions)
    -- Root can do anything
    if puser == 0 then return true end
    local user, userRW, group, groupRW = perms.decode(permissions)
    if puser == user or user == perms.ID_ALL then
        return bit32.btest(userRW, perms.BIT_WRITABLE)
    end
    if KOCOS.auth.isUserInGroup(user, group) or group == perms.ID_ALL then
        return bit32.btest(groupRW, perms.BIT_WRITABLE)
    end
    return false
end

function perms.canRead(puser, permissions)
    -- Root can do anything
    if puser == 0 then return true end
    local user, userRW, group, groupRW = perms.decode(permissions)
    if puser == user or user == perms.ID_ALL then
        return bit32.btest(userRW, perms.BIT_READABLE)
    end
    if KOCOS.auth.isUserInGroup(user, group) or group == perms.ID_ALL then
        return bit32.btest(groupRW, perms.BIT_READABLE)
    end
    return false
end

KOCOS.perms = perms

KOCOS.log("Auth and perms subsystems loaded")

end
do
local router = {}

router.drivers = {}

router.events = KOCOS.event.create(KOCOS.maxEventBacklog)

router.EVENT_CONNECT = "connect"
router.EVENT_DISCONNECT = "disconnect"
router.EVENT_PACKET = "packet"

---@class KOCOS.Network
---@field uuid string
---@field protocols string[]
---@field auth "none"|"password"|string
---@field manager table

---@type {[string]: KOCOS.Network}
router.networks = {}

---@type KOCOS.Network?
router.current = nil

---@type KOCOS.Network?
router.connectingTo = nil

function router.addDriver(driver)
    table.insert(router.drivers, driver)
end

---@return KOCOS.Network?
function router.networkFromUuid(uuid)
    return router.networks[uuid]
end

---@param network KOCOS.Network
function router.connectTo(network)
    router.connectingTo = network
    router.events.push(router.EVENT_CONNECT, network.uuid)
end

---@param network KOCOS.Network
---@param protocol string
---@return boolean
function router.networkSupports(network, protocol)
    for i=1,#network.protocols do
        if network.protocols[i] == protocol then
            return true
        end
    end
    return false
end

---@param network? KOCOS.Network
function router.isConnectingTo(network)
    if not router.connectingTo then return end
    if not network then return router.connectingTo ~= nil end
    return router.connectingTo.uuid == network.uuid
end

---@return string?
function router.currentNetworkUuid()
    if router.current then return router.current.uuid end
end

function router.isOnline()
    return router.current ~= nil
end

function router.isOffline()
    return router.current == nil
end

---@param network KOCOS.Network
function router.addNetwork(network)
    router.networks[network.uuid] = network
end

---@param uuid string
function router.forget(uuid)
    if router.currentNetworkUuid() == uuid then
        router.disconnect()
    end
    router.networks[uuid] = nil
end

function router.disconnect()
    if router.isOnline() then
        router.events.push(router.EVENT_DISCONNECT, router.current.uuid)
        router.current.manager:disconnect(router.current)
    end
    router.current = nil
end

function router.send(packet)
    local current = assert(router.current, "offline")
    current.manager:send(current, packet)
end

function router.receivedPacket(packet)
    router.events.push(router.EVENT_PACKET, packet)
end

function router.update()
    for id, network in pairs(router.networks) do
        network.manager:update(network)
    end
end

KOCOS.router = router

KOCOS.runOnLoop(router.update)

KOCOS.log("Router subsystem loaded")

end
do
--[[

DevFS structure:

/dev/components/<type>/<uuid> - Files to open proxies
/dev/parts/<drive uuid>/<part uuid> - Files to access partitions. Partitions on managed partitions appear as files
/dev/drives/<drive uuid> - Files to access whole drives.
/dev/zero
/dev/random
/dev/hex
]]

---@class KOCOS.DevFS
---@field partition KOCOS.Partition
---@field handles {[integer]: table}
local devfs = {}
devfs.__index = devfs

---@param partition KOCOS.Partition
function devfs.create(partition)
    if partition.drive.type ~= "devfs" then return nil end

    return setmetatable({
        partition = partition,
        handles = {},
    }, devfs)
end

function devfs.format()
    return false
end

function devfs:addProxy(proxy)
    local fd = 0
    while self.handles[fd] do fd = fd + 1 end
    self.handles[fd] = proxy
    return fd
end

function devfs:open(path)
    if path == "zero" then
        return self:addProxy {
            address = "zero",
            type = "devfs:zero",
            slot = -1,
        }
    end
    if path == "random" then
        return self:addProxy {
            address = "random",
            type = "devfs:random",
            slot = -1,
        }
    end
    if path == "hex" then
        return self:addProxy {
            address = "hex",
            type = "devfs:hex",
            slot = -1,
        }
    end
    return nil, "bad path"
end

function devfs:close(fd)
    self.handles[fd] = nil
    return true
end

function devfs:write(fd, data)
    local proxy = assert(self.handles[fd])
    return nil, "bad file descriptor"
end

function devfs:read(fd, limit)
    local proxy = assert(self.handles[fd])
    if proxy.address == "zero" then
        if limit == math.huge then limit = 1024 end
        return string.rep("\0", limit)
    end
    if proxy.address == "random" then
        if limit == math.huge then limit = 1024 end
        local s = ""
        for _=1,limit do
            s = s .. string.char(math.random(0, 255))
        end
        return s
    end
    if proxy.address == "hex" then
        if limit == math.huge then limit = 1024 end
        local s = ""
        local hex = "0123456789ABCDEF"
        for _=1,limit do
            local n = math.random(1, 16)
            s = s .. hex:sub(n, n)
        end
        return s
    end
    return nil, "bad file descriptor"
end

function devfs:seek(fd, whence, off)
    local proxy = assert(self.handles[fd])
    return nil, "bad file descriptor"
end

function devfs:ioctl(fd, action, ...)
    local proxy = assert(self.handles[fd])
    if not component.type(proxy.address) then
        error("bad file descriptor")
    end
    return component.invoke(proxy.address, action, ...)
end

function devfs:type(path)
    if path == "" then return "directory" end
    if path == "zero" then return "file" end
    if path == "random" then return "file" end
    if path == "hex" then return "file" end
    if path == "components" then return "directory" end
    if path == "parts" then return "directory" end
    if path == "drives" then return "directory" end
    for addr, type in component.list() do
        if path == "components/" .. type then return "directory" end
        if string.startswith(path, "components/" .. type .. "/") then
            local uuid = string.sub(path, #("components/" .. type .. "/"))
            if component.type(uuid) == type then return "file" end
            return "missing"
        end
        local parts = KOCOS.fs.getPartitions(component.proxy(addr))
        if path == "parts/" .. addr and #parts > 0 then
            return "directory"
        end
        for i=1,#parts do
            if path == "parts/" .. addr .. "/" .. parts[i].uuid then
                return "file"
            end
        end
    end
    return "missing"
end

function devfs:list(path)
    if path == "" then
        return {
            "zero",
            "random",
            "hex",
            "components",
            "parts",
            "drives",
        }
    end
    if path == "components" then
        local types = {}
        for _, type in component.list() do
            types[type] = true
        end
        local arr = {}
        for k in pairs(types) do table.insert(arr, k) end
        return arr
    end
    if path == "parts" then
        local drives = {}
        for addr in component.list() do
            if #KOCOS.fs.getPartitions(component.proxy(addr)) > 0 then
                table.insert(drives, addr)
            end
        end
        return drives
    end
    if path == "drives" then
        local drives = {}
        for addr, type in component.list() do
            if type == "drive" then
                table.insert(drives, addr)
            end
        end
        return drives
    end
    if string.startswith(path, "parts/") then
        for addr in component.list() do
            local parts = KOCOS.fs.getPartitions(component.proxy(addr))
            if path == "parts/" .. addr then
                local arr = {}
                for i=1,#parts do
                    table.insert(arr, parts[i].uuid)
                end
                return arr
            end
        end
    end
    if string.startswith(path, "components/") then
        for _, type in component.list() do
            if path == "components/" .. type then
                local t = {}
                for addr in component.list(type, true) do
                    table.insert(t, addr)
                end
                return t
            end
        end
    end
    return nil, "missing"
end

function devfs:isReadOnly()
    return true
end

function devfs:size(path)
    -- For drives and filesystems, the capacity is the size
    for addr, type in component.list() do
        if path == "components/" .. type .. "/" .. addr then
            if type == "filesystem" then
                return component.invoke(addr, "spaceTotal")
            end
            if type == "drive" then
                return component.invoke(addr, "getCapacity")
            end
        end
        if type == "drive" then
            if path == "drives/" .. addr then
                return component.invoke(addr, "getCapacity")
            end
        end
    end
    return 0
end

function devfs:remove(path)
    return false, "unsupported"
end

function devfs:spaceUsed()
    return 0
end

function devfs:spaceTotal()
    return 0
end

function devfs:mkdir()
    return false, "unsupported"
end

function devfs:touch()
    return false, "unsupported"
end

function devfs:getPartition()
    return self.partition
end

function devfs:permissionsOf()
    -- Root-only
    return 0
end

KOCOS.fs.addDriver(devfs)

KOCOS.defer(function()
    KOCOS.log("Mounting DevFS")
    ---@type KOCOS.Partition
    local partition = {
        name = "DevFS",
        drive = {
            type = "devfs",
            address = KOCOS.testing.uuid(),
            slot = -1,
            getLabel = function()
                return "DevFS"
            end,
            setLabel = function()
                return "DevFS"
            end,
        },
        uuid = KOCOS.testing.uuid(),
        startByte = 0,
        byteSize = 0,
        kind = "user",
        readonly = true,
        storedKind = KOCOS.testing.uuid(),
    }
    KOCOS.fs.mount("/dev", partition)
end, 2)

KOCOS.log("DevFS driver loaded")

end
do
local managedfs = {}
managedfs.__index = managedfs

---@param partition KOCOS.Partition
function managedfs.create(partition)
    if partition.drive.type ~= "filesystem" then return nil end

    return setmetatable({
        partition = partition,
        disk = partition.drive,
    }, managedfs)
end

function managedfs.format()
    return false
end

function managedfs:open(path, mode)
    local fd, err = self.disk.open(path, mode)
    if err then return nil, err end
    return fd
end

function managedfs:close(fd)
    return self.disk.close(fd)
end

function managedfs:read(fd, len)
    return self.disk.read(fd, len)
end

function managedfs:write(fd, data)
    return self.disk.write(fd, data)
end

function managedfs:seek(fd, whence, offset)
    return self.disk.seek(fd, whence, offset)
end

function managedfs:type(path)
    if self.disk.isDirectory(path) then
        return "directory"
    elseif self.disk.exists(path) then
        return "file"
    else
        return "missing"
    end
end

function managedfs:list(path)
    return self.disk.list(path)
end

function managedfs:isReadOnly(_)
    return self.disk.isReadOnly()
end

function managedfs:size(path)
    return self.disk.size(path)
end

function managedfs:remove(path)
    return self.disk.remove(path)
end

function managedfs:spaceUsed()
    return self.disk.spaceUsed()
end

function managedfs:spaceTotal()
    return self.disk.spaceTotal()
end

function managedfs:mkdir(path)
    return self.disk.makeDirectory(path)
end

function managedfs:touch(path)
    if self.disk.exists(path) then return end
    local f, err = self.disk.open(path, "w")
    if err then return nil, err end
    self.disk.close(f)
    return true
end

function managedfs:permissionsOf(path)
    return 2^16-1 -- Don't ask
end

function managedfs:ioctl(fd, action, ...)
    if action == "disk" then
        return self.disk.address
    end

    error("unsupported")
end

function managedfs:getPartition()
    return self.partition
end

KOCOS.fs.addDriver(managedfs)

KOCOS.fs.addPartitionParser(function(parser)
    if parser.type == "filesystem" then
        ---@type KOCOS.Partition[]
        local parts = {
            {
                startByte = 0,
                byteSize = parser.spaceTotal(),
                kind = "root",
                name = parser.getLabel() or parser.address:sub(1, 6),
                uuid = parser.address,
                storedKind = "root",
                readonly = parser.isReadOnly(),
                drive = parser,
            },
        }

        return parts, "managed"
    end
end)

KOCOS.log("ManagedFS driver loaded")

end
do
-- Little endian my beloved
local function readNum(drive, pos, size)
    local n = 0
    local m = 1

    for i=1,size do
        local byte = drive.readByte(pos+i-1)
        n = n + byte * m
        m = m * 256
    end
    return n
end

local function readBinary(drive, pos, size)
    local s = ""

    for i=1,size do
        s = s .. string.char(drive.readByte(pos+i-1))
    end

    return s
end

local function readGUID(drive, pos)
    local bin = readBinary(drive, pos, 16)
    if bin == string.rep("\0", 16) then return nil end
    local partA = bin:sub(1, 4)
    local partB = bin:sub(5, 6)
    local partC = bin:sub(7, 8)
    bin = partA:reverse() .. partB:reverse() .. partC:reverse() .. bin:sub(9)
    local digits4 = "0123456789ABCDEF"

    local base16d = ""
    for i=1,16 do
        local byte = string.byte(bin, i, i)
        local upper = math.floor(byte / 16) + 1
        local lower = byte % 16 + 1
        base16d = base16d .. digits4:sub(upper, upper) .. digits4:sub(lower, lower)
    end

    local guid = base16d:sub(1, 8) .. "-"
        .. base16d:sub(9, 12) .. "-"
        .. base16d:sub(13, 16) .. "-"
        .. base16d:sub(17, 20) .. "-"
        .. base16d:sub(21)

    return guid
end

local function align(pos, blockSize)
    local remaining = blockSize - pos % blockSize
    if remaining == blockSize then return pos end
    return pos + remaining
end

---@type KOCOS.PartitionParser
function KOCOS.gpt(drive)
    if drive.type ~= "drive" then return end
    -- This is currently NOT OPTIMIZED AT ALL
    local blockSize = drive.getSectorSize()

    local off = blockSize+1 -- skip LBA 0 since we don't care about MBR

    -- Check if GPT drive

    local sig = readBinary(drive, off, 8)
    if sig ~= "EFI PART" then return end -- not GPT drive.

    -- Revision number and CRC32 checksums are skipped, not needed
    local startOfPartitionsLBA = readNum(drive, off + 72, 8)
    local partitionCount = readNum(drive, off + 80, 4)
    local partitionEntrySize = readNum(drive, off + 84, 4)

    off = startOfPartitionsLBA * blockSize

    local visited = {}
    local partitions = {}

    for i=1, partitionCount do
        local typeGUID = readGUID(drive, off)
        if typeGUID then
            local partGUID = readGUID(drive, off+16)
            if partGUID == nil then return end
            if not visited[partGUID] then
                local startLBA = readNum(drive, off+32, 8)
                local endLBA = readNum(drive, off+40, 8)
                local attrs = readNum(drive, off+48, 8)
                local name = ""

                for i=1,36 do
                    local byte = readNum(drive, off+56+(i-1)*2, 1)
                    if byte == 0 then break end -- null termination ftw
                    name = name .. string.char(byte) -- fuck your safety
                end

                local kind = "user"
                if typeGUID == "C12A7328-F81F-11D2-BA4B-00A0C93EC93B" then -- actually EFI BOOT
                    kind = "boot"
                elseif typeGUID == "4F68BCE3-E8CD-4DB1-96E7-FBCAF984B709" then -- actually Linux amd64 root
                    kind = "root"
                elseif typeGUID == "9E1A2D38-C612-4316-AA26-8B49521E5A8B" then -- actually PReP boot
                    kind = "reserved"
                end

                local readonly = bit32.btest(attrs, (2^59))

                ---@type KOCOS.Partition
                local part = {
                    ---@diagnostic disable-next-line
                    uuid = partGUID,
                    name = name,
                    startByte = startLBA * blockSize,
                    byteSize = (endLBA - startLBA + 1) * blockSize,
                    ---@diagnostic disable-next-line
                    storedKind = typeGUID,
                    kind = kind,
                    readonly = readonly,
                    drive = drive,
                }

                table.insert(partitions, part)
            end
            visited[partGUID] = true
            off = off + partitionEntrySize
        end
    end

    return partitions, "gpt"
end

KOCOS.fs.addPartitionParser(KOCOS.gpt)

KOCOS.log("GPT driver loaded")

end
do
-- OKFFS
-- "Original KOCOS Fast File System" for when it works consistently
-- "Obfuscating Killer For File Systems" for when it breaks
-- "ok, ffs" for when it works again

--[[
Structs:

struct okffs_dir_entry {
  // NULL terminator is optional
  uint8_t name[32];
  // modified time in milliseconds
  uint64_t mtimeMS;
  uint16_t permissions;
  uint8_t ftype;
  uint24_t blockList;
  uint32_t fileSize;
  uint8_t reserved[14];
}

struct okffs_block_prefix {
    uint24_t next;
    uint16_t len;
}

struct okffs_header {
    char header[6] = "OKFFS\0";
    uint24_t nextFree;
    uint24_t freeList;
    uint24_t root;
    uint24_t activeBlockCount;
}

]]

---@class KOCOS.OKFFS.FileState
---@field path string
---@field entry KOCOS.OKFFS.Entry
---@field lastModification integer
---@field rc integer

---@class KOCOS.OKFFS.Handle
---@field state KOCOS.OKFFS.FileState
---@field mode "w"|"r"|"a"
---@field pos integer
-- Used to determine if cached stuff is valid
---@field syncedWith integer
---@field curBlock integer
---@field curOff integer
---@field lastPosition integer

---@class KOCOS.OKFFS.Driver
---@field partition KOCOS.Partition
---@field drive table
---@field start integer
---@field sectorSize integer
---@field capacity integer
---@field platterSize integer
---@field platterCount integer
---@field readonly boolean
---@field fileStates {[string]: KOCOS.OKFFS.FileState}
---@field handles {[integer]: KOCOS.OKFFS.Handle}
---@field sectorCache {sector: integer, data: string}[]
---@field maxSectorCacheLen integer
local okffs = {}
okffs.__index = okffs

local SECTOR_CACHE_LIMIT = 16*1024

---@param partition KOCOS.Partition
function okffs.create(partition)
    if partition.kind == "reserved" then return end -- fast ass skip
    if partition.drive.type ~= "drive" then return end
    local sectorSize = partition.drive.getSectorSize()
    local platterCount = partition.drive.getPlatterCount()
    local manager = setmetatable({
        partition = partition,
        drive = partition.drive,
        start = math.floor(partition.startByte / sectorSize) + 1,
        sectorSize = sectorSize,
        capacity = math.floor(partition.byteSize / sectorSize),
        platterSize = math.floor(partition.byteSize / sectorSize / platterCount),
        platterCount = platterCount,
        readonly = partition.readonly,
        fileStates = {},
        handles = {},
        sectorCache = {},
        maxSectorCacheLen = math.floor(SECTOR_CACHE_LIMIT / sectorSize),
    }, okffs)
    -- Failed signature check
    if not manager:fetchState() then
        return
    end
    return manager
end

---@param sector integer
---@return string
function okffs:lowLevelReadSector(sector)
    for i=#self.sectorCache,1,-1 do
        local entry = self.sectorCache[i]
        if entry.sector == sector then
            return entry.data
        end
    end
    local sec = assert(self.drive.readSector(sector+self.start))
    table.insert(self.sectorCache, {
        sector = sector,
        data = sec,
    })
    while #self.sectorCache > self.maxSectorCacheLen do
        table.remove(self.sectorCache, 1)
    end
    return sec
end

---@param sector integer
---@param data string
function okffs:lowLevelWriteSector(sector, data)
    for i=#self.sectorCache,1,-1 do
        local entry = self.sectorCache[i]
        if entry.sector == sector then
            entry.data = data
            break
        end
    end
    self.drive.writeSector(sector+self.start, data)
end

function okffs:padToSector(data)
    return data .. string.rep("\0", self.sectorSize - #data)
end

---@return string
function okffs:readSectorBytes(sector, off, len)
    local sec = self:lowLevelReadSector(sector)
    local data = sec:sub(off+1, off+len)
    assert(#data == len, "bad offset + len")
    return data
end

function okffs:readUint24(sector, off)
    local bytes = self:readSectorBytes(sector, off, 3)
    local low, middle, high = string.byte(bytes, 1, 3)
    return low
    + middle * 0x100
    + high * 0x10000
end

local function uint24ToBytes(num)
    local low = num % 256
    local middle = math.floor(num / 0x100) % 256
    local high = math.floor(num / 0x10000) % 256

    return low, middle, high
end

---@param sector integer
---@param off integer
---@param len integer
---@return integer
function okffs:readUintN(sector, off, len)
    local n = 0
    local m = 1
    local b = self:readSectorBytes(sector, off, len)
    for i=1,len do
        n = n + b:byte(i, i) * m
        m = m * 256
    end
    return n
end


function okffs:writeUint24(sector, off, num)
    assert(num >= 0 and num < 2^24, "bad uint24")


    local bytes = string.char(uint24ToBytes(num))
    self:writeSectorBytes(sector, off, bytes)
end

local function uintNToBytes(num, len)
    -- Little endian btw
    local b = ""
    for _=1,len do
        b = b .. string.char(num % 256)
        num = math.floor(num / 256)
    end
    return b
end

function okffs:writeUintN(sector, off, num, len)
    assert(num >= 0 and num < 2^(len*8), "bad uint" .. (len*8))

    self:writeSectorBytes(sector, off, uintNToBytes(num, len))
end

function okffs:writeSectorBytes(sector, off, data)
    local sec = self:lowLevelReadSector(sector)
    local pre = sec:sub(1, off)
    local post = sec:sub(off+#data+1)
    local written = pre .. data .. post
    self:lowLevelWriteSector(sector, written)
end

okffs.signature = "OKFFS\0"

function okffs:fetchState()
    local header = self:readSectorBytes(0, 0, 6)
    if header ~= okffs.signature then return false end
    self.nextFree = self:readUint24(0, 6)
    self.freeList = self:readUint24(0, 9)
    self.root = self:readUint24(0, 12)
    self.activeBlockCount = self:readUint24(0, 15)
    return true
end

function okffs:saveState()
    local data = okffs.signature
    data = data .. string.char(uint24ToBytes(self.nextFree))
    data = data .. string.char(uint24ToBytes(self.freeList))
    data = data .. string.char(uint24ToBytes(self.root))
    data = data .. string.char(uint24ToBytes(self.activeBlockCount))
    data = self:padToSector(data)
    self:lowLevelWriteSector(0, data)
end

---@param partition KOCOS.Partition
---@param format string
---@param opts table?
---@return boolean, string?
function okffs.format(partition, format, opts)
    local drive = partition.drive
    if drive.type ~= "drive" then return false end
    if format ~= "okffs" then return false end
    opts = opts or {}
    local sectorSize = drive.getSectorSize()
    local blockSize = opts.blockSize or sectorSize
    if blockSize ~= sectorSize then return false end
    if blockSize >= (2^16 + 5) then return false, "illegal block size" end
    local sectorsPerBlock = blockSize / sectorSize
    if sectorsPerBlock ~= math.floor(sectorsPerBlock) then return false, "block size not sector aligned" end
    local off = math.floor(partition.startByte / sectorSize)
    local sector = ""
    -- Signature
    sector = sector .. okffs.signature
    -- Next free (block immediately after)
    sector = sector .. string.char(uint24ToBytes(1))
    -- Free list (empty)
    sector = sector .. string.char(uint24ToBytes(0))
    -- Root (unallocated)
    sector = sector .. string.char(uint24ToBytes(0))
    -- Active block count
    sector = sector .. string.char(uint24ToBytes(0))

    sector = sector .. string.rep("\0", sectorSize - #sector)
    drive.writeSector(off+1, sector)
    return true
end

---@param block integer
---@return integer, integer
function okffs:getPlatterSectorPostition(block)
    local platter = math.floor(block / self.platterSize)
    local sector = block % self.platterSize
    return platter, sector
end

---@param platter integer
---@param sector integer
---@return integer
function okffs:computeBlockLocation(platter, sector)
    return platter * self.platterSize + sector
end

---@return integer
function okffs:allocBlockUntracked()
    if self.freeList == 0 then
        local block = self.nextFree
        if block == self.capacity then
            error("out of space")
        end
        self.nextFree = self.nextFree + 1
        self.activeBlockCount = self.activeBlockCount + 1
        return block
    end
    local block = self.freeList
    self.freeList = self:readUint24(block, 0)
    self.activeBlockCount = self.activeBlockCount + 1
    return block
end

---@return integer
function okffs:allocBlock()
    local block = self:allocBlockUntracked()
    self:saveState()
    return block
end

local NULL_BLOCK = 0

---@param block integer
function okffs:freeBlockUntracked(block)
    if block == 0 then return end -- Freeing NULL is fine
    self:writeUint24(block, 0, self.freeList)
    self.freeList = block
    self.activeBlockCount = self.activeBlockCount - 1
    self:saveState()
end

---@param block integer
function okffs:freeBlock(block)
    self:freeBlockUntracked(block)
    self:saveState()
end

---@param block integer
function okffs:freeBlockList(block)
    if block == 0 then return end -- Freeing NULL is fine
    while block ~= NULL_BLOCK do
        local next = self:readUint24(block, 0)
        self:freeBlockUntracked(block)
        block = next
    end
    self:saveState()
end

function okffs:allocDirectoryBlockUntracked()
    local block = self:allocBlockUntracked()
    self:writeUint24(block, 0, 0) -- Next
    self:writeUintN(block, 3, 0, 2) -- File count
    return block
end

function okffs:allocDirectoryBlock()
    local block = self:allocDirectoryBlockUntracked()
    self:saveState()
    return block
end

function okffs:allocFileBlockUntracked()
    local block = self:allocBlockUntracked()
    self:writeUint24(block, 0, 0) -- Next
    self:writeUintN(block, 3, 0, 2) -- Used bytes
    return block
end

function okffs:allocFileBlock()
    local block = self:allocFileBlockUntracked()
    self:saveState()
    return block
end

---@type {[KOCOS.FileType]: integer}
local typeMap = {
    directory = 0,
    file = 1,
    missing = 2,
}

---@type {[integer]: KOCOS.FileType}
local invTypeMap = {
    [0] = "directory",
    "file",
    "missing",
}

-- See structs
local DIR_ENTRY_SIZE = 64

---@class KOCOS.OKFFS.Entry
---@field name string
---@field type KOCOS.FileType
---@field blockList integer
---@field permissions integer
---@field mtimeMS integer
---@field dirEntryBlock integer
---@field dirEntryOff integer
---@field fileSize integer

---@return KOCOS.OKFFS.Entry
function okffs:getDirectoryEntry(dirBlock, off)
    local name = self:readSectorBytes(dirBlock, off, 32)
    local terminator = name:find("\0", nil, true)
    if terminator then
        name = name:sub(1, terminator-1)
    end

    local mtimeMS = self:readUintN(dirBlock, off+32, 8)
    local permissions = self:readUintN(dirBlock, off+40, 2)
    local ftype = self:readUintN(dirBlock, off+42, 1)
    local blockList = self:readUint24(dirBlock, off+43)
    local fileSize = self:readUintN(dirBlock, off+46, 4)

    ---@type KOCOS.OKFFS.Entry
    return {
        name = name,
        type = invTypeMap[ftype],
        blockList = blockList,
        permissions = permissions,
        mtimeMS = mtimeMS,
        dirEntryBlock = dirBlock,
        dirEntryOff = off,
        fileSize = fileSize,
    }
end

---@param entry KOCOS.OKFFS.Entry
---@return string
function okffs:encodeDirectoryEntry(entry)
    assert(#entry.name <= 32, "name too big")
    assert(#entry.name > 0, "missing name")
    assert(not string.find(entry.name, "[/%z\\]"), "invalid name")
    local data = ""
    data = data .. entry.name .. string.rep("\0", 32 - #entry.name)
    data = data .. uintNToBytes(entry.mtimeMS, 8)
    data = data .. uintNToBytes(entry.permissions, 2)
    data = data .. string.char(typeMap[entry.type])
    data = data .. string.char(uint24ToBytes(entry.blockList))
    data = data .. uintNToBytes(entry.fileSize, 4)
    data = data .. string.rep("\0", 14)
    return data
end

---@param entry KOCOS.OKFFS.Entry
function okffs:saveDirectoryEntry(entry)
    if entry.dirEntryBlock == NULL_BLOCK then return end
    self:writeSectorBytes(entry.dirEntryBlock, entry.dirEntryOff, self:encodeDirectoryEntry(entry))
end

---@param dirBlock integer
---@param name string
---@return KOCOS.OKFFS.Entry?
function okffs:queryDirectoryEntry(dirBlock, name)
    while true do
        if dirBlock == NULL_BLOCK then return end
        local next = self:readUint24(dirBlock, 0)
        local len = self:readUintN(dirBlock, 3, 2)
        for i=1,len do
            local off = i * DIR_ENTRY_SIZE
            local entry = self:getDirectoryEntry(dirBlock, off)
            if entry.name == name then
                return entry
            end
        end
        dirBlock = next
    end
end

---@param dirBlock integer
---@param name string
function okffs:removeDirectoryEntry(dirBlock, name)
    local entry = self:queryDirectoryEntry(dirBlock, name)
    -- nothing to delete
    if not entry then return end
    self:freeBlockList(entry.blockList)
    -- If it works it works
    self:writeUintN(entry.dirEntryBlock, entry.dirEntryOff, 0, DIR_ENTRY_SIZE)
end

---@Param dirBlock integer
---@param entry KOCOS.OKFFS.Entry
--- Mutates entry to store its new position
function okffs:addDirectoryEntry(dirBlock, entry)
    local maxLen = (self.sectorSize / DIR_ENTRY_SIZE) - 1
    while true do
        local next = self:readUint24(dirBlock, 0)
        local len = self:readUintN(dirBlock, 3, 2)
        if len < maxLen then
            len = len + 1
            self:writeUintN(dirBlock, 3, len, 2)
            entry.dirEntryBlock = dirBlock
            entry.dirEntryOff = len * DIR_ENTRY_SIZE
            self:saveDirectoryEntry(entry)
            return
        else
            for i=1,len do
                local found = self:getDirectoryEntry(dirBlock, i * DIR_ENTRY_SIZE)
                if found.name == "" then
                    -- Actually, valid space
                    entry.dirEntryBlock = dirBlock
                    entry.dirEntryOff = i * DIR_ENTRY_SIZE
                    self:saveDirectoryEntry(entry)
                    return
                end
            end
            if next == NULL_BLOCK then
                next = self:allocDirectoryBlock()
                self:writeUint24(dirBlock, 0, next)
            end
            dirBlock = next
        end
    end
end

---@param dirBlock integer
---@return string[]
function okffs:listDirectoryEntries(dirBlock)
    ---@type string[]
    local arr = {}
    while true do
        if dirBlock == NULL_BLOCK then break end
        local next = self:readUint24(dirBlock, 0)
        local len = self:readUintN(dirBlock, 3, 2)
        for i=1,len do
            local off = i * DIR_ENTRY_SIZE
            local entry = self:getDirectoryEntry(dirBlock, off)
            if entry.name ~= "" then
                if entry.type == "directory" then
                    table.insert(arr, entry.name .. "/")
                else
                    table.insert(arr, entry.name)
                end
            end
        end
        dirBlock = next
    end
    return arr
end

function okffs:ensureRoot()
    if self.root == NULL_BLOCK then
        self.root = self:allocDirectoryBlockUntracked()
        self:saveState()
    end
end

---@return KOCOS.OKFFS.Entry?
function okffs:entryOf(path)
    self:ensureRoot()
    ---@type KOCOS.OKFFS.Entry
    local entry = {
        name = "/",
        dirEntryBlock = NULL_BLOCK,
        dirEntryOff = 0,
        permissions = 2^16-1,
        type = "directory",
        blockList = self.root,
        mtimeMS = 0,
        fileSize = 0,
    }
    ---@type string[]
    local parts = string.split(path, "/")
    for i=#parts,1,-1 do
        if parts[i] == "" then table.remove(parts, i) end
    end
    while #parts > 0 do
        -- bad path
        if entry.type ~= "directory" then return nil end
        local name = table.remove(parts, 1)
        local subentry = self:queryDirectoryEntry(entry.blockList, name)
        if not subentry then return nil end
        entry = subentry
    end
    return entry
end

function okffs:spaceUsed()
    -- +1 cuz the header takes up 1 block
    return (self.activeBlockCount + 1) * self.sectorSize
end

function okffs:spaceTotal()
    return self.capacity * self.sectorSize
end

function okffs:isReadOnly(path)
    return self.readonly
end

function okffs:getPartition()
    return self.partition
end

---@param path string
function okffs:parentOf(path)
    if path:sub(1, 1) == "/" then path = path:sub(2) end
    path = path:reverse()
    local l = path:find("/", nil, true)
    if l then return path:sub(l+1):reverse() end
    return ""
end

---@param path string
function okffs:nameOf(path)
    if path:sub(1, 1) == "/" then path = path:sub(2) end
    path = path:reverse()
    local l = path:find("/", nil, true)
    if l then path = path:sub(1, l-1) end
    return path:reverse()
end

---@param parent string
---@param entry KOCOS.OKFFS.Entry
---@return boolean, string
function okffs:mkentry(parent, entry)
    self:ensureRoot()
    local parentEntry = self:entryOf(parent)
    if not parentEntry then return false, "missing parent" end
    if parentEntry.type ~= "directory" then return false, "bad parent" end
    if parentEntry.blockList == NULL_BLOCK then
        parentEntry.blockList = self:allocDirectoryBlock()
        self:saveDirectoryEntry(parentEntry)
    end
    if self:queryDirectoryEntry(parentEntry.blockList, entry.name) then return false, "duplicate" end
    ---@type KOCOS.OKFFS.Entry
    self:addDirectoryEntry(parentEntry.blockList, entry)
    return true, ""
end

function okffs:mkdir(path, permissions)
    local parent = self:parentOf(path)
    local name = self:nameOf(path)
    return self:mkentry(parent, {
        name = name,
        permissions = permissions,
        blockList = NULL_BLOCK,
        mtimeMS = 0,
        type = "directory",
        dirEntryBlock = 0,
        dirEntryOff = 0,
        fileSize = 0,
    })
end

function okffs:touch(path, permissions)
    local parent = self:parentOf(path)
    local name = self:nameOf(path)
    return self:mkentry(parent, {
        name = name,
        permissions = permissions,
        blockList = NULL_BLOCK,
        mtimeMS = 0,
        type = "file",
        dirEntryBlock = 0,
        dirEntryOff = 0,
        fileSize = 0,
    })
end

function okffs:permissionsOf(path)
    local entry = self:entryOf(path)
    if entry then return entry.permissions end
    return 2^16-1
end

function okffs:sizeOfBlockList(block)
    local n = 0
    while true do
        if block == NULL_BLOCK then break end
        n = n + 1
        block = self:readUint24(block, 0)
    end
    return n * self.sectorSize
end

function okffs:size(path)
    local entry = self:entryOf(path)
    if entry then return entry.fileSize end
    return 0
end

function okffs:type(path)
    local entry = self:entryOf(path)
    if entry then return entry.type end
    return "missing"
end

function okffs:list(path)
    local entry = self:entryOf(path)
    if not entry then return nil, "missing" end
    if entry.type ~= "directory" then return nil, "not directory" end
    return self:listDirectoryEntries(entry.blockList)
end

function okffs:remove(path)
    if self.fileStates[path] then
        return false, "in use"
    end
    local parent = self:parentOf(path)
    local name = self:nameOf(path)
    local parentEntry = self:entryOf(parent)
    if not parentEntry then return false, "missing" end
    if parentEntry.type ~= "directory" then return false, "bad path" end
    local entry = self:queryDirectoryEntry(parentEntry.blockList, name)
    if not entry then return false, "missing" end
    if entry.type == "directory" then
        if #self:listDirectoryEntries(entry.blockList) > 0 then return false, "not empty" end
    end
    self:removeDirectoryEntry(parentEntry.blockList, name)
    return true
end

---@return KOCOS.OKFFS.FileState?, string
function okffs:getFileState(path)
    if self.fileStates[path] then
        local state = self.fileStates[path]
        state.rc = state.rc + 1
        return state, ""
    end
    local entry = self:entryOf(path)
    if not entry then return nil, "missing" end
    if entry.type ~= "file" then return nil, "not file" end
    ---@type KOCOS.OKFFS.FileState
    local state = {
        path = path,
        entry = entry,
        rc = 1,
        lastModification = math.huge,
    }
    return state, ""
end

---@param handle KOCOS.OKFFS.Handle
function okffs:recordModification(handle)
    handle.state.lastModification = handle.state.lastModification + 1
    -- we are the last modification
    handle.syncedWith = handle.state.lastModification
end

function okffs:open(path, mode)
    local state, err = self:getFileState(path)
    if not state then return nil, err end
    ---@type KOCOS.OKFFS.Handle
    local handle = {
        state = state,
        pos = 0,
        mode = mode,
        -- Default cache stuff
        curBlock = NULL_BLOCK,
        curOff = 0,
        lastPosition = 0,
        syncedWith = state.lastModification,
    }
    self:clearHandleCache(handle)
    local fd = #self.handles
    while self.handles[fd] do fd = fd + 1 end
    self.handles[fd] = handle
    if mode == "w" then
        local entry = handle.state.entry
        self:freeBlockList(entry.blockList)
        entry.fileSize = 0
        entry.blockList = NULL_BLOCK
        self:saveDirectoryEntry(entry)

        -- we did a modification!!!!
        self:recordModification(handle)
    end
    return fd
end

---@param handle KOCOS.OKFFS.Handle
function okffs:clearHandleCache(handle)
    handle.curBlock = NULL_BLOCK
end

---@param handle KOCOS.OKFFS.Handle
function okffs:ensureSync(handle)
    if handle.syncedWith ~= handle.state.lastModification then
        -- Other handle fucked over our cache
        self:clearHandleCache(handle)
        handle.syncedWith = handle.state.lastModification
    end
end

---@param handle KOCOS.OKFFS.Handle
---@param position integer
function okffs:computeSeek(handle, position)
    self:ensureSync(handle)
    local curBlock = handle.curBlock
    local curOff = handle.curOff
    if curBlock == NULL_BLOCK then
        curBlock = handle.state.entry.blockList
        curOff = 0 -- just in case
        handle.lastPosition = 0
    end

    curOff = curOff + position - handle.lastPosition
    if position < handle.lastPosition then
        curBlock = handle.state.entry.blockList
        curOff = position
    end

    while curOff > 0 do
        local next = self:readUint24(curBlock, 0)
        local len = self:readUintN(curBlock, 3, 2)
        -- Being past the last byte of the last block is a condition write expects
        -- Read accounts for it
        if curOff <= len then break end
        curOff = curOff - len
        curBlock = next
    end

    handle.curBlock = curBlock
    handle.curOff = curOff
    handle.lastPosition = position
    handle.syncedWith = handle.state.lastModification
end

---@param handle KOCOS.OKFFS.Handle
function okffs:getSeekBlockAndOffset(handle)
    self:ensureSync(handle)
    if handle.curBlock == NULL_BLOCK or handle.lastPosition ~= handle.pos then
        self:computeSeek(handle, handle.pos)
    end
    return handle.curBlock, handle.curOff
end

---@param fileBlockList integer
function okffs:getByteLength(fileBlockList)
    local size = 0
    local block = fileBlockList
    while true do
        if block == NULL_BLOCK then break end
        local next = self:readUint24(block, 0)
        local len = self:readUintN(block, 3, 2)
        size = size + len
        block = next
    end
    return size
end

---@param handle KOCOS.OKFFS.Handle
function okffs:getHandleSize(handle)
    return handle.state.entry.fileSize
end

---@param amount integer
function okffs:recommendedMemoryFor(amount)
    return self:spaceNeededFor(amount) * 3 -- internal copies lol
end

---@param amount integer
function okffs:spaceNeededFor(amount)
    local dataPerBlock = self.sectorSize - 5 -- block prefix is next (3 bytes) + len (2 bytes)
    local blocksNeeded = math.ceil(amount / dataPerBlock)
    return blocksNeeded * self.sectorSize
end

function okffs:appendToBlockList(curBlock, data)
    local dataPerSector = self.sectorSize - 5
    while #data > 0 do
        local next = self:readUint24(curBlock, 0)
        local len = self:readUintN(curBlock, 3, 2)
        local remaining = dataPerSector - len

        local chunk = ""
        if #data <= remaining then
            -- Fast case
            chunk = data
            data = ""
        else
            -- Slow case
            chunk = data:sub(1, remaining)
            data = data:sub(remaining+1)
        end
        local old = self:readSectorBytes(curBlock, 5, len)
        len = len + #chunk
        local bin = uintNToBytes(next, 3) .. uintNToBytes(len, 2) .. old .. chunk
        bin = self:padToSector(bin)
        self:lowLevelWriteSector(curBlock, bin)

        if #data == 0 then break end

        local afterwards = next
        next = self:allocFileBlockUntracked()
        self:writeUint24(next, 0, afterwards)
        self:writeUint24(curBlock, 0, next)

        curBlock = next
    end
    self:saveState()
end

---@param curBlock integer
---@param curOff integer
---@param count integer
function okffs:readFileBlockList(curBlock, curOff, count)
    local data = ""
    while #data < count do
        if curBlock == NULL_BLOCK then break end
        local next = self:readUint24(curBlock, 0)
        local len = self:readUintN(curBlock, 3, 2)
        if curOff < len then
            data = data .. self:readSectorBytes(curBlock, 5 + curOff, len - curOff)
        end
        curBlock = next
        curOff = 0
    end
    if #data > count then data = data:sub(1, count) end
    return data
end

function okffs:overwriteBlockList(curBlock, off, data)
    error("unimplemented")
    local dataPerSector = self.sectorSize - 5
    while #data > 0 do
    end
    self:saveState()
end

---@param fd integer
---@param data string
function okffs:write(fd, data)
    local handle = self.handles[fd]
    if not handle then return false, "bad file descriptor" end
    if handle.mode == "r" then return false, "bad file descriptor" end
    if computer.freeMemory() < self:recommendedMemoryFor(#data) then
        -- inconvenient OOMs may lead to corrupted data
        return false, "dangerously low ram"
    end
    -- once defragmenting is done, we could try to defragment here
    if self:spaceTotal() - self:spaceUsed() < self:spaceNeededFor(#data) then
        -- a failing block allocation during a write could corrupt data
        return false, "out of space"
    end
    self:ensureSync(handle)
    local entry = handle.state.entry
    if entry.blockList == NULL_BLOCK then
        entry.blockList = self:allocFileBlock()
        self:saveDirectoryEntry(entry)
        self:recordModification(handle)
    end
    -- we gonna modify it
    local curBlock, curOff = self:getSeekBlockAndOffset(handle)
    if handle.mode == "a" then
        local len = self:readUintN(curBlock, 3, 2)
        if curOff < len then
            -- when they're equal, we insert right after
            -- when they're not, we need to append
            local extradata = self:readSectorBytes(curBlock, 5 + curOff, len - curOff)
            data = data .. extradata -- we do it here in case of OOM
            self:writeUintN(curBlock, 3, curOff, 2) -- make them equal
        end
        self:appendToBlockList(curBlock, data)
        handle.state.entry.fileSize = handle.state.entry.fileSize + #data
        self:saveDirectoryEntry(handle.state.entry)
    elseif handle.mode == "w" then
        self:overwriteBlockList(curBlock, curOff, data)
        handle.state.entry.fileSize = math.max(handle.state.entry.fileSize, handle.pos + #data)
        self:saveDirectoryEntry(handle.state.entry)
    end
    handle.pos = handle.pos + #data
    self:recordModification(handle)
    return true
end


---@param fd integer
---@param len integer
function okffs:read(fd, len)
    -- TODO: make readFileBlockList support math.huge lengths
    if len == math.huge then len = 16384 end
    local handle = self.handles[fd]
    if not handle then return false, "bad file descriptor" end
    self:ensureSync(handle)
    local curBlock, curOff = self:getSeekBlockAndOffset(handle)
    local data = self:readFileBlockList(curBlock, curOff, len)
    handle.pos = handle.pos + #data
    if #data == 0 then return nil, nil end
    return data
end

---@param fd integer
---@param whence "set"|"cur"|"end"
---@param off integer
function okffs:seek(fd, whence, off)
    local handle = self.handles[fd]
    if not handle then return nil, "bad file descriptor" end
    local size = self:getHandleSize(handle)
    local pos = handle.pos
    if whence == "set" then
        pos = off
    elseif whence == "cur" then
        pos = pos + off
    elseif whence == "end" then
        pos = size - off
    end
    handle.pos = math.max(0, math.min(pos, size))
    return handle.pos
end

function okffs:close(fd)
    local handle = self.handles[fd]
    if not handle then return false, "bad file descriptor" end
    self.handles[fd] = nil
    handle.state.rc = handle.state.rc - 1
    if handle.state.rc <= 0 then
        self.fileStates[handle.state.path] = nil
    end
    return true
end

KOCOS.fs.addDriver(okffs)

KOCOS.test("OKFFS driver", function()
    local drive = KOCOS.testing.drive(512, 16384)
    ---@type KOCOS.Partition
    local partition = {
        name = "Test partition",
        drive = drive,
        readonly = false,
        startByte = 0,
        byteSize = drive.getCapacity(),
        kind = "user",
        uuid = KOCOS.testing.uuid(),
        storedKind = KOCOS.testing.uuid(),
    }

    assert(okffs.create(partition) == nil)

    assert(okffs.format(partition, "okffs"))

    local manager = assert(okffs.create(partition), "formatting failed")

    assert(manager:spaceTotal() == drive.getCapacity())

    assert(manager.nextFree == 1, "invalid nextfree")
    assert(manager.freeList == 0, "free list should not be allocated")
    assert(manager.root == 0, "root should not be allocated")

    local a, b, c = manager:allocBlock(), manager:allocBlock(), manager:allocBlock()
    assert(manager.nextFree == 4, "block allocator broken nextfree")
    assert(a == 1 and b == 2 and c == 3, "block allocator is behaving unexpectedly")
    manager:freeBlock(b)
    assert(manager.activeBlockCount == 2, "incorrectly tracked active block count")
    local d = manager:allocBlock()
    assert(b == d, "block allocator is not reusing blocks")
    assert(manager.nextFree == 4, "block allocator is wasting space pointlessly")
    assert(manager.activeBlockCount == 3, "incorrectly tracked active block count")

    manager:freeBlock(a)
    manager:freeBlock(b)
    manager:freeBlock(d)

    assert(manager.activeBlockCount == 0, "incorrectly tracked active block count")

    -- Nuke old state
    okffs.format(partition, "okffs")
    manager = assert(okffs.create(partition), "formatting failed")

    local perms = math.random(0, 2^16-1)
    assert(manager:touch("test", perms))
    assert(manager:mkdir("data", perms))
    assert(manager:touch("data/stuff", perms))
    assert(manager:touch("data/other", perms))
    assert(manager:permissionsOf("test") == perms, "perms dont work")
    assert(manager:permissionsOf("data") == perms, "perms dont work")

    KOCOS.testing.expectSameSorted(assert(manager:list("")), {
        "test",
        "data/",
    })

    KOCOS.testing.expectSameSorted(assert(manager:list("data")), {
        "stuff",
        "other",
    })

    local spaceUsed = manager:spaceUsed()
    assert(manager:mkdir("spam", perms))
    for i=1,100 do
        -- SPAM THIS BITCH
        assert(manager:touch("spam/" .. i, perms))
        assert(manager:type("spam/" .. i) == "file")
    end
    -- should fail as it is non-empty
    KOCOS.testing.expectFail(assert, manager:remove("spam"))
    for i=1,100 do
        -- SPAM THIS BITCH
        assert(manager:remove("spam/" .. i))
        assert(manager:type("spam/" .. i) == "missing")
    end
    assert(manager:remove("spam"))
    assert(manager:spaceUsed() == spaceUsed, "space is getting leaked (" .. (spaceUsed - manager:spaceUsed()) .. " blocks)")

    local test = assert(manager:open("test", "a"))
    assert(manager:seek(test, "cur", 0) == 0, "bad initial position")

    local smallData = "Hello, world!"
    local bigData = ""
    do
        local bytes = {}
        for _=1,1024 do
            table.insert(bytes, math.random(0, 255))
        end
        bigData = string.char(table.unpack(bytes))
    end
    assert(manager:write(test, smallData))
    assert(manager:seek(test, "cur", 0) == #smallData, "bad current position")
    assert(manager:seek(test, "set", 0) == 0, "seeking is messing us up")
    assert(manager:read(test, #smallData) == smallData, "reading data back is wrong")
    assert(manager:seek(test, "set", 0))
    assert(manager:seek(test, "end", 0) == #smallData, "bad end position")
    assert(manager:write(test, smallData))
    assert(manager:seek(test, "end", 0) == #smallData * 2, "bad file size")
    assert(manager:read(test, math.huge) == nil, "reading does not EOF")
    assert(manager:seek(test, "set", 0))
    assert(manager:read(test, 2 * #smallData) == string.rep(smallData, 2), "bad appends")
    local smallFileData = string.rep(smallData, 2, "___")
    assert(manager:seek(test, "set", #smallData))
    assert(manager:write(test, "___"))
    assert(manager:seek(test, "set", 0))
    assert(manager:read(test, #smallFileData) == smallFileData, "bad inserts")
    assert(manager:seek(test, "set", 0))
    assert(manager:write(test, bigData))
    assert(manager:seek(test, "set", 0))
    assert(manager:read(test, #bigData + #smallFileData) == bigData .. smallFileData, "bad prepends from big data")

    assert(manager:close(test))
    assert(next(manager.handles) == nil, "handles leaked")
    assert(next(manager.fileStates) == nil, "file states leaked")
end)

KOCOS.defer(function()
    if not KOCOS.fs.exists("/tmp") then return end
    -- 2^18 means 256KiB
    local drive = KOCOS.testing.drive(512, 2^18, "OKFFS tmp drive")
    ---@type KOCOS.Partition
    local partition = {
        name = "OKFFS mount",
        drive = drive,
        kind = "user",
        startByte = 0,
        byteSize = drive.getCapacity(),
        readonly = false,
        storedKind = KOCOS.testing.uuid(),
        uuid = KOCOS.testing.uuid(),
    }
    okffs.format(partition, "okffs")
    KOCOS.fs.mount("/tmp", partition)
    KOCOS.log("Mounted OKFFS tmp")
end, 1)

KOCOS.log("OKFFS driver loaded")

end
do
-- Internet network system

---@class KOCOS.InternetDriver
---@field internet table
---@field subprotocol "http"|"tcp"
---@field connection any
local internet = {}
internet.__index = internet

---@param protocol "internet"
---@param subprotocol "http"|"tcp"
---@param options any
---@param process KOCOS.Process
function internet.create(protocol, subprotocol, options, process)
    if protocol ~= "internet" then return end
    if subprotocol ~= "http" and subprotocol ~= "tcp" then return end
    local modem = component.internet
    if not modem then return nil, "offline" end
    if subprotocol == "tcp" and not modem.isTcpEnabled() then
        return nil, "disabled"
    end
    if subprotocol == "http" and not modem.isHttpEnabled() then
        return nil, "disabled"
    end
    ---@type KOCOS.InternetDriver
    local manager = setmetatable({
        internet = modem,
        subprotocol = subprotocol,
        connection = nil,
    }, internet)
    return manager
end

function internet:connect(socket, address, options)
    assert(type(address) == "string", "bad address")
    options = options or {}

    if self.subprotocol == "tcp" then
        self.connection = self.internet.connect(address, options.port)
    else
        self.connection = self.internet.request(address, options.postData, options.headers)
    end
    self.connection.finishConnect()
end

function internet:read(socket, len)
    return self.connection.read(len ~= math.huge and len or nil)
end

function internet:write(socket, data)
    if self.subprotocol == "http" then
        error("unsupported")
    end

    return self.connection.write(data)
end

function internet:ioctl(socket, action, ...)
    if self.subprotocol == "http" then
        if action == "response" then
            return self.connection.response()
        end
    end
    error("unsupported")
end

function internet:close(socket)
    if self.connection then
        self.connection.close()
    end
end

-- TODO: Async internet I/O

function internet:async_connect()
    error("unsupported")
end

function internet:async_write()
    error("unsupported")
end

function internet:async_read()
    error("unsupported")
end

-- No way to run an internet server

function internet:accept()
    error("unsupported")
end

function internet:listen(socket, options)
    error("unsupported")
end

KOCOS.network.addDriver(internet.create)

KOCOS.log("Internet driver loaded")

end
do
-- Domain sockets

---@class KOCOS.Domain.Connection
---@field id integer
---@field serverEvents KOCOS.EventSystem
---@field clientEvents KOCOS.EventSystem

---@class KOCOS.Domain.Server
---@field connections {[integer]: KOCOS.Domain.Connection}
---@field pending KOCOS.Domain.Connection[]
---@field maxPending integer
---@field globalServerEvents KOCOS.EventSystem
---@field nextConnection integer

---@type {[any]: KOCOS.Domain.Server}
local servers = {}

---@class KOCOS.Domain.Driver
---@field kind "none"|"server"|"client"|"serverConnection"
---@field address string
---@field id integer
---@field server string
local domain = {}
domain.__index = domain

---@return KOCOS.Domain.Driver
function domain.blank()
    return setmetatable({
        kind = "none",
        address = "",
        id = 0,
        server = "",
    }, domain)
end

---@param protocol "domain"
---@param subprotocol "channel"
---@param options any
---@param process KOCOS.Process
function domain.create(protocol, subprotocol, options, process)
    if protocol ~= "domain" then return end
    if subprotocol ~= "channel" then return end
    return domain.blank()
end

---@param socket KOCOS.NetworkSocket
---@param options {port: any, maxPending: integer?}
function domain:listen(socket, options)
    assert(type(options) == "table", "missing necessary config")
    local port = options.port
    local maxPending = options.maxPending or 128
    assert(type(maxPending) == "number", "bad maxPending")
    assert(not servers[port], "port already in use")
    self.server = port
    servers[port] = {
        connections = {},
        pending = {},
        maxPending = maxPending,
        globalServerEvents = socket.events,
        nextConnection = 0,
    }
end

---@param socket KOCOS.NetworkSocket
---@param address any
function domain:async_connect(socket, address, options)
    local server = servers[address]
    assert(server, "no domain server")
    self.server = address
    table.insert(server.pending, {
        id = 0,
        serverEvents = KOCOS.event.create(KOCOS.maxEventBacklog),
        clientEvents = socket.events,
    })
    server.globalServerEvents.push(KOCOS.network.EVENT_CONNECT_REQUEST)
    while #server.pending > server.maxPending do
        ---@type KOCOS.Domain.Connection
        local c = table.remove(server.pending, 1)
        c.clientEvents.push(KOCOS.network.EVENT_CONNECT_RESPONSE, 0, false, "timed out")
    end
    return ""
end

---@param socket KOCOS.NetworkSocket
---@param address any
---@param options any
function domain:connect(socket, address, options)
    if not socket.events.queued(KOCOS.network.EVENT_CONNECT_RESPONSE) then
        self:async_connect(socket, address, options)
    end
    while true do
        local e, id, ok, err = socket.events.pop(KOCOS.network.EVENT_CONNECT_RESPONSE)
        if e == KOCOS.network.EVENT_CONNECT_RESPONSE then
            assert(ok, err)
            self.id = id
            self.kind = "client"
            return
        end
        KOCOS.yield()
    end
end

---@param socket KOCOS.NetworkSocket
---@return KOCOS.NetworkSocket
function domain:accept(socket)
    while true do
        local server = servers[self.server]
        if #server.pending > 0 then
            ---@type KOCOS.Domain.Connection
            local c = table.remove(server.pending, 1)
            socket.events.pop(KOCOS.network.EVENT_CONNECT_REQUEST)
            local id = server.nextConnection
            server.nextConnection = id + 1
            local dom = domain.blank()
            dom.kind = "serverConnection"
            dom.id = id
            dom.server = self.server
            c.id = id
            server.connections[id] = c
            ---@type KOCOS.NetworkSocket
            local sock = {
                protocol = socket.protocol,
                subprotocol = socket.subprotocol,
                state = "connected",
                manager = dom,
                process = socket.process,
                events = c.serverEvents,
            }
            c.clientEvents.push(KOCOS.network.EVENT_CONNECT_RESPONSE, c.id, true)
            return sock
        end
        KOCOS.yield()
    end
end

---@param socket KOCOS.NetworkSocket
---@param data string
function domain:async_write(socket, data)
    local server = servers[self.server]
    if not server then
        socket.events.push(KOCOS.network.EVENT_WRITE_RESPONSE, "", false, "connection closed")
        return ""
    end
    local conn = server.connections[self.id]
    if not conn then
        socket.events.push(KOCOS.network.EVENT_WRITE_RESPONSE, "", false, "connection closed")
        return ""
    end
    if self.kind == "client" then
        -- Tell server we wrote data
        conn.serverEvents.push(KOCOS.network.EVENT_READ_RESPONSE, "", data)
    else
        -- Tell client we wrote data
        conn.clientEvents.push(KOCOS.network.EVENT_READ_RESPONSE, "", data)
    end
    socket.events.push(KOCOS.network.EVENT_WRITE_RESPONSE, "", true)
    KOCOS.yield()
    return ""
end

---@param socket KOCOS.NetworkSocket
---@param data string
--- Writes are instant
function domain:write(socket, data)
    self:async_write(socket, data)
    local _, _, ok, err = socket.events.pop(KOCOS.network.EVENT_WRITE_RESPONSE)
    assert(ok, err)
end

-- Most complex function there is
function domain:async_read(socket, len)
    return ""
end

---@param socket KOCOS.NetworkSocket
---@param len integer
function domain:read(socket, len)
    while true do
        if socket.events.queued(KOCOS.network.EVENT_CLOSE_RESPONSE) then
            return nil
        end
        local e, _, data, err = socket.events.pop(KOCOS.network.EVENT_READ_RESPONSE)
        if e == KOCOS.network.EVENT_READ_RESPONSE then
            if err then error(err) end
            return data
        end
        KOCOS.yield()
    end
end

---@param socket KOCOS.NetworkSocket
function domain:close(socket)
    if self.kind == "server" then
        servers[self.server] = nil
    elseif self.kind == "client" then
        local server = servers[self.server]
        if not server then return end
        local conn = server.connections[self.id]
        if not conn then return end
        conn.serverEvents.push(KOCOS.network.EVENT_CLOSE_RESPONSE)
        socket.events.push(KOCOS.network.EVENT_CLOSE_RESPONSE)
        server.connections[self.id] = nil
    elseif self.kind == "serverConnection" then
        local server = servers[self.server]
        if not server then return end
        local conn = server.connections[self.id]
        if not conn then return end
        socket.events.push(KOCOS.network.EVENT_CLOSE_RESPONSE)
        conn.clientEvents.push(KOCOS.network.EVENT_CLOSE_RESPONSE)
        server.connections[self.id] = nil
    end
end

---@param socket KOCOS.NetworkSocket
function domain:ioctl(socket)
    error("unsupported")
end

KOCOS.network.addDriver(domain.create)

KOCOS.log("Domain socket driver loaded")

end
do
-- We have prelude.lua, now we have postlude.lua

if not KOCOS_CONFIG.needsExtensions then
    KOCOS.loop()
end

end
