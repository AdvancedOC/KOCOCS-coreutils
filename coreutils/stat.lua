local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function stat(path)
    local err, info = syscall("stat", path)
    return info, err
end

local function main(argv)
    for i=1,#argv do
        local info = assert(stat(assert(io.resolved(argv[i]))))

        write(0, argv[i] .. "\n");
        write(0, "\tType: " .. info.type .. "\n");
        write(0, "\tSize: " .. info.size .. "\n");
        write(0, "\tUsed: " .. info.used .. "\n");
        write(0, "\tTotal: " .. info.total .. "\n");
        write(0, "\tLast Modified: " .. os.date("%x %X", info.mtime) .. "\n");
        write(0, "\tPartition: " .. info.partition .. "\n");
        write(0, "\tPartition Name: " .. info.deviceName .. "\n");
        write(0, "\tDrive Type: " .. info.driveType .. "\n");
        write(0, "\tDrive Name: " .. info.driveName .. "\n");
        write(0, "\tIs Mount: " .. tostring(info.isMount) .. "\n");
    end
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    os.exit(1);
end
os.exit(err or 0);