local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

write(0, "\x1b[2J");
os.exit(0);