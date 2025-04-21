local function write(fd, data)
    local err = syscall("write", fd, data)
    return err == nil, err
end

local function exit(status)
    -- WILL NEVER RETURN IF IT WORKED
    local err = syscall("exit", status)
    return err == nil, err
end

local function ftype(path)
    local err, x = syscall("ftype", path)
    return x, err
end

local function list(path)
    local err, x = syscall("list", path)
    return x, err
end

local function stat(path)
    local err, x = syscall("stat", path)
    return x, err
end

local function uinfo(user)
    local err, x = syscall("uinfo", user)
    return x, err
end

local function uginfo(group)
    local err, x = syscall("uginfo", group)
    return x, err
end

local function perms_decode(num)
    local userPerms = num % 256
    local groupPerms = math.floor(num / 256)

    local user = math.floor(userPerms / 4)
    local userRW = userPerms % 4

    local group = math.floor(groupPerms / 4)
    local groupRW = groupPerms % 4

    return user, userRW, group, groupRW
end

local function main(argv)
    local function do_thing(path, arg_l, arg_a)
        local stuff = assert(list(path));
        table.insert(stuff, 1, "../");
        table.insert(stuff, 1, "./");
        for i = 1, #stuff do
            if string.startswith(stuff[i], ".") and not arg_a then
                goto continue
            end

            local type = assert(ftype(stuff[i]));
            if arg_l then
                local info = assert(stat(stuff[i]));
                local inos = 1;
                if type == "directory" then
                    local contents = assert(list(stuff[i]));
                    inos = 2 + #contents;
                end
                local user, userRW, group, groupRW = perms_decode(info.perms);
                if user == 0 then
                    user = "root";
                elseif user == 63 then
                    user = "all"
                else
                    user = assert(uinfo(user));
                end
                if group == 0 then
                    group = "root";
                elseif group == 63 then
                    group = "all"
                else
                    group = assert(uginfo(group));
                end

                local perms = ""
                if type == "directory" then
                    perms = perms .. "d";
                else
                    perms = perms .. "-";
                end
                if userRW == 0 then
                    perms = perms .. "--";
                elseif userRW == 1 then
                    perms = perms .. "r-";
                elseif userRW == 2 then
                    perms = perms .. "-w";
                else
                    perms = perms .. "rw";
                end
                if groupRW == 0 then
                    perms = perms .. "--";
                elseif groupRW == 1 then
                    perms = perms .. "r-";
                elseif groupRW == 2 then
                    perms = perms .. "-w";
                else
                    perms = perms .. "rw";
                end

                -- TODO: Sometimes ls shows year instead of time
                write(0, string.format("%s %d %s %s %d %s ", perms, inos, user, group, info.size, os.date("%b %d %X", info.mtime)));
            end

            if type == "directory" then
                write(0, "\x1b[34m");
            end
            write(0, stuff[i] .. "\x1b[0m");

            if arg_l then
                write(0, "\n");
            else
                write(0, " ");
            end

            ::continue::
        end

        if not arg_l then
            write(0, "\n");
        end
    end

    local arg_l = false;
    local arg_a = false;

    for i = 1, #argv do
        local thing = argv[i];
        if string.startswith(thing, "-") and #thing > 1 then
            thing = string.sub(table.remove(argv, i), 2);
            while #thing > 0 do
                local char = thing:sub(1, 1);
                if char == "l" then
                    arg_l = true;
                elseif char == "a" then
                    arg_a = true;
                elseif char == "h" then
                    write(2, string.format("ls: h is unsupported\n"));
                else
                    write(2, string.format("ls: invalid option -- '%s'\nTry 'ls --help' for more information\n", char));
                    return 1;
                end
                thing = string.sub(thing, 2);
            end
        end
    end

    if #argv == 0 then
        argv[1] = "/"; -- TODO: Cwd
    end

    if #argv == 1 then
        local type = assert(ftype(argv[1]));
        if type ~= "file" then
            do_thing(argv[1], arg_l, arg_a);
        end
    else
        for i = 1, #argv do
            local type = assert(ftype(argv[i]));
            if type ~= "file" then
                write(0, argv[i] .. ":\n");
                do_thing(argv[i], arg_l, arg_a);
            end
        end
    end
end

local ok, err = xpcall(main, debug.traceback, arg);
if not ok then
    write(2, err .. "\n");
    assert(exit(1));
end
assert(exit(err or 0));