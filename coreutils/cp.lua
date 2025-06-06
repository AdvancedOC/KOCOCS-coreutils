-- Simplified cp for now
-- No recursion and only for files

local input = arg[1]
local output = arg[2]

if io.ftype(input) ~= "file" then
  eprintf("cp: %s is not file", input)
  os.exit(1)
end

if io.ftype(output) == "directory" then
  output = io.join(output, io.nameOf(input))
end

local inFile = io.open(input, "r")
local outFile = io.open(output, "w")

local data = inFile:read("a")
assert(outFile:write(data))

return 0
