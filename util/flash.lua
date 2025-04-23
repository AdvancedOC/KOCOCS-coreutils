local path
-- force will make it flash now and do nothing more. No fancy installer.
local opts_force = false

for _, a in ipairs(arg) do
  if a == "-f" then
    opts_force = true
  else
    path = a
  end
end

local file

if path then
  file = assert(io.open(file))
else
  file = io.stdin
  opts_force = true
end

local code = file:read("a")
if not code then
  error("Unable to read stream")
end

if not opts_force then
  print("Please insert EEPROM you wish to overwrite.")
  print("Press enter to flash")
  io.read("l")
end

local eeprom = assert(component.eeprom, "EEPROM not found")

assert(eeprom.set(code))

if not opts_force then
  print("Label (empty to leave changed)")
  local label = io.read("l")
  if label and #label > 0 then
    assert(eeprom.setLabel(label))
  end
end

return 0
