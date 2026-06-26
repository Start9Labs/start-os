local jsonpath = require("jsonpath")

local iter;
if arg[1] == nil or arg[1] == '' then
    iter = 5000;
else
    iter = tonumber(arg[1]);
end

print(string.format("%s - %u", "lua iter", iter));

local file = io.open("../../benchmark/example.json", "r");
io.input(file)
local data = io.read("*a");
io.close(file);

jsonpath.init('../target/release/deps/libjsonpath_lib.so')
local template = jsonpath.compile("$..book[?(@.price<30 && @.category==\"fiction\")]");
for i = 0, iter do
    local r = template(data);
--    print(r);
end
