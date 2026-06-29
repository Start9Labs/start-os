local ffi = require('ffi')

ffi.cdef [[
const char* ffi_select(const char *json_str, const char *path);
void *ffi_path_compile(const char *path);
const char* ffi_select_with_compiled_path(void *ptr, const char *json_str);
]]

local jsonpath
local cache = {}
local module = {}

local function existsVaiable(var)
    for k, _ in pairs(_G) do
        if k == var then
            return true
        end
    end
end

local _ngx
if existsVaiable('ngx') then
    _ngx = ngx
else
    _ngx = {}
    _ngx.log = function(level, msg)
        print('['..level..'] ' .. msg)
    end
end

function module.compile(path)
    assert(jsonpath, '"libjsonpath_lib" is not loaded')

    if(cache[path] == nil) then
        cache[path] = jsonpath.ffi_path_compile(path)
        _ngx.log(_ngx.INFO, 'compile : [' .. path .. ']')
    end
end

function module.exec(path)
    local compiledPath = cache[path]

    if(cache[path] == nil) then
        assert(jsonpath, path .. ": is not compiled")
    end

    return function(jsonStr)
        local result = jsonpath.ffi_select_with_compiled_path(compiledPath, jsonStr)
        return ffi.string(result);
    end
end

function module.init(path)
    if jsonpath == nil then
        jsonpath = ffi.load(path)
        _ngx.log(_ngx.INFO, '"' .. path .. '" initialized')
    end
end

return module