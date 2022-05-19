//@ts-check
// @ts-ignore
import * as mainModule from "/embassy.js";

// @ts-ignore
const oldDeno = Deno;
// @ts-ignore
const context = oldDeno.core.opSync('get_context')
// @ts-ignore
const writeFile = ({path, volumeId, toWrite}) => oldDeno.core.opAsync('write_file',context,  volumeId, path, toWrite)

// @ts-ignore
const readFile = ({volumeId, path}) => oldDeno.core.opAsync('read_file', context, volumeId, path)
// @ts-ignore
const removeFile = ({volumeId, path}) => oldDeno.core.opAsync('remove_file', context, volumeId, path)
// @ts-ignore
const isSandboxed = () => oldDeno.core.opSync('is_sandboxed')

// @ts-ignore
const writeJsonFile = ({volumeId, path, toWrite}) => oldDeno.core.opAsync('write_file',  context, volumeId, path, JSON.stringify(toWrite))
// @ts-ignore
const readJsonFile = ({volumeId, path}) => JSON.parse(oldDeno.core.opAsync('read_file',  context, volumeId, path))
// @ts-ignore
const createDir = ({volumeId, path}) => oldDeno.core.opAsync('create_dir',  context, volumeId, path)
// @ts-ignore
const removeDir = ({volumeId, path}) => oldDeno.core.opAsync('remove_dir', context,  volumeId, path)
// @ts-ignore
const trace = (x) => oldDeno.core.opSync("log_trace", x);
// @ts-ignore
const warn = (x) => oldDeno.core.opSync("log_warn", x);
// @ts-ignore
const error = (x) => oldDeno.core.opSync("log_error", x);
// @ts-ignore
const debug = (x) => oldDeno.core.opSync("log_debug", x);
// @ts-ignore
const info = (x) => oldDeno.core.opSync("log_info", x);

// @ts-ignore
const currentFunction = oldDeno.core.opSync("current_function")
//@ts-ignore
const input = oldDeno.core.opSync("get_input");
// @ts-ignore
const setState = x => oldDeno.core.opSync("set_value", x)
const effects = {
    writeFile,
    readFile,
    writeJsonFile,
    readJsonFile,
    error,
    warn,
    debug,
    trace,
    info,
    isSandboxed,
    removeFile,
    createDir,
    removeDir
}

const runFunction = mainModule[currentFunction]

if(typeof runFunction !== 'function') {{
    throw new Error(`Expecting ${{currentFunction}} to be a function`  );
}}
(async () => {
    // @ts-ignore
    Deno = undefined;
    const answer = await runFunction(effects, input)
    // @ts-ignore
    Deno = oldDeno
    setState(answer);
})()
