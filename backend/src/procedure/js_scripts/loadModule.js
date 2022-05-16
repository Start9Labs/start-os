//@ts-check
// @ts-ignore
import * as mainModule from "/embassy.js";

// @ts-ignore
const writeFile = ({path, volumeId, toWrite}) => Deno.core.opSync('write_file', volumeId, path, toWrite)

// @ts-ignore
const readFile = ({volumeId, path}) => Deno.core.opSync('read_file', volumeId, path)
// @ts-ignore
const removeFile = ({volumeId, path}) => Deno.core.opSync('remove_file', volumeId, path)
// @ts-ignore
const isSandboxed = () => Deno.core.opSync('is_sandboxed')

// @ts-ignore
const writeJsonFile = ({volumeId, path, toWrite}) => Deno.core.opSync('write_file', volumeId, path, JSON.stringify(toWrite))
// @ts-ignore
const readJsonFile = ({volumeId, path}) => JSON.parse(Deno.core.opSync('read_file', volumeId, path))
// @ts-ignore
const println = (x) => Deno.core.opSync("println", x);

// @ts-ignore
const currentFunction = Deno.core.opSync("current_function")
// @ts-ignore
const setState = x => Deno.core.opSync("set_value", x)
const effects = {
    writeFile,
    readFile,
    writeJsonFile,
    readJsonFile,
    println,
    isSandboxed,
    removeFile,
}

const runFunction = mainModule[currentFunction]

if(typeof runFunction !== 'function') {{
    throw new Error(`Expecting ${{currentFunction}} to be a function`  );
}}
(async () => {
    const answer = await runFunction(effects)
    setState(answer);
})()
