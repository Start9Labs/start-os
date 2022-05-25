//@ts-check
// @ts-ignore
import Deno from "/deno_global.js";
// @ts-ignore
import * as mainModule from "/embassy.js";
/**
 * This is using the simplified json pointer spec, using no escapes and arrays
 * @param {object} obj
 * @param {string} pointer
 * @returns
 */
function jsonPointerValue(obj, pointer) {
  const paths = pointer.substring(1).split("/");
  for (const path of paths) {
    if (obj == null) {
      return null;
    }
    obj = (obj || {})[path];
  }
  return obj;
}

// @ts-ignore
const context = Deno.core.opSync("get_context");
// @ts-ignore
const writeFile = ({ path, volumeId, toWrite }) => Deno.core.opAsync("write_file", context, volumeId, path, toWrite);

// @ts-ignore
const readFile = ({ volumeId, path }) => Deno.core.opAsync("read_file", context, volumeId, path);
// @ts-ignore
const removeFile = ({ volumeId, path }) => Deno.core.opAsync("remove_file", context, volumeId, path);
// @ts-ignore
const isSandboxed = () => Deno.core.opSync("is_sandboxed");

// @ts-ignore
const writeJsonFile = ({ volumeId, path, toWrite }) =>
  writeFile({
    volumeId,
    path,
    toWrite: JSON.stringify(toWrite),
  });
// @ts-ignore
const readJsonFile = async ({ volumeId, path }) => JSON.parse(await readFile({ volumeId, path }));
// @ts-ignore
const createDir = ({ volumeId, path }) => Deno.core.opAsync("create_dir", context, volumeId, path);
// @ts-ignore
const removeDir = ({ volumeId, path }) => Deno.core.opAsync("remove_dir", context, volumeId, path);
// @ts-ignore
const trace = (x) => Deno.core.opSync("log_trace", x);
// @ts-ignore
const warn = (x) => Deno.core.opSync("log_warn", x);
// @ts-ignore
const error = (x) => Deno.core.opSync("log_error", x);
// @ts-ignore
const debug = (x) => Deno.core.opSync("log_debug", x);
// @ts-ignore
const info = (x) => Deno.core.opSync("log_info", x);

// @ts-ignore
const currentFunction = Deno.core.opSync("current_function");
//@ts-ignore
const input = Deno.core.opSync("get_input");
// @ts-ignore
const setState = (x) => Deno.core.opSync("set_value", x);
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
  removeDir,
};

const runFunction = jsonPointerValue(mainModule, currentFunction);
(async () => {
  if (typeof runFunction !== "function") {
    error(`Expecting ${ currentFunction } to be a function`);
    throw new Error(`Expecting ${ currentFunction } to be a function`);
  }
  const answer = await runFunction(effects, input);
  setState(answer);
})();
