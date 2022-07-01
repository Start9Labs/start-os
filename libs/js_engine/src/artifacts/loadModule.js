
import Deno from "/deno_global.js";
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

function maybeDate(value) {
  if (!value) return value;
  return new Date(value)
}
const writeFile = ({ path, volumeId, toWrite }) => Deno.core.opAsync("write_file",  volumeId, path, toWrite);

const readFile = ({ volumeId, path }) => Deno.core.opAsync("read_file",  volumeId, path);
const metadata = async ({ volumeId, path }) => {
  const data = await Deno.core.opAsync("metadata",  volumeId, path)
  return {
    ...data,
    modified: maybeDate(data.modified),
    created: maybeDate(data.created),
    accessed: maybeDate(data.accessed),
  }
};
const removeFile = ({ volumeId, path }) => Deno.core.opAsync("remove_file",  volumeId, path);
const isSandboxed = () => Deno.core.opSync("is_sandboxed");

const writeJsonFile = ({ volumeId, path, toWrite }) =>
  writeFile({
    volumeId,
    path,
    toWrite: JSON.stringify(toWrite),
  });
const readJsonFile = async ({ volumeId, path }) => JSON.parse(await readFile({ volumeId, path }));
const createDir = ({ volumeId, path }) => Deno.core.opAsync("create_dir",  volumeId, path);
const removeDir = ({ volumeId, path }) => Deno.core.opAsync("remove_dir",  volumeId, path);
const trace = (x) => Deno.core.opSync("log_trace", x);
const warn = (x) => Deno.core.opSync("log_warn", x);
const error = (x) => Deno.core.opSync("log_error", x);
const debug = (x) => Deno.core.opSync("log_debug", x);
const info = (x) => Deno.core.opSync("log_info", x);
const fetch = async (url, options = null) => {
  const {body, ...response} = await Deno.core.opAsync("fetch", url, options);
  const textValue = Promise.resolve(body)
  return {
    ...response,
    text() {
      return textValue
    },
    json() {
      return textValue.then(x => JSON.parse(x))
    }
  }
};


const currentFunction = Deno.core.opSync("current_function");
const input = Deno.core.opSync("get_input");
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
  fetch,
  removeFile,
  createDir,
  removeDir,
  metadata  
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
