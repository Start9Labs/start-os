pub fn mime(extension: &str) -> Option<&'static str> {
    match extension {
        "apng" => Some("image/apng"),
        "avif" => Some("image/avif"),
        "flif" => Some("image/flif"),
        "gif" => Some("image/gif"),
        "jpg" | "jpeg" | "jfif" | "pjpeg" | "pjp" => Some("image/jpeg"),
        "jxl" => Some("image/jxl"),
        "png" => Some("image/png"),
        "svg" => Some("image/svg+xml"),
        "webp" => Some("image/webp"),
        "mng" | "x-mng" => Some("image/x-mng"),
        "css" => Some("text/css"),
        "csv" => Some("text/csv"),
        "html" => Some("text/html"),
        "php" => Some("text/php"),
        "plain" | "md" | "txt" => Some("text/plain"),
        "xml" => Some("text/xml"),
        "js" => Some("text/javascript"),
        "wasm" => Some("application/wasm"),
        _ => None,
    }
}

pub fn unmime(mime: &str) -> Option<&'static str> {
    match mime {
        "image/apng" => Some("apng"),
        "image/avif" => Some("avif"),
        "image/flif" => Some("flif"),
        "image/gif" => Some("gif"),
        "jpg" | "jpeg" | "jfif" | "pjpeg" | "image/jpeg" => Some("pjp"),
        "image/jxl" => Some("jxl"),
        "image/png" => Some("png"),
        "image/svg+xml" => Some("svg"),
        "image/webp" => Some("webp"),
        "mng" | "image/x-mng" => Some("x-mng"),
        "text/css" => Some("css"),
        "text/csv" => Some("csv"),
        "text/html" => Some("html"),
        "text/php" => Some("php"),
        "plain" | "md" | "text/plain" => Some("txt"),
        "text/xml" => Some("xml"),
        "text/javascript" => Some("js"),
        "application/wasm" => Some("wasm"),
        _ => None,
    }
}
