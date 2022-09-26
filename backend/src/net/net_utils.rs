use color_eyre::eyre::eyre;
use http::Request;
use hyper::Body;

use crate::Error;


pub fn host_addr(req: &Request<Body>) -> Result<String, Error> {
    let host = req.headers().get(http::header::HOST);

    match host {
        Some(host) => {
            let host = host
                .to_str()
                .map_err(|e| Error::new(eyre!("{}", e), crate::ErrorKind::AsciiError))?
                .to_string();

            Ok(host)
        }

        None => Err(Error::new(eyre!("No Host"), crate::ErrorKind::NoHost)),
    }
}
