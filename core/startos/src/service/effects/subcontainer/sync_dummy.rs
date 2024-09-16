use crate::service::effects::prelude::*;
use crate::service::effects::ContainerCliContext;

pub fn launch(_: ContainerCliContext) -> Result<(), Error> {
    Err(Error::new(
        eyre!("requires feature container-runtime"),
        ErrorKind::InvalidRequest,
    ))
}

pub fn launch_init(_: ContainerCliContext) -> Result<(), Error> {
    Err(Error::new(
        eyre!("requires feature container-runtime"),
        ErrorKind::InvalidRequest,
    ))
}

pub fn exec(_: ContainerCliContext) -> Result<(), Error> {
    Err(Error::new(
        eyre!("requires feature container-runtime"),
        ErrorKind::InvalidRequest,
    ))
}

pub fn exec_command(_: ContainerCliContext) -> Result<(), Error> {
    Err(Error::new(
        eyre!("requires feature container-runtime"),
        ErrorKind::InvalidRequest,
    ))
}
