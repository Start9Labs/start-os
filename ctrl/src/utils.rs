use clap::{ArgMatches, CommandFactory, FromArgMatches};
use imbl_value::{imbl::OrdMap, Value};
use rpc_toolkit::{CliBindings, Context, HandlerArgsFor, HandlerFor, HandlerTypes, PrintCliResult};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::{any::type_name, collections::VecDeque, io::stdout};

pub trait HandlerExtSerde<C: Context>: HandlerFor<C> {
    fn with_display_serializable(self) -> DisplaySerializable<Self>;
    /*fn with_stdin_deseriablizable(self) -> StdinDeserializable<Self>;*/
}

impl<T: HandlerFor<C>, C: Context> HandlerExtSerde<C> for T {
    fn with_display_serializable(self) -> DisplaySerializable<Self> {
        DisplaySerializable(self)
    }
    /*fn with_stdin_deseriablizable(self) -> StdinDeserializable<Self> {
        StdinDeserializable(self)
    }*/
}

#[derive(Debug, Clone)]
pub struct DisplaySerializable<T>(pub T);
impl<T: HandlerTypes> HandlerTypes for DisplaySerializable<T> {
    type Params = T::Params;
    type InheritedParams = T::InheritedParams;
    type Ok = T::Ok;
    type Err = T::Err;
}

impl<T: HandlerFor<C>, C: Context> HandlerFor<C> for DisplaySerializable<T> {
    fn handle_sync(&self, args: HandlerArgsFor<C, Self>) -> Result<Self::Ok, Self::Err> {
        self.0.handle_sync(args)
    }
    async fn handle_async(&self, args: HandlerArgsFor<C, Self>) -> Result<Self::Ok, Self::Err> {
        self.0.handle_async(args).await
    }
    fn metadata(&self, method: VecDeque<&'static str>) -> OrdMap<&'static str, imbl_value::Value> {
        self.0.metadata(method)
    }
    fn method_from_dots(&self, method: &str) -> Option<VecDeque<&'static str>> {
        self.0.method_from_dots(method)
    }
}
impl<T: HandlerTypes, C: Context> PrintCliResult<C> for DisplaySerializable<T>
where
    T::Ok: Serialize,
{
    fn print(&self, _: HandlerArgsFor<C, Self>, result: Self::Ok) -> Result<(), Self::Err> {
        use std::io::Write;
        let stdout = stdout();
        let mut stdout = stdout.lock();
        let _ = serde_json::to_writer_pretty(&mut stdout, &result);
        let _ = writeln!(stdout);
        Ok(())
    }
}
impl<C: Context, T: HandlerTypes> CliBindings<C> for DisplaySerializable<T>
where
    Self::Params: CommandFactory + FromArgMatches + Serialize,
    Self: PrintCliResult<C>,
{
    fn cli_command(&self) -> clap::Command {
        Self::Params::command()
    }
    fn cli_parse(
        &self,
        matches: &clap::ArgMatches,
    ) -> Result<(VecDeque<&'static str>, Value), clap::Error> {
        Self::Params::from_arg_matches(matches).and_then(|a| {
            Ok((
                VecDeque::new(),
                imbl_value::to_value(&a)
                    .map_err(|e| clap::Error::raw(clap::error::ErrorKind::ValueValidation, e))?,
            ))
        })
    }
    fn cli_display(
        &self,
        handle_args: HandlerArgsFor<C, Self>,
        result: Self::Ok,
    ) -> Result<(), Self::Err> {
        self.print(handle_args, result)
    }
}

/*
#[derive(Debug, Clone)]
pub struct StdinDeserializable<T>(pub T);
impl<T: HandlerTypes> HandlerTypes for StdinDeserializable<T> {
    type Params = DeserializeStdin<T::Params>;
    type InheritedParams = DeserializeStdin<T::InheritedParams>;
    type Ok = T::Ok;
    type Err = T::Err;
}

impl<T: HandlerFor<C>, C: Context> HandlerFor<C> for StdinDeserializable<T> {
    fn handle_sync(
        &self,
        HandlerArgs {
            context,
            parent_method,
            method,
            params,
            inherited_params,
            raw_params,
        }: HandlerArgsFor<C, Self>,
    ) -> Result<Self::Ok, Self::Err> {
        self.0.handle_sync(HandlerArgs {
            context,
            parent_method,
            method,
            params: params.0,
            inherited_params: inherited_params.0,
            raw_params,
        })
    }
    async fn handle_async(
        &self,
        HandlerArgs {
            context,
            parent_method,
            method,
            params,
            inherited_params,
            raw_params,
        }: HandlerArgsFor<C, Self>,
    ) -> Result<Self::Ok, Self::Err> {
        self.0
            .handle_async(HandlerArgs {
                context,
                parent_method,
                method,
                params: params.0,
                inherited_params: inherited_params.0,
                raw_params,
            })
            .await
    }
    fn metadata(&self, method: VecDeque<&'static str>) -> OrdMap<&'static str, imbl_value::Value> {
        self.0.metadata(method)
    }
    fn method_from_dots(&self, method: &str) -> Option<VecDeque<&'static str>> {
        self.0.method_from_dots(method)
    }
}
impl<T: HandlerTypes, C: Context> PrintCliResult<C> for StdinDeserializable<T>
where
    T: PrintCliResult<C>,
{
    fn print(
        &self,
        HandlerArgs {
            context,
            parent_method,
            method,
            params,
            inherited_params,
            raw_params,
        }: HandlerArgsFor<C, Self>,
        result: Self::Ok,
    ) -> Result<(), Self::Err> {
        PrintCliResult::print(
            &self.0,
            HandlerArgs {
                context,
                parent_method,
                method,
                params: params.0,
                inherited_params: inherited_params.0,
                raw_params,
            },
            result,
        )
    }
}
impl<C: Context, T: HandlerTypes> CliBindings<C> for StdinDeserializable<T>
where
    Self::Params: CommandFactory + FromArgMatches + Serialize,
    Self: PrintCliResult<C>,
{
    fn cli_command(&self) -> clap::Command {
        Self::Params::command()
    }
    fn cli_parse(
        &self,
        matches: &clap::ArgMatches,
    ) -> Result<(VecDeque<&'static str>, Value), clap::Error> {
        Self::Params::from_arg_matches(matches).and_then(|a| {
            Ok((
                VecDeque::new(),
                imbl_value::to_value(&a)
                    .map_err(|e| clap::Error::raw(clap::error::ErrorKind::ValueValidation, e))?,
            ))
        })
    }
    fn cli_display(
        &self,
        handle_args: HandlerArgsFor<C, Self>,
        result: Self::Ok,
    ) -> Result<(), Self::Err> {
        self.print(handle_args, result)
    }
}
*/

#[derive(Deserialize, Serialize, Clone)]
pub struct DeserializeStdin<T>(pub T);
impl<T> Default for DeserializeStdin<T>
where
    T: Default,
{
    fn default() -> Self {
        Self(T::default())
    }
}
impl<T> FromArgMatches for DeserializeStdin<T>
where
    T: DeserializeOwned,
{
    fn from_arg_matches(_: &ArgMatches) -> Result<Self, clap::Error> {
        Ok(Self(
            serde_json::from_reader(&mut std::io::stdin())
                .map_err(|e| clap::Error::raw(clap::error::ErrorKind::ValueValidation, e))?,
        ))
    }
    fn update_from_arg_matches(&mut self, _: &ArgMatches) -> Result<(), clap::Error> {
        self.0 = serde_json::from_reader(&mut std::io::stdin())
            .map_err(|e| clap::Error::raw(clap::error::ErrorKind::ValueValidation, e))?;
        Ok(())
    }
}
impl<T> CommandFactory for DeserializeStdin<T> {
    fn command() -> clap::Command {
        clap::Command::new(env!("CARGO_PKG_NAME"))
            .after_help(format!("will read {} from stdin", type_name::<T>()))
    }

    fn command_for_update() -> clap::Command {
        Self::command()
    }
}
