use std::fmt::Display;

use thiserror::Error;

use crate::lexer::Loc;

#[derive(Debug)]
pub(crate) struct Error {
    pub(crate) message: Option<String>,
    pub(crate) kind: ErrorKind,
    pub(crate) location: Loc,
}

pub(crate) type Result<T> = std::result::Result<T, Error>;

impl Error {
    pub(crate) fn new(kind: ErrorKind, location: Loc) -> Self {
        Self {
            message: None,
            kind,
            location,
        }
    }
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(message) = &self.message {
            write!(f, "[{}] {}: {}", self.location, self.kind, message)
        } else {
            write!(f, "[{}] {}", self.location, self.kind)
        }
    }
}

pub(crate) trait WithMessage {
    fn with_message(self, message: &str) -> Self;
    fn with_prefix(self, prefix: &str) -> Self;
}

impl<T> WithMessage for Result<T> {
    fn with_message(self, message: &str) -> Self {
        match self {
            Ok(val) => Ok(val),
            Err(mut err) => {
                err.message = Some(message.to_owned());
                Err(err)
            }
        }
    }

    fn with_prefix(self, prefix: &str) -> Self {
        match self {
            Ok(val) => Ok(val),
            Err(err) => Err(Error::new(
                ErrorKind::Inherited(format!("{}{}", prefix, err)),
                Loc::default(),
            )),
        }
    }
}

impl WithMessage for Error {
    fn with_message(mut self, message: &str) -> Self {
        self.message = Some(message.to_owned());
        self
    }

    fn with_prefix(mut self, prefix: &str) -> Self {
        if let Some(message) = self.message {
            self.message = Some(format!("{}{}", prefix, message));
        } else {
            self.message = Some(prefix.to_owned());
        }
        self
    }
}

pub(crate) trait IntoError<T> {
    fn runtime(self, kind: RuntimeError, location: Loc) -> Result<T>;
    fn parse(self, kind: ParseError, location: Loc) -> Result<T>;
    fn inherit(self, location: Loc) -> Result<T>;
}

impl<T, U: Display> IntoError<T> for std::result::Result<T, U> {
    fn runtime(self, kind: RuntimeError, location: Loc) -> Result<T> {
        self.map_err(|e| {
            Error::new(ErrorKind::RuntimeError(kind), location).with_message(&e.to_string())
        })
    }

    fn parse(self, kind: ParseError, location: Loc) -> Result<T> {
        self.map_err(|e| {
            Error::new(ErrorKind::ParseError(kind), location).with_message(&e.to_string())
        })
    }

    fn inherit(self, location: Loc) -> Result<T> {
        self.map_err(|e| Error::new(ErrorKind::Inherited(e.to_string()), location))
    }
}

#[derive(Debug, Error)]
pub(crate) enum ErrorKind {
    #[error("Parse error: {0}")]
    ParseError(ParseError),
    #[error("Runtime error: {0}")]
    RuntimeError(RuntimeError),
    #[error("{0}")]
    Inherited(String),
}

#[macro_export]
macro_rules! err {
    (Runtime $kind:expr, $loc:expr) => {
        Err(Error::new(ErrorKind::RuntimeError($kind), $loc))
    };
    (Parse $kind:expr, $loc:expr) => {
        Err(Error::new(ErrorKind::ParseError($kind), $loc))
    };
}

#[macro_export]
macro_rules! err_hl {
    ($val:expr) => {
        format!("{}", $val.red().bold())
    };
}

pub(crate) mod common {
    pub(crate) use super::Error;
    pub(crate) use super::ErrorKind;
    pub(crate) use super::IntoError;
    pub(crate) use super::Result;
    pub(crate) use super::WithMessage;
    pub(crate) use crossterm::style::Stylize;
}

#[derive(Debug, thiserror::Error)]
pub(crate) enum ParseError {
    #[error("Unexpected token {0}")]
    UnexpectedToken(String),
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("Invalid strategy {0}")]
    InvalidStrategy(String),
}

#[derive(Debug, thiserror::Error)]
pub(crate) enum RuntimeError {
    #[error("Invalid command {0}")]
    InvalidCommand(String),
    #[error("No rule named {0}")]
    RuleDoesNotExist(String),
    #[error("No shape defined")]
    NoShape,
    #[error("Already shaping")]
    AlreadyShaping,
    #[error("Nothing to undo")]
    NothingToUndo,
    #[error("Nothing to redo")]
    NothingToRedo,
    #[error("File error")]
    IOError,
    #[error("Apply error")]
    ApplyError,
    #[error("Eval error")]
    EvalError,
    #[error("Sub expression {0} does not exist.")]
    SubExprNotFound(usize),
}
