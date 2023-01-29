use std::{collections::HashMap, io::stdout};

use crossterm::{
    cursor::{self, MoveDown, MoveToColumn, Show},
    event::{read, Event, KeyCode, KeyEvent},
    execute,
    style::Print,
    terminal::{disable_raw_mode, enable_raw_mode, Clear, ClearType, ScrollDown, ScrollUp},
};
use thiserror::Error;

use crate::{
    lexer::Lexer,
    rule::{Expr, Rule},
};

pub struct Repl {
    prompt: String,
    input_buf: String,
    res: Vec<String>,
    history: Vec<String>,
    history_index: usize,
    state: ReplState,
    insert: usize,
}

struct ReplState {
    rules: HashMap<String, Rule>,
    shape: Option<Expr>,
    quit: bool,
}

#[derive(Debug, Error)]
#[error("Error: {0}")]
struct ReplError(String);

type ReplResult = anyhow::Result<Option<String>>;

impl Repl {
    pub fn run() {
        let mut repl = Repl {
            prompt: "> ".to_string(),
            input_buf: String::new(),
            res: Vec::new(),
            insert: 0,
            history: Vec::new(),
            history_index: 0,
            state: ReplState {
                rules: HashMap::new(),
                shape: None,
                quit: false,
            },
        };
        repl.run_loop();
    }

    fn panic(&mut self, _: String) -> ReplResult {
        panic!("REPL Explicit panic!");
        #[allow(unreachable_code)]
        Ok(None)
    }

    fn quit(&mut self, _: String) -> ReplResult {
        self.state.quit = true;
        Ok(None)
    }

    fn shape(&mut self, rest: String) -> ReplResult {
        if rest.trim().is_empty() {
            return Ok(if let Some(shape) = &self.state.shape {
                Some(format!("{}", shape))
            } else {
                Some(format!("No shape defined"))
            });
        }
        let shape = Expr::parse(Lexer::from(&*rest))?;
        self.state.shape = Some(shape.clone());
        Ok(Some(format!("{}", shape)))
    }

    fn rule(&mut self, rest: String) -> ReplResult {
        let Some(name) = rest.trim().split_whitespace().next() else {
            return Err(ReplError("Expected rule name and rule".into()).into());
        };
        let rule = Rule::parse(Lexer::from(&rest[name.len()..]))?;
        let res = format!("Added rule {}: {}", name, rule);
        self.state.rules.insert(name.to_owned(), rule);
        Ok(Some(res))
    }

    fn apply(&mut self, rest: String) -> ReplResult {
        let Some(rule_name) = rest.trim().split_whitespace().next() else {
            return Err(ReplError("Expected rule name and rule".into()).into());
        };
        let rule = if rule_name.trim() == "rule" {
            Rule::parse(Lexer::from(&rest[rule_name.len()..]))?
        } else {
            let Some(rule) = self.state.rules.get(rule_name) else {
                return Err(ReplError(format!("Unknown rule {}", rule_name)).into());
            };
            rule.clone()
        };

        let new_shape = rule.apply_all({
            let Some(shape) = &self.state.shape else {
                return Err(ReplError("No shape defined".into()).into());
            };
            shape
        })?;

        let new_shape_str = format!("{}", new_shape);
        self.state.shape = Some(new_shape);
        Ok(Some(new_shape_str))
    }

    fn run_loop(&mut self) {
        let mut stdout = stdout();
        enable_raw_mode().unwrap();
        execute!(
            stdout,
            MoveDown(1),
            Clear(ClearType::FromCursorDown),
            // Do thing
        )
        .unwrap();
        let mut cursor_start = cursor::position().unwrap().1;
        let mut cursor_offset_from_start = 0;

        while !self.state.quit {
            // Use crossterm to print the latest <terminal height>
            let mut term_height = crossterm::terminal::size().unwrap().1;

            for line in self.res.drain(..).take(term_height as usize - 1) {
                execute!(
                    stdout,
                    MoveToColumn(0),
                    Clear(ClearType::CurrentLine),
                    Print(line),
                    ScrollUp(1),
                )
                .unwrap();
            }

            execute!(
                stdout,
                MoveToColumn(0),
                Clear(ClearType::CurrentLine),
                Print(&(self.prompt.clone() + &self.input_buf)),
                MoveToColumn(self.prompt.len() as u16 + self.insert as u16),
                cursor::Show
            )
            .unwrap();

            match read().unwrap() {
                Event::Resize(_, y) => {
                    term_height = y;
                }
                Event::Key(KeyEvent {
                    code: crossterm::event::KeyCode::Up,
                    modifiers: crossterm::event::KeyModifiers::NONE,
                    kind: crossterm::event::KeyEventKind::Press,
                    ..
                }) => {
                    if self.history_index < self.history.len() {
                        self.history_index += 1;
                        self.input_buf =
                            self.history[self.history.len() - self.history_index].clone();
                        self.insert = self.input_buf.len();
                    }
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Down,
                    modifiers: crossterm::event::KeyModifiers::NONE,
                    kind: crossterm::event::KeyEventKind::Press,
                    ..
                }) => {
                    if self.history_index > 1 {
                        self.history_index -= 1;
                        self.input_buf =
                            self.history[self.history.len() - self.history_index].clone();
                        self.insert = self.input_buf.len();
                    } else {
                        self.input_buf.clear();
                        self.insert = 0;
                    }
                }
                Event::Key(KeyEvent {
                    code: code @ (KeyCode::Left | KeyCode::Right),
                    modifiers: crossterm::event::KeyModifiers::NONE,
                    kind: crossterm::event::KeyEventKind::Press,
                    ..
                }) => match code {
                    KeyCode::Left => {
                        self.insert.checked_sub(1).map(|idx| self.insert = idx);
                    }
                    KeyCode::Right => {
                        if self.insert < self.input_buf.len() {
                            self.insert += 1;
                        }
                    }
                    _ => (),
                },
                Event::Key(KeyEvent {
                    code: crossterm::event::KeyCode::Char('c'),
                    modifiers: crossterm::event::KeyModifiers::CONTROL,
                    ..
                }) => {
                    self.state.quit = true;
                }
                Event::Key(KeyEvent {
                    code: crossterm::event::KeyCode::Char(c),
                    ..
                }) => {
                    if self.insert == self.input_buf.len() {
                        self.input_buf.push(c);
                    } else {
                        self.input_buf.insert(self.insert, c);
                    }
                    self.insert += 1;
                }
                Event::Key(KeyEvent {
                    code: crossterm::event::KeyCode::Backspace,
                    ..
                }) => {
                    if self.insert == self.input_buf.len() {
                        self.input_buf.pop();
                    } else {
                        self.input_buf
                            .remove(self.insert.checked_sub(1).unwrap_or(0));
                    }
                    self.insert.checked_sub(1).map(|idx| self.insert = idx);
                }
                Event::Key(KeyEvent {
                    code: crossterm::event::KeyCode::Enter,
                    ..
                }) => {
                    self.history_index = 0;
                    if self.input_buf.trim().is_empty() {
                        continue;
                    }
                    self.history.push(self.input_buf.clone());
                    let input = self.input_buf.clone();

                    match self.handle_input() {
                        Ok(Some(output)) => {
                            self.res =
                                vec![self.prompt.clone() + &input, format!("  => {}", output)];
                        }
                        Ok(None) => (),
                        Err(err) => {
                            self.res =
                                vec![self.prompt.clone() + &input, format!("  => Error: {}", err)];
                        }
                    }
                    self.insert = 0;
                }
                _ => {}
            }
        }
        execute!(
            stdout,
            cursor::MoveDown(2),
            MoveToColumn(0),
            Print(""),
            Show
        )
        .unwrap();
        disable_raw_mode().unwrap();
    }

    fn handle_input(&mut self) -> ReplResult {
        let input = self.input_buf.drain(..).collect::<String>();

        let (cmd, args) = {
            let mut split = input.split_whitespace();
            let Some(cmd) = split.next() else {
                // No command given
                return Ok(None);
            };
            (cmd, split.collect::<Vec<&str>>().join(" "))
        };

        macro_rules! commands {
            (
                $(
                    $name:ident
                    $(
                        (
                            $($alias:ident),+
                        )
                    )?
                ),+
            ) => {
                match cmd {
                    $(
                        stringify!($name) $($(| stringify!($alias))+)? => self.$name(args),
                    )+
                    unknown => Err(
                        ReplError(
                            format!("Unknown command {}. Expected one of {}.", unknown, stringify!($($name$(($($alias),+))?),+))
                        ).into()
                    ),
                }
            }
        }

        commands!(shape, rule, apply, quit(q), panic)
    }
}
