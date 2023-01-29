use std::{io::stdout, str::Chars, string::Drain};

use crossterm::{
    cursor::{self, MoveDown, MoveTo, MoveToColumn, MoveToNextLine, MoveToPreviousLine, Show},
    event::{read, Event, KeyCode, KeyEvent},
    execute,
    style::Print,
    terminal::{disable_raw_mode, enable_raw_mode, Clear, ClearType, ScrollUp},
};
use linked_hash_map::LinkedHashMap;
use thiserror::Error;

use crate::{
    context::Context,
    lexer::Lexer,
    rule::{Expr, Rule},
};

pub struct Repl<'a> {
    prompt: String,
    input_buf: String,
    res: Vec<String>,
    history: Vec<String>,
    history_index: usize,
    insert: usize,
    quit: bool,
    state: Context<'a>,
}

struct ReplState {
    rules: LinkedHashMap<String, Rule>,
    shape: Option<Expr>,
}

#[derive(Debug, Error)]
#[error("Error: {0}")]
struct ReplError(String);

type ReplResult = anyhow::Result<Option<String>>;

static mut INPUT: String = String::new();

impl<'a> Repl<'a> {
    pub fn run() {
        let mut repl = Repl {
            prompt: std::env::var("PS1").unwrap_or_else(|_| ">> ".to_string()),
            input_buf: String::new(),
            res: Vec::new(),
            insert: 0,
            history: Vec::new(),
            history_index: 0,
            quit: false,
            state: Context::new(Lexer::from_iter("".chars().peekable())),
        };
        repl.run_loop();
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

        while !self.quit {
            // Use crossterm to print the latest <terminal height>
            let mut term_height = crossterm::terminal::size().unwrap().1;

            disable_raw_mode().unwrap();

            for line in self.res.drain(..) {
                execute!(stdout, MoveToColumn(0), Clear(ClearType::CurrentLine),).unwrap();
                println!("{}", line);
            }

            enable_raw_mode().unwrap();
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
                    self.quit = true;
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
                    self.insert = 0;
                    let input = self.input_buf.drain(..).collect();
                    match self.handle_input(input) {
                        Ok(Some(output)) => {
                            self.res = vec![
                                self.prompt.clone() + &self.history.last().unwrap(),
                                format!("  => {}", output),
                            ];
                        }
                        Ok(None) => (),
                        Err(err) => {
                            self.res = vec![
                                self.prompt.clone() + &self.history.last().unwrap(),
                                format!("  => Error: {}", err),
                            ];
                        }
                    }
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

    pub fn handle_input(&mut self, str: String) -> ReplResult {
        unsafe {
            INPUT = str;
        }
        let lexer = Lexer::from_iter(unsafe { INPUT.chars() }.peekable());
        let mut runner = self.state.rebuild(lexer);
        let out = runner.run_cmd()?;
        self.state = runner;
        return Ok(out);
    }
}
