use std::{
    collections::HashMap,
    io::{stdout, Write},
};

use crossterm::{
    cursor::{self, MoveDown, MoveToColumn, MoveUp, Show},
    event::{read, Event, KeyCode, KeyEvent},
    execute,
    style::{Print, StyledContent, Stylize},
    terminal::{disable_raw_mode, enable_raw_mode, Clear, ClearType},
};
use strip_ansi_escapes::strip;
use thiserror::Error;

use crate::{
    context::Context,
    lexer::{self, Lexer},
};

pub struct Repl {
    prompt: Vec<String>,
    input_buf: String,
    result_lines_buf: Vec<String>,
    command_history: Vec<String>,
    history_index: usize,
    insert: usize,
    quit: bool,
    context: Context,
}

#[derive(Debug, Error)]
#[error("Error: {0}")]
struct ReplError(String);

type ReplResult = anyhow::Result<Option<String>>;

static mut INPUT: String = String::new();

pub trait Highlight {
    fn highlight(&self) -> StyledContent<String>;
}

pub enum HighlightKind {
    Command,
    Comment,
}

impl Highlight for String {
    fn highlight(&self) -> StyledContent<String> {
        use HighlightKind::*;
        let mut highlights = HashMap::new();

        let mut start = 0;
        let mut curr = 0;
        let mut word = String::new();

        let mut chars = self.chars().enumerate();
        loop {
            match chars.next() {
                Some((idx, '#')) => {
                    if !word.is_empty() {
                        for command in lexer::COMMANDS {
                            if word.trim() == command {
                                let word = word.drain(..).collect::<String>();
                                highlights.insert(start, (Command, word));
                            }
                        }
                        word.clear();
                        start = idx + 1;
                    }
                    let mut comment = String::new();
                    comment.push('#');
                    loop {
                        match chars.next() {
                            Some((_, '\n')) => {
                                curr += 1;
                                comment.push('\n');
                                break;
                            }
                            Some((_, c)) => {
                                curr += 1;
                                comment.push(c);
                            }
                            None => {
                                break;
                            }
                        }
                    }
                    highlights.insert(start, (Comment, comment));
                    start = curr + 1;
                }
                Some((idx, c)) => {
                    if c.is_alphanumeric() {
                        word.push(c);
                    } else {
                        for command in lexer::COMMANDS {
                            if word.trim() == command {
                                let word = word.drain(..).collect::<String>();
                                highlights.insert(start, (Command, word));
                            }
                        }
                        word.clear();
                        start = idx + 1;
                    }
                }
                None => {
                    if !word.is_empty() {
                        for command in lexer::COMMANDS {
                            if word.trim() == command {
                                let word = word.drain(..).collect::<String>();
                                highlights.insert(start, (Command, word));
                            }
                        }
                    }
                    break;
                }
            }
        }

        let mut res = String::new();

        let mut chars = self.chars().enumerate();
        loop {
            let Some((idx, curr_char)) = &chars.next() else {
                break;
            };
            if let Some((kind, hl)) = highlights.get(&idx) {
                for _ in 0..hl.len() - 1 {
                    chars.next();
                }
                let hl = match kind {
                    Command => hl.clone().blue().to_string(),
                    Comment => hl.clone().yellow().to_string(),
                    //Num => hl.clone().yellow().to_string(),
                    //Str => hl.clone().red().to_string(),
                };
                res.push_str(&hl)
            } else {
                res.push(*curr_char);
            }
        }
        res.stylize()
    }
}

impl Repl {
    pub fn run() {
        let mut repl = Repl {
            prompt: std::env::var("NOQ")
                .map(|v| v.replace("\\[", ""))
                .map(|v| v.replace("\\]", ""))
                .map(|v| v.replace("\\033", "\x1b"))
                .map(|v| v.replace("\\e", "\x1b"))
                .map(|v| v.replace("\\n", "\n"))
                .map(|v| v.replace("\\r", "\r"))
                .map(|v| v.replace("\\u", "noq"))
                .map(|v| {
                    v.replace(
                        "\\h",
                        std::env::var("HOSTNAME")
                            .unwrap_or_else(|_| "hostname".to_string())
                            .as_str(),
                    )
                })
                .map(|v| v.replace("\\w", std::env::current_dir().unwrap().to_str().unwrap()))
                .map(|v| v.replace("\\$", "$"))
                //.map(|v| snailquote::unescape(&v).unwrap())
                .unwrap_or_else(|_| "noq >".to_string())
                .lines()
                .map(|s| s.to_string())
                .collect(),
            input_buf: String::new(),
            result_lines_buf: Vec::new(),
            insert: 0,
            command_history: Vec::new(),
            history_index: 0,
            quit: false,
            context: Context::new(),
        };
        repl.context.quiet = false;
        repl.run_loop();
    }

    fn prepare_prompt_line(&self, line: &String) -> String {
        let shape = self
            .context
            .show_shape()
            .unwrap_or("noq".to_string())
            .clone();
        line.replace("\\s", &shape)
    }

    fn run_loop(&mut self) {
        let mut stdout = stdout();
        execute!(stdout, MoveDown(1), Clear(ClearType::FromCursorDown)).unwrap();
        for (idx, line) in self.prompt.iter().enumerate() {
            if idx == self.prompt.len() - 1 {
                print!("{}", self.prepare_prompt_line(line));
                stdout.flush().unwrap();
            } else {
                println!("{}", self.prepare_prompt_line(line));
            }
        }
        enable_raw_mode().unwrap();

        while !self.quit {
            let mut _term_height = crossterm::terminal::size().unwrap().1;
            if self.context.quit {
                self.quit = true;
                continue;
            }

            let stripped = strip(self.prompt.last().unwrap().trim()).unwrap();
            let input_line = String::from_utf8_lossy(&stripped);
            let prompt_len = input_line.chars().count() as u16 + 1;
            execute!(
                stdout,
                MoveToColumn(prompt_len),
                Clear(ClearType::FromCursorDown),
                Print(&self.input_buf.highlight()),
                MoveToColumn(prompt_len + self.insert as u16),
                cursor::Show
            )
            .unwrap();

            match read().unwrap() {
                Event::Resize(_, y) => {
                    _term_height = y;
                }
                Event::Key(KeyEvent {
                    code: crossterm::event::KeyCode::Up,
                    modifiers: crossterm::event::KeyModifiers::NONE,
                    kind: crossterm::event::KeyEventKind::Press,
                    ..
                }) => {
                    if self.history_index < self.command_history.len() {
                        self.history_index += 1;
                        self.input_buf = self.command_history
                            [self.command_history.len() - self.history_index]
                            .clone();
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
                        self.input_buf = self.command_history
                            [self.command_history.len() - self.history_index]
                            .clone();
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
                    self.command_history.push(self.input_buf.clone());
                    self.insert = 0;
                    let input = self.input_buf.drain(..).collect();

                    match self.handle_input(input) {
                        Ok(Some(output)) => {
                            let mut res = vec![
                                self.prepare_prompt_line(&self.prompt.last().unwrap().clone())
                                    + " "
                                    + self.command_history.last().unwrap(),
                            ];
                            res.push(format!(
                                "{:width$}=> {}",
                                " ",
                                output,
                                width = prompt_len as usize
                            ));
                            self.result_lines_buf = res;
                        }
                        Ok(None) => {
                            self.result_lines_buf = vec![];
                        }
                        Err(err) => {
                            let mut res = vec![
                                self.prepare_prompt_line(&self.prompt.last().unwrap().clone())
                                    + " "
                                    + self.command_history.last().unwrap(),
                            ];
                            res.extend(
                                format!("{:width$}=> {}", " ", err, width = prompt_len as usize)
                                    .lines()
                                    .map(|s| s.to_string()),
                            );
                            self.result_lines_buf = res;
                        }
                    }
                    execute!(
                        stdout,
                        cursor::Hide,
                        MoveUp(self.prompt.len() as u16),
                        Clear(ClearType::FromCursorDown),
                    )
                    .unwrap();
                    disable_raw_mode().unwrap();
                    println!("");
                    for line in self.result_lines_buf.iter() {
                        println!("{}", line.highlight());
                        execute!(stdout, Clear(ClearType::FromCursorDown)).unwrap();
                    }
                    println!("");
                    for (idx, line) in self.prompt.iter().enumerate() {
                        if idx == self.prompt.len() - 1 {
                            print!("{}", self.prepare_prompt_line(&line.trim_end().to_owned()));
                            stdout.flush().unwrap();
                        } else {
                            println!("{}", self.prepare_prompt_line(&line.trim_end().to_owned()));
                        }
                    }
                    enable_raw_mode().unwrap();
                }
                _ => {}
            }
        }
        execute!(
            stdout,
            cursor::MoveUp((self.prompt.len()).checked_sub(1).unwrap_or(0) as u16),
            MoveToColumn(0),
            Clear(ClearType::FromCursorDown),
            Show
        )
        .unwrap();
        disable_raw_mode().unwrap();
    }

    pub fn handle_input(&mut self, str: String) -> ReplResult {
        unsafe {
            INPUT = str;
        }
        let mut lexer = Lexer::new(unsafe { INPUT.chars() }.peekable());
        let out = self.context.step(&mut lexer)?;
        return Ok(out);
    }
}
