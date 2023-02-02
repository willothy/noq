use std::{
    collections::HashMap,
    io::{stdout, Write},
};

use crossterm::{
    cursor::{self, MoveDown, MoveTo, MoveToColumn, MoveUp, Show},
    event::{read, Event, KeyCode, KeyEvent},
    execute, queue,
    style::{Color, Print, StyledContent, Stylize},
    terminal::{disable_raw_mode, enable_raw_mode, Clear, ClearType},
};
use strip_ansi_escapes::strip;
use thiserror::Error;

use crate::{
    error::Error,
    lexer::{self, Lexer},
    runtime::{InteractionResult, Runtime, StepResult, Verbosity},
};

pub(crate) struct Repl {
    prompt: Vec<String>,
    input_buf: String,
    result_lines_buf: Vec<String>,
    command_history: Vec<String>,
    history_index: usize,
    insert: usize,
    quit: bool,
    context: Runtime,
}

#[derive(Debug, Error)]
#[error("Error: {0}")]
struct ReplError(String);

static mut INPUT: String = String::new();

pub(crate) trait Highlight {
    fn highlight(&self) -> StyledContent<String>;
}

pub(crate) enum HighlightKind {
    Command,
    Comment,
    Path,
    Str,
    Num,
    Keyword,
    Strategy,
    Op,
    Invalid,
}

impl Highlight for String {
    fn highlight(&self) -> StyledContent<String> {
        use HighlightKind::*;
        let mut highlights = HashMap::new();

        let mut lexer = Lexer::new(self.chars().peekable());

        let mut invalidate = false;
        loop {
            if invalidate {
                while let Some(v) = lexer.next() {
                    highlights.insert(v.loc.offset, (Invalid, v.text));
                }
                break;
            } else {
                match lexer.next() {
                    Some(tok) => match tok.kind {
                        lexer::TokenKind::Comment => {
                            highlights.insert(tok.loc.offset, (Comment, tok.text));
                        }
                        lexer::TokenKind::Invalid => {
                            invalidate = true;
                            highlights.insert(tok.loc.offset, (Invalid, tok.text));
                        }
                        lexer::TokenKind::String => {
                            highlights.insert(tok.loc.offset, (Str, format!("\"{}\"", tok.text)));
                        }
                        lexer::TokenKind::UnclosedStr => {
                            invalidate = true;
                            highlights.insert(tok.loc.offset, (Invalid, format!("\"{}", tok.text)));
                        }
                        lexer::TokenKind::Path => {
                            highlights.insert(tok.loc.offset, (Path, tok.text));
                        }
                        lexer::TokenKind::Rules
                        | lexer::TokenKind::Commands
                        | lexer::TokenKind::Reverse
                        | lexer::TokenKind::Eval
                        | lexer::TokenKind::Bang => {
                            highlights.insert(tok.loc.offset, (Keyword, tok.text));
                        }
                        lexer::TokenKind::Op(_) => {
                            highlights.insert(tok.loc.offset, (Op, tok.text));
                        }
                        lexer::TokenKind::Command(_) => {
                            highlights.insert(tok.loc.offset, (Command, tok.text));
                        }
                        lexer::TokenKind::Strategy(_) => {
                            highlights.insert(tok.loc.offset, (Strategy, tok.text));
                        }
                        lexer::TokenKind::Number => {
                            highlights.insert(tok.loc.offset, (Num, tok.text));
                        }
                        _ => {}
                    },
                    None => break,
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
                if hl.len() > 0 {
                    for _ in 0..hl.len() - 1 {
                        chars.next();
                    }
                }

                let hl = match kind {
                    Command => format!("{}{}", hl.clone().blue(), "".reset()),
                    Comment => format!("{}{}", hl.clone().dark_magenta(), "".reset()),
                    Path => format!("{}{}", hl.clone().underlined(), "".reset()),
                    Str => format!("{}{}", hl.clone().green(), "".reset()),
                    Num => format!(
                        "{}{}",
                        hl.clone().blue().italic().with(Color::AnsiValue(172)),
                        "".reset()
                    ),
                    Keyword => format!("{}{}", hl.clone().magenta(), "".reset()),
                    Strategy => format!("{}{}", hl.clone().yellow(), "".reset()),
                    Op => format!("{}{}", hl.clone().reset(), "".reset()),
                    Invalid => {
                        format!("{}{}", hl.clone().dark_red().bold(), "".reset())
                    }
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
    pub(crate) fn run() {
        let mut repl = Repl {
            prompt: std::env::var("NOQ")
                .map(|v| v.replace("\\[", ""))
                .map(|v| v.replace("\\]", ""))
                .map(|v| v.replace("\\033", "\x1b"))
                .map(|v| v.replace("\\e", "\x1b"))
                .map(|v| v.replace("\\n", "\n"))
                .map(|v| v.replace("\\r", "\r"))
                .map(|v| v.replace("\\$", "$"))
                //.map(|v| snailquote::unescape(&v).unwrap())
                .unwrap_or_else(|_| "noq > ".to_string())
                .lines()
                .map(|s| s.to_string())
                .collect(),
            input_buf: String::new(),
            result_lines_buf: Vec::new(),
            insert: 0,
            command_history: Vec::new(),
            history_index: 0,
            quit: false,
            context: Runtime::new(),
        };
        repl.context.verbosity = Verbosity::Normal;
        repl.context.interaction_hook = Some(Box::new(Self::interaction_hook));
        repl.run_loop();
    }

    fn interaction_hook(mut trigger: Box<dyn crate::runtime::Interaction>) -> InteractionResult {
        let mut trigger_result = None;
        let trigger = trigger.as_mut();
        let mut stdout = std::io::stdout().lock();
        queue!(stdout, crossterm::terminal::EnterAlternateScreen).unwrap();
        crossterm::terminal::enable_raw_mode().unwrap();
        let mut curr_line = 0;

        while trigger_result.is_none() {
            let display = trigger.display();
            //let line_count = display.lines().count();

            // clear lines of element display
            queue!(
                stdout,
                cursor::Hide,
                MoveTo(0, 0),
                Clear(ClearType::FromCursorDown)
            )
            .unwrap();

            // print lines of element display via crossterm raw mode
            for line in display.lines() {
                queue!(
                    stdout,
                    MoveTo(0, curr_line),
                    Clear(ClearType::CurrentLine),
                    Print(line),
                )
                .unwrap();
                curr_line += 1;
            }
            curr_line = 0;

            stdout.flush().unwrap();
            trigger_result = trigger.on_event(read().unwrap());
        }
        trigger.on_complete(trigger_result.as_ref().unwrap());
        crossterm::terminal::disable_raw_mode().unwrap();
        queue!(
            stdout,
            crossterm::terminal::LeaveAlternateScreen,
            cursor::Show
        )
        .unwrap();
        stdout.flush().unwrap();
        trigger_result.unwrap()
    }

    fn prepare_prompt_line(&self, line: &String) -> String {
        let shape = self
            .context
            .show_shape()
            .unwrap_or("noq".to_string())
            .clone();
        let line = line.replace("\\u", "noq");
        let line = line.replace(
            "\\h",
            std::env::var("HOSTNAME")
                .unwrap_or_else(|_| "hostname".to_string())
                .as_str(),
        );
        let line = line.replace("\\w", std::env::current_dir().unwrap().to_str().unwrap());

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

                    let mut clear = false;

                    // Handle regular input result
                    match self.handle_input(input) {
                        Ok(StepResult {
                            results: Some(mut output),
                            cmd_for_each: cmd_per,
                            clear: do_clear,
                            ..
                        }) => {
                            clear = do_clear;
                            let mut res = vec![
                                self.prepare_prompt_line(&self.prompt.last().unwrap().clone())
                                    + &self.command_history.last().unwrap().highlight().to_string(),
                            ];
                            for mut output in output.drain(..) {
                                for (idx, line) in output.drain(..).enumerate() {
                                    if idx == 0 {
                                        res.push(format!(
                                            "{:width$}=> {}",
                                            " ",
                                            line,
                                            width = prompt_len as usize
                                        ));
                                    } else {
                                        if cmd_per {
                                            res.push(format!(
                                                "{:width$}=> {}",
                                                " ",
                                                line,
                                                width = prompt_len as usize
                                            ));
                                        } else {
                                            res.push(format!(
                                                "{:width$}   {}",
                                                " ",
                                                line,
                                                width = prompt_len as usize
                                            ));
                                        }
                                    }
                                }
                            }
                            self.result_lines_buf = res;
                        }
                        Ok(StepResult {
                            results: None,
                            clear: do_clear,
                            ..
                        }) => {
                            clear = do_clear;
                            self.result_lines_buf = vec![];
                        }
                        Err(err) => {
                            let mut res = vec![
                                self.prepare_prompt_line(&self.prompt.last().unwrap().clone())
                                    + &self.command_history.last().unwrap().highlight().to_string(),
                            ];
                            let err_col = err.location.col + prompt_len as usize;
                            res.push(format!("{:>err_col$}", "^"));
                            res.extend(
                                format!("{:width$}=> {}", " ", err, width = prompt_len as usize)
                                    .lines()
                                    .map(|s| s.to_string()),
                            );
                            self.result_lines_buf = res;
                        }
                    }
                    if clear {
                        disable_raw_mode().unwrap();
                        queue!(stdout, cursor::Hide, MoveTo(0, 0), Clear(ClearType::All)).unwrap();
                        for (idx, line) in self.prompt.iter().enumerate() {
                            if idx == self.prompt.len() - 1 {
                                print!("{}", self.prepare_prompt_line(&line.trim_end().to_owned()));
                                stdout.flush().unwrap();
                            } else {
                                println!(
                                    "{}",
                                    self.prepare_prompt_line(&line.trim_end().to_owned())
                                );
                            }
                        }
                        execute!(stdout, cursor::Show).unwrap();
                        enable_raw_mode().unwrap();
                    } else {
                        queue!(
                            stdout,
                            cursor::Hide,
                            MoveUp(self.prompt.len() as u16),
                            MoveToColumn(0),
                            Clear(ClearType::FromCursorDown),
                        )
                        .unwrap();
                        disable_raw_mode().unwrap();
                        if cursor::position().unwrap().1 > 0 {
                            println!("");
                        }
                        for line in self.result_lines_buf.iter() {
                            println!("{}", line);
                            queue!(stdout, Clear(ClearType::FromCursorDown)).unwrap();
                        }
                        println!("");
                        for (idx, line) in self.prompt.iter().enumerate() {
                            if idx == self.prompt.len() - 1 {
                                print!("{}", self.prepare_prompt_line(&line.trim_end().to_owned()));
                                stdout.flush().unwrap();
                            } else {
                                println!(
                                    "{}",
                                    self.prepare_prompt_line(&line.trim_end().to_owned())
                                );
                            }
                        }
                        enable_raw_mode().unwrap();
                    }
                }
                _ => {}
            }
        }
        queue!(
            stdout,
            cursor::MoveUp((self.prompt.len()).checked_sub(1).unwrap_or(0) as u16),
            MoveToColumn(0),
            Clear(ClearType::FromCursorDown),
            Show
        )
        .unwrap();
        disable_raw_mode().unwrap();
    }

    pub(crate) fn handle_input(&mut self, str: String) -> Result<StepResult, Error> {
        unsafe {
            INPUT = str;
        }
        let mut lexer = Lexer::new(unsafe { INPUT.chars() }.peekable());
        self.context.step(&mut lexer)
    }
}
